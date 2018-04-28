{- |
 - This module defines the necessary functionality common to Huffman compression and decompression
   modules.
 -}
module Huffman_shared (
        -- * Type @Byte@
        Byte,
        -- * Type @HuffTree@
        HuffTree,
        -- ** Common Operations on Huffman Compression and Decompression
        readInputFile,
        printHufFile,
        byteToBits,
        bitsToBytes,
        frequency,
        mergeHuffTrees,
        constructHeap,
        makeHuffTree,
        retSym,
        getTuples
)

where

    import Data.Word
    import Data.List
    import qualified Data.ByteString as BS
    import BinomialHeap as BHeap


    {- |
        A Byte is an 8 bit word
     -}
    type Byte = Data.Word.Word8


    {- |
        Function that accepts a (relative) file path and returns the content
        of the file as a list of bytes (@Byte@)
    -}
    readInputFile :: String -> IO [Byte]

    readInputFile inputFile = BS.readFile inputFile >>= return . BS.unpack


    {- |
        Function that accepts a (relative) file path and a list of bytes (@Byte@)
        and prints the bytes in the input file.
    -}
    printHufFile :: String -> [Byte] -> IO()

    printHufFile inputFile = BS.writeFile inputFile . BS.pack

    {- |
        Function that accepts a list of booleans (bits) and outputs a list of bytes
    -}
    bitsToBytes :: [Bool] -> [Byte]

    bitsToBytes boolList = reverse $ bitsToBytesRev [] boolList 0 0 where

        -- Base Case
        bitsToBytesRev x [] xi 8 = xi:x

        -- Recursive definition
        bitsToBytesRev x [] xi n = bitsToBytesRev x [] (xi*2) (n+1)
        bitsToBytesRev x list xi 8 = bitsToBytesRev (xi:x) list 0 0
        bitsToBytesRev x (False:list) xi n = bitsToBytesRev x list (xi*2) (n+1)
        bitsToBytesRev x (True:list) xi n = bitsToBytesRev x list (xi*2+1) (n+1)


    {- |
        Function that accepts a byte (@Byte) and outputs the corresponding list of bits.
    -}
    byteToBits :: Byte -> [Bool]

    byteToBits = reverse . byteToBitsRev 8 where

        -- Base Case
        byteToBitsRev 0 _ = []

        -- Recursive Definition
        byteToBitsRev n x
            | even x = False : byteToBitsRev (n-1) (div x 2)
            | otherwise = True : byteToBitsRev (n-1) (div x 2)


    {- |
        Huffman Tree type definition
    -}

    data HuffTree a = Leaf Integer a
                    | Branch Integer (HuffTree a) (HuffTree a)
                    deriving (Show, Read)


    {- |
        Huffman Trees equivalence instance
    -}

    instance (Eq a) => Eq (HuffTree a) where

        (Leaf f e) == (Leaf f2 e2) = (f == f2) && (e == e2)

        (Branch freq i d) == (Branch freq2 i2 d2) = (freq == freq2) && (i == i2) && (d == d2)

        _ == _ = False

    {-|
        Huffman Trees ordering instance
    -}

    instance(Ord a) => Ord (HuffTree a) where

        Leaf f e <= Leaf f2 e2 = f <= f2

        Branch f i d <= Leaf f2 e = f <= f2

        Leaf f e <= Branch f2 i d = f <= f2

        Branch f i d <= Branch f2 i2 d2 = f <= f2


    {-|
        Function that accepts a Huffman tree and gets the accumulated frequency in the root of the tree.
    -}
    frequency :: HuffTree huf -> Integer

    frequency (Leaf f _) = f
    frequency (Branch f _ _) = f


    {-|
        Function that accepts two Huffman trees and returns a new tree with the original ones as children
        of this tree. The accumulated frequency of this tree is the sum of the frequencies of the two children.
    -}
    mergeHuffTrees :: HuffTree huf -> HuffTree huf -> HuffTree huf

    mergeHuffTrees l r = Branch (frequency l + frequency r) l r

    {-|
       Function that accepts a list of elements with its corresponding frequencies and constructs
       a Binomial Heap of associated Huffman trees.
    -}
    constructHeap :: (Ord a) => [(a, Integer)] -> BinomialHeap (HuffTree a)

    constructHeap = foldl add (BH []) where
        add acc (element,frequency) = BHeap.insert (Leaf frequency element) acc
        
    {-|
     Accepts a Huffman tree and a list that represents a chain of bits,
    returns a list of symbols that corresponds to the decoding of the tree's information
    It is called by huffDecompress
  -}
    retSym :: HuffTree a -> [Bool] -> [a]
    retSym a [] = []
    retSym a c  = fetchSym a c []
        where
            fetchSym (Leaf _ e) [] acc = acc
            fetchSym (Leaf _ e) (_:rem) acc = acc ++ [e] ++ (retSym a rem)
            fetchSym (Branch _ _ d) (True:xs) acc = fetchSym d xs acc
            fetchSym (Branch _ i _) (False:xs) acc = fetchSym i xs acc

    {- |
       Function whcih accepts  a list of tuples with a set of symbols, next totheir binary coding (False represents zero and True the one).
       It Builds a Huffman Tree such that these association they could have been generated.The accumulated frequency will be assigned to zero (0) for
       all the leaves and branches of it
     -}
    makeHuffTree :: [(a, [Bool])] -> HuffTree a
    makeHuffTree ((e,[True]):[]) = Leaf 0 e
    makeHuffTree list            = Branch 0 (makeHuffTree left) (makeHuffTree right)
        where
            left =  [(e,xs)| (e,(False:xs)) <- list]
            right = [(e,xs)| (e,(True:xs))  <- list]

    {- |
       Function accepts Huffman Tree and outputs a list of tuples with entry as
       (Symbol ,Coding Binary)
     -}

    getTuples :: (Ord a) => HuffTree a -> [(a, [Bool])]

    getTuples a = getTuples' a [] []


    getTuples' :: (Ord a) => HuffTree a -> [(a, [Bool])] -> [Bool] -> [(a, [Bool])]

    getTuples' (Leaf _ elem)  comb accum = (elem, accum ++ [True]):comb
    getTuples' (Branch _ i d) comb accum = left ++ right
        where
            left  = getTuples' i comb (accum ++ [False])
            right = getTuples' d comb (accum ++ [True])
