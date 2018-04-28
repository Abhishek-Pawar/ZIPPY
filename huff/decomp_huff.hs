module Decomp_huff (
    -- ** Compression function
    huffDecompress
)
where

import Huffman_shared
import BinomialHeap as BHeap

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
    Function reads the encoding of a Huffman Tree and reconstructs the
    Tree from which it was generated, separating the rest of the binary data.
 -}
decodeHuff :: [Bool] -> (HuffTree Byte, [Bool])
decodeHuff code = (huff, trim rem)
    where
        (attr, rem) = parseAssocsInit [] code
        huff           = makeHuffTree attr
        trim           = reverse . tail . dropWhile not . reverse
        parseAssocsInit accum (True:False:xs) = (accum, xs)
        parseAssocsInit accum (True:True:xs)  = parseAssocs accum [True] xs
        parseAssocs accum code (False:xs)      = parseAssocsInit ((head $ bitsToBytes e, code):accum) r
            where (e, r) = splitAt 8 xs
        parseAssocs accum code (True:x:xs)     = parseAssocs accum (x:code) xs


{- |
 - Given a Huffman tree and a list that represents a chain of bits,
   returns a list of symbols that corresponds to the decoding of said
   chain with the tree's information
 -}
huffDecomp :: HuffTree a -> [Bool] -> [a]
huffDecomp a [] = []
huffDecomp a c  = fetchSym a c []
    where
        fetchSym (Leaf _ e)     []         acc = acc
        fetchSym (Leaf _ e)     (_:rem)   acc = acc ++ [e] ++ (huffDecomp a rem)
        fetchSym (Branch _ _ d) (True:xs)  acc = fetchSym d xs acc
        fetchSym (Branch _ i _) (False:xs) acc = fetchSym i xs acc

{- |
   Function transforms a byte list into another one,  decompressing it
 -}

huffDecompress :: [Byte] -> [Byte]

huffDecompress code = huffDecomp huffTr cods
    where
        (huffTr, cods) = decodeHuff . concat $ map byteToBits code
