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
makeHuffTree :: [(a, [Bool])] -> Huffman a
makeHuffTree ((e,[True]):[]) = Leaf 0 e
makeHuffTree list            = Branch 0 (makeHuffTree left) (makeHuffTree right)
    where
        left =  [(e,xs)| (e,(False:xs)) <- list]
        right = [(e,xs)| (e,(True:xs))  <- list]

{- |
  Lee una codificacion de un Arbol de Huffman y reconstruye el
  Arbol del que fue generado, separando el resto de los datos binarios.
  (El Arbol de Huffman generado tendra frecuencias acumuladas triviales,
  iguales a cero).
 -}
decodeHuff :: [Bool] -> (Huffman Byte, [Bool])
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
huffDecomp :: Huffman a -> [Bool] -> [a]
huffDecomp a [] = []
huffDecomp a c  = fetchSym a c []
    where
        fetchSym (Leaf _ e)     []         acc = acc
        fetchSym (Leaf _ e)     (_:rest)   acc = acc ++ [e] ++ (huffDecomp a rest)
        fetchSym (Branch _ _ d) (True:xs)  acc = fetchSym d xs acc
        fetchSym (Branch _ i _) (False:xs) acc = fetchSym i xs acc



{- |
  Transforms a byte list into another one, compressing or decompressing it
  as specified (True is compress, False is decompress).
  When compressing, the last True is added in order to have an end of file character
  (All of the False found before the last True will be ignored by the decoder)
 -}

huffDecompress :: [Byte] -> [Byte]

huffDecompress code = huffDecomp huff cods
    where
        (huff, cods) = decodeHuff . concat $ map byteToBits code
