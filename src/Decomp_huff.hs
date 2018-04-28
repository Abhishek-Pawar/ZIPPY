{- |
   This module defines the necessary functionality for HUFF decompression
   Includes functions for decompression alongwith some some decode functions
 -}
module Decomp_huff (
  -- ** Decompression Related function
    module Decomp_huff
)
where

import Huffman_shared
import BinomialHeap as BHeap
import Data.Word


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
huffDecomp a c = retSym a c

{- |
   Function transforms a byte list into another one,  decompressing it
 -}

huffDecompress :: [Byte] -> [Byte]

huffDecompress code = huffDecomp huffTr cods
    where
        (huffTr, cods) = decodeHuff . concat $ map byteToBits code
