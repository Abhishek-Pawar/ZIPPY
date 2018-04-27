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
