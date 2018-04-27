
module Comp_huff (
    -- ** Compression function
    huffCompress
)
where

import Huffman_shared
import BinomialHeap as BHeap


{- |
   Function accepts a list  consisting of  a set of (symbol ,ocurrence) pair,builds
   a Huffman tree representation
 -}
newHuffTree :: (Ord a) => [(a, Integer)] -> HuffTree a

newHuffTree list = newHuffTree' (newSet list)

{- |
   Function accepts a BinomialHeap which is a set of Huffman trees ,and returns
   a single Huffman tree consisting of the first values
 -}
newHuffTree' :: (Ord a) => BinomialHeap (HuffTree a) -> HuffTree a

newHuffTree' (Hp [(Node 0 t _)]) = t
newHuffTree' hp = newHuffTree' (BHeap.insert newBr heapExclude)
    where
        br1           = findMin hp
        br2           = findMin $ deleteMinimum hp
        newBr         = newBranch br1 br2
        heapExclude   = deleteMinimum $ deleteMinimum hp
