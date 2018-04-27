
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

{- |
   Function accepts Huffman Tree and outputs a list of tuples with entry as
   (Symbol ,Coding Binary)
 -}
getTuples :: (Ord a) => HuffTree a -> [(a, [Bool])]
getTuples a = getTuples' a [] []

getTuples' :: (Ord a) => HuffTree a -> [(a, [Bool])] -> [Bool] -> [(a, [Bool])]
getTuples' (Leaf _ elem)  comb accum = (elem, accum ++ [True]):comb
getTuples' (Branch _ i d) comb accum = lefts ++ rights
    where
        lefts  = getTuples' i comb (accum ++ [False])
        rights = getTuples' d comb (accum ++ [True])


{- |
 - Function accepts  a symbol list and returns a list of tuples (symbol ,occurrence)
 -}
getInstances :: (Eq a) => [a] -> [(a,Integer)] -> [(a, Integer)]
getInstances [] acc = acc
getInstances (y:ys) acc = getInstances remain ((x, 1 + lp):acc)
    where
        remain = [b| b <- xs, b /= x]
        lp = sum [1| a <- xs, a == x]
