
{- |
   This module defines the necessary functionality for huffman compression
   Includes functions for compression alongwith some encode functions
 -}
module Comp_huff (
-- ** Compression Related function
    module Comp_huff
)
where

import Huffman_shared
import BinomialHeap as BHeap
import Data.List


{- |
   Function accepts a list  consisting of  a set of (symbol ,ocurrence) pair,builds
   a Huffman tree representation
 -}
newHuffTree :: (Ord a) => [(a, Integer)] -> HuffTree a

newHuffTree list = newHuffTree' (constructHeap list)

{- |
   Function accepts a BinomialHeap which is a set of Huffman trees and returns
   a single Huffman tree consisting of the first values
 -}
newHuffTree' :: (Ord a) => BinomialHeap (HuffTree a) -> HuffTree a

newHuffTree' (BH [(Node 0 t _)]) = t
newHuffTree' hp = newHuffTree' (BHeap.insert newBr heapExclude)
    where
        br1           = findMinimum hp
        br2           = findMinimum $ deleteMinimum hp
        newBr         = mergeHuffTrees br1 br2
        heapExclude   = deleteMinimum $ deleteMinimum hp

{- |
    Function which reads input.If it comes across a True, then it can be first True
    (either at the beginning of the code or after a False)
    then the next bit is part of the tree (represented inversely).Otherwise, it is considered as
    finalized the code. If a False is read, then the next 8 bits,
    correspond to a symbol.
 -}
encodeHuff :: HuffTree Byte -> [Bool]
encodeHuff h = (concatMap encodeHuff' assocs) ++ [True, False]
    where
        assocs = getTuples h
        encodeHuff' (e, cods)
            = (True : intersperse True (reverse cods)) ++ (False : byteToBits e)


{- |
 - The function accepts a list of symbols, return a tuple(elem ,binarylist).elem is the first of
   given symbols. binarylist is a list where symbols have been replaced by binary and concatenated
 -}

huffComp :: (Ord a) => [a] -> (HuffTree a, [Bool])
huffComp l = (tree, concatMap codelist l)
  where
      tree       = newHuffTree $ getInstances l []
      code       = getTuples tree
      codelist s = snd (head (dropWhile (\(e,c) -> e/=s) code))


{- |
  The function changes a byte list into another byte list , compressing it
  The last True is added in order to have an end of file character
  (All of the False found before the last True are ignored by the decoder)
 -}

huffCompress :: [Byte] -> [Byte]
huffCompress inputCode = bitsToBytes $ encodeHuff huffIp ++ list ++ [True]
    where
        (huffIp, list) = huffComp inputCode



{- |
 - Function accepts a symbol list and returns a list of tuples (symbol ,occurrence)
 -}
getInstances :: (Eq a) => [a] -> [(a,Integer)] -> [(a, Integer)]

getInstances [] acc = acc
getInstances (y:ys) acc = getInstances remain ((y, 1 + lp):acc)
    where
      remain = [b | b <- ys, b /= y]
      lp = sum [1 | a <- ys, a == y]
