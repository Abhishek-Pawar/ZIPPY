
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
   Function accepts a BinomialHeap which is a set of Huffman trees and returns
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
huffComp list = (tree, concatMap codelist list)
  where
      tree      = newHuffTree $ getInstances list []
      code     = getInstances tree
      codelist s = snd (head (dropWhile (\(e,c) -> e/=s) code))


{- |
  The function changes a byte list into another byte list , compressing it
  The last True is added in order to have an end of file character
  (All of the False found before the last True are ignored by the decoder)
 -}

huffCompress :: [Byte] -> [Byte]
huffCompress inputCode = bitsToBytes $ encodeHuff huffIp ++ list ++ [True]
    where
        (huffIp, list) = compress inputCode


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


{- |
 - Function accepts a symbol list and returns a list of tuples (symbol ,occurrence)
 -}
getInstances :: (Eq a) => [a] -> [(a,Integer)] -> [(a, Integer)]

getInstances [] acc = acc
getInstances (y:ys) acc = getInstances remain ((x, 1 + lp):acc)
    where
      remain = [b| b <- xs, b /= x]
      lp = sum [1| a <- xs, a == x]
