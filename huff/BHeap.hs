{- |
 - Data Structure: Binomial Heap
 - For Huffman Compression
 -}

module BinomialHeap (
    -- * Class @BinomialHeap@
    module BinomialHeap
)
where

data BinomialTree a = Node Int a [BinomialTree a] deriving (Show, Read)

data BinomialHeap a = BH [BinomialTree a] deriving (Show, Read)

-- | Get the height of the binomial tree
height :: BinomialTree a -> Int

height (Node h root r) = h

-- | Get the root of the binomial tree
root :: BinomialTree a -> a

root (Node h root r) = root

{- | Function that accepts two trees of height h
    | Returns a tree of height (h+1) 
    | containing same elements.
-}
linkTrees :: (Ord a) => BinomialTree a -> BinomialTree a -> BinomialTree a

linkTrees tree1@(Node h1 root1 r1) tree2@(Node h2 root2 r2)
    root1 <= root2 = Node (h1+1) root1 (tree2:r1)
    otherwise = Node (h1+1) root2 (tree1:r2)

{- | Function that accepts a list of trees and a tree
    | Inserts this tree at corresponding position in the list
    | Returns the modified list
-}
insertTree :: (Ord a) => BinomialTree a -> [BinomialTree a] -> [BinomialTree a]

insertTree tree [] = [t]        -- If the list of trees is empty (Base Case)
insertTree tree (t:ts) = if height tree < height t then tree:(t:ts) else insertTree (linkTrees tree t) ts

{- | Function to insert a Node instance in the @BinomialHeap@
-}
insertHeap :: (Ord a) => a -> BinomialHeap a -> BinomialHeap a

insertHeap x (BH ts) = BH (insertTree (Node 0 root []) ts)

{- | Function that accepts two @BinomialTree@ instances
    | Merges the two @BinomialTree@ and returns it
-}
mergeTrees :: (Ord a) => [BinomialTree a] -> [BinomialTree a] -> [BinomialTree a]

-- Base Cases

mergeTrees ts [] = ts
mergeTrees [] ts = ts

-- Recursive Definition

mergeTrees ts1@(t1:tr1) ts2@(t2:tr2)
    height t1 < height t2 = t1 : mergeTrees tr1 ts2
    height t2 < height t1 = t2 : mergeTrees ts1 tr2
    otherwise = insertTree (linkTrees t1 t2) (mergeTrees tr1 tr2)



{- | Auxillary function that accepts a @BinomialTree@ and removes the minimum.
    | Returns minimum and the remainder tree.
-}
deleteMinimumTree :: (Ord a) => [BinomialTree a] -> (BinomialTree a, [BinomialTree a])

-- Base Cases

deleteMinimumTree [] = error "Error (deleteMinimumTree) : Empty Heap!"
deleteMinimumTree [t] = (t,[])

-- Recursive Definition
deleteMinimumTree (t:ts) = if root t < root tRem then (t,ts) else (tRem, t:tsRem) where
    (tRem,tsRem) = deleteMinimumTree ts


{- | Function that accepts a @BinomialHeap@ and returns the minimum element.
-}
findMinimum :: (Ord a) => BinomialHeap a -> a

findMinimum (BH ts) = root t
    where (t,_) = deleteMinimumTree ts


{- | Function that accepts a @BinomialHeap@ and eliminates the minimum
-}
deleteMinimum :: (Ord a) => BinomialHeap a -> BinomialHeap a

deleteMinimum (BH ts) = BH (mergeTrees (reverse ts1) ts2) where
    (Node _ x ts1, ts2) = deleteMinimumTree ts