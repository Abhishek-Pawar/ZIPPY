{- |
 -  Haskell adaptation of Dr. Okasaki's binomial heap functional implementation
 -
 -  Source:     Purely Functional Data Structures
 -              Chris Okasaki
 -              Cambridge University Press, 1998
 -}
module BinomialHeap (
    -- * Class @BinomialHeap@ 
    module BinomialHeap)
where

data BinomialTree a = Nodo Int a [BinomialTree a] deriving (Show, Read)

data BinomialHeap a = BH [BinomialTree a] deriving (Show, Read)

-- | Given a tree it obtains its height
orden :: BinomialTree a -> Int
orden (Nodo o e h) = o

-- | Given a tree it obtains its root
raiz :: BinomialTree a -> a
raiz (Nodo o e h) = e

{- | Given two trees of order r returns a tree of order r + 1
     with the same elements of the trees of order
-}
enlazar :: (Ord a) => BinomialTree a -> BinomialTree a -> BinomialTree a
enlazar t1@(Nodo r x1 c1) t2@(Nodo _ x2 c2)
    | x1 <= x2 = Nodo (r+1) x1 (t2:c1)
    | otherwise = Nodo (r+1) x2 (t1:c2)

{-| Given a tree and a list of trees it returns a new list with the tree
    linked in the corresponding position
-}
insTree :: (Ord a) => BinomialTree a -> [BinomialTree a] -> [BinomialTree a]
insTree t [] = [t]
insTree t (x:xs) = if orden t < orden x then t:(x:xs) else insTree (enlazar t x) xs

-- | Allows to insert a Node instance in the @BinomialHeap@
insert :: (Ord a) => a -> BinomialHeap a -> BinomialHeap a
insert x (BH ts) = BH (insTree (Nodo 0 x []) ts)

-- | Merges two @BinomialTree@ instances and returns one
mrg :: (Ord a) => [BinomialTree a] -> [BinomialTree a] -> [BinomialTree a]
mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2')
    | orden t1 < orden t2 = t1 : mrg ts1' ts2
    | orden t2 < orden t1 = t2 : mrg ts1 ts2'
    | otherwise = insTree (enlazar t1 t2) (mrg ts1' ts2')

-- | Funcion auxiliar a deleteMin para eliminar el minimo de un @BinomialTree@ dado.
removeMinTree :: (Ord a) => [BinomialTree a] -> (BinomialTree a, [BinomialTree a])
removeMinTree [] = error "Heap vacio"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if raiz t < raiz t' then (t, ts) else (t',t:ts')
    where (t',ts') = removeMinTree ts

-- | Funcion que permite encontrar el minimo elemento de un @BinomialHeap@
findMin :: (Ord a) => BinomialHeap a -> a
findMin (BH ts) = raiz t
    where (t,_) = removeMinTree ts

-- | Eliminates the minimum element of a @BinomialTree@
deleteMin :: (Ord a) => BinomialHeap a -> BinomialHeap a
deleteMin (BH ts) = BH (mrg (reverse ts1) ts2)
    where (Nodo _ x ts1, ts2) = removeMinTree ts
