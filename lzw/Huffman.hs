{-|
 -  This module defines a Huffman tree
 -}
module Huffman (
    -- * Type @Huffman@
    Huffman,
    -- ** Operations on @Huffman@ trees
    getFrequency,
    createBranch,
    createHuffman,
    generateAssociations,
    reconstructHuffman,
    compress,
    decompress
)
where

import BinomialHeap as BHeap

{- |
    Huffman tree type definition
 -}
data Huffman a = Leaf Integer a
               | Branch Integer (Huffman a) (Huffman a)
               deriving (Show, Read)

{- |
  Huffman trees' equivalence instance
 -}
instance (Eq a) => Eq (Huffman a) where
    (Leaf f e)        == (Leaf f2 e2)         = (f == f2) && (e == e2)
    (Branch frec i d) == (Branch frec2 i2 d2) = (frec == frec2) && (i == i2) && (d == d2)
    _ == _ = False

{- |
  Huffman trees' ordering instance
 -}
instance (Ord a) => Ord (Huffman a) where
    Leaf f e     <= Leaf f2 e2      = f <= f2
    Branch f i d <= Leaf f2 e       = f <= f2
    Leaf f e     <= Branch f2 i d   = f <= f2
    Branch f i d <= Branch f2 i2 d2 = f <= f2

{- |
   Gets the accumulated frequency in a Huffman tree root
 -}
getFrequency :: Huffman a -> Integer
getFrequency (Leaf f _)     = f
getFrequency (Branch f _ _) = f

{- |
   Given two Huffman trees it returns a new one with the original ones
   as its children. The resulting tree accumulated frequency is the sum
   of the frequency of its children.
 -}
createBranch :: Huffman a -> Huffman a -> Huffman a
createBranch l r = Branch (getFrequency l + getFrequency r) l r

{- |
   Given a list of elements with its corresponding frequencies constructs a
   BinomialHeap of Huffman trees that contains said elements along with its
   frequencies
-}
createSet :: (Ord a) => [(a, Integer)] -> BinomialHeap (Huffman a)
createSet = foldl add (BH [])
    where
        add acc (elem, freq) = BHeap.insert (Leaf freq elem) acc

{- |
   Given a list that contains a set of symbol and ocurrence pairs, constructs
   a Huffman tree that represents such a set
 -}
createHuffman :: (Ord a) => [(a, Integer)] -> Huffman a
createHuffman l = createHuffman' (createSet l)

{- |
   Given a set of Huffman trees represented by a BinomialHeap, returns
   a single Huffman tree with the values of all of the initial trees
 -}
createHuffman' :: (Ord a) => BinomialHeap (Huffman a) -> Huffman a
createHuffman' (BH [(Nodo 0 t _)]) = t
createHuffman' c = createHuffman' (BHeap.insert newBranch heapWithoutAB)
    where
        branchA       = findMin c
        branchB       = findMin $ deleteMin c
        newBranch     = createBranch branchA branchB
        heapWithoutAB = deleteMin $ deleteMin c

{- |
   Dado un Arbol de Huffman, devuelve una lista de tuplas con el conjunto
   de todos los simbolos ocurrentes en el arbol, junto a la codificacion
   binaria de los mismos (La lista de booleanos representa la lista de bits
   de la codificacion donde False representa el cero y True el uno)
 -}
generateAssociations :: (Ord a) => Huffman a -> [(a, [Bool])]
generateAssociations a = generateAssociations' a [] []

generateAssociations' :: (Ord a) => Huffman a -> [(a, [Bool])] -> [Bool] -> [(a, [Bool])]
generateAssociations' (Leaf _ elem)  assoc acc = (elem, acc ++ [True]):assoc
generateAssociations' (Branch _ i d) assoc acc = lefts ++ rights
    where
        lefts  = generateAssociations' i assoc (acc ++ [False])
        rights = generateAssociations' d assoc (acc ++ [True])

{- |
   Dada una lista de tuplas con un conjunto de simbolos, junto a una
   codificacion binaria de los mismos (La lista de booleanos representa
   la lista de bits de la codificacion, donde False representa el cero
   y True el uno). Construye un Arbol de Huffman tal que dichas asociaciones
   pudieran haber sido generadas. Para conservar las propiedades de un buen
   Arbol de Huffman, la frecuencia acumulada sera asignada a cero(0) para
   todas las hojas y ramas del mismo
 -}
reconstructHuffman :: [(a, [Bool])] -> Huffman a
reconstructHuffman ((e,[True]):[]) = Leaf 0 e
reconstructHuffman l               = Branch 0 (reconstructHuffman left) (reconstructHuffman right)
    where
        left =  [(e,xs)| (e,(False:xs)) <- l]
        right = [(e,xs)| (e,(True:xs))  <- l]

{- |
 - Given a symbol list returns a list of tuples where the first value of each
   tuple is a symbol of the original list and the second value is the number
   of occurrences of that symbol in the original list.
 -}
getOccurrences :: (Eq a) => [a] -> [(a,Integer)] -> [(a, Integer)]
getOccurrences [] acc = acc
getOccurrences (x:xs) acc = getOccurrences rest ((x, 1 + repet):acc)
    where
        rest = [e| e <- xs, e /= x]
        repet = sum [1| a <- xs, a == x]

{- |
 - Dado una lista de simbolos, devuelve una tupla. Esta tupla tiene como
   primer elemento el Arbol de Huffman generado a partir de la lista de
   simbolos dada. Como segundo elemento, tiene una lista donde cada uno
   de los simbolos en la entrada, ha sido reemplazado por su representacion
   binaria y concatenados para formar una sola cadena.
 -}
compress :: (Ord a) => [a] -> (Huffman a, [Bool])
compress l = (tree, concatMap getCode l)
    where
        tree      = createHuffman $ getOccurrences l []
        codes     = generateAssociations tree
        getCode s = snd (head (dropWhile (\(e,c) -> e/=s) codes))

{- |
 - Given a Huffman tree and a list that represents a chain of bits,
   returns a list of symbols that corresponds to the decoding of said
   chain with the tree's information
 -}
decompress :: Huffman a -> [Bool] -> [a]
decompress a [] = []
decompress a c  = getSymbol a c []
    where
        getSymbol (Leaf _ e)     []         acc = acc
        getSymbol (Leaf _ e)     (_:rest)   acc = acc ++ [e] ++ (decompress a rest)
        getSymbol (Branch _ _ d) (True:xs)  acc = getSymbol d xs acc
        getSymbol (Branch _ i _) (False:xs) acc = getSymbol i xs acc
