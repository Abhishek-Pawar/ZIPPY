{- |
 - This module defines the necessary functionality to compress/decompress
   files using the Huffman coding algorithm
 - Authors: Ricardo Monascal, Gabriela Montoya, Eleazar Leal
 - Version: 1.0
 -}
module FileCompressor (
    -- ** Operations on @FileCompressor@
    readFromFile,
    printFile,
    transform
)
where

import Huffman
import Data.Word
import Data.List
import qualified Data.ByteString as BS

{- |
  A byte is an 8 bit word
 -}
type Byte = Data.Word.Word8

{- |
  Given a relative file path, returns the content of that file as a
  list of bytes
 -}
readFromFile :: String -> IO [Byte]
readFromFile file = BS.readFile file >>= return . BS.unpack

{- |
  Given a relative file path and a list of bytes, prints said list
  in the file path specified
 -}
printFile :: String -> [Byte] -> IO ()
printFile file = BS.writeFile file . BS.pack

{- |
  Transforms a list of bits (booleans) into a list of bytes
 -}
bitsToBytes :: [Bool] -> [Byte]
bitsToBytes list = reverse $ bitsABytes' [] list 0 0
    where
        bitsABytes' all []           acc 8 = acc:all
        bitsABytes' all []           acc n = bitsABytes' all       []   (acc*2)   (n+1)
        bitsABytes' all list         acc 8 = bitsABytes' (acc:all) list 0         0
        bitsABytes' all (False:list) acc n = bitsABytes' all       list (acc*2)   (n+1)
        bitsABytes' all (True:list)  acc n = bitsABytes' all       list (acc*2+1) (n+1)

{- |
  Transforms a byte into a list of bits (booleans)
 -}
byteToBits :: Byte -> [Bool]
byteToBits = reverse . byteToBits' 8
    where
        byteToBits' 0 _ = []
        byteToBits' n x
            | even x    = False : byteToBits' (n-1) (div x 2)
            | otherwise = True  : byteToBits' (n-1) (div x 2)

{- |
If a True is read, there are two possibilities. If it is the first True that is read
(either at the beginning of the code or after a False), then the next bit
it is part of the tree (represented inversely). Otherwise, it is considered
finalized the code. If a False is read, then the next 8 bits,
correspond to a symbol.
 -}
encodeHuffman :: Huffman Byte -> [Bool]
encodeHuffman h = (concatMap encodeAssociations assocs) ++ [True, False]
    where
        assocs = generateAssociations h
        encodeAssociations (e, cods)
            = (True : intersperse True (reverse cods)) ++ (False : byteToBits e)

{- |
  Lee una codificacion de un Arbol de Huffman y reconstruye el
  Arbol del que fue generado, separando el resto de los datos binarios.
  (El Arbol de Huffman generado tendra frecuencias acumuladas triviales,
  iguales a cero).
 -}
parseHuffman :: [Bool] -> (Huffman Byte, [Bool])
parseHuffman code = (huff, trim rest)
    where
        (assocs, rest) = parseAssocsInit [] code
        huff           = reconstructHuffman assocs
        trim           = reverse . tail . dropWhile not . reverse
        parseAssocsInit acc (True:False:xs) = (acc, xs)
        parseAssocsInit acc (True:True:xs)  = parseAssocs acc [True] xs
        parseAssocs acc cod (False:xs)      = parseAssocsInit ((head $ bitsToBytes e, cod):acc) r
            where (e, r) = splitAt 8 xs
        parseAssocs acc cod (True:x:xs)     = parseAssocs acc (x:cod) xs

{- |
  Transforms a byte list into another one, compressing or decompressing it
  as specified (True is compress, False is decompress).
  When compressing, the last True is added in order to have an end of file character
  (All of the False found before the last True will be ignored by the decoder)
 -}
transform :: Bool -> [Byte] -> [Byte]
transform True code = bitsToBytes $ encodeHuffman huff ++ cods ++ [True]
    where
        (huff, cods) = compress code
transform _ code = decompress huff cods
    where
        (huff, cods) = parseHuffman . concat $ map byteToBits code
