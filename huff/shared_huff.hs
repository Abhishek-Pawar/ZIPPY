{- |
 - This module defines the necessary functionality common to Huffman compression and decompression
   modules.
 -}
module Huffman_shared (
        -- ** Common Operations on Huffman Compression and Decompression
        readInputFile,
        printCompFile,
        bytesToBits,
        bitsToBytes
)

where

    import Data.Word
    import Data.List
    import qualified Data.ByteString as BS


    {- |
        A Byte is an 8 bit word
     -}    
    type Byte = Data.Word.Word8

    
    {- |
        Function that accepts a (relative) file path and returns the content 
        of the file as a list of bytes (@Byte@)
    -}
    readInputFile :: String -> IO [Byte]

    readInputFile inputFile = BS.readFile inputFile >>= return . BS.unpack
    

    {- |
        Function that accepts a (relative) file path and a list of bytes (@Byte@)
        and prints the bytes in the input file.
    -}
    printCompFile :: String -> [Byte] -> IO()

    printCompFile inputFile = BS.writeFile inputFile . BS.pack

    {- |
        Function that accepts a list of booleans (bits) and outputs a list of bytes
    -}
    bitsToBytes :: [Bool] -> [Byte]

    bitsToBytes boolList = reverse $ bitsToBytesRev [] boolList 0 0 where

        -- Base Case
        bitsToBytesRev x [] xi 8 = xi:x

        -- Recursive definition
        bitsToBytesRev x [] xi n = bitsToBytesRev x [] (xi*2) (n+1)
        bitsToBytesRev x list xi 8 = bitsToBytesRev (x1:x) list 0 0
        bitsToBytesRev x (False:list) xi n = bitsToBytesRev x list (xi*2) (n+1)
        bitsToBytesRev x (True:list) xi n = bitsToBytesRev x list (xi*2+1) (n+1)
    
    
    {- |
        Function that accepts a byte (@Byte) and outputs the corresponding list of bits.
    -}
    byteToBits :: Byte -> [Bool]

    bytesToBits = reverse . bytesToBitsRev 8 where

        -- Base Case
        bytesToBitsRev 0 _ = []

        -- Recursive Definition
        bytesToBitsRev n x 
            | even x = False : bytesToBitsRev (n-1) (div x 2)
            | otherwise = True : bytesToBitsRev (n-1) (div x 2)

    


    
    