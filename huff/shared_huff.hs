{- |
 - This module defines the necessary functionality common to Huffman compression and decompression
   modules.
 -}
module Huffman_shared (

        -- ** Common Operations on Huffman Compression and Decompression
        readInputFile,
        printCompFile
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


    
    