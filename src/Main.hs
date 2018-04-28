{- |
   This  module  runs the application in command line
 -}

module Main(

module Main
) where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import qualified Data.Map as M
import System.Environment
import Data.Tuple
import GHC.Word

import Lzw_shared
import Lzw_Compress
import Lzw_Decompress

import Huffman_shared
import Comp_huff
import Decomp_huff

{-|
  Main function: Runs the app in CLI.
-}
main =
  do
      (option:inputfilename:outputfilename:_) <- getArgs -- The 1st arg to choose type of comp-decomp /the 2nd arg is input file/the 3rd arg is output file/the 4th arg is optional
      content <- BS.readFile inputfilename               --Read the file in a Bytestring format
      dataRead <- readInputFile inputfilename            --Reads the file as a list of bytes

      case option of

        "-c-lzw" -> printLzwFile outputfilename $ compressByteString content --lzw compression and write the compressed content to output file
        "-d-lzw" -> printLzwFile outputfilename $ BS.pack $ decompressString $ runGet getAbbrList content--The result otained by lzw decompression is written to output file
        "-c-huff"-> printHufFile outputfilename $ huffCompress dataRead      --huff compression and write the compressed content to output file
        "-d-huff"-> printHufFile outputfilename $ huffDecompress dataRead    --The result otained by huf decompression is written to output file

        _-> putStr "Invalid option"
