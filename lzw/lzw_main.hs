module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import qualified Data.Map as M
import System.Environment
import Data.Tuple
import GHC.Word

import Shared
import Lzw_Compress
import Lzw_Decompress

-- Function returns Get of an Abbreviation
getAbbreviation::Get Abbreviation

getAbbreviation =
    do
        index <- getWord16le
        ch <- getWord8

        return (index,ch)

-- Function to get a list of abbreciations from a ByteString (uncompressed file)
getAbbrList::Get [Abbreviation]

getAbbrList =
    do
        empty <- isEmpty

        -- Base Case; Return empty list
        if empty then return [] else do

            thisAbbr <- getAbbreviation     -- Get the next abbreviation
            remAbbr <- getAbbrList          -- Get the abbreviations from remainder list

            return(thisAbbr:remAbbr)        -- Append this abbreviation to abbreviations from remainder list

main =
  do
      --The first argument is '-c' for compress or '-d' for decompress.  The
      --second argument is the name of the input file and the third argument is the
      --name of the output file.
      (option:inputfilename:outputfilename:_) <- getArgs --the 1st arg is option /the 2nd arg is input file/the 3rd arg is output file/the 4th arg is optional
      content <- BS.readFile inputfilename
      case option of

        "-c-lzw" -> BS.writeFile outputfilename $ compressByteString content --compressed content of input is written to outputfile
        "-d-lzw" ->  BS.writeFile outputfilename $ BS.pack $ decompressString $ runGet getAbbrList content--The result otained by decompression is
                                                                                                       --written to the  outputfile
        _ -> putStr "Invalid option"
