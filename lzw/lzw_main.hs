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

