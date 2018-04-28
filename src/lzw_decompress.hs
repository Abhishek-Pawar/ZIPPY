{- |
   This module defines the necessary functionality for Lzw decompression
   Includes functions for decompression alongwith some some decode functions
 -}

  module Lzw_Decompress(
  -- * Class @Lzw_Decompress@
  module Lzw_Decompress
  ) where

import Lzw_shared
import qualified Data.Map as M
import Data.Binary.Get
import Data.Tuple
import GHC.Word

{- |
   Function returns Get of an Abbreviation
-}
getAbbr::Get Abbreviation

getAbbr =
    do
        index <- getWord16le
        ch <- getWord8

        return (index,ch)

{-|
  Function to get a list of abbreviations from a ByteString (uncompressed file)
-}
getAbbrList::Get [Abbreviation]

getAbbrList =
    do
        empty <- isEmpty                    -- Base Case; Return empty list

        if empty then return [] else do

            thisAbbr <- getAbbr             -- This gets  the next abbreviation
            remAbbr <- getAbbrList          -- This gets the abbreviations from remainder list

            return(thisAbbr:remAbbr)        -- Appends  abbreviation to abbreviations from remainder list

{-|
  Function which accepts an abbreviation and a (decompression) dictionary and outputs the string based on the dictionary
-}
decodeAbbreviation::Abbreviation->DecompDict->[Word8]

decodeAbbreviation abbreviation dict = reverse $ constructString abbreviation where     -- reverse the string returned by constructString

    {- |
       Function to construct the string based on abbreviation; constructs string in reverse
    -}
    constructString::Abbreviation->[Word8]

    constructString (0,ch) = ch:[]      -- If first index (entry) is 0, nothing comes before ch

    constructString (index,ch) = (ch:(constructString abbreviation)) where        -- Insert ch just before the abbreviation mapped by index in the dictionary
        Just abbreviation = index `M.lookup` dict                                   -- Find index in the dictionary


{- |
   Function that accepts a list of entries and generates corresponding list of words
-}
decompressString::[Abbreviation]->[Word8]

decompressString abbreviationList = decoder abbreviationList 256 initDecompDict where

    {- |
       |Function accepts a list of remainder abbreviations, and the current index and current state of the (decompression) dictionary
        Returns the expansion of the abbreviation list
    -}
    decoder::[Abbreviation]->Id->DecompDict->[Word8]

    decoder [] _ _ = []     -- Return empty list if no abbreviations

    decoder list  0 _ = decoder list 256 initDecompDict     -- If index is 0, reset the (decompression) dictionary

    -- Expand the first abbreviation and append it to expansion of rest of the list
    decoder (first:rest) index dict = (decodeAbbreviation first dict) ++ (decoder rest (index + 1) (M.insert index first dict))
