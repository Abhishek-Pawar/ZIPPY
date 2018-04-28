{- |
   This module defines the necessary functionality for LZW compression
   Includes functions for compression alongwith some encode functions
 -}
  module Lzw_Compress (
  -- * Class @Lzw_Compress@
  module Lzw_Compress
  ) where

import Lzw_shared
import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.Map as M
import Data.Tuple
import GHC.Word

{- | Function that accepts a string and  a (compression) dictionary and ouptus the abbreviation (based on the string ) & remaining part ofthe string.
-}
compChar :: [Word8] -> CompDict -> ([Word8],Abbreviation)

compChar string  dict = encodeChar string 0 where

  {- | Function  which accepts remaining chars and an index corresponding to compressed chars and returns an abbreviation of the initials and the rest of the string
  -}
  encodeChar:: [Word8]->Id-> ([Word8],Abbreviation)

  encodeChar (ch:[]) index = ([],(index,ch))        --If we encounter the last char in the string, we do not search the dict but write to file.

  encodeChar (first:rest) index  =                  --If the string is found in the dict,recursive call with its index and next char

    case (index,first) `M.lookup` dict of

        (Just newIndex) -> encodeChar rest newIndex  -- Write the index of new character if the character is the first one after an abbreviation

        Nothing -> (rest,(index,first))

{- | Function which accepts a list of Word8s which are compressed into a list of abbrevations and  outputs this new list
-}
compressString::[Word8]->[Abbreviation]

compressString = encoder 256 initCompDict where

  {- | Function which takes a index in the dictionary,the (compression) dictionary and the remaining list and returns a new list with the abbrevation appended to it
  -}
  encoder::Id-> CompDict -> [Word8] -> [Abbreviation]

  encoder _ _ []  = []                       --Base Case

  encoder 0 _ remainder = encoder 256 initCompDict remainder            --If the index is  zero then pass the initialized dict with the rest of the arguements kept same

  -- Encode the beginning of the string and append the abbrevation. Recursive call on remainder part.

  encoder index dict string = currentAbbr:(encoder (index + 1) (M.insert currentAbbr index dict) remainder) where

    (remainder,currentAbbr) = compChar string dict

{- | Function which accepts a byteString (uncompressed file) and outputs a byteString (compressed file).
-}
compressByteString::BS.ByteString -> BS.ByteString

compressByteString byteString = do

  let contents = BS.unpack byteString

  let compressed = compressString contents

  --Turn the list of abbreviations into a list of puts (byteStrings)
  let asPut = map (\ (index,word) -> (putWord16le index) >> (putWord8 word)) compressed

  --Turn the list of puts (byteStrings) into one long put and call runput on that
  runPut (sequence_ asPut)
