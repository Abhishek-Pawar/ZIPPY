Module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Get
import qualified Data.Map as M
import System.Environment
import Data.Tuple
import GHC.Word
    
import Lzw_Compress
import Lzw_Decompress

-- Function returns Get of an Abbreviation
getAbbreviation::Get Abbreviation

getAbbreviation =
    do
        index <- getWord16le
        ch <- getWord8

        return (index,ch)
