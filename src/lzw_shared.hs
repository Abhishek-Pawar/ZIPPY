module Lzw_shared (Id,Abbreviation,CompDict,DecompDict,initCompDict,initDecompDict,printLzwFile) where


import System.Environment
import Data.Tuple
import GHC.Word
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS

-- Index in the table
type Id = Word16

type Abbreviation = (Id,Word8)

-- Type of Dictionary used for compression
type CompDict = M.Map Abbreviation Id

-- Function to initialize the compression dictionary
initCompDict :: CompDict
initCompDict = M.fromList [((0,n), fromIntegral(n + 1)) | n <- [0..255]]

-- Type of Dictionary used for decompression
type DecompDict = M.Map Id Abbreviation

-- Function to initialize the compression dictionary
initDecompDict :: DecompDict
initDecompDict = M.fromList $ map swap $ M.toList initCompDict

--Function to write the content to Output file
printLzwFile :: FilePath -> BS.ByteString -> IO ()
printLzwFile outputfilename = BS.writeFile outputfilename
