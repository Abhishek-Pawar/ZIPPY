import Shared

--Function that accepts a string and  a (compression)dictionary and ouptus an abbreviation (from the string ) & remaining part ofthe string ,.
compChar :: [Word8] -> Compdict -> ([Word8],Abbreviation)

compChar string  dict = encodeChar string 0 where

  --Function  which accepts remaining chars and an Id mapped to compressed chars and the rest of the string alongwith a abbreviation of the initials
  encodeChar:: [Word8]->Id-> ([Word8],Abbreviation)

  encodeChar (char:[]) index = ([],(index,char))    --If we encounter the last char in the string, we do not search the dict but write to file.

  encodeChar (first:rest) index  =                  --If the string parsed is found in the dict,recursive call with  its ID and next char

    case (index,first) `M.lookup` dict of

        (Just newIndex) -> encodeChar rest newIndex  --If the character is the first one after a sequence from the dict write the index of
                                                     -- next character
          Nothing -> (rest,(index,first))
