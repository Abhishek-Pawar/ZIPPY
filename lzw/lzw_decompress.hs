import Shared

-- Function which accepts an abbreviation and a (decompression) dictionary and outputs the string based on the dictionary
decodeAbbreviation::Abbreviation->DecompDict->[Word8]

decodeAbbreviation abbreviation dict = reverse $ constructString abbreviation where     -- reverse the string returned by constructString

    -- Function to construct the string based on abbreviation; constructs string in reverse
    constructString::Abbreviation->[Word8]

    constructString (0,ch) = ch:[]      -- If first index is 0, nothing comes before ch

    constructString (index,ch) = (char:(constructString abbreviation)) where        -- Insert ch just before the abbreviation mapped by index in the dictionary 
        Just abbreviation = index `M.lookup` dict                                   -- Find index in the dictionary
