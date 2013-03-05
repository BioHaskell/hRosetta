-- | Module defines functions necessary for generating PyMol files that
--   describe secondary structure from Silent files.
module Rosetta.PyMol( pymolSSCode
                    , pymolShow
                    , pymolScript
                    , pymolScriptFile
                    )
where

import System.IO( withFile
                , IOMode(WriteMode)
                , hPutStrLn )

import Rosetta.SS
import Rosetta.Silent


-- | Convert `Rosetta.SS.SSCode` to a PyMol secondary structure code.
pymolSSCode Loop   = 'L'
pymolSSCode Strand = 'S'
pymolSSCode Helix  = 'H'

--alter A/447:451/ and "MyModel", ss='S'
--rebuild
-- | PyMol command that assigns secondary structure type for a residue given by `SilentRec`.
pymolShow      silentRec = "alter A/"++shows (resId silentRec) "/, ss='"++pymolSSCode (ss silentRec):"'"

-- | PyMol command that assigns secondary structure type for a range of residues
--   given by two `AtId` objects, and a secondary structure code.
pymolShowRange (resid1, resid2, ss) = ("alter A/"++) . shows resid1 . (":"++) . shows resid2 .
                                      shows "/, ss='" $ pymolSSCode ss:"'"

-- Example PyMol script generation wit pymolScript
-- | PyMol script epilogue
epilogue = "hide all\nshow cartoon\n"

-- | Converts a list of silent records into a list of tuples that indicate ranges
--   with the same secondary structure.
extractRanges []                            = []
extractRanges (silentRec:rs) = extractRanges' (resId silentRec, resId silentRec, ss silentRec) rs

-- | Helper function for extractRanges.
extractRanges' (resid1, resid2, ss ) []                                       = [(resid1, resid2, ss)]
extractRanges' (resid1, resid2, ss1) (silentRec:rs) | ss1 == ss silentRec = extractRanges' (resid1, resId silentRec, ss1) rs
extractRanges' (resid1, resid2, ss1) (silentRec:rs)                       = (resid1, resid2, ss1):extractRanges' (resid3, resid3, ss3) rs
  where
    resid3 = resId silentRec
    ss3    = ss    silentRec

-- | Writes PyMol script visualizing secondary structures within a given `SilentModel` to a given file handle.
pymolScript outHandle mdl = 
  do mapM_ (hPutStrLn outHandle . pymolShow) $ residues mdl
     hPutStrLn outHandle epilogue

-- | Given a filename, writes a PyMol script visualizing secondary structure from a given `SilentModel`.
pymolScriptFile fname mdl = 
  withFile fname WriteMode $ \fhandle ->
    pymolScript fhandle mdl

