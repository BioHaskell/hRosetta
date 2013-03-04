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

pymolSSCode Loop   = 'L'
pymolSSCode Strand = 'S'
pymolSSCode Helix  = 'H'

--alter A/447:451/ and "MyModel", ss='S'
--rebuild
pymolShow      silentRec = "alter A/"++shows (resId silentRec) "/, ss='"++pymolSSCode (ss silentRec):"'"
pymolShowRange (resid1, resid2, ss) = ("alter A/"++) . shows resid1 . (":"++) . shows resid2 .
                                      shows "/, ss='" $ pymolSSCode ss:"'"

-- Example PyMol script generation wit pymolScript
epilogue = "hide all\nshow cartoon\n"

extractRanges []                            = []
extractRanges (silentRec:rs) = extractRanges' (resId silentRec, resId silentRec, ss silentRec) rs

extractRanges' (resid1, resid2, ss ) []                                       = [(resid1, resid2, ss)]
extractRanges' (resid1, resid2, ss1) (silentRec:rs) | ss1 == ss silentRec = extractRanges' (resid1, resId silentRec, ss1) rs
extractRanges' (resid1, resid2, ss1) (silentRec:rs)                       = (resid1, resid2, ss1):extractRanges' (resid3, resid3, ss3) rs
  where
    resid3 = resId silentRec
    ss3    = ss    silentRec

pymolScript outHandle mdl = 
  do mapM_ (hPutStrLn outHandle . pymolShow) $ residues mdl
     hPutStrLn outHandle epilogue

pymolScriptFile fname mdl = 
  withFile fname WriteMode $ \fhandle ->
    pymolScript fhandle mdl

