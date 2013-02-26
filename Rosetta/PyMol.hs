module Rosetta.PyMol(pymolSSCode  ,
                     pymolShow    )
where

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

