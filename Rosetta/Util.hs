module Rosetta.Util(splitsAt) where

-- | Splits a list at indices given by another list, yielding a list of lists
--   Obeys the following law:
--   flatten . splitsAt a l == l
splitsAt (i:is) l = x:splitsAt (map (+(-i)) is) r
  where
    (x, r) = Prelude.splitAt i l
splitsAt [] l = [l]

