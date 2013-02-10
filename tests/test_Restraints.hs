{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Rosetta.Restraints(parse, exampleString, splitsAt)
import Test.QuickCheck.All
import Test.QuickCheck.Property

--prop_test_example = length (parse exampleString) == 3

prop_splitsAt_length bs cs = length        (splitsAt bs cs) == length bs + 1

-- NOTE: this precond is too difficult!
inactive_prop_splitsAt_first  b bs cs = (length xs > 1 && length cs >= head xs)
                                  ==> length (head $ splitsAt xs cs) == head xs
  where
    xs = map abs (b:bs)

pairs (b:c:cs) = (b,c):pairs (c:cs)

--prop_splitsAt_endings bs cs = length 

prop_splitsAt_concat bs cs = concat (splitsAt bs cs) == cs

main = $quickCheckAll

