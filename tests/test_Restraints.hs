{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Rosetta.Restraints(parseRestraintsFile, exampleString, splitsAt)
import Test.QuickCheck.All
import Test.QuickCheck.Property

import Control.Exception(assert)

test_example_file = do (dat, errs) <- parseRestraintsFile "examples/restraints/short.newcst"
                       assert   (length dat  == 36) $
                         assert (length errs ==  0) $ 
                           return True

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

main = do $quickCheckAll
          test_example_file

