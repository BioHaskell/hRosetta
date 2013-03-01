{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Property

import Rosetta.Util(splitsAt)

--prop_test_example = length (parse exampleString) == 3
prop_splitsAt_length bs cs = length (splitsAt bs cs) == length bs + 1

prop_splitsAt_concat bs cs = concat (splitsAt bs cs) == cs 

{- Hard to write property?
difs (a:b:cs) = a - b:difs (b:cs)
difs 

clampList n = map $ clamp n

clamp n l | l > n = n
clamp n l         = l
-}
--prop_splitsAt_endings bs cs = length 

-- NOTE: this precond is too difficult!
inactive_prop_splitsAt_first  b bs cs = (length xs > 1 && length cs >= head xs)
                                  ==> length (head $ splitsAt xs cs) == head xs
  where
    xs = map abs (b:bs)

main = $quickCheckAll
