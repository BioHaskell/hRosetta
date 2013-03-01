{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Rosetta.Restraints(parseRestraintsFile)
import Test.QuickCheck.All
import Test.QuickCheck.Property

import Control.Exception(assert)
import qualified Data.ByteString.Char8 as BS

exampleString = BS.concat ["AtomPair  78 VAL CA    1 VAL CA GAUSSIAN 4.80 1.00\n"            ,
                           "Dihedral  N 307 CA 307 C 307 N 308 BOUNDED 2.20 2.69 0.52 DIHE\n",
                           "AtomPair  148 ASN CG  185 GLU CB BOUNDED 0.00 7.70 1.00 EXPDAT\n"]


test_example_file = do (dat, errs) <- parseRestraintsFile "examples/restraints/short.newcst"
                       assert   (length dat  == 36) $
                         assert (length errs ==  0) $ 
                           return True

pairs (b:c:cs) = (b,c):pairs (c:cs)

main = do $quickCheckAll
          test_example_file

