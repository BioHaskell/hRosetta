{-# LANGUAGE OverloadedStrings #-}
module Main where

import Rosetta.RDC

main = do rdcs  <- parseRDCRestraintsFile "examples/restraints/ideal.rdc"
          print rdcs
          rdcs2 <- parseRDCRestraintsFile "examples/restraints/asyn_gs_long.rdc"
          print rdcs2
          
