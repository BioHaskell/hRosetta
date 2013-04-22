{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector as V
import Numeric(showFFloat)
import Rosetta.RDC

showRDCParams name rdcSet = ("RDC set " ++)                    .
                            (name       ++)                    .
                            (" of "     ++)                    .
                            shows (V.length $ unRDCSet rdcSet) .
                            (" restraints has D_a=" ++)        .
                            showFFloat (Just 3) d_a            .
                            (" and D_r=" ++)                   .
                            showFFloat (Just 3) d_r            $ ""
  where
    (d_a, d_r) = rdcParameters rdcSet

main = do rdcs  <- parseRDCRestraintsFile "examples/restraints/ideal.rdc"
          putStrLn $ showRDCParams "ideal.rdc" rdcs
          rdcs2 <- parseRDCRestraintsFile "examples/restraints/asyn_gs_long.rdc"
          putStrLn $ showRDCParams "asyn_gs_long" rdcs2
          
