{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector           as V
import qualified Data.ByteString.Char8 as BS
import Numeric(showFFloat)
import Rosetta.RDC

showRDCParams name rdcSet = ("RDC set " ++)                    .
                            (name       ++)                    .
                            (" of "     ++)                    .
                            shows (V.length $ unRDCSet rdcSet) .
                            (" restraints has D_a=" ++)        .
                            showFFloat (Just 3) d_a            .
                            (" and D_r=" ++)                   .
                            showFFloat (Just 3) d_r            .
                            (" and R="   ++)                   .
                            showFFloat (Just 3) (d_r/d_a)      $ ""
  where
    (d_a, d_r) = rdcParameters rdcSet

testInput fname = (parseRDCRestraintsFile . BS.pack) fname >>= (putStrLn . showRDCParams fname)
           
main = mapM_ testInput [ "examples/restraints/ideal.rdc"
                       , "examples/restraints/asyn_gs_long.rdc"
                       , "examples/restraints/prion_Da4_Dr3.rdc"
                       , "examples/restraints/prion_Da5_Dr2.rdc" ]

