{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Vector           as V
import qualified Data.ByteString.Char8 as BS
import System.FilePath(splitExtension)
import Numeric(showFFloat)
import Rosetta.RDC

showRDCParams name rdcSet = ("RDC set " ++)                    .
                            (name       ++)                    .
                            (" of "     ++)                    .
                            shows (V.length $ unRDCSet rdcSet) .
                            (" restraints has "++)             .
                            show                               .
                            rdcParameters                      $ rdcSet

testInput fname = do rdcSet <- parseRDCRestraintsFile $ BS.pack fname
                     putStrLn $ showRDCParams fname rdcSet
                     writeFile ((++".dat") . fst . splitExtension $ fname) $ showKDE rdcSet
           
main = mapM_ testInput [ "examples/restraints/ideal.rdc"
                       , "examples/restraints/asyn_gs_long.rdc"
                       , "examples/restraints/prion_Da4_Dr3.rdc"
                       , "examples/restraints/prion_Da5_Dr2.rdc" ]
      

