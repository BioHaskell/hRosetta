module Main where

import System.IO(hPrint, stderr)
import qualified Data.ByteString.Char8 as BS

import Rosetta.Fragments(processFragmentsFile)

main = do frags <- processFragmentsFile "examples/aat000_03.200_R3"
          print $ take 10 frags
          print $ length frags

