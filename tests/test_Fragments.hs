module Main where

import System.IO(hPrint, stderr)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

import Rosetta.Fragments( processFragmentsFile
                        , unRFragSet           )

main = do frags <- unRFragSet `fmap` processFragmentsFile "examples/aat000_03.200_R3"
          print $ V.take 10 frags
          print $ V.length  frags

