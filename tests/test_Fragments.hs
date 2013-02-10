module Main where

import qualified Data.ByteString.Char8 as BS

import Rosetta.Fragments(parseFragments)

main = do input <- BS.readFile "examples/aat000_03.200_R3"
          print $ take 10 $ parseFragments input

