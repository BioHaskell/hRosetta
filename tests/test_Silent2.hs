{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO(hPrint, stderr)
import qualified Data.ByteString.Char8 as BS

import Rosetta.Silent(processSilentFile, bestSilentModel, name)

-- TODO: make examples/.. path relative to the module path
main = do models <- processSilentFile "examples/silent/newsilent.out"
          print $ name $ bestSilentModel models

