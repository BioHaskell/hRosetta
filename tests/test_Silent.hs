{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO(hPrint, stderr)
import qualified Data.ByteString.Char8 as BS

import Rosetta.Silent(processSilent, processSilentEvents)

-- TODO: make examples/.. path relative to the module path
main = do evts <- processSilentEvents "examples/silent/best.out"
          print "EVENTS:"
          print evts
          models <- processSilent "examples/silent/best.out"
          print $ take 10 models
          print $ length models

