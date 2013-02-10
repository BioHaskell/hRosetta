{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO(hPrint, stderr)
import qualified Data.ByteString.Char8 as BS

import Rosetta.Silent(processSilent, processSilentEvents)

main = do evts <- processSilentEvents "examples/best.out"
          print "EVENTS:"
          print evts
          models <- processSilent "examples/best.out"
          print $ take 10 models
          print $ length models

