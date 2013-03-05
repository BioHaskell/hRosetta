{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO(hPrint, stderr)
import qualified Data.ByteString.Char8 as BS

import Rosetta.Silent( processSilentFile
                     , writeSilentFile
                     , processSilentEvents
                     , bestSilentModel
                     , name)

-- TODO: make examples/.. path relative to the module path
main = do evts <- processSilentEvents "examples/silent/best.out"
          print "EVENTS:"
          print evts
          models <- processSilentFile "examples/silent/best.out"
          print $ take 10 models
          print $ length models
          writeSilentFile "tests/best10.out" $ take 10 models

