{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Data.List(minimumBy)
import Control.Monad(forM)

import Rosetta.Silent

--   TODO: optional trailing arguments - extract only given decoys
main = do [number, silentInputFilename, pdbOutputFilename] <- getArgs
          let ((n :: Int, []):_) = reads number
          mdls <- processSilentFile $ BS.pack silentInputFilename
          let bestMdls = take n $ sortModelsByScore mdls
          forM bestMdls $ \bestMdl ->
            putStrLn $ BS.unpack (name bestMdl) ++ ": " ++ show (modelScoreIfAvailable bestMdl)
          -- TODO: write silent output



