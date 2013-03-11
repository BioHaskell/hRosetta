{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Data.List(minimumBy)
import Control.Monad(forM_)

import Rosetta.Silent

--   TODO: optional trailing arguments - extract only given decoys
main = do [number, silentInputFilename, silentOutputFilename] <- getArgs
          let ((n :: Int, []):_) = reads number
          mdls <- processSilentFile silentInputFilename
          let bestMdls = take n $ sortModelsByScore mdls
          forM_ bestMdls $ \bestMdl ->
            putStrLn $ BS.unpack (name bestMdl) ++ ": " ++ show (modelScoreIfAvailable bestMdl)
          writeSilentFile silentOutputFilename bestMdls



