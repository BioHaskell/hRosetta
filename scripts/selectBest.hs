{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import System.IO
import System.Environment
import System.Exit(exitFailure)
import qualified Data.ByteString.Char8 as BS
import Control.Monad(forM_, when)

import Rosetta.Silent

mainWithParams ::  Int -> FilePath -> FilePath -> IO ()
mainWithParams n silentInputFilename silentOutputFilename = do
          mdls <- processSilentFile silentInputFilename
          let bestMdls = take n $ sortModelsByScore mdls
          forM_ bestMdls $ \bestMdl ->
            putStrLn $ BS.unpack (name bestMdl) ++ ": " ++ show (modelScoreIfAvailable bestMdl)
          writeSilentFile silentOutputFilename bestMdls

--   TODO: optional trailing arguments - extract only given decoys
main :: IO ()
main = do args <- getArgs 
          when (length args /= 3) $ do hPutStrLn stderr "Usage: selectBest <N> <input.out> <NBestDecoys.out>"
                                       exitFailure
          let [number, silentInputFilename, silentOutputFilename] = args
          let ((n :: Int, []):_) = reads number
          mainWithParams n silentInputFilename silentOutputFilename



