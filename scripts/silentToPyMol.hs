{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main( main ) where

import System.Exit
import System.Environment
import System.Console.GetOpt
import System.IO
import Control.Monad.Instances
import Control.Monad(when)
import Data.Either(partitionEithers)
import qualified Data.ByteString.Char8 as BS
import Prelude

import Rosetta.SS
import Rosetta.Silent
import Rosetta.PyMol

epilogue = "hide all\nshow cartoon\n"

extract = concatMap takeRec

takeRec (Rec r) = [r]
takeRec _       = []

extractRanges []                            = []
extractRanges (silentRec:rs) = extractRanges' (resId silentRec, resId silentRec, ss silentRec) rs

extractRanges' (resid1, resid2, ss ) []                                       = [(resid1, resid2, ss)]
extractRanges' (resid1, resid2, ss1) (silentRec:rs) | ss1 == ss silentRec = extractRanges' (resid1, resId silentRec, ss1) rs
extractRanges' (resid1, resid2, ss1) (silentRec:rs)                       = (resid1, resid2, ss1):extractRanges' (resid3, resid3, ss3) rs
  where
    resid3 = resId silentRec
    ss3    = ss    silentRec

--  do mapM_ (hPutStrLn outHandle . pymolShowRange) $ extractRanges recs
pymolScript outHandle recs = 
  do mapM_ (hPutStrLn outHandle . pymolShow) $ extract recs
     hPutStrLn outHandle epilogue

data Options = Options { optVerbosity :: Int
                       }

showHelp :: IO ()
showHelp =
        do prg <- getProgName
           hPutStrLn stdout (usageInfo prg options)

showVersion = hPutStrLn stdout "Version 0.2"

exitAfter function exitCode =
  \opts -> do function
              exitWith exitCode
              return opts

defaultOptions = Options { optVerbosity = 0
                         }

changeVerbosity :: (Int -> Int) -> Options -> IO Options
changeVerbosity aChange = \opt -> return opt { optVerbosity = aChange (optVerbosity opt) }

--options :: Options -> IO Options
options  :: [OptDescr (Options -> IO Options)]
options = 
  [Option ['v'] ["verbose"] (NoArg (changeVerbosity (\a -> a + 1)))
           "Increases log verbosity.",
   Option ['q'] ["quiet"]   (NoArg (changeVerbosity (\a -> a - 1)))
           "Decreases log verbosity.",
   Option ['V'] ["version"] (NoArg (exitAfter showVersion ExitSuccess))
           "Print program version.",
   Option ['h'] ["help"]    (NoArg (exitAfter showHelp    ExitSuccess))
           "Prints help"
  ]

withFile s f = putStr . unlines . f . lines =<< open s
  where
    open f = if f == "-" then getContents else readFile f

processFile :: Options -> String -> IO ()
processFile opts filename = do
  evts <- processSilentEvents $ BS.pack filename
  pymolScript stdout evts
  return ()

main = do
  args <- getArgs
  let (actions, filenames, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optVerbosity = verbosity } = opts
  -- <- Put your code here...
  -- mapM processFile
  foldl (>>) (return ())
             (map (processFile opts) filenames)
  
  exitWith ExitSuccess


