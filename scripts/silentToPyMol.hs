{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main( main ) where

import System.Exit
import System.Environment
import System.Console.GetOpt
import System.IO
import System.FilePath((<.>))
import Control.Monad.Instances
import Control.Monad(when, forM_)
import Data.Either(partitionEithers)
import qualified Data.ByteString.Char8 as BS
import Prelude

import Rosetta.SS
import Rosetta.Silent
import Rosetta.PyMol

data Options = Options { optVerbosity :: Int
                       }

extract = makeModel . concatMap takeRec

makeModel recs = SilentModel { name              = undefined
                             , otherDescriptions = undefined
                             , scores            = undefined
                             , residues          = recs
                             , fastaSeq          = undefined
                             }


takeRec (Rec r) = [r]
takeRec _       = []

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
  mdls <- processSilentFile filename
  forM_ mdls $ \mdl -> pymolScriptFile (BS.unpack (name mdl) <.> "pml") mdl

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


