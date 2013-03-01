module Main( main ) where

import Rosetta.Restraints
import System.Environment(getProgName,getArgs)
import System.Console.GetOpt
import System.IO hiding(readFile,getContents)
import System.Exit(exitWith,exitFailure,exitSuccess,ExitCode(..))
import Prelude hiding(readFile,getContents)
import Data.ByteString.Char8(readFile,getContents)

data Options = Options  { optVerbosity :: Int
                        }

showHelp :: IO ()
showHelp =
        do prg <- getProgName
           hPutStrLn stdout (usageInfo prg options)

showVersion = hPutStrLn stdout "Version 0.1"

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

processFile :: Options -> String -> IO ()
processFile opts filename = do
  dat <- processRestraintsFile filename
  print dat

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

