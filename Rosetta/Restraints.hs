{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, PatternGuards #-}
-- | Module for parsing and processing ROSETTA 3.x restraints.
module Rosetta.Restraints( Restraint        (..)
                         , RestraintFunction(..)
                         , AtomId           (..)
                         , parseRestraints
                         , parseRestraintsFile
                         , processRestraintsFile
                         ) where

import Prelude
import System.IO(hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Data.Data
import Data.Typeable
import Control.DeepSeq
import Control.Monad( when
                    , forM_ )
import Control.Monad.Instances()

import Rosetta.Util

-- TODO: Use unique strings for atom and residue codes.
-- Packages: stringtable-atom, monad-atom, simple-atom

-- | Scoring function for a restraint.
data RestraintFunction = RGaussian { gAvg,     gStDev,   gGoal  :: !Double }
                       | RBounded  { bLoBound, bHiBound, bStDev :: !Double }
  deriving (Read, Eq, Data, Typeable)

-- | Description of ROSETTA 3.x restraint type.
data Restraint = DistR { at1, at2 :: !AtomId
                       , func     :: !RestraintFunction }
               | DiheR { at1, at2, at3, at4 :: !AtomId
                       , func               :: !RestraintFunction }
  deriving (Read, Eq, Data, Typeable)

-- TODO: define Show that prints out Rosetta format?

instance Show RestraintFunction where
  showsPrec n (RGaussian avg dev goal) = ("GAUSSIAN "++) . showsPrec n avg . (' ':) . showsPrec n dev . (' ':) . showsPrec n goal
  showsPrec n (RBounded  lo  hi  est ) = ("BOUNDED "++)  . showsPrec n lo  . (' ':) . showsPrec n hi  . (' ':) . showsPrec n est

instance Show Restraint where
  showsPrec n (DistR at1 at2 func) = ("AtomPair "++) . showsAt n at1 . (' ':) . showsAt n at2 . showsPrec n func

showsAt :: Int -> AtomId -> String -> String
showsAt n at s = showsPrec n (resId at) $ " " ++ BS.unpack (resName at) ++ " " ++ BS.unpack (atName at) ++ s

-- | Datatype pointing to a given atom in a molecule.
data AtomId = AtomId { resName :: !BS.ByteString, -- may be empty!
                       atName  :: !BS.ByteString,
                       resId   :: !Int
                     }
  deriving (Show, Read, Eq, Data, Typeable)
-- TODO: define Eq that ignores missing resname
-- TODO: define nicer Show/Read

instance NFData AtomId    where
instance NFData Restraint where

-- | Parse constraint function header and parameters
parseFunc ::  Show a => a -> [BS.ByteString] -> Either [Char] RestraintFunction
parseFunc lineNo (funcs:spec) | funcs == "GAUSSIAN" =
                                  do when (length spec <  2) $ Left $ "Not enough arguments to GAUSSIAN in line " ++ show lineNo
                                     [avg, dev] <- mapM (\i -> parseFloat lineNo $ spec !! i) [0, 1]
                                     est <- if length spec == 2
                                              then return avg
                                              else parseFloat lineNo $ spec !! 2
                                     return $! RGaussian avg dev est
parseFunc lineNo (funcs:spec) | funcs == "BOUNDED"  = 
                                  do when (length spec <= 3) $ Left $ "Not enough arguments to BOUNDED in line "  ++ show lineNo
                                     [hi, lo, stdev] <- mapM (\i -> parseFloat lineNo $ spec !! i) [0..2]
                                     return $! RBounded { bLoBound = lo, bHiBound = hi, bStDev = stdev }
parseFunc lineNo (funcs:spec)                       = 
                                     Left $ "Unknown function name " ++ show (BS.unpack funcs)

-- | Parse float, or output comprehensible error message with line number.
parseFloat lineNo floatStr | [(f, [])] <- reads $ BS.unpack floatStr = Right f
parseFloat lineNo floatStr                                           =
                           Left $ "Cannot parse float " ++ show floatStr ++ " in line " ++ show lineNo

-- | Parse an distance restraint (AtomPair)
parsePair lineNo ws = do when (len <= 8) $ Left $ "Too few ("++ show len ++") words in AtomPair line!"
                         [at1, at2] <- mapM (mkAtId3 lineNo) [at1s, at2s]
                         func <- parseFunc lineNo funcs
                         Right $ DistR at1 at2 func
  where
    len = length ws
    [at1s, at2s, funcs] = splitsAt [3, 6] ws

-- | Make an AtId object out of three entries in a line (with residue name.)
mkAtId3 lineNo [residStr, resname, atName] =
    case reads $ BS.unpack residStr of
      [(resid, "")] -> Right AtomId { resName = resname,
                                      atName  = atName ,
                                      resId   = resid  }
      otherwise     -> Left ("Cannot parse atom id string " ++
                             BS.unpack residStr ++ " in line " ++
                             show lineNo)

-- | Make an AtId object out of two entries in a line (without residue name.)
mkAtId2 lineNo [atName, resnum] = mkAtId3 [resnum, "", atName]

-- | Parse dihedral restraint (not yet implemented.)
parseDihe lineNo ws = Left "Dihedral restraints are not yet implemented"

-- | Parse restraint line with a given line number.
--   Returns either restraint object, or an error message.
parseRestraint :: Int -> BS.ByteString -> Either String Restraint
parseRestraint lineNo line = case recType of
                               "AtomPair" -> parsePair lineNo rec
                               "Dihedral" -> parseDihe lineNo rec
                               otherwise  -> Left $ "Unknown restraint type" ++ BS.unpack recType
  where
    recType:rec = BS.words line

-- | Parse a ByteString with a set of restraint, and yield all successfully
--   parsed restraints and a list of error messages.
parseRestraints :: BS.ByteString -> ([Restraint], [String])
parseRestraints input = (restraints, errs)
  where
    (errs, restraints) = partitionEithers             .
                         zipWith parseRestraint [1..] .
                         BS.lines                     $ input

-- | Open a file with a given name, and yield tuple with list of restraints,
--   and error messages.
parseRestraintsFile fname = parseRestraints `fmap` BS.readFile fname

-- | Read restraints list from a given file, print out all error messages to stderr,
--   and yield list of restraints.
processRestraintsFile fname = do (restraints, errors) <- parseRestraintsFile fname
                                 forM_ errors $ \msg -> hPutStrLn stderr $ concat [ "ERROR parsing restraints file "
                                                                                  , fname
                                                                                  , ": "
                                                                                  , msg                              ]
                                 return restraints



