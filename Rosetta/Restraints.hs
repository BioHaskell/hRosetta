{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
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

import Rosetta.Util
import qualified Rosetta.Util

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
  showsPrec n (RGaussian avg dev goal) = ("GAUSSIAN "++) .
                                         showsPrec n avg . (' ':) .
                                         showsPrec n dev . (' ':) .
                                         showsPrec n goal
  showsPrec n (RBounded  lo  hi  est ) = ("BOUNDED " ++) .
                                         showsPrec n lo  . (' ':) .
                                         showsPrec n hi  . (' ':) .
                                         showsPrec n est

instance Show Restraint where
  showsPrec n (DistR at1 at2 func) = ("AtomPair "++) .
                                     showsAt n at1   . (' ':) .
                                     showsAt n at2   . (' ':) .
                                     showsPrec n func

showsAt :: Int -> AtomId -> String -> String
showsAt n at s = showsPrec n (resId at) $
                 " " ++ BS.unpack (resName at) ++
                 " " ++ BS.unpack (atName  at) ++ s

-- | Datatype pointing to a given atom in a molecule.
data AtomId = AtomId { resName :: !BS.ByteString, -- may be empty!
                       atName  :: !BS.ByteString,
                       resId   :: !Int
                     }
  deriving (Read, Eq, Data, Typeable)
-- TODO: define Eq that ignores missing resname
-- TODO: define nicer Show/Read

instance Show AtomId where
  showsPrec _ a = (++) (BS.unpack $ resName a) .
                  shows            (resId   a) .
                  (++) (BS.unpack $ atName  a)

instance NFData AtomId    where

instance NFData Restraint where

-- | Parse constraint function header and parameters
parseFunc :: Int -> [BS.ByteString] -> Either BS.ByteString RestraintFunction
parseFunc lineNo (funcs:spec) | funcs == "GAUSSIAN" =
                                  do when (length spec <  2) $ Left $
                                       "Not enough arguments to GAUSSIAN in line "
                                         `BS.append` bshow lineNo
                                     [avg, dev] <- mapM
                                                     (\i -> parseFloat' lineNo $ spec !! i)
                                                     [0, 1]
                                     est <- if length spec == 2
                                              then return avg
                                              else parseFloat' lineNo $ spec !! 2
                                     return $! RGaussian avg dev est
parseFunc lineNo (funcs:spec) | funcs == "BOUNDED"  = 
                                  do when (length spec <= 3) $ Left $
                                       "Not enough arguments to BOUNDED in line " `BS.append` bshow lineNo
                                     [hi, lo, stdev] <- mapM (parseFloat' lineNo . (spec !!))
                                                             [0..2]
                                     return RBounded { bLoBound = lo
                                                     , bHiBound = hi
                                                     , bStDev   = stdev }
parseFunc lineNo (funcs:spec)                       = 
                                     Left $ BS.concat ["Unknown function name ", funcs,
                                                       " in line ", bshow lineNo]

-- | Parse float, or output comprehensible error message with line number.
parseFloat' lineNo floatStr = parseFloat recName Nothing floatStr
  where
    recName = BS.concat ["float ", BS.pack $ show floatStr, " in line ", BS.pack $ show lineNo]

-- | Parse an distance restraint (AtomPair)
-- "AtomPair  CA 43A CA 516B  HARMONIC 12 0.2"
-- "AtomPair    CB     6   1HA    33 BOUNDED 1.500 5.650 0.300"
parsePair lineNo ws = do when (len < 7) $ Left $ BS.concat [ "Too few (", bshow len
                                                           , ") words in AtomPair line "
                                                           , bshow lineNo ]
                         [at1, at2] <- mapM (mkAtId2 lineNo) [at1s, at2s]
                         func <- parseFunc lineNo funcs
                         Right $ DistR at1 at2 func
  where
    len = length ws
    [at1s, at2s, funcs] = splitsAt [2, 4] ws

-- | Make an AtId object out of three entries in a line (with residue name.)
mkAtId3 lineNo [residStr, resname, atName] = do resid <- parseInt ("atom id string in line "
                                                                     `BS.append` bshow lineNo)
                                                                  residStr
                                                return AtomId { resName = resname
                                                              , atName  = atName
                                                              , resId   = resid
                                                              }

-- | Make an AtId object out of two entries in a line (without residue name.)
-- TODO: handle chain name!!! 123 -> 123A
mkAtId2 lineNo [atName, resnum] = mkAtId3 lineNo [resnum, "", atName]

-- | Parse dihedral restraint (not yet implemented.)
parseDihe lineNo ws = Left $ "Dihedral restraints are not yet implemented in line " `BS.append`
                             bshow lineNo

-- | Parse restraint line with a given line number.
--   Returns either restraint object, or an error message.
parseRestraint :: Int -> BS.ByteString -> Either BS.ByteString Restraint
parseRestraint lineNo line = case recType of
                               "AtomPair" -> parsePair lineNo rec
                               "Dihedral" -> parseDihe lineNo rec
                               otherwise  -> Left $ BS.concat ["Unknown restraint type", recType
                                                              ," in line ", bshow lineNo]
  where
    recType:rec = BS.words line

-- | Parse a ByteString with a set of restraint, and yield all successfully
--   parsed restraints and a list of error messages.
parseRestraints :: BS.ByteString -> ([Restraint], [BS.ByteString])
parseRestraints input = (restraints, errs)
  where
    (errs, restraints) = partitionEithers             .
                         zipWith parseRestraint [1..] .
                         BS.lines                     $ input

-- | Open a file with a given name, and yield tuple with list of restraints,
--   and error messages.
parseRestraintsFile fname = parseRestraints `fmap`
                              Rosetta.Util.readFile fname

-- | Read restraints list from a given file, print out all error messages to stderr,
--   and yield list of restraints.
processRestraintsFile fname = do (restraints, errors) <- parseRestraintsFile fname
                                 forM_ errors $ \msg -> hPutStrLn stderr $
                                                          concat [ "ERROR parsing restraints file "
                                                                 , fname
                                                                 , ": "
                                                                 , BS.unpack msg                    ]
                                 return restraints



