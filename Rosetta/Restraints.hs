{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}
-- | Module for parsing and processing ROSETTA 3.x restraints.
module Rosetta.Restraints( Restraint(..)
                         , AtomId   (..)
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
                    , forM )
import Control.Monad.Instances()

import Rosetta.Util

-- TODO: Use unique strings for atom and residue codes.
-- Packages: stringtable-atom, monad-atom, simple-atom

-- | Description of ROSETTA 3.x restraint type.
data Restraint = DistR { at1, at2 :: !AtomId ,
                         goal     :: !Double }
               | DiheR { at1, at2, at3, at4 :: !AtomId ,
                         goal               :: !Double }
  deriving (Read, Eq, Data, Typeable)
-- TODO: define Show that prints out Rosetta format?

instance Show Restraint where
  showsPrec n (DistR at1 at2 goal) s = ("AtomPair "++) . showsAt at1 . (" GAUSSIAN "++) . showsPrec n goal . (" 1.00"++) $ s
    where
      showsAt :: AtomId -> String -> String
      showsAt at s = showsPrec n (resId at) $ " " ++ BS.unpack (resName at) ++ " " ++ BS.unpack (atName at) ++ s

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
parseFunc lineNo (funcs:spec) = if funcs == "GAUSSIAN"
                                  then do when (length spec <2) $ Left $ "Not enough arguments to GAUSSIAN in line " ++ show lineNo
                                          parseFloat lineNo $ spec !! 0
                                  else if funcs == "BOUNDED"
                                         then do when (length spec <=3) $ Left $ "Not enough arguments to BOUNDED in line " ++ show lineNo
                                                 parseFloat lineNo $ spec !! 1
                                         else Left $ "Unknown function name " ++ (show $ BS.unpack $ funcs)

-- | Parse float, or output comprehensible error message with line number.
parseFloat lineNo floatStr = case reads $ BS.unpack floatStr of
                        [(f, [])] -> Right f
                        otherwise -> Left $ "Cannot parse float " ++ show floatStr ++ " in line " ++ show lineNo

-- | Parse an distance restraint (AtomPair)
parsePair lineNo ws = do when (len <= 8) $ Left $ "Too few ("++ show len ++") words in AtomPair line!"
                         [at1, at2] <- mapM (mkAtId3 lineNo) [at1s, at2s]
                         goal <- parseFunc lineNo funcs
                         Right $ DistR at1 at2 goal
  where
    len = length ws
    [at1s, at2s, funcs] = splitsAt [3, 6] ws

-- | Make an AtId object out of three entries in a line (with residue name.)
mkAtId3 lineNo [residStr, resname, atName] =
    case reads $ BS.unpack residStr of
      [(resid, "")] -> Right $ AtomId { resName = resname,
                                        atName  = atName ,
                                        resId   = resid  }
      otherwise     -> Left ("Cannot parse atom id string " ++
                             BS.unpack residStr ++ " in line " ++
                             show lineNo)

-- | Make an AtId object out of two entries in a line (without residue name.)
mkAtId2 lineNo [atName, resnum] = mkAtId3 [resnum, "", atName]

-- | Parse dihedral restraint (not yet implemented.)
parseDihe lineNo ws = Left "not implemented"

-- | Parse restraint line with a given line number.
--   Returns either restraint object, or an error message.
parseRestraint :: Int -> BS.ByteString -> Either String Restraint
parseRestraint lineNo line = if recType == "AtomPair"
                               then parsePair lineNo rec
                               else parseDihe lineNo rec
  where
    recType:rec = BS.words line

-- | Parse a ByteString with a set of restraint, and yield all successfully
--   parsed restraints and a list of error messages.
parseRestraints :: BS.ByteString -> ([Restraint], [String])
parseRestraints input = (restraints, errs)
  where
    (errs, restraints) = partitionEithers             .
                         map (uncurry parseRestraint) .
                         zip [1..]                    .
                         BS.lines                     $ input

-- | Open a file with a given name, and yield tuple with list of restraints,
--   and error messages.
parseRestraintsFile fname = parseRestraints `fmap` BS.readFile fname

-- | Read restraints list from a given file, print out all error messages to stderr,
--   and yield list of restraints.
processRestraintsFile fname = do (restraints, errors) <- parseRestraintsFile fname
                                 forM errors $ \msg -> do hPutStrLn stderr $ concat [ "ERROR parsing restraints file "
                                                                                    , fname
                                                                                    , ": "
                                                                                    , msg                              ]
                                 return restraints



