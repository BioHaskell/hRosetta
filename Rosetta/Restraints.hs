{-# LANGUAGE OverloadedStrings #-}
module Rosetta.Restraints(parse, exampleString, splitsAt) where

import Prelude
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Control.Monad(when)
import Control.Monad.Instances()

data Restraint = DistR { at1, at2 :: AtomId ,
                         goal     :: Double }
               | DiheR { at1, at2, at3, at4 :: AtomId ,
                         goal               :: Double }
  deriving (Read, Eq)
-- TODO: define Show that prints out Rosetta format?

instance Show Restraint where
  showsPrec n (DistR at1 at2 goal) s = ("AtomPair "++) . showsAt at1 . (" GAUSSIAN "++) . showsPrec n goal . (" 1.00"++) $ s
    where
      showsAt :: AtomId -> String -> String
      showsAt at s = showsPrec n (resId at) $ " " ++ BS.unpack (resName at) ++ " " ++ BS.unpack (atName at) ++ s

data AtomId = AtomId { resName :: BS.ByteString, -- may be empty!
                       atName  :: BS.ByteString,
                       resId   :: Int
                     }
  deriving (Show, Read, Eq)
-- TODO: define Eq that ignores missing resname
-- TODO: define nicer Show/Read

splitsAt (i:is) l = x:splitsAt (map (+(-i)) is) r
  where
    (x, r) = Prelude.splitAt i l
splitsAt [] l = [l]

exampleString = BS.concat ["AtomPair  78 VAL CA    1 VAL CA GAUSSIAN 4.80 1.00\n"            ,
                           "Dihedral  N 307 CA 307 C 307 N 308 BOUNDED 2.20 2.69 0.52 DIHE\n",
                           "AtomPair  148 ASN CG  185 GLU CB BOUNDED 0.00 7.70 1.00 EXPDAT\n"]

parseFunc lineNo (funcs:spec) = if funcs == "GAUSSIAN"
                                  then do when (length spec <2) $ Left $ "Not enough arguments to GAUSSIAN in line " ++ show lineNo
                                          parseFloat lineNo $ spec !! 0
                                  else if funcs == "BOUNDED"
                                         then do when (length spec <=3) $ Left $ "Not enough arguments to BOUNDED in line " ++ show lineNo
                                                 parseFloat lineNo $ spec !! 1
                                         else Left $ "Unknown function name " ++ (show $ BS.unpack $ funcs)

parseFloat lineNo floatStr = case reads $ BS.unpack floatStr of
                        [(f, [])] -> Right f
                        otherwise -> Left $ "Cannot parse float " ++ show floatStr ++ " in line " ++ show lineNo


parsePair lineNo ws = do when (len <= 8) $ Left $ "Too few ("++ show len ++") words in AtomPair line!"
                         [at1, at2] <- mapM (mkAtId3 lineNo) [at1s, at2s]
                         goal <- parseFunc lineNo funcs
                         Right $ DistR at1 at2 goal
  where
    len = length ws
    [at1s, at2s, funcs] = splitsAt [3, 6] ws

mkAtId3 lineNo [residStr, resname, atName] =
    case reads $ BS.unpack residStr of
      [(resid, "")] -> Right $ AtomId { resName = resname,
                                        atName  = atName ,
                                        resId   = resid  }
      otherwise     -> Left ("Cannot parse atom id string " ++
                             BS.unpack residStr ++ " in line " ++
                             show lineNo)


mkAtId2 lineNo [atName, resnum] = mkAtId3 [resnum, "", atName]

parseDihe lineNo ws = Left "not implemented"

parseRestraint :: Int -> BS.ByteString -> Either String Restraint
parseRestraint lineNo line = if recType == "AtomPair"
                               then parsePair lineNo rec
                               else parseDihe lineNo rec
  where
    recType:rec = BS.words line

parse :: BS.ByteString -> ([Restraint], [String])
parse input = (restraints, errs)
  where
    (errs, restraints) = partitionEithers             .
                         map (uncurry parseRestraint) .
                         zip [1..]                    .
                         BS.lines                     $ input


