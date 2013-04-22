{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of ROSETTA RDC restraint format. It is separate from all
-- other kinds of restraints.
module Rosetta.RDC where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import System.IO(stderr)
import Control.Monad(when)
import Data.List(intercalate)
import Numeric(showFFloat)

import Rosetta.Restraints(AtomId(..))
import Rosetta.Util

-- | Set of RDC restraints.
newtype RDCSet = RDCSet { unRDCSet :: V.Vector RDCRestraint }

instance Show RDCSet
  where
    show = intercalate "\n" . map show . V.toList . unRDCSet

-- | A single RDC restraint.
data RDCRestraint = RDCR { at1, at2           :: !AtomId
                         , rdcValue           :: !Double
                         }
-- TODO: implement proper Show

instance Show RDCRestraint where
  showsPrec _ r = ("RDC "++) . shows (at1 r) . (' ':) . shows (at2 r) . (' ':) . showFFloat (Just 2) (rdcValue r)

-- a    1  HN      1  N         7.67
-- | Parses a single RDC restraint.
parseRDCRestraint  :: Int -> BS.ByteString -> Either BS.ByteString RDCRestraint
parseRDCRestraint lineNo line = do when (length ws /= 6) $ Left $ BS.concat ["Expecting 6 entries in line "
                                                                            , bshow lineNo
                                                                            , ", but found "
                                                                            , bshow $ length ws, "."]
                                   when (lbl /= "a") $ Left $ BS.concat ["Expected record label, but found ", lbl]
                                   resi1 <- parseInt   (withLineNo "left residue number" )         resi1str
                                   resi2 <- parseInt   (withLineNo "right residue number")         resi2str
                                   val   <- parseFloat (withLineNo "RDC value"           ) Nothing val
                                   return $! RDCR { at1      = AtomId "" at1 resi1
                                                  , at2      = AtomId "" at2 resi2
                                                  , rdcValue = val
                                                  }
  where
    [lbl, resi1str, at1, resi2str, at2, val] = ws
    withLineNo bs = BS.concat [bs, "in line #", bshow lineNo]
    ws = BS.words line

-- | Parses RDC restraint input from BS.ByteString, and returns list of
-- restraints and errors.
--
-- TODO: hint at restraint set size to get faster code (and check its validity.)
parseRDCRestraints :: BS.ByteString -> [Either BS.ByteString RDCRestraint]
parseRDCRestraints input = result ++ lenErr
  where
    lenErr = case parseInt "number of records" $ lines !! 3 of
               Left  msg            -> [Left msg]
               Right expectedLength -> if lenResult /= expectedLength
                                         then [Left $ BS.concat [ "Found ",                     bshow lenResult
                                                                , " restraints, but expected ", bshow expectedLength ]]
                                         else []
    restraintLines   = drop 4 lines
    lines            = BS.lines input
    lenResult        = length result
    result           = zipWith parseRDCRestraint [1..] restraintLines

-- | Applies a function to the list, and prepends results to the other list.
mapAppend f argList leftOver = foldr (\a b -> f a:b) leftOver argList

-- | Reads an RDC restraints in ROSETTA 2.x format, and returns an RDCSet.
parseRDCRestraintsFile :: BS.ByteString -> IO RDCSet
parseRDCRestraintsFile fname = do rdcEvts <- parseRDCRestraints `fmap` BS.readFile (BS.unpack fname)
                                  goodEvts <- mapM (either report pack) rdcEvts
                                  return $! RDCSet $ V.fromList $ concat goodEvts
  where
    report msg = do BS.hPutStrLn stderr msg
                    return []
    pack   x   = return [x]

