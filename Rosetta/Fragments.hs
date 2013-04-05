{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | File for reading ROSETTA 3.x fragment format.
module Rosetta.Fragments( RFrag   (..)
                        , RFragRes(..)
                        , RFragSet(..)
                        , parseFragments
                        , parseFragmentsFile
                        , processFragmentsFile
                        ) where

import Data.Typeable
import Data.Data
import System.IO              (hPrint, stderr)
import Control.Monad          (when, forM_)
import Control.Monad.Instances()
import Control.DeepSeq        (NFData(..))
import Control.Exception      (assert)
import Data.Either            (partitionEithers)
import Rosetta.Util(parse, parseFloat3, parseInt, bshow)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

import Rosetta.SS

-- | Set of fragment sites, with each having a vector of fragments picked for the site.
newtype RFragSet = RFragSet { unRFragSet :: V.Vector (V.Vector RFrag) }

instance NFData RFragSet where
  rnf = V.foldl' (V.foldl' $ flip seq) () . unRFragSet

-- | Record describing single residue within a fragment.
data RFragRes = RFragRes { rescode         :: !Char
                         , ss              :: !SSCode
                         , phi, psi, omega :: !Double
                         }
  deriving (Show, Read, Typeable, Data)

instance NFData RFragRes where

-- | A single fragment.
data RFrag = RFrag { startPos :: !Int
                   , endPos   :: !Int
                   , res      :: V.Vector RFragRes
                   }
  deriving (Show, Read, Typeable)

instance NFData RFrag where
  rnf a = V.foldl' (flip seq) () $ res a

-- | Temporary fragment holder during parsing
data TFrag = TFrag { tStartPos :: !Int     ,
                     tEndPos   :: !Int     ,
                     tRes      :: [RFragRes]
                   }
  deriving (Show, Read, Typeable, Data)

{- Example entry:
FRAME    1   3
         1     1 1ryi BBTorsion G L     84.947  -172.572  -169.926
         2     1 1ryi BBTorsion M E   -128.774   133.712   162.000
         3     1 1ryi BBTorsion F E    -96.435   112.599   164.256

         1     1 1q8f BBTorsion G L    -76.588   -11.404  -173.341
         2     1 1q8f BBTorsion I E   -131.601   131.262   171.338
         3     1 1q8f BBTorsion K E    -90.250   125.167   173.159

FRAME    2   4
         1     2 1wcu BBTorsion K E   -130.800   129.900   171.200
         2     2 1wcu BBTorsion M E    -93.700   124.300   171.500
         3     2 1wcu BBTorsion S E   -132.400   178.400   177.600

FRAME  181 183
         1   181 1te5 BBTorsion R H    -67.409   -35.897  -179.220
         2   181 1te5 BBTorsion R H    -71.338   -47.943   180.402
         3   181 1te5 BBTorsion A H    -72.098   -25.972  -178.065

FRAME <target_seq_startPos> <target_seq_endPos>
       <resid_in_fragment_no> <target_seq_startPos> <pdbid of origin> BBTorsion <aa FASTA code> <SS code> <phi angle> <psi angle> <omega angle>

NOTE: check that omega is omega, not chi. Normally omega = +-180+-10?
-}
-- | Parses a single residue entry within a fragment.
parseFragEntry :: [BS.ByteString] -> Either BS.ByteString (Int, Int, RFragRes)
parseFragEntry ws = do entry_no <- parseInt    "entry"  entry_no_s
                       pos      <- parseInt    "pos"    pos_s
                       ss       <- parse       "sscode" sscode_s
                       phi      <- parseFloat3 "phi"    phi_s
                       psi      <- parseFloat3 "psi"    psi_s
                       omega    <- parseFloat3 "omega"  omega_s
                       when (BS.length rescode /= 1) $ Left $! BS.concat ["rescode which is not a single character, but ", bshow rescode]
                       return (entry_no, pos,
                               RFragRes { rescode = BS.head rescode,
                                          ss      = ss     ,
                                          phi     = phi    ,
                                          psi     = psi    ,
                                          omega   = omega 
                                        })
  where
    [entry_no_s, pos_s, pdbid, "BBTorsion", rescode, sscode_s,
     phi_s, psi_s, omega_s] = ws

-- | Parses a single FRAME of fragment file (which contains set of fragments
--   that start at a given residue, and end at the other given residue.)
parseFrame ws = do startPos <- parse "FRAME starting position" startPos_s
                   endPos   <- parse "FRAME ending position"   endPos_s
                   return $! TFrag startPos endPos []
  where
    [_frame_string, startPos_s, endPos_s] = ws

-- | Reads a line, completing a temporary fragment TFrag object.
readLine' :: [(Int, BS.ByteString)] -> TFrag -> [Either BS.ByteString TFrag]
readLine' []                    tfrag                                  = addFragment tfrag (-1) [] -- fix -1 as meaning end of file
readLine' ((lineNo, line):rest) curFrag | "FRAME" `BS.isPrefixOf` line = -- yield fragment and then continue parsing
        case parseFrame $ BS.words line of
          Right newRFrag -> addFragment curFrag lineNo $ readLine' rest newRFrag
          Left  errMsg   -> mkError     lineNo errMsg  : readLine' rest curFrag
readLine' ((lineNo, line):rest) curFrag | null $ BS.words line         = -- finish fragment
        addFragment curFrag lineNo $ readLine' rest $ curFrag { tRes = [] }
readLine' ((lineNo, line):rest) curFrag                                = -- add entry to a current fragment
        case parseFragEntry $ BS.words line of
          -- TODO: validate entry_no, pos
          Right (entry_no, pos, rfentry) -> readLine' rest $ curFrag { tRes = rfentry : tRes curFrag }
          Left  msg                      -> mkError lineNo msg : readLine' rest curFrag
  where
    cleanFrag tfrag = tfrag { tRes = [] }

-- | Enriches error message with a line number.
mkError lineNo msg = Left $ BS.concat ["Error in line ", bshow lineNo, " parsing ", msg]

-- | Finalizes a temporary fragment TFrag object, and adds it to a list of fragments.
--   Throws error message with line number, if anything goes wrong.
addFragment tfrag lineNo fragList | null (tRes tfrag)          = fragList
addFragment tfrag lineNo fragList | fragLen /= expected_length = mkError lineNo errMsg                       : fragList
  where
    fragLen         = length $ tRes tfrag
    errMsg          = BS.concat ["fragment length is supposed to be ", bshow expected_length, " but is ", bshow fragLen, "."]
    expected_length = tEndPos tfrag - tStartPos tfrag + 1
addFragment tfrag lineNo fragList                              = Right (tfrag { tRes = reverse $ tRes tfrag }): fragList

-- | Groups fragments by the site (starting position.)
groupFragments []     []                                  = [  ]
groupFragments []     fs                                  = [fs]
groupFragments (f:fs) []                                  =    groupFragments fs [f]
groupFragments (f:fs) gs@(g:_) | startPos f == startPos g =    groupFragments fs (f:gs)
groupFragments (f:fs) gs@(g:_)                            = gs:groupFragments fs [f]

-- | Creates `RFragSet` from a list of fragments grouped by their site.
vectorizeFragmentLists ::  [[RFrag]] -> RFragSet
vectorizeFragmentLists listOfLists = assertions $ RFragSet $
                                       V.replicate len V.empty V.// vectorsWithIndices
  where
    len                = maximum indices + 1
    indices            = map ((+(-1)) . startPos . head) listOfLists
    vectorsWithIndices = zip indices $ map V.fromList listOfLists
    assertions         = assert $ len == length listOfLists

-- | Parses fragments from input given as a ByteString,
--   and yields list of error messages, and a list of fragments.
parseFragments :: BS.ByteString -> ([BS.ByteString], RFragSet)
parseFragments input = (errs, vectorizeFragmentLists $ groupFragments frags [])
  where
    (errs, frags) = partitionEithers . map finalize $ readLine' (zip [1..] $ BS.lines input) (TFrag (-1) (-1) [])
    finalize :: Either BS.ByteString TFrag -> Either BS.ByteString RFrag
    finalize = either (Left . id) (Right . finalizeTFrag)

-- | Converts temporary TFrag object to a final `RFrag` object.
finalizeTFrag (TFrag start end res) = RFrag start end (V.fromList res)

-- | Read fragments from a given file, and returns two lists: fragments, and errors.
parseFragmentsFile :: FilePath -> IO ([BS.ByteString], RFragSet)
parseFragmentsFile fname = parseFragments `fmap` BS.readFile fname

-- | Reads a fragment set form a file with given filename,
-- prints all error messages to stderr, and returns a list of fragments.
processFragmentsFile :: FilePath -> IO RFragSet
processFragmentsFile fname = do (errs, frags) <- parseFragmentsFile fname
                                forM_ errs $ 
                                  \e -> BS.hPutStrLn stderr $ BS.concat ["Error in fragments file ", BS.pack fname, ": ", e]
                                return $! frags -- TODO: strict spine of the list?


