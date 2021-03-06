{-# LANGUAGE OverloadedStrings #-}
-- | Parsing of ROSETTA RDC restraint format. It is separate from all
-- other kinds of restraints.
module Rosetta.RDC where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector           as V
import qualified Data.Vector.Generic   as VG
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Unboxed.Base as VUB
import System.IO(stderr)
import Control.Monad(when)
import Control.Arrow((***))
import Data.List(intercalate, sort)
import Numeric(showFFloat)

import Statistics.Sample.KernelDensity

import Rosetta.Restraints(AtomId(..))
import Rosetta.Util

-- | A single RDC restraint.
data RDCRestraint = RDCR { at1, at2           :: !AtomId
                         , rdcValue           :: !Double
                         }
-- TODO: implement proper Show

-- | Set of RDC restraints.
newtype RDCSet = RDCSet { unRDCSet :: V.Vector RDCRestraint }

instance Show RDCSet
  where
    show = intercalate "\n" . map show . V.toList . unRDCSet

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
                                   return RDCR { at1      = AtomId "" at1 resi1
                                               , at2      = AtomId "" at2 resi2
                                               , rdcValue = val
                                               }
  where
    [lbl, resi1str, at1, resi2str, at2, val] = ws
    withLineNo bs = BS.concat [bs, "in line #", bshow lineNo]
    ws            = BS.words  line

-- | Parses RDC restraint input from BS.ByteString, and returns list of
-- restraints and errors.
--
-- TODO: hint at restraint set size to get faster code (and check its validity.)
parseRDCRestraints :: BS.ByteString -> [Either BS.ByteString RDCRestraint]
parseRDCRestraints input = result ++ lenErr
  where
    lenErr = case parseInt "number of records" $ lines !! 3 of
               Left  msg            -> [Left msg]
               Right expectedLength ->
                   [Left $ BS.concat [ "Found ",                     bshow lenResult
                                     , " restraints, but expected ", bshow expectedLength ]
                     | lenResult /= expectedLength]
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

-- | Shows Kernel Density Estimation of a distribution.
-- May be used to write it to GNUPlot .dat file.
showKDE = foldr mkRow ""                                .
          uncurry zip                                   .
          (VG.toList *** VG.toList)                     .
          Statistics.Sample.KernelDensity.kde kdePoints .
          VG.convert                                    .
          V.map rdcValue                                .
          unRDCSet
  where
    mkRow ::  (Show a, Show a1) => (a, a1) -> ShowS
    mkRow (x, y) = shows x . (' ':) . shows y . ('\n':)

-- | Number of points in Kernel Density Estimation of RDC distribution.
kdePoints = 20

-- | Description of parameters of RDC distribution.
-- Convenience datatype to hold a variety of results returned by
-- rdcParameters.
data RDCParams = RDCParams { d_a, d_r, r, rdc_min, rdc_max, rdc_mode,
                             d_a2, r2, trace, d_xx, d_yy, d_zz :: Double }

instance Show RDCParams where
  show params = intercalate " and " $ map showf [("D_ax",      d_a     )
                                                ,("D_rh",      d_r     )
                                                ,("D_a_2",     d_a2    )
                                                ,("R1",        r       )
                                                ,("R2",        r2      )
                                                ,("min(RDC)",  rdc_min )
                                                ,("max(RDC)",  rdc_max )
                                                ,("mode(RDC)", rdc_mode)
                                                ,("tr(D)",     trace   )
                                                ,("D_xx",      d_xx    )
                                                ,("D_yy",      d_yy    )
                                                ,("D_zz",      d_zz    )
                                                ]
    where
      showf :: (String, RDCParams -> Double) -> String
      showf (label, fun) = label ++ ('=':showFFloat (Just 3)
                                                    (fun params) "")

-- TODO: split atom pairs into distinct sets, and solve params separately for each set!
-- Or scale to common base, e.g. max(D_NH)
-- | Computes descriptive parameters of RDC distribution.
-- For now doesn't distinguish atom types.
rdcParameters :: RDCSet -> RDCParams
rdcParameters rdcSet = RDCParams d_a      d_r     r
                                 rdc_min  rdc_max rdc_mode
                                 d_a2     r2      aSum
                                 rdc_mode rdc_min rdc_max
  where
    -- Computing 5 minimal and 5 maximal elements
    aSum = rdc_min + rdc_max + rdc_mode
    aList :: [Double]
    aList = sort $ map rdcValue $ V.toList $ unRDCSet rdcSet
    (kdeMesh, kdeValues) = Statistics.Sample.KernelDensity.kde kdePoints $
                             VG.convert $ V.map rdcValue $ unRDCSet rdcSet 
    rdc_mode :: Double
    rdc_mode = kdeMesh VG.! VG.maxIndex kdeValues
    numExtremal = 5
    minimal = take 5                                aList
    maximal = drop (V.length (unRDCSet rdcSet) - 5) aList
    -- Computing minimum and maximum RDC estimate
    avg l   = sum l / fromIntegral (length l)
    rdc_min = avg minimal
    rdc_max = avg maximal
    --rdc_min = minimum aList
    --rdc_max = maximum aList
    -- Computing D_a
    d_a  = (rdc_min + rdc_mode)/(-2)/rdc_max
    d_a2 = rdc_max/2.0 
    d_r  = (rdc_min - rdc_mode)/(-3)/rdc_max
    r    = d_r/d_a
    r2   = (rdc_min - rdc_mode)/(rdc_min + rdc_mode)/2

{-
http://cwp.embo.org/wpc09-07/lecture/zweckstetterRDC.pdf

Tr D = 0 => Dxx + Dyy + Dzz == 0
-- Maximum (maybe just use as upper bound?)
Dzz = 2*Da

-- Minimum
Dyy = –Da (1 + 1.5*R)

-- Mode
Dxx = -Da (1 - 1.5*R)
To be computed by nonlinear minimization from histogram of the ensemble?
R= Dr/Da

should be
Dxx + Dyy + Dzz == Tr D == 0

Dyy + Dxx ~= -2Da ~= Dzz
Dyy - Dxx ~= -3R*Da=-3Dr

(Dyy - Dxx) / (Dyy + Dxx) == R

Note: Dyy - Dxx = -Da (1-1+1.5R+1.5R) = -Da(0+3*R) = -Da * 3 * R = -3*Dr
Dr = (Dyy - Dxx)/(-3)
 -}

