{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Rosetta.Silent where

import System.IO(stderr)
import Control.Monad.Instances
import Control.Monad(when, forM)
import Data.Either(partitionEithers)
import qualified Data.ByteString.Char8 as BS
import Prelude

import Rosetta.SS

data SilentEvent = Rec SilentRec
                 | ScoreHeader [BS.ByteString]
                 | Score       [Double]
                 | Seq         BS.ByteString
data SilentRec = SilentRec { resId :: Int
                           , ss    :: SSCode -- use SSType
                           }
  deriving (Show)

{-
SEQUENCE: VLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKKGSGSGSGSGSGSGSGSVLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKKGSGSGSGSGSGSGSGSVLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKKGSGSGSGSGSGSGSGSVLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKKGSGSGSGSGSGSGSGSVLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKK
SCORE:      score     env    pair     vdw      hs      ss   sheet      cb  rsigma hb_srbb hb_lrbb      rg      co contact    rama   bk_tot   fa_atr   fa_rep   fa_sol  h2o_sol     hbsc   fa_dun fa_intra  fa_pair fa_plane  fa_prob   fa_h2o   h2o_hb    gsolt     sasa      pc pc_viol omega_sc rlxfilt1 rlxfilt2 description
SCORE:    2954.02   44.38  -99.13   11.30    0.00 -101.36   21.74   82.24  -11.74  -12.96  -35.55   29.68   82.95    0.00 1049.23  2369.39  -957.50   610.63   547.90     0.00   -43.91   307.99     0.97   -18.23     0.00   -43.19     0.00     0.00   323.27 22609.70 2069.07 3071.97   693.44  5446.99  2958.41 S_0319_8954
   1 L     0.000   17.891 -171.655    0.000    0.000    0.000  -81.139    0.000    0.000    0.000 S_0319_8954
 -}

parseSilentEventLine :: BS.ByteString -> Either BS.ByteString SilentEvent
parseSilentEventLine line = parse' $ BS.words line
  where
    parse' :: [BS.ByteString] -> Either BS.ByteString SilentEvent
    parse' ("SCORE:"    :s) = parseScoreOrHeader s
    parse' ("SEQUENCE:" :s) = parseSequence      s
    parse' []               = Left ""
    parse' (numStr:ssStr:_) = do i  :: Int    <- parse "residue number"           numStr
                                 ss :: SSCode <- parse "secondary structure code" ssStr
                                 return $ Rec $ SilentRec i ss
    parse :: (Read a) => BS.ByteString -> BS.ByteString -> Either BS.ByteString a
    parse recName str = case reads $ BS.unpack str of
                          [(i, [])] -> Right i
                          _         -> Left $ BS.concat ["Cannot parse ", recName,
                                                         " ", BS.pack (show str) ]
    parseScoreOrHeader :: [BS.ByteString] -> Either BS.ByteString SilentEvent
    parseScoreOrHeader lbls@("score":_) = Right $ ScoreHeader lbls
    parseScoreOrHeader vals             = do vals :: [Double] <- sequence $ zipWith parseCol vals [1..]
                                             return $ Score vals
      where
        parseCol :: BS.ByteString -> Int -> Either BS.ByteString Double
        parseCol col num = parse ("score column " `BS.append` BS.pack (show num)) col
    parseSequence [seq] = Right $ Seq seq
    parseSequence r     = Left $ BS.concat ["Cannot parse sequence record with words:", BS.pack $ show r]
-- TODO: parse other parts of each entry

-- TODO: parse header
-- TODO: parse SCORE: entries and join records within each model
parseSilentEvents :: BS.ByteString -> ([BS.ByteString],
                                       [SilentEvent]  )
parseSilentEvents = partitionEithers . map parseSilentEventLine . BS.lines

processSilentEvents fname = do (errs, evts) <- parseSilentEvents `fmap` BS.readFile (BS.unpack fname)
                               forM errs $ \s -> BS.hPutStrLn stderr $
                                                   BS.concat ["Error parsing ", fname, ":", s]
                               return $! evts

-- TODO: make an output data structure
processSilent = processSilentEvents

