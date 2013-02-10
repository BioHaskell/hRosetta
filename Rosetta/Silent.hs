{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Rosetta.Silent where

import System.IO(stderr)
import Control.Monad.Instances
import Control.Monad(when, forM_)
import Data.List(unfoldr)
import Data.Either(partitionEithers)
import qualified Data.ByteString.Char8 as BS
import Prelude

import Rosetta.SS

data SilentEvent = Rec SilentRec
                 | ScoreHeader [BS.ByteString]
                 | Score       { values      :: [Double]
                               , description :: BS.ByteString }
                 | Seq         BS.ByteString
  deriving (Show)

data SilentRec = SilentRec { resId :: Int
                           , ss    :: SSCode -- use SSType
                           }
  deriving (Show)

data SilentModel = SilentModel { name     :: BS.ByteString
                               , scores   :: [(BS.ByteString, Double)]
                               , residues :: [SilentRec]
                               }
  deriving (Show)

-- TODO: nice way of merging error streams
--mergeEither f (Left  e:es) = Left e:mergeEither f es
--mergeEither f (Right e:es) = mergeEither f es
-- f :: [a] -> [Either b c]
-- g :: [Either b c]
-- f .. g :: [c] -> [
-- Maybe available in Control.Arrow? or Control.Applicative?
parseSilent :: BS.ByteString -> BS.ByteString -> ([BS.ByteString], [SilentModel])
parseSilent fname input = (allErrors, mdls)
  where
    allErrors     = errs ++ merrs
    (errs,  evts) = parseSilentEvents input
    (merrs, mdls) = partitionEithers  $ parseSilent' evts
    parseSilent' :: [SilentEvent] -> [Either BS.ByteString SilentModel]
    parseSilent' (Seq seq:ScoreHeader lbls:r) = unfoldr (buildModel $ init lbls) r

processSilent :: BS.ByteString -> IO [SilentModel]
processSilent fname = do input <- BS.readFile $ BS.unpack fname
                         let (errs, mdls) = parseSilent fname input
                         processErrors fname errs
                         return $! mdls

--TODO: validate models in buildModel
buildModel :: [BS.ByteString] -> [SilentEvent] -> Maybe (Either BS.ByteString SilentModel, [SilentEvent])
buildModel lbls []             = Nothing
buildModel lbls (Score s n:rs) = takeModel rs [] (mCont lbls s n)
  where
    mCont :: [BS.ByteString] -> [Double] -> BS.ByteString -> [SilentRec] -> [SilentEvent] -> Maybe (Either BS.ByteString SilentModel, [SilentEvent])
    mCont lbls scores n recs rs | length scores == length lbls = Just $ (Right $ SilentModel { scores   = zip lbls scores
                                                                                             , residues = recs
                                                                                             , name     = n
                                                                                             }
                                                                        , rs)
    mCont lbls scores n rec rs = Just $ (Left $ BS.concat ["Score list for model "
                                                          , n
                                                          , " is of different length than headers: "
                                                          , BS.pack $ show scores
                                                          , " vs "
                                                          , BS.pack $ show lbls  ]
                                        , rs)
    takeModel :: [SilentEvent] -> [SilentRec] -> ([SilentRec] -> [SilentEvent] -> a) -> a
    takeModel []                aList cont = cont (reverse aList) []
    takeModel rs@(Score _ _:_ ) aList cont = cont (reverse aList) rs
    takeModel    (Rec   r  :rs) aList cont = takeModel rs (r:aList) cont
buildModel lbls (r:_)        = error $ "buildModel with argument starting with " ++ show r

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
    parseScoreOrHeader cols             = do vals :: [Double] <- sequence $ zipWith parseCol (init cols) [1..]
                                             return $ Score { values      = vals
                                                            , description = (last cols) }
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

processErrors fname errs = forM_ errs $ \s -> BS.hPutStrLn stderr $
                                                BS.concat ["Error parsing ", fname, ":", s]

processSilentEvents fname = do (errs, evts) <- parseSilentEvents `fmap` BS.readFile (BS.unpack fname)
                               processErrors fname errs
                               return $! evts

