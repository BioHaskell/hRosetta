{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable, NoMonomorphismRestriction #-}
module Rosetta.Silent( SilentModel(..)
                     , SilentRec  (..)
                     , parseSilent
                     , parseSilentFile
                     , processSilentFile

                     , writeSilent
                     , writeSilentFile

                     , parseSilentEvents
                     , processSilentEvents

                     , modelScore
                     , modelScoreIfAvailable
                     , bestSilentModel
                     , sortModelsByScore
                     ) where

import System.IO(stderr, withFile, Handle, IOMode(WriteMode))
import Control.Monad.Instances
import Control.Monad(when, forM_)
import Data.List(unfoldr, minimumBy, sortBy)
import Data.Either(partitionEithers)
import qualified Data.ByteString.Char8 as BS
import Prelude hiding(seq)
import Data.Typeable
import Data.Data
import qualified Data.Set as Set
import Data.List(foldl1')
import Numeric(showFFloat)

import Rosetta.SS

data SilentEvent = Rec         { unRec        :: SilentRec }
                 | ScoreHeader { labels
                               , descLabels :: [BS.ByteString] }
                 | Score       { values       :: [Double       ]
                               , descriptions :: [BS.ByteString]
                               }
                 | Seq         BS.ByteString
  deriving (Show, Data, Typeable)

data SilentRec = SilentRec { resId                  :: Int
                           , ss                     :: SSCode -- use SSType
                           , phi, psi, omega        :: Double
                           , caX, caY, caZ          :: Double
                           , chi1, chi2, chi3, chi4 :: Double
                           }
  deriving (Show, Data, Typeable)
-- TODO: replace Show/Read with parser and printer for ROSETTA format.

data SilentModel = SilentModel { name              :: BS.ByteString
                               , otherDescriptions :: [(BS.ByteString, BS.ByteString)]
                               , scores            :: [(BS.ByteString,
                                                        Double       )]
                               , residues          :: [SilentRec]
                               , fastaSeq          :: BS.ByteString
                               }
  deriving (Show, Data, Typeable)


showSequence seq = "SEQUENCE: " `BS.append` seq
-- TODO: memoize genLabels result somehow
genLabels labels descLabels = labels ++ descLabels ++ ["description"]
-- TODO: what to do when label sets are inconsistent? (Take set maximum, BUT preserve ordering.)
scoreColumns       scores descs      = map (max 8 . BS.length) $ genLabels scores descs
showScoreLine cols scores descs name = BS.intercalate " " . zipWith adj cols $ ["SCORE:"] ++ scores ++ descs ++ [name]

adj i s = BS.replicate (i - BS.length s) ' ' `BS.append` s

writeSilent :: Handle -> [SilentModel] -> IO ()
writeSilent handle mdls = do BS.hPutStrLn handle $ showSequence $ fastaSeq mdl
                             BS.hPutStrLn handle $ showScoreLine colLens scoreLbls descLbls "description"
                             forM_ mdls writeModel
  where
    mdl       = head mdls -- assuming all have the same score header
    colLens   = scoreColumns scoreLbls descLbls
    scoreLbls = map fst $ scores            mdl
    descLbls  = map fst $ otherDescriptions mdl
    writeModel :: SilentModel -> IO ()
    writeModel mdl = do BS.hPutStrLn handle $ showScoreLine colLens (map (showScoreCol . snd) $ scores            mdl)
                                                                    (map snd                  $ otherDescriptions mdl)
                                                                    (name mdl)
                        forM_ (residues mdl) $ BS.hPutStrLn handle . showSilentRecord (name mdl)
    showScoreCol x = BS.pack $ showFFloat (Just 2) x ""
    showSilentRecord :: BS.ByteString -> SilentRec -> BS.ByteString
    showSilentRecord name rec = BS.intercalate " " $ [adj 4 $ bshow $ resId $ rec, bshow $ ss $ rec, ""] ++ scoreStrings ++ [name]
      where
        scoreStrings = map (showCoordCol . (flip ($) rec)) [phi, psi, omega, caX, caY, caZ, chi1, chi2, chi3, chi4]
    showCoordCol x = adj 8 $ BS.pack $ showFFloat (Just 3) x ""
    bshow :: (Show a) => a -> BS.ByteString
    bshow = BS.pack . show

writeSilentFile :: FilePath -> [SilentModel] -> IO ()
writeSilentFile fname mdls = withFile fname WriteMode $ flip writeSilent mdls

{- FORMAT EXAMPLE:
SEQUENCE: VLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKKGSGSGSGSGSGSGSGSVLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKKGSGSGSGSGSGSGSGSVLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKKGSGSGSGSGSGSGSGSVLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKKGSGSGSGSGSGSGSGSVLYVGSKTKEGVVHGVATVAEKTKEQVTNVGGAVVTGVTAVAQKTVEGAGSIAAATGFVKK
SCORE:      score     env    pair     vdw      hs      ss   sheet      cb  rsigma hb_srbb hb_lrbb      rg      co contact    rama   bk_tot   fa_atr   fa_rep   fa_sol  h2o_sol     hbsc   fa_dun fa_intra  fa_pair fa_plane  fa_prob   fa_h2o   h2o_hb    gsolt     sasa      pc pc_viol omega_sc rlxfilt1 rlxfilt2 description
SCORE:    2954.02   44.38  -99.13   11.30    0.00 -101.36   21.74   82.24  -11.74  -12.96  -35.55   29.68   82.95    0.00 1049.23  2369.39  -957.50   610.63   547.90     0.00   -43.91   307.99     0.97   -18.23     0.00   -43.19     0.00     0.00   323.27 22609.70 2069.07 3071.97   693.44  5446.99  2958.41 S_0319_8954
   1 L     0.000   17.891 -171.655    0.000    0.000    0.000  -81.139    0.000    0.000    0.000 S_0319_8954
 -}
-- TODO: nice way of merging error streams
--mergeEither f (Left  e:es) = Left e:mergeEither f es
--mergeEither f (Right e:es) = mergeEither f es
-- f :: [a] -> [Either b c]
-- g :: [Either b c]
-- f .. g :: [c] -> [
-- Maybe available in Control.Arrow? or Control.Applicative?

-- | Parses input filename and contents as a Silent file format input.
--   Filename is first argument, and is prepended to all error messages.
--   Result is a tuple of list of error messages, and `SilentModel`s.
parseSilent :: BS.ByteString -> BS.ByteString -> ([BS.ByteString], [SilentModel])
parseSilent fname input = (allErrors, mdls)
  where
    allErrors     = errs ++ merrs
    (errs,  evts) = parseSilentEvents input
    (merrs, mdls) = partitionEithers  $ parseSilent' evts
    parseSilent' :: [SilentEvent] -> [Either BS.ByteString SilentModel]
    parseSilent' (Seq seq:ScoreHeader lbls descLbls:r) = unfoldr (buildModel seq lbls descLbls) r

parseSilentFile :: BS.ByteString -> IO ([BS.ByteString], [SilentModel])
parseSilentFile fname = do input <- BS.readFile $ BS.unpack fname
                           return $ parseSilent fname input

processSilentFile :: BS.ByteString -> IO [SilentModel]
processSilentFile fname = do (errs, mdls) <- parseSilentFile fname
                             processErrors fname errs
                             return $! mdls

--TODO: validate models in buildModel
buildModel :: BS.ByteString ->
              [BS.ByteString] ->
              [BS.ByteString] ->
              [SilentEvent] ->
              Maybe (Either BS.ByteString SilentModel
                    ,[SilentEvent])
buildModel mSeq lbls descLbls []             = Nothing
buildModel mSeq lbls descLbls (Score s ds:rs) = takeModel rs [] $ mCont mSeq lbls descLbls s ds
  where
{- mCont :: BS.ByteString   ->
             [BS.ByteString] ->
             [BS.ByteString] ->
             [Double]        ->
             [BS.ByteString] ->
             BS.ByteString   ->
             [SilentRec]     ->
             [SilentEvent]   ->
             Maybe ( Either BS.ByteString SilentModel
                   , [SilentEvent] ) -}
    mCont mSeq lbls descLbls scores descs recs rs | length scores == length lbls =
                                                    Just $ (Right $ SilentModel { scores   = zip lbls scores
                                                                                , residues = recs
                                                                                , name     = last descs
                                                                                , otherDescriptions = zip descLbls $ init descs
                                                                                , fastaSeq = mSeq
                                                                                }
                                                           , rs)
    mCont mSeq lbls descLbls scores descs rec rs  = Just $ (Left $ BS.concat ["Score list for model "
                                                                             , last descs
                                                                             , " is of different length than headers: "
                                                                             , BS.pack $ show scores
                                                                             , " vs "
                                                                             , BS.pack $ show lbls   ]
                                                           , rs)
    takeModel :: [SilentEvent] -> [SilentRec] -> ([SilentRec] -> [SilentEvent] -> a) -> a
    takeModel []                aList cont = cont (reverse aList) []
    takeModel rs@(Score _ _:_ ) aList cont = cont (reverse aList) rs
    takeModel    (Rec   r  :rs) aList cont = takeModel rs (r:aList) cont
buildModel mSeq lbls descLbls (r:_)          = error $ "buildModel with argument starting with " ++ show r

parseSilentEventLine :: BS.ByteString -> Either BS.ByteString SilentEvent
parseSilentEventLine line = parse' $ BS.words line
  where
    parse' :: [BS.ByteString] -> Either BS.ByteString SilentEvent
    parse' ("SCORE:"              :s) = parseScoreOrHeader s
    parse' ("SEQUENCE:"           :s) = parseSequence      s
    parse' ("ANNOTATED_SEQUENCE:" :s) = Left "" -- ignore ROSETTA 3.x ANNOTATED_SEQUENCE
    parse' ("REMARK"              :s) = Left "" -- ignore ROSETTA 3.x REMARKs 
    parse' []                         = Left ""
    -- TODO: validate mName with model name in SCORE header!
    parse' [numStr, ssStr,
            phiStr, psiStr,  omegaStr,
            caXStr, caYStr, caZStr,
            chi1Str, chi2Str, chi3Str, chi4Str, mName] = do
           i     :: Int    <- parse "residue number"           numStr
           ss    :: SSCode <- parse "secondary structure code" ssStr
           phi   :: Double <- parse "phi"                      phiStr
           psi   :: Double <- parse "psi"                      psiStr
           omega :: Double <- parse "omega"                    omegaStr
           caX   :: Double <- parse "C-alpha X coordinate"     caXStr
           caY   :: Double <- parse "C-alpha Y coordinate"     caYStr
           caZ   :: Double <- parse "C-alpha Z coordinate"     caZStr
           chi1  :: Double <- parse "chi1"                     chi1Str
           chi2  :: Double <- parse "chi2"                     chi2Str
           chi3  :: Double <- parse "chi3"                     chi3Str
           chi4  :: Double <- parse "chi4"                     chi4Str
           return $ Rec $ SilentRec i ss phi psi omega caX caY caZ chi1 chi2 chi3 chi4
    parse' other                      = error $ "Cannot parse:" ++ (BS.unpack . BS.concat) other
    --   1 L     0.000   17.891 -171.655    0.000    0.000    0.000  -81.139    0.000    0.000    0.000 S_0319_8954
    parse :: (Read a) => BS.ByteString -> BS.ByteString -> Either BS.ByteString a
    parse recName str = case reads $ BS.unpack str of
                          [(i, [])] -> Right i
                          _         -> Left $ BS.concat ["Cannot parse ", recName,
                                                         " ", BS.pack (show str) ]
    parseScoreOrHeader :: [BS.ByteString] -> Either BS.ByteString SilentEvent
    parseScoreOrHeader entries@("score":_) = Right $ ScoreHeader lbls descs
      where
        descs = takeDescriptions                     entries
        lbls  = take (length entries - length descs) entries
        takeDescriptions l@[            "description"] = l
        takeDescriptions l@["user_tag", "description"] = l
        takeDescriptions (l:ls)                        = takeDescriptions ls
    parseScoreOrHeader cols             = do vals <- sequence prevals'
                                             return $ Score { values       = vals
                                                            , descriptions = descs
                                                            }
      where
        prevals :: [Either BS.ByteString Double] = zipWith parseCol cols [1..]
        splittingPoint [Left _, Left _] = 2 -- may also have "user_tag"
        splittingPoint [Left _]         = 1
        splittingPoint (l:ls)           = splittingPoint ls
        splittingPoint []               = error $ "Cannot find splitting point in:" ++ (BS.unpack . BS.concat) cols
        (prevals', descs) = let n = length cols - splittingPoint prevals
                            in (take n prevals, drop n cols)
        parseCol :: BS.ByteString -> Int -> Either BS.ByteString Double
        parseCol col num = parse ("score column " `BS.append` BS.pack (show num)) col
    parseSequence [seq] = Right $ Seq seq
    parseSequence r     = Left $ BS.concat ["Cannot parse sequence record with words:", BS.pack $ show r]
-- TODO: parse other parts of each entry

-- TODO: parse header
-- TODO: parse SCORE: entries and join records within each model
parseSilentEvents :: BS.ByteString -> ([BS.ByteString],
                                       [SilentEvent]  )
parseSilentEvents = partitionEithers . filter goodOrError . map parseSilentEventLine . BS.lines
  where
    goodOrError (Left "") = False
    goodOrError other     = True

processErrors fname errs = forM_ errs $ \s -> BS.hPutStrLn stderr $
                                                BS.concat ["Error parsing ", fname, ":", s]

processSilentEvents fname = do (errs, evts) <- parseSilentEvents `fmap` BS.readFile (BS.unpack fname)
                               processErrors fname errs
                               return $! evts

modelScore = lookup "score" . scores

inf = 0/0

modelScoreIfAvailable = maybe inf id . modelScore

a `compareTotalScores` b = modelScoreIfAvailable a `compare` modelScoreIfAvailable b

bestSilentModel = minimumBy compareTotalScores

sortModelsByScore = sortBy compareTotalScores

