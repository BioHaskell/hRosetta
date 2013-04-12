{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | This model allows for parsing, and writing ROSETTA's silent files.
module Rosetta.Silent( SilentEvent    (..)
                     , SilentModel    (..)
                     , SilentRec      (..)
                     , emptySilentRec
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

import           Prelude hiding (readFile) 
import           System.IO(stderr, withFile, Handle, IOMode(WriteMode))
import           Control.Monad.Instances
import           Control.Monad(when, forM_)
import           Control.Exception(assert)
import           Data.List(unfoldr, minimumBy, sortBy)
import           Data.Either(partitionEithers)
import           Data.Maybe(fromMaybe)
import qualified Data.ByteString.Char8 as BS
import           Data.Typeable
import           Data.Data
import           Control.DeepSeq(deepseq, NFData(..))
import           Numeric(showFFloat)


import Rosetta.SS
import Rosetta.Util(adj,
                    rnfList, rnfListDublets,
                    parseFloat3, parseInt, parse, bshow,
                    readFile)

-- | Represents a single line of information within a silent file.
data SilentEvent = Rec         { unRec        :: SilentRec       }
                 | ScoreHeader { labels
                               , descLabels   :: [BS.ByteString] }
                 | Score       { values       :: [Double       ]
                               , descriptions :: [BS.ByteString]
                               }
                 | Seq         { unSeq        :: !BS.ByteString }
  deriving (Show, Data, Typeable)

-- | Represents a single residue record with torsion angles etc.
data SilentRec = SilentRec { resId                  :: !Int
                           , ss                     :: !SSCode -- use SSType
                           , phi, psi, omega        :: !Double
                           , caX, caY, caZ          :: !Double
                           , chi1, chi2, chi3, chi4 :: !Double
                           }
  deriving (Show, Data, Typeable)
-- TODO: replace Show/Read with parser and printer for ROSETTA format.

instance NFData SilentRec where

-- | Represents a single model from a ROSETTA silent file.
data SilentModel = SilentModel { name              :: !BS.ByteString
                               , otherDescriptions :: [(BS.ByteString, BS.ByteString)]
                               , scores            :: [(BS.ByteString,
                                                        Double       )]
                               , residues          :: [SilentRec]
                               , fastaSeq          :: !BS.ByteString
                               }
  deriving (Show, Data, Typeable)

-- | Evaluates spine of a list of dublets.
instance NFData SilentModel where
  rnf sm = rnfListDublets (otherDescriptions sm) `seq`
           rnfListDublets (scores            sm) `seq`
           rnf            (residues          sm)

-- | Default length of column in SCORE: record
colScoreLength = 7

-- | Clean empty `SilentRec`.
emptySilentRec = SilentRec { resId = -1
                           , ss    = Loop
                           , phi   = 0
                           , psi   = 0
                           , omega = 0
                           , caX   = 0
                           , caY   = 0
                           , caZ   = 0
                           , chi1  = 0
                           , chi2  = 0
                           , chi3  = 0
                           , chi4  = 0
                           }
-- | Generates a SEQUENCE: record string.
showSequence ::  BS.ByteString -> BS.ByteString
showSequence aSeq = "SEQUENCE: " `BS.append` aSeq
-- TODO: memoize genLabels result somehow

-- | Generates list of all labels within SCORE: record,
-- given a list of score labels, and description labels.
genLabels ::  [BS.ByteString] -> [BS.ByteString] -> [BS.ByteString]
genLabels labels descLabels = labels ++ descLabels ++ ["description"]

-- TODO: what to do when label sets are inconsistent? (Take set maximum, BUT preserve ordering.)
-- | Computes number of columns for each score or description, given their respective labels.
scoreColumns ::  [BS.ByteString] -> [BS.ByteString] -> [Int]
scoreColumns       scores descs      = map (mkLength colScoreLength      ) first_lbls  ++
                                       map (mkLength (colScoreLength + 1)) second_lbls ++
                                       map (mkLength colScoreLength      ) third_lbls
  where
    lbls = genLabels scores descs
    -- | Critical column..
    critCol1 = 16
    critCol2 = 31 - 16
    (first_lbls,  rest      ) = splitAt critCol1 lbls
    (second_lbls, third_lbls) = splitAt critCol2 lbls
    mkLength i s = max i $ BS.length s

-- | Given number of columns for each score or description, and lists of scores, descriptions, and a model name,
--   it it generates a ByteString that shows a SCORE: header, or model summary record.
showScoreLine :: [Int]-> [BS.ByteString]-> [BS.ByteString]-> BS.ByteString-> BS.ByteString
showScoreLine cols scores descs name = BS.intercalate " " . zipWith adj cols $ ["SCORE:   "] ++ scores ++ descs ++ [name]

-- | Takes a list of silent models, and writes them to the given file handle.
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
    showSilentRecord name rec = BS.intercalate " " $ [ adj 4 $ bshow $ resId rec
                                                     ,         bshow $ ss    rec
                                                     , ""                        ] ++ coordStrings ++ [name]
      where
        coordStrings = map (showCoordCol . ($ rec)) [phi, psi, omega, caX, caY, caZ, chi1, chi2, chi3, chi4]
    showCoordCol x = adj 8 $ BS.pack $ showFFloat (Just 3) x ""

-- | Writes a set of SilentMode
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
-- Filename is first argument, and is prepended to all error messages.
-- Result is a tuple of list of error messages, and `SilentModel`s.
parseSilent :: BS.ByteString -> BS.ByteString -> ([BS.ByteString], [SilentModel])
parseSilent fname input = (allErrors, mdls)
  where
    allErrors     = errs ++ merrs
    (errs,  evts) = parseSilentEvents input
    (merrs, mdls) = partitionEithers  $ parseSilent' evts
    parseSilent' :: [SilentEvent] -> [Either BS.ByteString SilentModel]
    parseSilent' (Seq aSeq:ScoreHeader lbls descLbls:r) = unfoldr (buildModel aSeq lbls descLbls) r
    -- unrecoverable errors:
    parseSilent' (       a:                        b:r) = [Left $ BS.concat ["First two records of the silent file cannot be parsed as Seq _:ScoreHeader _, but: ",
                                                                            bshow a, ":", bshow b]]
    parseSilent' r                                      = [Left $ BS.concat ["Silent file events end early: ", bshow r]]

-- | Parses a silent file and returns lists of error messages and models.
parseSilentFile :: FilePath -> IO ([BS.ByteString], [SilentModel])
parseSilentFile fname = parseSilent (BS.pack fname) `fmap` readFile fname

-- | Parses a silent file, prints out all error messages to stderr,
--   and returns a list of models
processSilentFile :: FilePath -> IO [SilentModel]
processSilentFile fname = do (errs, mdls) <- parseSilentFile fname
                             -- Need to evaluate models before errors, otherwise stack overflow happens!
                             rnfList mdls `seq` processErrors (BS.pack fname) errs
                             return mdls

-- | Takes sequence, both score and description labels from SCORE header,
--    and a list of `SilentEvent`s  and may produce pair of error message or model,
--    and a leftover events. It may also return Nothing, if there are no more events.
--    (To be used with `unfoldr`.)
--TODO: validate models in buildModel
buildModel ::  BS.ByteString  ->
              [BS.ByteString] ->
              [BS.ByteString] ->
              [SilentEvent]   ->
              Maybe ( Either BS.ByteString SilentModel
                    , [SilentEvent] )
buildModel mSeq lbls descLbls []              = Nothing
buildModel mSeq lbls descLbls (Score s ds:rs) = takeModel rs [] $ mCont mSeq lbls descLbls s ds
  where
    mCont mSeq lbls descLbls scores descs recs rs | length scores == length lbls =
                                                    Just (Right SilentModel { scores            = zip lbls scores
                                                                            , residues          = recs
                                                                            , name              = last descs
                                                                            , otherDescriptions = zip descLbls $ init descs
                                                                            , fastaSeq          = mSeq
                                                                            }
                                                         , rs)
    mCont mSeq lbls descLbls scores descs rec rs  = Just (Left $ BS.concat [ "Score list for model "
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

-- | Parses a single line of silent file, and returns either error message,
--   or a SilentEvent object.
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
            chi1Str, chi2Str, chi3Str, chi4Str, name] = do
           i     :: Int    <- {-# SCC parse_resi  #-} parseInt "residue number"           numStr
           ss    :: SSCode <- {-# SCC parse_ss    #-} parse "secondary structure code" ssStr
           phi   :: Double <- {-# SCC parse_phi   #-} parseFloat3 "phi"                      phiStr
           psi   :: Double <- {-# SCC parse_phi   #-} parseFloat3 "psi"                      psiStr
           omega :: Double <- {-# SCC parse_omega #-} parseFloat3 "omega"                    omegaStr
           caX   :: Double <- {-# SCC parse_cax   #-} parseFloat3 "C-alpha X coordinate"     caXStr
           caY   :: Double <- {-# SCC parse_cay   #-} parseFloat3 "C-alpha Y coordinate"     caYStr
           caZ   :: Double <- {-# SCC parse_caz   #-} parseFloat3 "C-alpha Z coordinate"     caZStr
           chi1  :: Double <- {-# SCC parse_chi1  #-} parseFloat3 "chi1"                     chi1Str
           chi2  :: Double <- {-# SCC parse_chi2  #-} parseFloat3 "chi2"                     chi2Str
           chi3  :: Double <- {-# SCC parse_chi3  #-} parseFloat3 "chi3"                     chi3Str
           chi4  :: Double <- {-# SCC parse_chi4  #-} parseFloat3 "chi4"                     chi4Str
           return $! Rec $! SilentRec i ss phi psi omega caX caY caZ chi1 chi2 chi3 chi4
    parse' other                      = error $ "Cannot parse:" ++ (BS.unpack . BS.concat) other
    --   1 L     0.000   17.891 -171.655    0.000    0.000    0.000  -81.139    0.000    0.000    0.000 S_0319_8954
    parseScoreOrHeader :: [BS.ByteString] -> Either BS.ByteString SilentEvent
    parseScoreOrHeader entries@("score":_) = Right $! ScoreHeader lbls descs
      where
        descs = takeDescriptions                     entries
        lbls  = take (length entries - length descs) entries
        takeDescriptions l@[            "description"] = l
        takeDescriptions l@["user_tag", "description"] = l
        takeDescriptions (l:ls)                        = takeDescriptions ls
    parseScoreOrHeader cols             = do vals <- sequence prevals'
                                             return Score { values       = vals
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
-- | Takes input, and splits it into lists of error messages, and `SilentEvent`s.
parseSilentEvents :: BS.ByteString -> ([BS.ByteString],
                                       [SilentEvent]  )
parseSilentEvents = partitionEithers . filter goodOrError . map parseSilentEventLine . BS.lines
  where
    goodOrError (Left "") = False
    goodOrError other     = True

{- Alternative:
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' l = (takeLeft l, takeRight l)
  where
    takeLeft   (Left  l:ls) = l:takeLeft ls
    takeLeft   (Right _:ls) = takeLeft ls
    takeLeft   []           = []
    takeRight  (Right l:ls) = l:takeRight ls
    takeRight  (Left  _:ls) = takeRight ls
    takeRight  []           = []
-}

{- Problem with stack overflow with normal Data.Either.partitionEithers:
partitionEithers'' :: [Either a b] -> ([a],[b])
partitionEithers'' = foldr (either left right) ([],[])
 where
  left  a ~(l, r) = (a:l, r)
  right a ~(l, r) = (l, a:r)
 -}

-- | Processes all errors that occured while parsing a file of given name,
--   and prints them.
processErrors fname errs = forM_ errs $ \s -> BS.hPutStrLn stderr $
                                                BS.concat ["Error parsing ", fname, ":", s]

-- | Parses a silent file with a given filename, prints errors to stderr, and returns list of events.
processSilentEvents fname = do (errs, evts) <- parseSilentEvents `fmap` readFile (BS.unpack fname)
                               rnfList evts `seq` processErrors fname errs
                               return $! evts
-- NOTE: Problem with stack overflow with normal Data.Either.partitionEithers
-- Solved temporarily by deepseq.

-- | Looks up total score of a given model.
modelScore = lookup "score" . scores

-- | Value of infinity, used instead of total score, when the score is not present.
inf = 0/0

-- | Gives total score of a model, or positive infinity if it is not found.
modelScoreIfAvailable = fromMaybe inf . modelScore

-- | Compares two models by their total score.
a `compareTotalScores` b = modelScoreIfAvailable a `compare` modelScoreIfAvailable b

-- | Takes a list, and finds the best model by total score
bestSilentModel = minimumBy compareTotalScores

-- | Sorts a list of models by total score.
sortModelsByScore = sortBy compareTotalScores

