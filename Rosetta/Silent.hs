{-# LANGUAGE ScopedTypeVariables #-}
module Rosetta.Silent where

import System.IO
import Control.Monad.Instances
import Control.Monad(when)
import Data.Either(partitionEithers)
import Prelude

import Rosetta.SS

data SilentRec = SilentRec { resId :: Int
                           , ss    :: SSCode -- use SSType
                           }
  deriving (Show)

parseSilentLine :: String -> Either String SilentRec
parseSilentLine line = parse' $ words line
  where
    parse' :: [String] -> Either String SilentRec
    parse' ("SCORE:"    :_) = Left ""
    parse' ("SEQUENCE:" :_) = Left ""
    parse' []               = Left ""
    parse' (numStr:ssStr:_) = do i  :: Int    <- parse "residue number"           numStr
                                 ss :: SSCode <- parse "secondary structure code" ssStr
                                 return $ SilentRec i ss
    parse :: (Read a) => String -> String -> Either String a
    parse recName str = case reads str of
                          [(i, [])] -> Right i
                          _         -> Left $ "Cannot parse " ++ recName ++ " " ++ show str
-- TODO: parse other parts of each entry

parseSilentEvents = map parseSilentLine . lines

-- TODO: parse header
-- TODO: parse SCORE: entries and join records within each model
parseSilentOut :: String -> ([String], [SilentRec])
parseSilentOut = partitionEithers . parseSilentEvents


