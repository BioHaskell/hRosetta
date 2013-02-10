{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Rosetta.Silent where

import System.IO(stderr)
import Control.Monad.Instances
import Control.Monad(when, forM)
import Data.Either(partitionEithers)
import qualified Data.ByteString.Char8 as BS
import Prelude

import Rosetta.SS

data SilentRec = SilentRec { resId :: Int
                           , ss    :: SSCode -- use SSType
                           }
  deriving (Show)

parseSilentLine :: BS.ByteString -> Either BS.ByteString SilentRec
parseSilentLine line = parse' $ BS.words line
  where
    parse' :: [BS.ByteString] -> Either BS.ByteString SilentRec
    parse' ("SCORE:"    :_) = Left ""
    parse' ("SEQUENCE:" :_) = Left ""
    parse' []               = Left ""
    parse' (numStr:ssStr:_) = do i  :: Int    <- parse "residue number"           numStr
                                 ss :: SSCode <- parse "secondary structure code" ssStr
                                 return $ SilentRec i ss
    parse :: (Read a) => BS.ByteString -> BS.ByteString -> Either BS.ByteString a
    parse recName str = case reads $ BS.unpack str of
                          [(i, [])] -> Right i
                          _         -> Left $ BS.concat ["Cannot parse ", recName, " ", BS.pack (show str)]
-- TODO: parse other parts of each entry

parseSilentEvents = map parseSilentLine . BS.lines

-- TODO: parse header
-- TODO: parse SCORE: entries and join records within each model
parseSilent :: BS.ByteString -> ([BS.ByteString],
                                    [SilentRec]    )
parseSilent = partitionEithers . parseSilentEvents

processSilent fname = do (errs, evts) <- parseSilent `fmap` BS.readFile (BS.unpack fname)
                         forM errs $ \s -> BS.hPutStrLn stderr $
                                             BS.concat ["Error parsing ", fname, ":", s]
                         return $! evts

