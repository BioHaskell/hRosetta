-- | Various utility functions that can be QuickChecked separately
{-# LANGUAGE OverloadedStrings #-}
module Rosetta.Util( splitsAt
                   , adj
                   , rnfList
                   , rnfListDublets
                   , readEither
                   , parse
                   , parseInt
                   , parseFloat ) where

import qualified Data.ByteString.Char8 as BS
import Control.Exception(assert)

-- | This is temporary, until readMaybe gets into Prelude!
--   Or replace it with readE that returns a parse error with line no
--   Using Control.Monad.Error, Monad (ErrorT e IdentityT)
readEither :: (Read a) => String -> BS.ByteString -> Either String a 
readEither msg s =  case [x | (x , t) <- reads $ BS.unpack s,
                              ("","") <- lex t              ] of
                      [x] -> Right x
                      _   -> Left  msg

-- | Splits a list at indices given by another list, yielding a list of lists
--   Obeys the following law:
--   flatten . splitsAt a l == l
splitsAt (i:is) l = x:splitsAt (map (+(-i)) is) r
  where
    (x, r) = Prelude.splitAt i l
splitsAt [] l = [l]

-- | Right justify a given ByteString up to a given length.
adj i s = BS.replicate (i - BS.length s) ' ' `BS.append` s

-- | Normal form of spine of list of dublets.
rnfListDublets []          = ()
rnfListDublets ((a, b):ls) = a `seq` b `seq` rnfListDublets ls 

-- | Normal form of spine of a list.
rnfList []     = ()
rnfList (a:ls) = a `seq` rnfList ls

{-# INLINE parseFloat #-} 
-- | Fast parsing routine for floating point numbers with three digits after the dot.
-- Approximately reduces parse time by 20x over naive use of ReadS-based parse.
parseFloat :: BS.ByteString -> Int -> BS.ByteString -> Either BS.ByteString Double
parseFloat recName expectedDigitsAfterComma str = case BS.readInt str of
                                                    Nothing -> errMsg
                                                    Just (a, rest) -> assert (BS.head rest == '.') $
                                                      case BS.readInt $ BS.tail rest of
                                                        Nothing      -> errMsg
                                                        Just (b, "") -> assert (expectedDigitsAfterComma == BS.length rest) $
                                                                          Right $! fromIntegral a + fromIntegral b / fromIntegral (10^expectedDigitsAfterComma)
  where
    errMsg = reportErr recName str

{-# INLINE parseInt #-}
parseInt :: BS.ByteString -> BS.ByteString -> Either BS.ByteString Int
parseInt recName str = case BS.readInt str of
                         Just (i, "") -> Right $! i
                         Nothing      -> reportErr recName str

{-# INLINE parse #-}
parse :: (Read a) => BS.ByteString -> BS.ByteString -> Either BS.ByteString a
parse recName str = case reads $ BS.unpack str of
                      [(i, [])] -> Right $! i
                      _         -> reportErr recName str

{-# INLINE reportErr #-}
reportErr recName input = Left $! BS.concat ["Cannot parse ", recName,
                                             " ", BS.pack $ show input ]
