-- | Various utility functions that can be QuickChecked separately
{-# LANGUAGE OverloadedStrings #-}
module Rosetta.Util( splitsAt
                   , adj
                   , rnfList
                   , rnfListDublets
                   , readEither
                   , parse
                   , parseInt
                   , parseFloat
                   , parseFloat3
                   , bshow          ) where

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
-- | Fast parsing routine for floating point numbers with given number digits after the dot.
-- Approximately reduces parse time by 20x over naive use of ReadS-based parse.
-- (Giving a number of digits may help compiler to optimize some things away - untested!)
parseFloat :: BS.ByteString -> Maybe Int -> BS.ByteString -> Either BS.ByteString Double
parseFloat recName expectedDigitsAfterComma str = case BS.readInt str of
                                                    Nothing -> errMsg
                                                    Just (a, rest) -> assert (BS.head rest == '.') $
                                                      case BS.readInt $ BS.tail rest of
                                                        Nothing      -> errMsg
                                                        Just (b, "") -> assertion rest $
                                                                          Right $! fromIntegral a + fromIntegral b / fromIntegral (10^denominator rest)
  where
    errMsg = reportErr recName str
    assertion rest = case expectedDigitsAfterComma of
                       Just d  -> assert $ d == BS.length rest
                       Nothing -> id -- check nothing...
    denominator rest = case expectedDigitsAfterComma of
                         Just d  -> d -- optimized away...
                         Nothing -> BS.length rest -- compute hard way

{-# INLINE parseFloat3 #-}
-- | Fast parsing of conventional ROSETTA floats with 3 digits after the dot.
parseFloat3 r = parseFloat r $ Just 3

{-# INLINE parseInt #-}
-- | Fast parsing of integer.
parseInt :: BS.ByteString -> BS.ByteString -> Either BS.ByteString Int
parseInt recName str = case BS.readInt str of
                         Just (i, "") -> Right $! i
                         Nothing      -> reportErr recName str

{-# INLINE parse #-}
-- | Generic parsing routine for anything with a Read class.
parse :: (Read a) => BS.ByteString -> BS.ByteString -> Either BS.ByteString a
parse recName str = case reads $ BS.unpack str of
                      [(i, [])] -> Right $! i
                      _         -> reportErr recName str

-- | Standard error reporting format for parsing routines.
{-# INLINE reportErr #-}
reportErr recName input = Left $! BS.concat ["Cannot parse ", recName,
                                             " ", BS.pack $ show input ]

{-# INLINE bshow #-}
-- | Shows a type and packs into ByteString.
bshow :: (Show a) => a -> BS.ByteString
bshow = BS.pack . show

