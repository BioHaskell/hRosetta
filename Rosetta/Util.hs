-- | Various utility functions that can be QuickChecked separately
{-# LANGUAGE OverloadedStrings #-}
module Rosetta.Util( splitsAt
                   , adj
                   , rnfList
                   , rnfListDublets
                   , parse
                   , parseInt
                   , parseFloat
                   , parseFloat3
                   , bshow      
                   , readFile
                   , writeFile      ) where

import           Prelude hiding (readFile, writeFile)
import           Data.List(isSuffixOf)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Codec.Compression.GZip     as GZip
import           Control.Exception(assert)
import           Data.Maybe(fromMaybe)

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
--rnfList = foldr seq () -- or foldr'
rnfList []     = ()
rnfList (a:ls) = a `seq` rnfList ls

{-# INLINE parseFloat #-} 
-- | Fast parsing routine for floating point numbers with given number digits after the dot.
-- Approximately reduces parse time by 20x over naive use of ReadS-based parse.
-- (Giving a number of digits may help compiler to optimize some things away - untested!)
parseFloat :: BS.ByteString -> Maybe Int -> BS.ByteString -> Either BS.ByteString Double
parseFloat recName expectedDigitsAfterComma str =
    case BS.readInt str of
      Nothing -> errMsg
      Just (a, rest) -> assert (BS.head rest == '.') $
        case BS.readInt $ BS.tail rest of
          Nothing      -> errMsg
          Just (b, "") -> assertion rest $
                            Right $! fromIntegral a +
                                     fromIntegral b /
                                     fromIntegral (10^denominator rest)
  where
    errMsg = reportErr recName str
    -- | Checks number of digits after comma, if got this information.
    -- Useful to make sure that it is optimized away. :-)
    assertion :: BS.ByteString -> a -> a
    assertion rest = case expectedDigitsAfterComma of
                       Just d  -> assert $ d == BS.length rest
                       Nothing -> id -- check nothing... -}
    -- | Number of digits in the denominator.
    denominator :: BS.ByteString -> Int
    denominator rest = --fromMaybe (BS.length rest) id expectedDigitsAfterComma
                       case expectedDigitsAfterComma of
                         Just d  -> d -- optimized away...
                         Nothing -> BS.length rest - 1 -- compute hard way, subtract 1 for comma

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

{-# INLINE decompress #-}
decompress content = BS.concat $ BSL.toChunks $ GZip.decompress $ BSL.fromChunks [content]

{-# INLINE compress #-}
compress content = BS.concat $ BSL.toChunks $ GZip.compress $ BSL.fromChunks [content]

{-# INLINE readFile #-}
-- | Reads file and applies decoding routine, if input is compressed.
readFile :: String -> IO BS.ByteString
readFile fname = codec `fmap` BS.readFile fname
  where
    codec = fst $ getCodec fname

-- | Type of coders/decoders.
type Codec = BS.ByteString -> BS.ByteString

-- | Function that looks at filename suffix to select coding/decoding
-- routine applied after reading file.
--
-- Useful for automatic recognition of compressed input/output by filename.
getCodec :: String -> (Codec, Codec)
getCodec fname = if ".gz" `isSuffixOf` fname
                   then (decompress, compress)
                   else (id, id)

-- | Writes a file, possibly compressed, if so indicated by suffix.
writeFile :: String -> BS.ByteString -> IO ()
writeFile fname content = BS.writeFile fname $ snd (getCodec fname) content

