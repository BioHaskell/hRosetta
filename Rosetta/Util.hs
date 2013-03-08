-- | Various utility functions that can be QuickChecked separately
module Rosetta.Util( splitsAt
                   , adj
                   , rnfList
                   , readEither ) where
import qualified Data.ByteString.Char8 as BS

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
rnfList []          = ()
rnfList ((a, b):ls) = a `seq` b `seq` rnfList ls 

