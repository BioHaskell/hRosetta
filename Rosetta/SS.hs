{-# LANGUAGE DeriveDataTypeable #-}
-- | Module defines secondary structure type used by other modules.
module Rosetta.SS(SSCode(..)) where

import Data.Typeable
import Data.Data

-- | Secondary structure type.
data SSCode = Loop   |
              Strand |
              Helix
  deriving (Eq, Ord, Enum, Typeable, Data)

instance Show SSCode where
  showsPrec _ Loop   = ('L':)
  showsPrec _ Strand = ('E':)
  showsPrec _ Helix  = ('H':)

instance Read SSCode where
  readsPrec _ ('L':s) = [(Loop,   s)]
  readsPrec _ ('E':s) = [(Strand, s)]
  readsPrec _ ('H':s) = [(Helix,  s)]
  readsPrec _ s       = [] -- no parse

