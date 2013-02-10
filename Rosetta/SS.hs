module Rosetta.SS where
-- NOTE data structure and instances are duplicated with RosettaFrag.hs
data SSCode = Loop   |
              Strand |
              Helix
  deriving (Eq, Ord)

instance Show SSCode where
  showsPrec _ Loop   = ('L':)
  showsPrec _ Strand = ('E':)
  showsPrec _ Helix  = ('H':)

instance Read SSCode where
  readsPrec _ ('L':s) = [(Loop,   s)]
  readsPrec _ ('E':s) = [(Strand, s)]
  readsPrec _ ('H':s) = [(Helix,  s)]
  readsPrec _ s       = [] -- no parse

