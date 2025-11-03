module ProtoDoll.ParseResult where

type Foot = [Phoneme]

data Voicing = White | Brown | Nasal
  deriving (Enum, Show, Eq, Ord, Read)

data Manner = P | S | C
  deriving (Enum, Show, Eq, Ord, Read)

data Place = Front | Mid | Back
  deriving (Show, Eq)

data Liminal = T0 | H2W
  deriving (Show, Eq)


data Consonant = MkConsonant
  { manner :: Manner
  , voice  :: Voicing
  , place  :: Place
  }
  deriving (Show, Eq)

data VowelName = A | E | I | O | U | Q
  deriving (Show, Eq)

data Phoneme
  = Chord [VowelName]
  | Consonant Consonant
  | Liminal Liminal
  | Silence Silence
  deriving (Show, Eq)

data Silence = Gap | PhraseBoundary | UtteranceBoundary
  deriving (Show, Eq)
