module ProtoDoll.ParseResult where

type Feet = [Phoneme]

data Voicing = White | Brown | Nasal
  deriving (Enum, Show, Eq, Ord, Read)

data Manner = P | S | C
  deriving (Enum, Show, Eq, Ord, Read)

data Place = Front | Mid | Back

data Liminal = T0 | H2W

data Consonant = Consonant
  { manner :: Manner
  , voice  :: Voice
  , place  :: Place
  }

data VowelName = A | E | I | O | U | Q

data InputPhoneme
  = Chord [VowelName]
  | Consonant Consonant
  | Liminal Liminal
  | Silence Silence

data Silence = Gap | PhraseBoundary | UtteranceBoundary