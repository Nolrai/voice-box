module ProtoDoll.SynthTypes where

import Prelude (Float, Double, Show, Eq)
import Data.IntSet (IntSet)
import Data.Word (Word8, Word16)

-- | Base timing unit: 1/256 of a beat
type Ticks = Word16

-- | Represents a sound that fills available space.
type Weight = Word8

type Portion = Double

oneBeat :: Ticks
oneBeat = 256

data EnvTime = EnvTime
  { min :: Ticks
  , weight :: Weight
  } deriving (Show, Eq)

data ADSR = ADSR
  { attackTime    :: EnvTime
  , decayTime     :: EnvTime
  , sustainTime   :: EnvTime
  , releaseTime   :: EnvTime
  , sustainLevel  :: Portion
  }
  deriving (Show, Eq)

data Realization = Realization
  { dur  :: EnvTime
  , form :: Realization'
  }
  deriving (Show, Eq)

data Realization'
  = Tones
      { start :: IntSet
      , middle :: IntSet
      , end :: IntSet
      }
  | Noise
      { noiseEnvelope     :: ADSR
      , noiseFilter       :: Float
      , noiseSubharmonic  :: Portion
      , noiseReverb       :: Ticks
      }
  deriving (Show, Eq)