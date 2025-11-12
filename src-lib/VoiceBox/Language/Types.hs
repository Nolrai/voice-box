module VoiceBox.Language.Types where

import Data.IntSet (IntSet)
import Data.Word (Word16, Word8)
import Prelude (Eq, Float, Show)

-- | Base timing unit: 1/256 of a beat
type Ticks = Word16

-- | Duration measured in ticks (alias for clarity in the codebase).
--   One tick = 1/256 of a beat; higher-level helpers convert ticks -> seconds.
type DurationTicks = Ticks

-- | Represents an envelope time value expressed in ticks.
--   (Kept for backward compatibility: EnvTime is the name used across synth code.)
type EnvTime = DurationTicks

-- | Represents a sound that fills available space.
type Weight = Word8

type Portion = Float

oneBeat :: Ticks
oneBeat = 256

-- | ADSR described in ticks. Each time field is measured in EnvTime (ticks).
--   Use 'fitADSR' or similar helpers to scale an ADSR to match a phoneme's total duration.
data ADSR = ADSR
  { -- | attack in ticks
    attackTime :: EnvTime,
    -- | decay in ticks
    decayTime :: EnvTime,
    -- | sustain duration in ticks
    sustainTime :: EnvTime,
    -- | release in ticks
    releaseTime :: EnvTime,
    -- | sustain level (0..1)
    sustainLevel :: Portion
  }
  deriving (Show, Eq)

-- | A Realization represents how a phoneme will be synthesized.
--   'dur' is the phoneme's total duration expressed in ticks.
data Realization = Realization
  { -- | total phoneme duration in ticks
    dur :: EnvTime,
    -- | synthesis form (tones, noise, silence)
    form :: Realization'
  }
  deriving (Show, Eq)

data Realization'
  = Tones
      { start :: IntSet,
        middle :: IntSet,
        end :: IntSet
      }
  | Noise
      { -- | envelope times in ticks
        noiseEnvelope :: ADSR,
        -- | color / filter parameter
        noiseFilter :: Float,
        -- | amplitude scaling for the sub-tone
        noiseSubharmonic :: Portion,
        -- | reverb parameter stored as ticks (converted when applied)
        noiseReverb :: Ticks
      }
  | Silence
  deriving (Show, Eq)
