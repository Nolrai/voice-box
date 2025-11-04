module ProtoDoll.SynthTypes where

import Prelude (Float, Show, Eq)
import Data.IntSet (IntSet)
import Data.Word (Word8, Word16)

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
  { attackTime    :: EnvTime   -- ^ attack in ticks
  , decayTime     :: EnvTime   -- ^ decay in ticks
  , sustainTime   :: EnvTime   -- ^ sustain duration in ticks
  , releaseTime   :: EnvTime   -- ^ release in ticks
  , sustainLevel  :: Portion   -- ^ sustain level (0..1)
  }
  deriving (Show, Eq)

-- | A Realization represents how a phoneme will be synthesized.
--   'dur' is the phoneme's total duration expressed in ticks.
data Realization = Realization
  { dur  :: EnvTime        -- ^ total phoneme duration in ticks
  , form :: Realization'   -- ^ synthesis form (tones, noise, silence)
  }
  deriving (Show, Eq)

data Realization'
  = Tones
      { start :: IntSet
      , middle :: IntSet
      , end :: IntSet
      }
  | Noise
      { noiseEnvelope     :: ADSR     -- ^ envelope times in ticks
      , noiseFilter       :: Float    -- ^ color / filter parameter
      , noiseSubharmonic  :: Portion  -- ^ amplitude scaling for the sub-tone
      , noiseReverb       :: Ticks    -- ^ reverb parameter stored as ticks (converted when applied)
      }
  | Silence
  deriving (Show, Eq)