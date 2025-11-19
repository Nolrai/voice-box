module VoiceBox.Audio.Ear.Data where

import Data.Vector.Storable hiding ((++))
import Data.Complex
import LambdaSound (Hz(Hz))

-- | The type of resonant cavity accessible to the doll.
-- 'Drum' refers to a broad, shallow pool or reservoir of red ink, typically filling the interior of a limb cap or body part.
-- These act as the main low-frequency resonators, analogous to drums, but not made of stretched material.
data CavityType
  = ShellResonance         -- ^ Physical ringing of the shell (ceramic, etc.)
  | Drum                   -- ^ Red ink pool/reservoir (main auditory resonator)
  | BismuthWrapping        -- ^ Large bismuth wrapping, lubricated with dream oil
  deriving (Eq, Show, Ord)

-- | Represents the phase state of a single resonant cavity (frequency band).
data ResonantCavity = ResonantCavity
  { cavityType     :: CavityType   -- ^ What kind of cavity is this?
  , cavityFrequency :: Hz      -- ^ Resonant frequency (Hz)
  } deriving (Eq, Show, Ord)

-- | The doll's "ear" is a collection of saturated resonant cavities.
newtype DollEar = DollEar [ResonantCavity]

newtype AudioInput = AudioInput (Vector Double)
newtype FrequencyDomain = FrequencyDomain (Vector (Complex Double))

newtype Seconds = Seconds Double
  deriving (Eq, Show, Ord, Num, Fractional, Real, RealFrac)

-- Physical model parameters for drum delay calculation
powerDrumRadius, powerFrequency :: Double
powerDrumRadius = 20.0 -- in cm
powerFrequency = 60.0 -- in Hz

-- | Example: Standard hearing set of resonances for a doll, covering Drums (low),
-- BismuthWrapping (mid), and ShellResonance (high) with logarithmic spacing.
-- This mimics the band distribution found in biology (e.g., cochlea) and engineering (e.g., filterbanks).
-- - Drums: 360–1800 Hz (5–1 cm, red ink pools)
-- - BismuthWrapping: 2 kHz–12 kHz (2–30 cm, alchemical pulse speed ~100 m/s)
-- - ShellResonance: 6–20 kHz (10–30 cm, ceramic shell, v ~4000 m/s)
standardHearingCavities :: [ResonantCavity]
standardHearingCavities =
  -- Drums: low frequencies, fewest bands
  [ ResonantCavity Drum 360
  , ResonantCavity Drum 500
  , ResonantCavity Drum 700
  , ResonantCavity Drum 1000
  , ResonantCavity Drum 1400
  , ResonantCavity Drum 1800
  ]
  ++
  -- BismuthWrapping: mid frequencies, more bands
  -- BismuthWrapping: mid frequencies, more bands
  -- ShellResonance: high frequencies, fewest bands (physical limit)
  -- BismuthWrapping: mid frequencies, more bands
  [ ResonantCavity BismuthWrapping 2000
  , ResonantCavity BismuthWrapping 2800
  , ResonantCavity BismuthWrapping 4000
  , ResonantCavity BismuthWrapping 5600
  , ResonantCavity BismuthWrapping 8000
  , ResonantCavity BismuthWrapping 11200
  ]
  ++
  -- ShellResonance: high frequencies, fewest bands (physical limit)
  [ ResonantCavity ShellResonance 6000
  , ResonantCavity ShellResonance 9000
  , ResonantCavity ShellResonance 13000
  , ResonantCavity ShellResonance 18000
  , ResonantCavity ShellResonance 20000
  ]

-- | Choose a quality factor (Q) based on the cavity type.
cavityQ :: CavityType -> Double
cavityQ Drum = 5.0              -- Broad, shallow pool: low Q
cavityQ ShellResonance = 20.0   -- Ceramic shell: high Q
cavityQ BismuthWrapping = 10.0  -- Bismuth: medium Q

-- | A decay coefficcient influencing phase smoothing.
cavityK :: CavityType -> Double
cavityK Drum = 0.2
cavityK ShellResonance = 0.3
cavityK BismuthWrapping = 0.5

minDelay :: Seconds
minDelay = Seconds 0.001 -- Minimum delay in seconds

hzToPeriod :: Hz -> Seconds
hzToPeriod (Hz f) = Seconds (1.0 / realToFrac f)

class Scaled a where
  scaleF :: Real b => b -> a -> a
  scaleI :: Integral b => b -> a -> a

instance Scaled Seconds where
  scaleF s (Seconds t) = Seconds (realToFrac s * t)
  scaleI s (Seconds t) = Seconds (fromIntegral s * t)

cavityPeriod :: ResonantCavity -> Seconds
cavityPeriod ResonantCavity {cavityFrequency = freq} = hzToPeriod freq

audioPhaseBitdepth :: Int
audioPhaseBitdepth = 4 -- 4 bits for phase representation

-- Maximum pulse count based on bit depth
maxPulses :: Int
maxPulses = (2 ^ audioPhaseBitdepth) - 1