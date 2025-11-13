module VoiceBox.Audio.Ear where
-- quick guess at needed imports
import Control.Error.Util (note)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Array.CArray qualified as CA
import Data.Complex qualified as C
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Vector.Storable qualified as V
import Data.WAVE qualified as WAVE
import Math.FFT qualified as FFT
import Safe (headMay)
import System.IO (hPutStrLn, hSetEncoding, stderr, utf8)
import VoiceBox.Types

-- | The type of resonant cavity accessible to the doll.
-- 'Drum' refers to a broad, shallow pool or reservoir of red ink, typically filling the interior of a limb cap or body part.
-- These act as the main low-frequency resonators, analogous to drums, but not made of stretched material.
data CavityType
  = ShellResonance         -- ^ Physical ringing of the shell (ceramic, etc.)
  | Drum                   -- ^ Red ink pool/reservoir (main auditory resonator)
  | BismuthWrapping        -- ^ Large bismuth wrapping, lubricated with dream oil
  deriving (Eq, Show)

-- | Represents the phase state of a single resonant cavity (frequency band).
data ResonantCavity = ResonantCavity
  { cavityType     :: CavityType   -- ^ What kind of cavity is this?
  , cavityFrequency :: Double      -- ^ Resonant frequency (Hz)
  , cavityPhase     :: Double      -- ^ Current phase (radians, mod 2π)
  }

-- | The doll's "ear" is a collection of saturated resonant cavities.
newtype DollEar = DollEar [ResonantCavity]

-- | An external sound event perturbs the phase of each cavity.
--   The function models how incoming sound (as a function of time and frequency)
--   shifts the phase of each cavity.
perturbCavities
  :: (Double -> Double -> Double)  -- ^ Sound field: (time, frequency) -> phase shift (radians)
  -> Double                        -- ^ Current time
  -> DollEar                       -- ^ Current state of the ear
  -> DollEar                       -- ^ Updated state with new phases
perturbCavities soundField t (DollEar cavities) =
  DollEar [ c { cavityPhase = cavityPhase c + soundField t (cavityFrequency c) }
          | c <- cavities
          ]

-- | Example: drum auditory patches (low-frequency hearing)
lowFreqCavities :: [ResonantCavity]
lowFreqCavities =
  [ ResonantCavity { cavityType = Drum, cavityFrequency = 360, cavityPhase = 0 }
  , ResonantCavity { cavityType = Drum, cavityFrequency = 360, cavityPhase = 0 }
  , ResonantCavity { cavityType = Drum, cavityFrequency = 400, cavityPhase = 0 }
  , ResonantCavity { cavityType = Drum, cavityFrequency = 400, cavityPhase = 0 }
  , ResonantCavity { cavityType = Drum, cavityFrequency = 400, cavityPhase = 0 }
  ]

-- | General function to populate drum cavities given band counts and frequencies.
populateDrumCavities :: [(Int, Double)] -> [ResonantCavity]
populateDrumCavities bands =
  [ ResonantCavity { cavityType = Drum, cavityFrequency = freq, cavityPhase = 0 }
  | (count, freq) <- bands, _ <- [1..count]
  ]
-- Example usage: populateDrumCavities [(2,360),(3,400)]

-- ...existing code...

-- | The doll's "ear" is a collection of saturated resonant cavities.
newtype DollEar = DollEar [ResonantCavity]

-- | An external sound event perturbs the phase of each cavity.
--   The function models how incoming sound (as a function of time and frequency)
--   shifts the phase of each cavity.
perturbCavities
	:: (Double -> Double -> Double)  -- ^ Sound field: (time, frequency) -> phase shift (radians)
	-> Double                        -- ^ Current time
	-> DollEar                       -- ^ Current state of the ear
	-> DollEar                       -- ^ Updated state with new phases
perturbCavities soundField t (DollEar cavities) =
	DollEar [ c { cavityPhase = cavityPhase c + soundField t (cavityFrequency c) }
					| c <- cavities
					]
-- | This file converts a .wav audio file into something modeling the internal percetion of sound for Dolls.


newtype TimeDomain = TimeDomain (Vector Double)
newtype FrequencyDomain = FrequencyDomain (Vector (Complex Double))

-- | Example: Standard hearing set of resonances for a doll, covering Drums (low),
-- BismuthWrapping (mid), and ShellResonance (high) with logarithmic spacing.
-- This mimics the band distribution found in biology (e.g., cochlea) and engineering (e.g., filterbanks).
-- - Drums: 360–1800 Hz (5–1 cm, red ink pools)
-- - BismuthWrapping: 2 kHz–12 kHz (2–30 cm, alchemical pulse speed ~100 m/s)
-- - ShellResonance: 6–20 kHz (10–30 cm, ceramic shell, v ~4000 m/s)
standardHearingCavities :: [ResonantCavity]
standardHearingCavities =
  -- Drums: low frequencies, fewest bands
  [ ResonantCavity Drum 360 0
  , ResonantCavity Drum 500 0
  , ResonantCavity Drum 700 0
  , ResonantCavity Drum 1000 0
  , ResonantCavity Drum 1400 0
  , ResonantCavity Drum 1800 0
  ]
  ++
  -- BismuthWrapping: mid frequencies, more bands
  [ ResonantCavity BismuthWrapping 2000 0
  , ResonantCavity BismuthWrapping 2800 0
  , ResonantCavity BismuthWrapping 4000 0
  , ResonantCavity BismuthWrapping 5600 0
  , ResonantCavity BismuthWrapping 8000 0
  , ResonantCavity BismuthWrapping 11200 0
  ]
  ++
  -- ShellResonance: high frequencies, fewest bands (physical limit)
  [ ResonantCavity ShellResonance 6000 0
  , ResonantCavity ShellResonance 9000 0
  , ResonantCavity ShellResonance 13000 0
  , ResonantCavity ShellResonance 18000 0
  , ResonantCavity ShellResonance 20000 0
  ]