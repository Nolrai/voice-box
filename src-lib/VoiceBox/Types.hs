module VoiceBox.Types where

import Data.Vector.Storable (Vector)

-- | Types for audio analysis and feature extraction
--   - Pitch tracking results
--   - Energy/amplitude envelopes
--   - Spectral features
--   - Formant estimates
--   - Audio frame data

-- | Represents a single pitch sample (time, frequency)
data Pitch = Pitch
  { pitchTime :: Double,
    pitchFreq :: Double
  }
  deriving (Show, Eq)

-- | Represents a single envelope sample (time, amplitude)
data AmplitudeSample = AmplitudeSample
  { ampTime :: Double,
    ampMagnitude :: Double
  }
  deriving (Show, Eq)

-- | Formant frequency with bandwidth
data Formant = Formant
  { -- | Center frequency in Hz
    formantFreq :: Double,
    -- | Bandwidth in Hz
    formantBandwidth :: Double
  }
  deriving (Show, Eq)

-- | Formant estimate at a specific time
data FormantFrame = FormantFrame
  { ffTime :: Double,
    -- | Typically F1, F2, F3, F4...
    ffFormants :: [Formant]
  }
  deriving (Show, Eq)

-- | Audio segment boundaries (for voice activity detection, etc.)
data Segment = Segment
  { -- | Start time in seconds
    segStart :: Double,
    -- | End time in seconds
    segEnd :: Double,
    -- | Optional label (e.g., "voiced", "silence")
    segLabel :: String
  }
  deriving (Show, Eq)

-- | Spectral frame (magnitude spectrum)
data SpectrumFrame = SpectrumFrame
  { sfTime :: Double,
    -- | Magnitude spectrum (positive frequencies only)
    sfMagnitudes :: Vector Double
  }
  deriving (Show, Eq)

-- | Parameters for audio analysis
data AnalysisParams = AnalysisParams
  { -- | Frame size in samples (e.g., 2048)
    apWindowSize :: Int,
    -- | Hop size between frames (e.g., 512)
    apHopSize :: Int,
    -- | Minimum pitch to detect (Hz)
    apMinPitch :: Double,
    -- | Maximum pitch to detect (Hz)
    apMaxPitch :: Double
  }
  deriving (Show, Eq)

-- | Default analysis parameters
defaultAnalysisParams :: AnalysisParams
defaultAnalysisParams =
  AnalysisParams
    { apWindowSize = 2048,
      apHopSize = 512,
      apMinPitch = 75.0,
      apMaxPitch = 600.0
    }

-- | Aggregate of extracted features
data AudioFeatures = AudioFeatures
  { afPitch :: [Pitch],
    afEnvelope :: [AmplitudeSample],
    afFormants :: [FormantFrame],
    afSegments :: [Segment],
    -- | Full spectral data for debugging/visualization
    afSpectrums :: [SpectrumFrame]
  }
  deriving (Show, Eq)
