module VoiceBox.Audio.Transform
  ( -- * Transformation
    transformAudio
  , transformAudioWithParams
  , transformStages
  , transformWavFile
    -- * Parameters
  , TransformParams (..)
  , VocoderToggles (..)
  , preDoll0Params
  , applyHarmonicVocoder
  , applySimpleSpeedup
  ) where

-- | VoiceBox.Transform - Harmonic vocoder for PreDoll-0 voice transformation
--
-- Current pipeline (as of artifact investigation):
--   1. FFT → frequency domain
--   2. Harmonic filtering: extract harmonics at 240 Hz fundamental with optional
--      Gaussian softband (controlled by vtEnableSoftBand)
--   3. Inverse FFT → vocoded time-domain signal
--   4. Simple speedup by 2x (duration shrinks, pitch rises)
--   5. Output
--
-- Removed features (caused artifacts):
--   - Spectral smoothing (amplified low-frequency drone)
--   - Envelope application (caused pops from ratio discontinuities)
--   - Blend (mixed original low freqs back - made output too human)
--   - Mix (dry/wet mixing caused phase interference pops)
--
-- The vocoded output has a constant low drone in quiet parts (harmonics extracted
-- from noise floor), but is otherwise clean and artifact-free.

import qualified Data.Vector.Storable as V
import Data.Complex (Complex ((:+)))
import qualified Math.FFT as FFT
import qualified Data.Array.CArray as CA
import VoiceBox.Audio.Analyze (readWaveFile, writeWaveFile)

--------------------------------------------
-- | Transformation Parameters
--------------------------------------------

-- | Parameters for audio transformation effects
data TransformParams = TransformParams
  { tpAutotuneSteps     :: Int    -- ^ Fundamental frequency (Hz) - pitch snaps to multiples (e.g., 120 for harmonic vocoder)
  , tpAutotuneAmount    :: Double -- ^ Autotune quantization strength (0.0-1.0)
  , tpPitchShift        :: Double -- ^ Pitch shift factor (1.0 = no change, 1.25 = up, 0.8 = down)
  } deriving (Show, Eq)

-- | PreDoll-0 speech transformation parameters
-- Harmonic vocoder with 240 Hz fundamental
-- Tuned for synthetic/artificial child-like feminine voice
preDoll0Params :: TransformParams
preDoll0Params = TransformParams
  { tpAutotuneSteps   = 240     -- 240 Hz fundamental (harmonics 1-20 full, 21-25 fade, >25 drop = 240-6000 Hz)
  , tpAutotuneAmount  = 0.9     -- Very strong autotune effect
  , tpPitchShift      = 3.0     -- 3x pitch shift → 360 Hz (child voice range)
  }

--------------------------------------------
-- | Audio Transformation
--------------------------------------------

-- | Transform audio with default PreDoll-0 parameters
transformAudio :: Int -> V.Vector Double -> V.Vector Double
transformAudio sampleRate = transformAudioWithParams sampleRate preDoll0Params defaultVocoderToggles

-- | Transform audio with custom parameters
transformAudioWithParams :: Int -> TransformParams -> VocoderToggles -> V.Vector Double -> V.Vector Double
transformAudioWithParams sampleRate params toggles samples =
  let -- Vocoder → speedup (cleans signal first, then pitch shifts)
      fundamental = fromIntegral (tpAutotuneSteps params)
      speedupFactor = 2.0
      samples0 = applyHarmonicVocoder sampleRate fundamental toggles samples
      samples1 = applySimpleSpeedup speedupFactor samples0
  in samples1

-- | Simple speedup by resampling - duration shrinks, pitch rises
applySimpleSpeedup :: Double -> V.Vector Double -> V.Vector Double
applySimpleSpeedup factor samples =
  let oldLen = V.length samples
      newLen = floor (fromIntegral oldLen / factor)
      getSample i =
        let srcIdx = fromIntegral i * factor
            idx1 = floor srcIdx
            idx2 = min (oldLen - 1) (idx1 + 1)
            frac = srcIdx - fromIntegral idx1
        in if idx1 >= oldLen then 0
           else (samples V.! idx1) * (1 - frac) + (samples V.! idx2) * frac
  in V.generate newLen getSample

-- | Return named intermediate stages of the transformation pipeline
transformStages :: Int -> TransformParams -> V.Vector Double -> [(String, V.Vector Double)]
transformStages sampleRate params samples =
  let original = samples
      fundamental = fromIntegral (tpAutotuneSteps params)
      speedupFactor = 2.0
      toggles = defaultVocoderToggles

      -- Get internal vocoder stages
      (vocoded, vocoderStages) = applyHarmonicVocoderWithStages sampleRate fundamental toggles samples
      vocodedThenSpeedUp = applySimpleSpeedup speedupFactor vocoded

      just_speedup = applySimpleSpeedup speedupFactor original
      speedupThenVocoded = applyHarmonicVocoder sampleRate fundamental toggles just_speedup

  in [ ("00_original", samples)
     , ("01_speedup_only", just_speedup)
     ] ++ vocoderStages ++
     [ ("08_vocoder_final", vocoded)
     , ("09_vocoder_then_speedup", vocodedThenSpeedUp)
     , ("10_speedup_then_vocoder", speedupThenVocoded)
     ]

--------------------------------------------
-- | Effect Implementations
--------------------------------------------

-- | Vocoder controls
-- Smoothing removed (was amplifying drone)
-- Envelope application removed (was causing pops)
-- Mix removed (caused phase interference pops)
-- Blend removed (made output sound too human, never worked well)
newtype VocoderToggles = VocoderToggles
  { vtEnableSoftBand :: Bool  -- ^ Enable soft band Gaussian filtering around harmonics
  } deriving (Show, Eq)

newtype VocoderNumericParams = VocoderNumericParams
  { vnpTolerance   :: Double  -- Gaussian bandwidth for harmonic filtering (Hz)
  } deriving (Show, Eq)

-- | Default "PreDoll-0" style toggles: SoftBand enabled
defaultVocoderToggles :: VocoderToggles
defaultVocoderToggles = VocoderToggles
  { vtEnableSoftBand = True
  }

-- | Default "PreDoll-0" numeric parameters
defaultVocoderNumericParams :: VocoderNumericParams
defaultVocoderNumericParams = VocoderNumericParams
  { vnpTolerance   = 8.0
  }

applyHarmonicVocoder :: Int -> Double -> VocoderToggles -> V.Vector Double -> V.Vector Double
applyHarmonicVocoder sampleRate fundamentalHz toggles samples =
  fst $ applyHarmonicVocoderWithStages sampleRate fundamentalHz toggles samples

-- | Version that returns intermediate stages for debugging
applyHarmonicVocoderWithStages :: Int -> Double -> VocoderToggles -> V.Vector Double -> (V.Vector Double, [(String, V.Vector Double)])
applyHarmonicVocoderWithStages sampleRate fundamentalHz
    VocoderToggles
      { vtEnableSoftBand = enableSoftBand
    }
    samples =
  let
    VocoderNumericParams
      { vnpTolerance = tolerance
      } = defaultVocoderNumericParams

    ------------------------------------------------------------
    -- Core setup
    n  = V.length samples
    sr = fromIntegral sampleRate
    detectFundamental = 60.0
    minFreq = fundamentalHz

    complexSamples = V.map (:+ 0) samples
    spectrum0 = fftForward complexSamples

    ------------------------------------------------------------
    -- Harmonic filtering with optional Gaussian tolerance
    filtered = V.imap (\i val ->
      let freq = if i <= n `div` 2
                 then fromIntegral i * sr / fromIntegral n
                 else sr - fromIntegral (n - i) * sr / fromIntegral n

          harmonic = round (freq / detectFundamental) :: Int
          harmonicFreq = detectFundamental * fromIntegral harmonic
          distance = abs (freq - harmonicFreq)

          bandWeight
            | enableSoftBand = exp (- ((distance * distance) / (2 * tolerance * tolerance)))
            | distance <= tolerance = 1
            | otherwise = 0

          harmonicAttenuation
            | freq < minFreq  = 0.0
            | freq <= 3000.0  = 1.0
            | freq >= 4000.0  = 0.0
            | otherwise       = (4000.0 - freq) / 1000.0
      in val * (harmonicAttenuation * bandWeight :+ 0)
      ) spectrum0

    ------------------------------------------------------------
    -- Back to time domain
    result = fftInverse filtered
    vocoded = V.map (\(r :+ _) -> r) result

    -- Collect intermediate stages for debugging
    stages =
      [ ("02_after_harmonic_filter", vocoded)
      , ("03_final_output", vocoded)
      ]

  in (vocoded, stages)

--------------------------------------------
-- | FFT Utilities
--------------------------------------------

-- | Forward FFT
fftForward :: V.Vector (Complex Double) -> V.Vector (Complex Double)
fftForward vec =
  let arr = CA.listArray (0, V.length vec - 1) (V.toList vec)
      result = FFT.dft arr
  in V.fromList (CA.elems result)

-- | Inverse FFT
fftInverse :: V.Vector (Complex Double) -> V.Vector (Complex Double)
fftInverse vec =
  let arr = CA.listArray (0, V.length vec - 1) (V.toList vec)
      result = FFT.idft arr
  in V.fromList (CA.elems result)

--------------------------------------------
-- | High-level Pipeline
--------------------------------------------

-- | Transform a WAV file and write the result to another WAV file
-- Reads WAV → applies PreDoll-0 transformation → writes WAV
-- Returns Nothing on success, Just error message on failure
transformWavFile :: FilePath -> FilePath -> IO (Maybe String)
transformWavFile inputPath outputPath = do
  audioResult <- readWaveFile inputPath
  case audioResult of
    Left err -> pure $ Just err
    Right (samples, sampleRate) -> do
      let transformed = transformAudio sampleRate samples
      writeWaveFile outputPath sampleRate transformed
      pure Nothing
