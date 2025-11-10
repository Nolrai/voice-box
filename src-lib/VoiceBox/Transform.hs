module VoiceBox.Transform
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

import qualified Data.Vector.Storable as V
import Data.Complex (Complex ((:+)))
import qualified Math.FFT as FFT
import qualified Data.Array.CArray as CA
import VoiceBox.Analyze (readWaveFile, writeWaveFile, extractEnvelope)
import VoiceBox.Types (AmplitudeSample(..), defaultAnalysisParams)

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

-- | Detailed controls for spectral and mixing refinements
-- NOTE: Smooth (spectral smoothing) is always enabled
-- NOTE: Warp (spectral warping) is always disabled (found to degrade quality)
data VocoderToggles = VocoderToggles
  { vtEnableSoftBand :: Bool
  , vtEnableBlend    :: Bool
  , vtEnableMix      :: Bool
  } deriving (Show, Eq)

data VocoderNumericParams = VocoderNumericParams
  { vnpSmoothRadius :: Int
  , vnpTolerance    :: Double
  , vnpBlendCutoff  :: Double
  , vnpBlendAmount  :: Double
  , vnpWarpAlpha    :: Double
  , vnpWetRatio     :: Double
  } deriving (Show, Eq)

-- | Default "PreDoll-0" style toggles: YNY (SoftBand + Mix)
-- Based on tournament evaluation - best balance of intelligibility and robotic character
defaultVocoderToggles :: VocoderToggles
defaultVocoderToggles = VocoderToggles
  { vtEnableSoftBand = True
  , vtEnableBlend    = False  -- Blend makes it sound too human
  , vtEnableMix      = True
  }

-- | Default "PreDoll-0" numeric parameters
defaultVocoderNumericParams :: VocoderNumericParams
defaultVocoderNumericParams = VocoderNumericParams
  { vnpSmoothRadius = 4
  , vnpTolerance    = 8.0
  , vnpBlendCutoff  = 800.0
  , vnpBlendAmount  = 0.3
  , vnpWarpAlpha    = 0.85
  , vnpWetRatio     = 0.8
  }

applyHarmonicVocoder :: Int -> Double -> VocoderToggles -> V.Vector Double -> V.Vector Double
applyHarmonicVocoder sampleRate fundamentalHz toggles samples =
  fst $ applyHarmonicVocoderWithStages sampleRate fundamentalHz toggles samples

-- | Version that returns intermediate stages for debugging
applyHarmonicVocoderWithStages :: Int -> Double -> VocoderToggles -> V.Vector Double -> (V.Vector Double, [(String, V.Vector Double)])
applyHarmonicVocoderWithStages sampleRate fundamentalHz
    VocoderToggles
      { vtEnableSoftBand = enableSoftBand
      , vtEnableBlend = enableBlend
      , vtEnableMix = enableMix
    }
    samples =
  let
    VocoderNumericParams
      { vnpSmoothRadius = smoothRadius
      , vnpTolerance = tolerance
      , vnpBlendCutoff = blendCutoff
      , vnpBlendAmount = blendAmount
      , vnpWetRatio = wetRatio} = defaultVocoderNumericParams

    ------------------------------------------------------------
    -- Core setup
    n  = V.length samples
    sr = fromIntegral sampleRate
    detectFundamental = 60.0
    minFreq = fundamentalHz

    analysisParams = defaultAnalysisParams
    originalEnvelope = extractEnvelope sampleRate analysisParams samples
    complexSamples = V.map (:+ 0) samples
    spectrum0 = fftForward complexSamples

    ------------------------------------------------------------
    -- Optional spectral warping (always disabled - found to degrade quality)
    spectrumWarped = spectrum0

    ------------------------------------------------------------
    -- Harmonic filtering with Gaussian tolerance
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
      ) spectrumWarped

    ------------------------------------------------------------
    -- Spectral smoothing - DISABLED for testing (was causing drone amplification)
    -- The old smoothing caused phase cancellation (quieting at phrase ends)
    -- The new magnitude-preserving smoothing amplifies the drone
    -- TODO: Consider selective smoothing (only smooth high frequencies, not the fundamental/harmonics)
    smoothed = filtered  -- No smoothing for now

    ------------------------------------------------------------
    -- Low-frequency blend
    blended = V.imap (\i val ->
      let freq = if i <= n `div` 2
                 then fromIntegral i * sr / fromIntegral n
                 else sr - fromIntegral (n - i) * sr / fromIntegral n
          blend = if enableBlend && freq < blendCutoff then blendAmount else 0.0
      in val * (1 :+ 0) + (blend :+ 0) * (spectrum0 V.! i)
      ) smoothed

    ------------------------------------------------------------
    -- Back to time domain
    result = fftInverse blended
    vocoded = V.map (\(r :+ _) -> r) result

    -- SKIP ENVELOPE APPLICATION - it was causing pops
    -- Just use the raw vocoded signal
    envelopeApplied_raw = vocoded  -- Skip envelope for now
    envelopeApplied = vocoded      -- Use raw vocoded output

    mixDryWet = V.zipWith (\d w -> (1 - wetRatio) * d + wetRatio * w)

    -- Apply short fade-in/fade-out to eliminate pops at file boundaries
    withFades signal =
      let len = V.length signal
          fadeSamples = min 441 (len `div` 10)  -- 10ms at 44.1kHz, or 10% of file
          applyFade i val
            | i < fadeSamples =
                let gain = fromIntegral i / fromIntegral fadeSamples
                in val * gain
            | i >= len - fadeSamples =
                let gain = fromIntegral (len - 1 - i) / fromIntegral fadeSamples
                in val * gain
            | otherwise = val
      in V.imap applyFade signal

    beforeFades = if enableMix
      then mixDryWet samples envelopeApplied
      else envelopeApplied

    -- TEMPORARILY DISABLED: Testing if fades introduce pops
    finalOutput = beforeFades  -- withFades beforeFades

    -- Collect intermediate stages for debugging
    stages =
      [ ("02_after_harmonic_filter", V.map (\(r :+ _) -> r) $ fftInverse filtered)
      , ("03_after_smoothing", V.map (\(r :+ _) -> r) $ fftInverse smoothed)
      , ("04_after_blend", V.map (\(r :+ _) -> r) $ fftInverse blended)
      , ("05_vocoded_raw", vocoded)
      , ("06_envelope_applied_raw", envelopeApplied_raw)
      , ("06b_envelope_normalized", envelopeApplied)
      , ("06c_before_fades", beforeFades)
      , ("07_with_fades_DISABLED", finalOutput)
      ]

  in (finalOutput, stages)

-- | Apply envelope ratio from original to vocoded signal
-- For each sample, interpolate between envelope points and multiply by ratio
applyEnvelopeRatio :: Int -> [AmplitudeSample] -> [AmplitudeSample] -> V.Vector Double -> V.Vector Double
applyEnvelopeRatio sampleRate origEnv vocodedEnv samples =
  let sr = fromIntegral sampleRate

      -- Build lookup function for envelope ratio at any time
      getRatio :: Double -> Double
      getRatio t =
        let -- Find surrounding envelope points
            findEnvelope env =
              case dropWhile (\s -> ampTime s < t) env of
                [] -> case reverse env of
                      [] -> 1.0
                      (lastSample:_) -> ampMagnitude lastSample
                (current:_) ->
                  case takeWhile (\s -> ampTime s < t) env of
                    [] -> ampMagnitude current
                    prevSamples ->
                      let prev = last prevSamples
                          -- Linear interpolation
                          alpha = (t - ampTime prev) / (ampTime current - ampTime prev)
                      in ampMagnitude prev + alpha * (ampMagnitude current - ampMagnitude prev)

            origAmp = findEnvelope origEnv
            vocodedAmp = findEnvelope vocodedEnv

            -- Calculate ratio, avoiding division by zero
            ratio = if vocodedAmp > 1e-6
                    then origAmp / vocodedAmp
                    else 1.0
        in ratio

      -- Apply ratio to each sample
      applyToSample i sample =
        let t = fromIntegral i / sr
            ratio = getRatio t
        in sample * ratio

  in V.imap applyToSample samples

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
