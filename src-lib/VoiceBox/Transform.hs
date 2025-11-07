module VoiceBox.Transform
  ( -- * Transformation
    transformAudio
  , transformAudioWithParams
  , transformStages
  , transformWavFile
    -- * Parameters
  , TransformParams (..)
  , preDoll0Params
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
transformAudio sampleRate = transformAudioWithParams sampleRate preDoll0Params

-- | Transform audio with custom parameters
transformAudioWithParams :: Int -> TransformParams -> V.Vector Double -> V.Vector Double
transformAudioWithParams sampleRate params samples =
  let -- Vocoder → speedup (cleans signal first, then pitch shifts)
      fundamental = fromIntegral (tpAutotuneSteps params)
      speedupFactor = 2.0
      samples0 = applyHarmonicVocoder sampleRate fundamental samples
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
  let fundamental = fromIntegral (tpAutotuneSteps params)
      speedupFactor = 2.0
      s0 = samples
      s1 = applyHarmonicVocoder sampleRate fundamental s0
      s2 = applySimpleSpeedup speedupFactor s0  -- Speedup from original
      s3 = applySimpleSpeedup speedupFactor s1  -- Vocoder → Speedup
      s4 = applyHarmonicVocoder sampleRate fundamental s2  -- Speedup → Vocoder
  in [ ("00_original", s0)
     , ("01_vocoder", s1)  -- Harmonic vocoder (60Hz detection, drop <240Hz, rolloff 3-4kHz)
     , ("02_speedup", s2)  -- 2x speedup (chipmunk effect) from original
     , ("03_vocoder_speedup", s3)  -- Vocoder → speedup (FINAL - clean harmonics then pitch shift)
     , ("04_speedup_vocoder", s4)  -- Speedup → vocoder (less intelligible)
     , ("05_final", s3)  -- Using vocoder → speedup
     ]

--------------------------------------------
-- | Effect Implementations
--------------------------------------------

-- | Harmonic vocoder - keeps only exact harmonics of fundamental frequency
-- Creates pure synthetic voice by removing all non-harmonic content
-- Preserves vowel character at harmonic frequencies
-- Preserves amplitude envelope from original signal for natural dynamics
applyHarmonicVocoder :: Int -> Double -> V.Vector Double -> V.Vector Double
applyHarmonicVocoder sampleRate fundamentalHz samples =
  let n = V.length samples
      -- Extract envelope from original signal
      analysisParams = defaultAnalysisParams
      originalEnvelope = extractEnvelope sampleRate analysisParams samples

      -- Apply vocoding
      complexSamples = V.map (:+ 0) samples
      spectrum = fftForward complexSamples

      sr = fromIntegral sampleRate
      detectFundamental = 60.0  -- Detect harmonics of 60Hz for finer resolution
      minFreq = fundamentalHz   -- Drop everything below this (e.g., 240Hz)
      tolerance = 5.0  -- Hz - bandwidth around each harmonic to keep

      -- Keep only frequencies near harmonics of 60Hz, but drop low frequencies
      filtered = V.imap (\i val ->
        let freq = if i <= n `div` 2
                   then fromIntegral i * sr / fromIntegral n
                   else sr - fromIntegral (n - i) * sr / fromIntegral n
            -- Find nearest 60Hz harmonic
            harmonic = round (freq / detectFundamental) :: Int
            harmonicFreq = detectFundamental * fromIntegral harmonic
            distance = abs (freq - harmonicFreq)

            -- Drop frequencies below minFreq (e.g., 240Hz)
            -- Bandwidth rolloff at high end: full strength up to 3000Hz, fade to 4000Hz, drop >4000Hz
            harmonicAttenuation
              | freq < minFreq = 0.0  -- Drop low frequencies
              | freq <= 3000.0 = 1.0  -- Full strength up to 3kHz
              | freq >= 4000.0 = 0.0  -- Drop above 4kHz
              | otherwise = (4000.0 - freq) / 1000.0  -- Linear fade 3kHz→4kHz

        in if distance <= tolerance && harmonic > 0
           then val * (harmonicAttenuation :+ 0)
           else 0 :+ 0) spectrum

      -- Convert back to time domain
      result = fftInverse filtered
      vocoded = V.map (\(r :+ _) -> r) result

      -- Extract envelope from vocoded signal
      vocodedEnvelope = extractEnvelope sampleRate analysisParams vocoded

      -- Apply original envelope to vocoded signal
      envelopeApplied = applyEnvelopeRatio sampleRate originalEnvelope vocodedEnvelope vocoded

  in envelopeApplied

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

-- | Apply pitch shift (simple resampling approach)
-- For factor > 1.0, shifts pitch up; < 1.0 shifts down
-- PRESERVES original duration by reading at different rate
applyPitchShift :: Double -> V.Vector Double -> V.Vector Double
applyPitchShift factor samples
  | factor == 1.0 = samples
  | otherwise =
      let len = V.length samples
          -- Keep same length, but read from source at different rate
          getSample :: Int -> Double
          getSample i =
            let srcIdx = fromIntegral i * factor
                idx1 = floor srcIdx
                idx2 = min (len - 1) (idx1 + 1)
                frac = srcIdx - fromIntegral idx1
            in if idx1 >= len then 0
               else (samples V.! idx1) * (1 - frac) + (samples V.! idx2) * frac
      in V.generate len getSample

-- | Apply autotune effect (quantize pitch to harmonics)
-- Amount controls strength: 0.0 = no effect, 1.0 = full quantization
-- Steps is the fundamental frequency (Hz) - pitch snaps to multiples of this (e.g., 60 Hz)
-- PitchShift is applied before quantization (e.g., 1.25 to shift up)
applyAutotune :: Int -> Double -> Int -> Double -> V.Vector Double -> V.Vector Double
applyAutotune sampleRate amount fundamentalHz pitchShift samples
  | amount <= 0.0 = samples
  | otherwise =
      -- Simple autotune: detect pitch via zero-crossings and quantize
      -- This is a simplified version - real autotune uses phase vocoder
      let windowSize = 1024  -- ~23ms at 44.1kHz
          hopSize = windowSize `div` 2
          numWindows = max 1 ((V.length samples - windowSize) `div` hopSize + 1)

          fundamental = fromIntegral fundamentalHz

          -- Process in overlapping windows with overlap-add
          processWindow :: Int -> V.Vector Double
          processWindow winIdx =
            let start = min (V.length samples - windowSize) (winIdx * hopSize)
                window = V.slice start (min windowSize (V.length samples - start)) samples
                -- Detect pitch via zero crossings (crude but fast)
                crossings = countZeroCrossings window
                estimatedFreq = fromIntegral (crossings * sampleRate) / (2.0 * fromIntegral (V.length window))
                -- Apply pitch shift before quantization
                targetFreq = estimatedFreq * pitchShift
                -- Quantize to nearest multiple of fundamental frequency
                harmonic = round (targetFreq / fundamental) :: Int
                quantizedFreq = fundamental * fromIntegral (max 1 harmonic)  -- At least 1x fundamental
                shiftRatio = if estimatedFreq > 20 then quantizedFreq / estimatedFreq else 1.0
                -- Blend between original and quantized
                blendedRatio = 1.0 + amount * (shiftRatio - 1.0)
            in applyPitchShift blendedRatio window

          -- Overlap-add: blend processed windows back together
          output = V.replicate (V.length samples) 0.0
          addWindow :: V.Vector Double -> Int -> V.Vector Double
          addWindow acc winIdx =
            let start = winIdx * hopSize
                processed = processWindow winIdx
                -- Simple triangular window for blending
                envelope i = min 1.0 (min (fromIntegral i / 256.0) ((fromIntegral (V.length processed) - fromIntegral i) / 256.0))
            in V.imap (\i v ->
                 if i >= start && i < start + V.length processed
                 then v + (processed V.! (i - start)) * envelope (i - start)
                 else v) acc

      in V.foldl' addWindow output (V.enumFromN 0 numWindows)

-- | Count zero crossings in a signal (for pitch detection)
countZeroCrossings :: V.Vector Double -> Int
countZeroCrossings samples =
  V.sum $ V.zipWith (\a b -> if signum a /= signum b && a /= 0 && b /= 0 then 1 else 0)
    samples (V.tail samples)

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
