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
  { tpBandwidthHz       :: Double -- ^ Maximum frequency (Hz) - typically ~4000 for PreDoll-0
  , tpHumFreq           :: Double -- ^ Power hum frequency (Hz) - typically 60
  , tpHumLevel          :: Double -- ^ Hum amplitude (0.0-1.0)
  , tpMetallicDepth     :: Double -- ^ Ring modulation depth (0.0-1.0)
  , tpMetallicFreq      :: Double -- ^ Ring modulation frequency (Hz)
  , tpDecayRate         :: Double -- ^ Energy decay rate (exponential factor)
  , tpRecoveryTime      :: Double -- ^ Recovery time between segments (seconds)
  , tpNoiseLevel        :: Double -- ^ Transition noise level (0.0-1.0)
  , tpPitchShift        :: Double -- ^ Pitch shift factor (1.0 = no change, 1.5 = up, 0.67 = down)
  , tpAutotuneAmount    :: Double -- ^ Autotune quantization strength (0.0-1.0)
  , tpAutotuneSteps     :: Int    -- ^ Fundamental frequency (Hz) - pitch snaps to multiples (e.g., 60 for power line harmonics)
  } deriving (Show, Eq)

-- | PreDoll-0 speech transformation parameters
-- Based on electromagnetic resonance constraints and ~4kHz bandwidth
-- Tuned for synthetic/artificial child-like feminine voice
preDoll0Params :: TransformParams
preDoll0Params = TransformParams
  { tpBandwidthHz     = 3500.0  -- Less restrictive bandwidth
  , tpHumFreq         = 60.0    -- 60 Hz mains hum (North America)
  , tpHumLevel        = 0.03    -- Reduced electronic hum
  , tpMetallicDepth   = 0.5     -- Resonant cavity boost (now using filter, not ring mod)
  , tpMetallicFreq    = 1000.0  -- Cavity resonance frequency
  , tpDecayRate       = 0.95    -- Gradual energy decay
  , tpRecoveryTime    = 0.1     -- 100ms recovery between phrases
  , tpNoiseLevel      = 0.04    -- Subtle transition artifacts
  , tpPitchShift      = 1.25    -- Moderate pitch shift (still feminine)
  , tpAutotuneAmount  = 0.9     -- Very strong autotune effect (increased from 0.7)
  , tpAutotuneSteps   = 120     -- Quantize to 120 Hz harmonics (larger steps = more robotic)
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
  let -- Apply harmonic vocoder first with envelope preservation and bandwidth rolloff
      fundamental = fromIntegral (tpAutotuneSteps params)
      samples0 = applyHarmonicVocoder sampleRate fundamental samples
      -- Then apply autotune for intelligibility
      samples1 = applyAutotune sampleRate (tpAutotuneAmount params) (tpAutotuneSteps params) (tpPitchShift params) samples0
  in samples1

-- | Return named intermediate stages of the transformation pipeline
transformStages :: Int -> TransformParams -> V.Vector Double -> [(String, V.Vector Double)]
transformStages sampleRate params samples =
  let fundamental = fromIntegral (tpAutotuneSteps params)
      s0 = samples
      s1 = applyHarmonicVocoder sampleRate fundamental s0
      s2 = applyAutotune sampleRate (tpAutotuneAmount params) (tpAutotuneSteps params) (tpPitchShift params) s1
  in [ ("00_original", s0)
     , ("01_vocoder", s1)  -- Harmonic vocoder with envelope + bandwidth rolloff (120Hz fundamental: harmonics 1-20 full, 21-25 fade, >25 drop)
     , ("02_autotune", s2)  -- Pitch shift + autotune (improves intelligibility)
     , ("03_final", s2)
     ]

--------------------------------------------
-- | Effect Implementations
--------------------------------------------

-- | Apply bandwidth limiting via lowpass filter
-- Simulates hardware constraint of ~4kHz resonator system
applyBandwidthLimit :: Int -> Double -> V.Vector Double -> V.Vector Double
applyBandwidthLimit sampleRate cutoffHz samples =
  -- FFT-based lowpass filter with smooth rolloff to reduce ringing
  let n = V.length samples
      -- Convert to frequency domain
      complexSamples = V.map (:+ 0) samples
      spectrum = fftForward complexSamples

      -- Smooth rolloff to prevent ringing artifacts
      sr = fromIntegral sampleRate
      transitionWidth = 500.0  -- Hz - width of rolloff region
      filtered = V.imap (\i val ->
        let freq = if i <= n `div` 2
                   then fromIntegral i * sr / fromIntegral n
                   else sr - fromIntegral (n - i) * sr / fromIntegral n
            -- Smooth transition using cosine rolloff
            attenuation
              | freq <= cutoffHz = 1.0
              | freq >= cutoffHz + transitionWidth = 0.0
              | otherwise = (1.0 + cos (pi * (freq - cutoffHz) / transitionWidth)) / 2.0
        in val * (attenuation :+ 0)) spectrum

      -- Convert back to time domain
      result = fftInverse filtered
  in V.map (\(r :+ _) -> r) result

-- | Inject 60 Hz electromagnetic hum
-- Simulates power system interference in Doll hardware
injectHum :: Double -> Double -> Double -> V.Vector Double -> V.Vector Double
injectHum sampleRate humFreq level = V.imap (\i s ->
    let t = fromIntegral i / sampleRate
        hum = level * sin (2 * pi * humFreq * t)
    in s + hum)

-- | Apply metallic/harmonic effect via resonant filtering
-- Simulates electromagnetic cavity resonance by boosting a narrow frequency band
applyMetallicEffect :: Double -> Double -> Double -> V.Vector Double -> V.Vector Double
applyMetallicEffect sampleRate resonantFreq depth samples =
  -- Use FFT to boost frequencies around the resonant frequency
  let n = V.length samples
      complexSamples = V.map (:+ 0) samples
      spectrum = fftForward complexSamples

      sr = sampleRate
      -- Create a resonance curve (Gaussian-like boost around resonantFreq)
      bandwidth = 200.0  -- Width of the resonance peak
      boosted = V.imap (\i val ->
        let freq = fromIntegral i * sr / fromIntegral n
            -- Gaussian-like boost centered at resonantFreq
            distance = abs (freq - resonantFreq)
            boost = 1.0 + depth * exp (negate (distance * distance) / (2 * bandwidth * bandwidth))
        in val * (boost :+ 0)) spectrum

      -- Convert back to time domain
      result = fftInverse boosted
  in V.map (\(r :+ _) -> r) result

-- | Apply energy decay envelope
-- Simulates gradual power loss during speech
-- TODO: Make this segment-based rather than global
applyEnergyDecay :: Double -> V.Vector Double -> V.Vector Double
applyEnergyDecay _decayRate samples =
  -- DISABLED for now - the global exponential decay kills the signal
  -- This should be applied per speech segment, not to the entire audio
  -- For now, just pass through unchanged
  samples
  -- Original (broken) implementation:
  -- let modulate i s = (_decayRate ** fromIntegral i) * s
  -- in V.imap modulate samples

-- | Add noise during transitions
-- Simulates chaotic emission between stable resonances
applyTransitionNoise :: Double -> V.Vector Double -> V.Vector Double
applyTransitionNoise level samples =
  -- Detect transitions by looking at amplitude changes
  -- Add noise where change is rapid
  let deltas = V.zipWith (\a b -> abs (b - a)) samples (V.tail samples V.++ V.singleton 0)
      threshold = 0.01 -- Threshold for detecting transitions
  in V.zipWith (\s delta ->
    if delta > threshold
    then s + (pseudoRandom s * level)  -- Add noise at transitions
    else s) samples deltas
  where
    -- Simple deterministic "noise" based on sample value
    -- In production, use a proper PRNG
    pseudoRandom x = sin (x * 12345.6789) * 0.5

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
      tolerance = 5.0  -- Hz - bandwidth around each harmonic to keep

      -- Keep only frequencies near harmonics of fundamental, with bandwidth rolloff
      filtered = V.imap (\i val ->
        let freq = if i <= n `div` 2
                   then fromIntegral i * sr / fromIntegral n
                   else sr - fromIntegral (n - i) * sr / fromIntegral n
            -- Find nearest harmonic
            harmonic = round (freq / fundamentalHz) :: Int
            harmonicFreq = fundamentalHz * fromIntegral harmonic
            distance = abs (freq - harmonicFreq)

            -- Bandwidth rolloff: full strength up to harmonic 20, fade 21-25, drop >25
            -- (120Hz fundamental: harmonic 20 = 2400Hz, harmonic 25 = 3000Hz)
            harmonicAttenuation
              | harmonic <= 20 = 1.0
              | harmonic >= 25 = 0.0
              | otherwise = fromIntegral (25 - harmonic) / 5.0  -- Linear fade 21→25

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
