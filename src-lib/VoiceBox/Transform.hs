module VoiceBox.Transform
  ( -- * Transformation
    transformAudio
  , transformAudioWithParams
  , transformWavFile
    -- * Parameters
  , TransformParams (..)
  , preDoll0Params
  ) where

import qualified Data.Vector.Storable as V
import Data.Complex (Complex ((:+)))
import qualified Math.FFT as FFT
import qualified Data.Array.CArray as CA
import VoiceBox.Analyze (readWaveFile, writeWaveFile)

--------------------------------------------
-- | Transformation Parameters
--------------------------------------------

-- | Parameters for audio transformation effects
data TransformParams = TransformParams
  { tpBandwidthHz    :: Double -- ^ Maximum frequency (Hz) - typically ~4000 for PreDoll-0
  , tpHumFreq        :: Double -- ^ Power hum frequency (Hz) - typically 60
  , tpHumLevel       :: Double -- ^ Hum amplitude (0.0-1.0)
  , tpMetallicDepth  :: Double -- ^ Ring modulation depth (0.0-1.0)
  , tpMetallicFreq   :: Double -- ^ Ring modulation frequency (Hz)
  , tpDecayRate      :: Double -- ^ Energy decay rate (exponential factor)
  , tpRecoveryTime   :: Double -- ^ Recovery time between segments (seconds)
  , tpNoiseLevel     :: Double -- ^ Transition noise level (0.0-1.0)
  , tpPitchShift     :: Double -- ^ Pitch shift factor (1.0 = no change, 1.5 = up, 0.67 = down)
  , tpAutotuneAmount :: Double -- ^ Autotune quantization strength (0.0-1.0)
  } deriving (Show, Eq)

-- | PreDoll-0 speech transformation parameters
-- Based on electromagnetic resonance constraints and ~4kHz bandwidth
-- Tuned for synthetic/artificial child-like feminine voice
preDoll0Params :: TransformParams
preDoll0Params = TransformParams
  { tpBandwidthHz    = 3500.0  -- Less restrictive bandwidth
  , tpHumFreq        = 60.0    -- 60 Hz mains hum (North America)
  , tpHumLevel       = 0.03    -- Reduced electronic hum
  , tpMetallicDepth  = 0.35    -- Moderate metallic character
  , tpMetallicFreq   = 1000.0  -- Balanced resonance
  , tpDecayRate      = 0.95    -- Gradual energy decay
  , tpRecoveryTime   = 0.1     -- 100ms recovery between phrases
  , tpNoiseLevel     = 0.04    -- Subtle transition artifacts
  , tpPitchShift     = 1.25    -- Moderate pitch shift (still feminine)
  , tpAutotuneAmount = 0.35    -- Moderate autotune effect
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
  let sr = fromIntegral sampleRate
      -- Apply effects pipeline
      samples1 = applyPitchShift (tpPitchShift params) samples  -- Pitch shift first
      samples2 = applyAutotune sampleRate (tpAutotuneAmount params) samples1  -- Then autotune
      samples3 = applyBandwidthLimit sampleRate (tpBandwidthHz params) samples2
      samples4 = injectHum sr (tpHumFreq params) (tpHumLevel params) samples3
      samples5 = applyMetallicEffect sr (tpMetallicFreq params) (tpMetallicDepth params) samples4
      samples6 = applyEnergyDecay (tpDecayRate params) samples5
      samples7 = applyTransitionNoise (tpNoiseLevel params) samples6
  in samples7

--------------------------------------------
-- | Effect Implementations
--------------------------------------------

-- | Apply bandwidth limiting via lowpass filter
-- Simulates hardware constraint of ~4kHz resonator system
applyBandwidthLimit :: Int -> Double -> V.Vector Double -> V.Vector Double
applyBandwidthLimit sampleRate cutoffHz samples =
  -- Simple FFT-based lowpass filter
  let n = V.length samples
      -- Convert to frequency domain
      complexSamples = V.map (:+ 0) samples
      spectrum = fftForward complexSamples

      -- Zero out frequencies above cutoff
      sr = fromIntegral sampleRate
      filtered = V.imap (\i val ->
        let freq = fromIntegral i * sr / fromIntegral n
        in  if freq <= cutoffHz || freq >= (sr - cutoffHz)
            then val
            else 0 :+ 0) spectrum

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

-- | Apply metallic/harmonic effect via ring modulation
-- Creates "bell-like" or resonant character
applyMetallicEffect :: Double -> Double -> Double -> V.Vector Double -> V.Vector Double
applyMetallicEffect sampleRate modFreq depth = V.imap (\i s ->
    let t = fromIntegral i / sampleRate
        carrier = sin (2 * pi * modFreq * t)
        -- Ring modulation: multiply signal by carrier
        modulated = s * carrier
    in s * (1 - depth) + modulated * depth)

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

-- | Apply autotune effect (quantize pitch to musical notes)
-- Amount controls strength: 0.0 = no effect, 1.0 = full quantization
applyAutotune :: Int -> Double -> V.Vector Double -> V.Vector Double
applyAutotune sampleRate amount samples
  | amount <= 0.0 = samples
  | otherwise =
      -- Simple autotune: detect pitch via zero-crossings and quantize
      -- This is a simplified version - real autotune uses phase vocoder
      let windowSize = 1024  -- ~23ms at 44.1kHz
          hopSize = windowSize `div` 2
          numWindows = max 1 ((V.length samples - windowSize) `div` hopSize + 1)

          -- Process in overlapping windows with overlap-add
          processWindow :: Int -> V.Vector Double
          processWindow winIdx =
            let start = min (V.length samples - windowSize) (winIdx * hopSize)
                window = V.slice start (min windowSize (V.length samples - start)) samples
                -- Detect pitch via zero crossings (crude but fast)
                crossings = countZeroCrossings window
                estimatedFreq = fromIntegral (crossings * sampleRate) / (2.0 * fromIntegral (V.length window))
                -- Quantize to nearest semitone
                semitone = round (12 * logBase 2 (max 20 estimatedFreq / 440.0)) :: Int
                quantizedFreq = 440.0 * (2 ** (fromIntegral semitone / 12.0))
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
