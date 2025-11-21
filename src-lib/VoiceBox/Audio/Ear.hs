{-# LANGUAGE ScopedTypeVariables #-}

module VoiceBox.Audio.Ear where

import Control.Monad (guard)
import Control.Monad.Reader (Reader, ask, asks, runReader)
import DSP.Filter.IIR.IIR (iir_df1)
import Data.Array
import Data.Complex (Complex (..), phase)
import Data.Fixed (mod')
import Data.Int (Int8)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import LambdaSound (Hz (..))
import Numeric.Transform.Fourier.FFT (fft, ifft)
import VoiceBox.Audio.Ear.Data
import VoiceBox.Audio.Ear.Types
import Debug.Trace

-- might change to a newtype later
type PulseCounts = Vector (Map Hz (Map Int Int8))

-- | Top-level function: convert audio samples into the doll's perceived pulses.
-- Includes cavity filtering and biological lag in makeCavityFilter.
runDollEar ::
  -- | The doll's ear configuration
  DollEar ->
  -- | Sample rate of the audio
  Hz ->
  -- | Input audio samples
  Vector Double ->
  Maybe EarResult
runDollEar (DollEar cavities) sampleRate samples = do
  _ <- traceM $ "[Ear] runDollEar: sampleRate = " <> show sampleRate
  _ <- traceM $ "[Ear] runDollEar: number of samples = " <> show (V.length samples)
  let pulsesRaw = runReader (processDollEar samples) (sampleRate, DollEar cavities)
  _ <- traceM $ "[Ear] runDollEar: pulsesRaw keys = " <> show (Map.keys pulsesRaw)
  let allLens = [V.length v | leafMap <- Map.elems pulsesRaw, v <- Map.elems leafMap]
  _ <- traceM $ "[Ear] runDollEar: vector lengths range = " <> show (minimum allLens, maximum allLens)
  _ <- traceM $ "[Ear] runDollEar: consistentLength = " <> show (consistentLength pulsesRaw)
  pulses <- unzipMapOfVectors pulsesRaw
  _ <- traceM $ "[Ear] runDollEar: pulses vector length = " <> show (V.length pulses)
  pure (EarResult sampleRate pulses)

-- | Transpose a Map of Maps of Vectors into a Vector of Maps of Maps.
unzipMapOfVectors ::
  forall k1 k2 v.
  (Ord k1, Ord k2) =>
  Map k1 (Map k2 (Vector v)) ->
  Maybe (Vector (Map k1 (Map k2 v)))
unzipMapOfVectors m =
  if Map.null mTrimmed then Just V.empty
  else do
      vecLen <- consistentLength mTrimmed
      V.fromList <$> traverse buildMap [0 .. vecLen - 1]
  where
    mTrimmed = trimMapOfVectorsToMinLength m
    keys1 = Map.keys mTrimmed

    buildLeaf :: k2 -> Map k2 (Vector v) -> Int -> Maybe (k2, v)
    buildLeaf k2 leafMap i = do
      vec <- Map.lookup k2 leafMap
      v <- vec V.!? i
      return (k2, v)

    buildInner :: k1 -> Int -> Maybe (k1, Map k2 v)
    buildInner k1 i = do
      leafMap <- Map.lookup k1 mTrimmed
      fmap (k1,) $ Map.fromList <$> traverse (\k2 -> buildLeaf k2 leafMap i) (Map.keys leafMap)

    buildMap :: Int -> Maybe (Map k1 (Map k2 v))
    buildMap i = Map.fromList <$> traverse (`buildInner` i) keys1

-- | Return the consistent length of all vectors in a Map of Maps of Vectors.
consistentLength :: Map k1 (Map k2 (Vector v)) -> Maybe Int
consistentLength mm = do
  let lengths = [V.length v | leafMap <- Map.elems mm, v <- Map.elems leafMap]
  firstLength <- listToMaybe lengths
  guard (all (== firstLength) lengths)
  return firstLength

-- | Trim all vectors in a Map of Maps to the minimum length, from the beginning.
-- Why do we trim vectors from the beginning?
-- Each drift vector is computed as phaseTrace[i + delay] - phaseTrace[i],
-- so its first element corresponds to the *later* time point (i + delay).
-- To align all drift vectors at the latest possible time (so their last sample
-- represents the same time index across all delays), we trim from the beginning,
-- keeping only the last N elements (where N is the minimum vector length).
-- This ensures all vectors are aligned at the end, preserving correct time correspondence
-- for downstream analysis and synthesis.
trimMapOfVectorsToMinLength :: Map k1 (Map k2 (Vector v)) -> Map k1 (Map k2 (Vector v))
trimMapOfVectorsToMinLength m =
  let allLens = [V.length v | leafMap <- Map.elems m, v <- Map.elems leafMap]
      minLen = if null allLens then 0 else minimum allLens
      trimVec v = V.drop (V.length v - minLen) v
      trimLeafMap = Map.map trimVec
  in Map.map trimLeafMap m

--------------------------------------------
-- | Reader environment for ear processing.
--------------------------------------------
data EarEnv = EarEnv
  { earEnvSampleRate :: Hz,
    earEnvCavity :: ResonantCavity
  }

type EarR = Reader EarEnv

askSampleRate :: EarR Hz
askSampleRate = asks earEnvSampleRate

askCavityType :: EarR CavityType
askCavityType = asks (cavityType . earEnvCavity)

askCavityFrequency :: EarR Hz
askCavityFrequency = asks (cavityFrequency . earEnvCavity)

askCavityPeriod :: EarR Seconds
askCavityPeriod = asks (cavityPeriod . earEnvCavity)

-- | Construct a bandpass filter from the Reader environment.
-- Now includes both physical resonance and biological lag.
makeCavityFilter :: EarR (Vector Double -> Vector Double)
makeCavityFilter = do
  EarEnv fs cavity <- ask
  let fc = cavityFrequency cavity
      q = cavityQ (cavityType cavity)
      coeffs = bandpassCoeffs fs fc q
  return $ V.fromList . iir_df1 coeffs . V.toList

-- | Process input audio for all cavities in a DollEar.
processDollEar :: V.Vector Double -> Reader (Hz, DollEar) (Map Hz (Map Int (Vector Int8)))
processDollEar samples = do
  (sampleRate, DollEar cavities) <- ask
  let runCavity cavity = runReader (extractCavityPulseCounts samples) (EarEnv sampleRate cavity)
  return $ fromSetWithMonotone cavityFrequency runCavity (Set.fromList cavities)

-- | Uses 'mapKeysMonotonic' for efficiency, since cavity frequencies (keys) are strictly monotonic and never reordered. This is safe because the input set is always ordered, and avoids the overhead of rebalancing the map. Only use when you are certain the key mapping preserves order!
fromSetWithMonotone :: (k1 -> k2) -> (k1 -> v) -> Set k1 -> Map k2 v
fromSetWithMonotone keyMap valueMap set =
  Map.mapKeysMonotonic keyMap (Map.fromSet valueMap set)

-- | For a single cavity, filter the input samples, compute analytic phase,
-- and extract quantized pulse counts for each available delay tap.
-- Returns a map from delay tap to vector of pulse counts.
extractCavityPulseCounts :: V.Vector Double -> EarR (Map Int (Vector Int8))
extractCavityPulseCounts samples = do
  filtered <- makeCavityFilter <*> pure samples
  let phases = hilbertPhase filtered
  quantizePhaseDriftsToPulses phases

-- | Compute instantaneous phase from analytic signal.
hilbertPhase :: Vector Double -> Vector Double
hilbertPhase x = V.map phase (hilbertTransform x)

hilbertTransform :: Vector Double -> Vector (Complex Double)
hilbertTransform v = runOnVector ifft xh
  where
    n = V.length v
    x = runOnVector fft (V.map (:+ 0) v)
    xh = V.zipWith (\c m -> c * (m :+ 0)) x (hilbertMask n)

-- | The mask doubles positive frequencies and zeros out negative frequencies,
-- following the analytic signal construction for Hilbert transform.
-- Special handling for DC (k==0) and Nyquist (k==half, for even n) bins.
hilbertMask :: Int -> Vector Double
hilbertMask size = V.generate size go
  where
    half = size `div` 2
    go k
      | k == 0 = 1
      | k < half = 2
      | even size && k == half = 1
      | otherwise = 0

-- | Run a function on a Vector by converting to Array and back.
runOnVector :: (Array Int a -> Array Int a) -> Vector a -> Vector a
runOnVector f v =
  let arr = listArray (0, V.length v - 1) (V.toList v)
      result = f arr
   in V.fromList (elems result)

-- | Compute available delay taps in samples.
availableDelays :: EarR (Set Int)
availableDelays = do
  cType <- askCavityType
  delaySeconds <- case cType of
    Drum -> drumDelays
    BismuthWrapping -> bismuthDelays
    ShellResonance -> pure shellDelays
  Set.fromList <$> mapM numSamplesDuring delaySeconds

-- | Convert seconds to number of samples based on current sample rate.
numSamplesDuring :: Seconds -> EarR Int
numSamplesDuring sec = do
  Hz sr <- askSampleRate
  return $ round (sec * realToFrac sr)

-- Drum delays based on integer multiples of period
drumDelays :: EarR [Seconds]
drumDelays = do
  period <- askCavityPeriod
  freq <- askCavityFrequency
  let cycles = drumDelayCyclesFromFreq freq
  pure [scaleI n period | n <- [0 .. cycles]]

-- BismuthWrapping: fixed 4 taps
bismuthDelays :: EarR [Seconds]
bismuthDelays = do
  period <- askCavityPeriod
  pure [scaleI n period | (n :: Int) <- [0 .. 3]]

-- ShellResonance: odd multiples
shellDelays :: [Seconds]
shellDelays = (`scaleI` minDelay) <$> ([1, 3, 5, 7] :: [Int])

-- Calculate phase differences for a cavity
quantizePhaseDriftsToPulses :: V.Vector Double -> EarR (Map Int (V.Vector Int8))
quantizePhaseDriftsToPulses phaseTrace = do
  delays <- Set.filter (<= V.length phaseTrace) <$> availableDelays
  let n = V.length phaseTrace
      differencesFor delay =
        V.zipWith phasesToPulseCount
          (V.drop delay phaseTrace)
          (V.take (n - delay) phaseTrace)
  return $ differencesFor `Map.fromSet` delays

-- | For a given phase trace, compute phase differences for all delay taps,
-- then quantize each difference to a pulse count.
-- Returns a map from delay tap to vector of pulse counts.
phasesToPulseCount :: Double -> Double -> Int8
phasesToPulseCount phi0 phi1 =
  let intensity = linearPhaseIntensity phi0 phi1
   in floor (intensity * fromIntegral maxPulses + 0.5)

-- | Compute linear intensity from phase difference.
-- Intensity scales linearly from -1 to 1 as phase difference goes from -pi to π.
linearPhaseIntensity :: Double -> Double -> Double
linearPhaseIntensity phi0 phi1 = min 1.0 (phaseDiff phi0 phi1 / pi)

-- | Compute the wrapped phase difference between two phase values.
phaseDiff :: Double -> Double -> Double
phaseDiff a b = wrapPhase (a - b)
  where
  -- | Wraps phase to [-π, π] for consistent intensity calculation,
  -- avoiding discontinuities at ±π.
  wrapPhase :: Double -> Double
  wrapPhase x =
    let r = mod' x (2 * pi)
    in if r > pi then r - 2 * pi else r

-- Bandpass coefficients from RBJ cookbook
bandpassCoeffs :: Hz -> Hz -> Double -> (Array Int Double, Array Int Double)
bandpassCoeffs fs fc q =
  let Hz omega = 2 * pi * fc / fs
      alpha = sin omega / (2 * realToFrac q)
      b0 = realToFrac alpha
      b1 = 0
      b2 = -realToFrac alpha
      a0 = 1 + realToFrac alpha
      a1 = -(2 * cos (realToFrac omega))
      a2 = 1 - realToFrac alpha
   in (listArray (0, 2) [b0 / a0, b1 / a0, b2 / a0], listArray (0, 2) [1, a1 / a0, a2 / a0])

-- Drum radius → cycles
drumRadiusFromFreq :: Hz -> Double
drumRadiusFromFreq freq = powerFrequency * powerDrumRadius * realToFrac (hzToPeriod freq)

drumDelayCyclesFromFreq :: Hz -> Int
drumDelayCyclesFromFreq freq =
  let radius = drumRadiusFromFreq freq
   in floor ((radius - 0.25) * 2)
