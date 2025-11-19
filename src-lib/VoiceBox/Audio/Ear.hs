{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module VoiceBox.Audio.Ear (getDollHearing) where

import Control.Monad.Reader (Reader, ask, runReader, asks)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Maybe (listToMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Word (Word8)
import Data.Set (Set)
import Data.Set qualified as Set
import LambdaSound (Hz(..))
import Data.Complex (Complex(..), phase)
import Numeric.Transform.Fourier.FFT (fft, ifft)
import DSP.Filter.IIR.IIR (iir_df1)
import VoiceBox.Audio.Ear.Data
import Data.Array
import Data.Fixed (mod')
import Control.Monad (guard)

-- | Top-level function: convert audio samples into the doll's perceived pulses.
-- Includes cavity filtering and biological lag in makeCavityFilter.
getDollHearing ::
  DollEar ->       -- ^ The doll's ear configuration
  Hz ->            -- ^ Sample rate of the audio
  Vector Double -> -- ^ Input audio samples
  Maybe (Vector (Map Hz (Map Int Word8)))
getDollHearing dollEar sampleRate samples =
  unzipMapOfVectors $ runReader (processDollEar samples) (sampleRate, dollEar)

-- | Transpose a Map of Maps of Vectors into a Vector of Maps of Maps.
-- Returns Nothing if any vector has a different length, or the map is empty.
unzipMapOfVectors :: forall k1 k2 v.
  (Ord k1, Ord k2)
  => Map k1 (Map k2 (Vector v))
  -> Maybe (Vector (Map k1 (Map k2 v)))
unzipMapOfVectors m
  | Map.null m = Just V.empty
  | otherwise = do
      vecLen <- consistentLength m
      V.fromList <$> traverse buildMap [0 .. vecLen - 1]
  where
    keys1 = Map.keys m

    buildLeaf :: k2 -> Map k2 (Vector v) -> Int -> Maybe (k2, v)
    buildLeaf k2 leafMap i = do
      vec <- Map.lookup k2 leafMap
      v   <- vec V.!? i
      return (k2, v)

    buildInner :: k1 -> Int -> Maybe (k1, Map k2 v)
    buildInner k1 i = do
      leafMap <- Map.lookup k1 m
      fmap (k1,) $ Map.fromList <$> traverse (\k2 -> buildLeaf k2 leafMap i) (Map.keys leafMap)

    buildMap :: Int -> Maybe (Map k1 (Map k2 v))
    buildMap i = Map.fromList <$> traverse (`buildInner` i) keys1

    -- | Return length if all vectors have the same length
    consistentLength :: Map k1 (Map k2 (Vector v)) -> Maybe Int
    consistentLength mm = do
      let lengths = [ V.length v | leafMap <- Map.elems mm, v <- Map.elems leafMap ]
      firstLength <- listToMaybe lengths
      guard (all (== firstLength) lengths)
      return firstLength

-- | Reader environment for ear processing.
data EarEnv = EarEnv
  { earSampleRate :: Hz
  , earCavity     :: ResonantCavity
  }

type EarR = Reader EarEnv

askSampleRate :: EarR Hz
askSampleRate = asks earSampleRate

askCavityType :: EarR CavityType
askCavityType = asks (cavityType . earCavity)

askCavityFrequency :: EarR Hz
askCavityFrequency = asks (cavityFrequency . earCavity)

askCavityPeriod :: EarR Seconds
askCavityPeriod = asks (cavityPeriod . earCavity)

-- | Construct a bandpass filter from the Reader environment.
-- Now includes both physical resonance and biological lag.
makeCavityFilter :: EarR (Vector Double -> Vector Double)
makeCavityFilter = do
  EarEnv fs cavity <- ask
  let fc = cavityFrequency cavity
      q  = cavityQ (cavityType cavity)
      coeffs = bandpassCoeffs fs fc q
  return $ V.fromList . iir_df1 coeffs . V.toList

-- | Process input audio for all cavities in a DollEar.
processDollEar :: V.Vector Double -> Reader (Hz, DollEar) (Map Hz (Map Int (Vector Word8)))
processDollEar samples = do
  (sampleRate, DollEar cavities) <- ask
  let runCavity cavity = runReader (tapPulses samples) (EarEnv sampleRate cavity)
  return $ fromSetWithMonotone cavityFrequency runCavity (Set.fromList cavities)

fromSetWithMonotone :: (k1 -> k2) -> (k1 -> v) -> Set k1 -> Map k2 v
fromSetWithMonotone keyMap valueMap set =
  Map.mapKeysMonotonic keyMap (Map.fromSet valueMap set)

-- | Compute pulse counts for a single cavity from audio samples.
tapPulses :: V.Vector Double -> EarR (Map Int (Vector Word8))
tapPulses samples = do
  filtered <- makeCavityFilter <*> pure samples
  let phases = hilbertPhase filtered
  diffs <- phaseDifferencesForCavity phases
  pure (V.map phaseToPulseCount <$> diffs)

-- | Compute instantaneous phase from analytic signal.
hilbertPhase :: Vector Double -> Vector Double
hilbertPhase x = V.map phase (hilbertTransform x)

hilbertTransform :: Vector Double -> Vector (Complex Double)
hilbertTransform v = runOnVector ifft xh
  where
    n  = V.length v
    x  = runOnVector fft (V.map (:+ 0) v)
    xh = V.zipWith (\c m -> c * (m :+ 0)) x (hilbertMask n)
    hilbertMask :: Int -> Vector Double
    hilbertMask size = V.generate size go
      where
        half = size `div` 2
        go k
          | k == 0               = 1
          | k < half             = 2
          | even n && k == half  = 1
          | otherwise            = 0

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
    Drum            -> drumDelays
    BismuthWrapping -> bismuthDelays
    ShellResonance  -> pure shellDelays
  Set.fromList <$> mapM numSamplesDuring delaySeconds

numSamplesDuring :: Seconds -> EarR Int
numSamplesDuring sec = do
  Hz sr <- askSampleRate
  return $ round (sec * realToFrac sr)

-- Drum delays based on integer multiples of period
drumDelays :: EarR [Seconds]
drumDelays = do
  period <- askCavityPeriod
  freq   <- askCavityFrequency
  let cycles = drumDelayCyclesFromFreq freq
  pure [ scaleI n period | n <- [0 .. cycles] ]

-- BismuthWrapping: fixed 4 taps
bismuthDelays :: EarR [Seconds]
bismuthDelays = do
  period <- askCavityPeriod
  pure [ scaleI n period | (n :: Int) <- [0 .. 3] ]

-- ShellResonance: odd multiples
shellDelays :: [Seconds]
shellDelays = (`scaleI` minDelay) <$> ([1,3,5,7] :: [Int])

-- Calculate phase differences for a cavity
phaseDifferencesForCavity :: V.Vector Double -> EarR (Map Int (V.Vector Double))
phaseDifferencesForCavity phaseTrace = do
  delays <- Set.filter (<= V.length phaseTrace) <$> availableDelays
  let n = V.length phaseTrace
      differencesFor delay = V.generate (n - delay) $ \i ->
        phaseTrace V.! (i + delay) - phaseTrace V.! i
  return $ differencesFor `Map.fromSet` delays

-- | Convert phase difference to pulse counts
phaseToPulseCount :: Double -> Word8
phaseToPulseCount deltaPhi =
  let intensity = linearPhaseIntensity deltaPhi
  in floor (intensity * fromIntegral maxPulses + 0.5)

linearPhaseIntensity :: Double -> Double
linearPhaseIntensity deltaPhi = min 1.0 (abs (wrapPhase deltaPhi) / pi)

wrapPhase :: Double -> Double
wrapPhase x =
  let r = mod' x (2*pi)
  in if r > pi then r - 2*pi else r

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
  in (listArray (0,2) [b0/a0,b1/a0,b2/a0], listArray (0,2) [1,a1/a0,a2/a0])

-- Drum radius → cycles
drumRadiusFromFreq :: Hz -> Double
drumRadiusFromFreq freq = powerFrequency * powerDrumRadius * realToFrac (hzToPeriod freq)

drumDelayCyclesFromFreq :: Hz -> Int
drumDelayCyclesFromFreq freq =
  let radius = drumRadiusFromFreq freq
  in floor ((radius - 0.25) * 2)
