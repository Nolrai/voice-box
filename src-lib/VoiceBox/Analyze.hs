{-# LANGUAGE RecordWildCards #-}

-- | VoiceBox.Analyze
--   Extracts low-level acoustic features (pitch, amplitude, formants)
--   from real audio input. This is the "auditory sensorium" of the Dolls.
--
--   Uses Haskell libraries for DSP:
--     - WAVE for audio file I/O
--     - vector for efficient sample storage
--     - fft (FFTW3 bindings) for fast FFT computation
--     - Autocorrelation for pitch detection
--
--   Implementation: Real FFT-based analysis for pitch, envelope, and formants.
module VoiceBox.Analyze
  ( analyzeAudio,
    analyzeAudioWithParams,
    extractPitch,
    extractEnvelope,
    extractFormants,
    readWaveFile,
  )
where

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

--------------------------------------------

-- | Audio file I/O

--------------------------------------------

-- | Read a WAVE file and convert to a vector of Double samples
readWaveFile :: FilePath -> IO (Either String (V.Vector Double, Int))
readWaveFile path = do
  result <- try (WAVE.getWAVEFile path) :: IO (Either SomeException WAVE.WAVE)
  case result of
    Left err -> pure $ Left $ "Failed to read WAVE file: " ++ show err
    Right wave -> do
      let sampleRate = fromIntegral $ WAVE.waveFrameRate $ WAVE.waveHeader wave
      pure $ do
        samples <- waveSamplesToDouble wave
        Right (samples, sampleRate)

-- | Convert WAVE samples to normalized Double vector [-1.0, 1.0]
waveSamplesToDouble :: WAVE.WAVE -> Either String (V.Vector Double)
waveSamplesToDouble wave = do
  let samples = WAVE.waveSamples wave

  -- Ensure we have at least one channel
  when (null samples) $
    Left "WAVE file has no audio channels"

  -- Extract first channel (mono or left)
  firstChannel <- traverse (note "Empty channel in WAVE data" . headMay) samples

  -- Normalize based on bit depth (assuming 16-bit)
  let maxVal = 32768.0 -- 2^15 for 16-bit audio
  let normalized = map (\s -> fromIntegral s / maxVal) firstChannel
  pure $ V.fromList normalized

--------------------------------------------

-- | High-level analysis pipeline

--------------------------------------------

-- | Analyze an audio file and extract all features (with default parameters)
analyzeAudio :: FilePath -> IO AudioFeatures
analyzeAudio = analyzeAudioWithParams defaultAnalysisParams

-- | Analyze an audio file with custom parameters
analyzeAudioWithParams :: AnalysisParams -> FilePath -> IO AudioFeatures
analyzeAudioWithParams params path = do
  hSetEncoding stderr utf8 -- Ensure UTF-8 output
  putStrLn $ "Analyzing audio file: " ++ path

  audioResult <- readWaveFile path
  case audioResult of
    Left err -> do
      hPutStrLn stderr $ "Warning: " ++ err
      pure emptyFeatures
    Right (samples, sampleRate) -> do
      putStrLn $ "Sample rate: " ++ show sampleRate ++ " Hz"
      putStrLn $ "Samples: " ++ show (V.length samples)
      putStrLn $ "Duration: " ++ show (fromIntegral (V.length samples) / fromIntegral sampleRate :: Double) ++ " seconds"

      -- Pass sampleRate separately to extraction functions
      let pitch = extractPitch sampleRate params samples
      let env = extractEnvelope sampleRate params samples
      let formants = extractFormants sampleRate params samples
      let spectra = extractSpectra sampleRate params samples
      let segments = detectSegments env

      pure
        AudioFeatures
          { afPitch = pitch,
            afEnvelope = env,
            afFormants = formants,
            afSegments = segments,
            afSpectrums = spectra
          }
  where
    emptyFeatures = AudioFeatures [] [] [] [] []

--------------------------------------------

-- | Pitch extraction (autocorrelation)

--------------------------------------------

-- | Extract pitch contour using autocorrelation method
extractPitch :: Int -> AnalysisParams -> V.Vector Double -> [Pitch]
extractPitch sampleRate AnalysisParams {..} samples =
  let frames = frameSignal apWindowSize apHopSize samples
      timeStep = fromIntegral apHopSize / fromIntegral sampleRate
   in zipWith
        (\i frame -> Pitch (fromIntegral i * timeStep) (detectPitch sampleRate apMinPitch apMaxPitch frame))
        countUp
        frames

-- | Detect pitch in a single frame using autocorrelation
detectPitch :: Int -> Double -> Double -> V.Vector Double -> Double
detectPitch sampleRate minPitch maxPitch frame
  | V.length frame < 2 = 0.0
  | otherwise =
      let maxLag = floor (fromIntegral sampleRate / minPitch)
          minLag = floor (fromIntegral sampleRate / maxPitch)
          autocorr = computeAutocorrelation frame maxLag
          -- Find the peak in the autocorrelation (excluding lag 0)
          peakLag = findPeakInRange minLag maxLag autocorr
       in if peakLag > 0
            then fromIntegral sampleRate / fromIntegral peakLag
            else 0.0 -- No pitch detected

-- | Compute autocorrelation using naive method
computeAutocorrelation :: V.Vector Double -> Int -> V.Vector Double
computeAutocorrelation signal maxLag =
  let n = V.length signal
      lags = [0 .. min maxLag (n - 1)]
      acf lag =
        V.sum $
          V.zipWith
            (*)
            (V.slice 0 (n - lag) signal)
            (V.slice lag (n - lag) signal)
   in V.fromList $ map acf lags

-- | Find the index of maximum value in a range
findPeakInRange :: Int -> Int -> V.Vector Double -> Int
findPeakInRange start end vec
  | start >= end || end > V.length vec = 0
  | otherwise =
      let range = V.slice start (end - start) vec
          maxVal = V.maximum range
          relativeIdx = V.findIndex (== maxVal) range
       in maybe start (+ start) relativeIdx

--------------------------------------------

-- | Envelope extraction

--------------------------------------------

-- | Extract amplitude envelope (RMS over frames)
extractEnvelope :: Int -> AnalysisParams -> V.Vector Double -> [AmplitudeSample]
extractEnvelope sampleRate AnalysisParams {..} samples =
  let frames = frameSignal apWindowSize apHopSize samples
      timeStep = fromIntegral apHopSize / fromIntegral sampleRate
      rms frame = sqrt $ V.sum (V.map (\x -> x * x) frame) / fromIntegral (V.length frame)
   in zipWith
        (\i frame -> AmplitudeSample (fromIntegral i * timeStep) (rms frame))
        countUp
        frames

--------------------------------------------

-- | Formant extraction (peak picking)

--------------------------------------------

-- | Extract formant frequencies using simple spectral peak picking
extractFormants :: Int -> AnalysisParams -> V.Vector Double -> [FormantFrame]
extractFormants sampleRate AnalysisParams {..} samples =
  let frames = frameSignal apWindowSize apHopSize samples
      timeStep = fromIntegral apHopSize / fromIntegral sampleRate
      getFormants = estimateFormantsFromSpectrum sampleRate apWindowSize
   in zipWith
        (\i frame -> FormantFrame (fromIntegral i * timeStep) (getFormants frame))
        countUp
        frames

-- | Estimate formants by finding peaks in the magnitude spectrum
estimateFormantsFromSpectrum :: Int -> Int -> V.Vector Double -> [Formant]
estimateFormantsFromSpectrum sampleRate windowSize frame =
  let spectrum = computeMagnitudeSpectrum frame
      -- Find peaks in spectrum (simplified)
      peaks = findSpectralPeaks spectrum
      -- Convert bin indices to frequencies
      binToFreq bin = (fromIntegral bin * fromIntegral sampleRate) / fromIntegral windowSize
      formantFreqs = map binToFreq $ take 4 peaks -- Take first 4 formants
      -- Estimate bandwidth (simplified - use fixed value)
      bandwidth = 100.0
   in map (`Formant` bandwidth) formantFreqs

-- | Find local maxima in spectrum
findSpectralPeaks :: V.Vector Double -> [Int]
findSpectralPeaks spectrum =
  let n = V.length spectrum
      isPeak i =
        i > 0
          && i < n - 1
          && spectrum V.! i > spectrum V.! (i - 1)
          && spectrum V.! i > spectrum V.! (i + 1)
      peaks = filter isPeak [1 .. n - 2]
      -- Sort by magnitude and return top peaks
      sortedPeaks = sortBy (comparing (\i -> negate (spectrum V.! i))) peaks
   in sortedPeaks

--------------------------------------------

-- | Spectral analysis

--------------------------------------------

-- | Extract full magnitude spectra for all frames
extractSpectra :: Int -> AnalysisParams -> V.Vector Double -> [SpectrumFrame]
extractSpectra sampleRate AnalysisParams {..} samples =
  let frames = frameSignal apWindowSize apHopSize samples
      timeStep = fromIntegral apHopSize / fromIntegral sampleRate
   in zipWith
        (\i frame -> SpectrumFrame (fromIntegral i * timeStep) (computeMagnitudeSpectrum frame))
        countUp
        frames

-- | Compute magnitude spectrum of a frame using FFT
computeMagnitudeSpectrum :: V.Vector Double -> V.Vector Double
computeMagnitudeSpectrum frame =
  let -- Apply Hamming window
      windowed = applyHammingWindow frame
      -- Convert to complex and pad to power of 2
      padded = padToPowerOf2 windowed
      -- Convert Vector to CArray for FFT
      complexVec = V.map (C.:+ 0) padded
      complexList = V.toList complexVec
      inputArray = CA.listArray (0, V.length padded - 1) complexList
      -- Compute FFT using FFTW3
      fftResult = FFT.dft inputArray
      -- Convert back to list and take magnitude of positive frequencies only
      fftList = CA.elems fftResult
      halfN = length fftList `div` 2
      magnitudes = map C.magnitude $ take halfN fftList
   in V.fromList magnitudes

-- | Apply Hamming window to reduce spectral leakage
applyHammingWindow :: V.Vector Double -> V.Vector Double
applyHammingWindow signal =
  let n = V.length signal
      window i = 0.54 - 0.46 * cos (2.0 * pi * fromIntegral i / fromIntegral (n - 1))
   in V.imap (\i x -> x * window i) signal

-- | Pad signal to next power of 2 for efficient FFT
padToPowerOf2 :: V.Vector Double -> V.Vector Double
padToPowerOf2 vec =
  let n = V.length vec
      nextPow2 = 2 ^ (ceiling (logBase 2 (fromIntegral n) :: Double) :: Int)
      padding = V.replicate (nextPow2 - n) 0.0
   in vec V.++ padding

--------------------------------------------

-- | Framing and segmentation utilities

--------------------------------------------

-- | Break signal into overlapping frames
frameSignal :: Int -> Int -> V.Vector Double -> [V.Vector Double]
frameSignal windowSize hopSize signal =
  let n = V.length signal
      numFrames = max 0 ((n - windowSize) `div` hopSize + 1)
      getFrame i =
        let startIdx = i * hopSize
         in if startIdx + windowSize <= n
              then V.slice startIdx windowSize signal
              else V.empty -- Skip incomplete frames
   in filter (not . V.null) $ map getFrame [0 .. numFrames - 1]

-- | Simple voice activity detection based on energy threshold
detectSegments :: [AmplitudeSample] -> [Segment]
detectSegments envelope =
  let threshold = 0.02 -- Energy threshold for voice activity
      labeled = map (\s -> (ampTime s, ampMagnitude s > threshold)) envelope
      -- Group consecutive voiced/unvoiced segments
      segments = groupSegments labeled
   in segments

grabEnds :: [a] -> Maybe (a, a)
grabEnds [] = Nothing
grabEnds (x : xs) = Just (x, go x xs)
  where
    go end [] = end
    go _ (y : ys) = go y ys

-- | Group consecutive samples into segments
groupSegments :: [(Double, Bool)] -> [Segment]
groupSegments samples =
  let grouped = groupBy (\(_, v1) (_, v2) -> v1 == v2) samples
      makeSegment :: [(Double, Bool)] -> Maybe Segment
      makeSegment grp = do
        ((startTime, isVoiced), (endTime, _)) <- grabEnds grp
        Just $
          Segment
            { segStart = startTime,
              segEnd = endTime,
              segLabel = if isVoiced then "voiced" else "unvoiced"
            }
   in mapMaybe makeSegment grouped

-- helper function to constrain types
countUp :: [Int]
countUp = [0 ..]
