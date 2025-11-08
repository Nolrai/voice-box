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
      just_speedup =
        let speedupFactor = 2.0
        in applySimpleSpeedup speedupFactor original
  in [("00_original", samples), ("01_speedup", just_speedup)] ++ do
  let fundamental = fromIntegral (tpAutotuneSteps params)
      speedupFactor = 2.0
  let blist = [False, True]
  toggles <- VocoderToggles <$> blist <*> blist <*> blist <*> blist <*> blist
  let tag :: String = ('_':) $
        (\ b -> if b then 'Y' else 'N')
          <$> [ vtEnableSmooth toggles
              , vtEnableSoftBand toggles
              , vtEnableBlend toggles
              , vtEnableWarp toggles
              , vtEnableMix toggles
              ]
  let vocoded = applyHarmonicVocoder sampleRate fundamental toggles samples
      vocodedThenSpeedUp = applySimpleSpeedup speedupFactor vocoded
      speedupThenVocoded = applyHarmonicVocoder sampleRate fundamental toggles just_speedup
  [ ( tag ++ "_02_vocoder", vocoded )
    , ( tag ++ "_03_vocoder_speedup", vocodedThenSpeedUp )
    , ( tag ++ "_04_speedup_vocoder", speedupThenVocoded )
    ]

--------------------------------------------
-- | Effect Implementations
--------------------------------------------

-- | Detailed controls for spectral and mixing refinements
data VocoderToggles = VocoderToggles
  { vtEnableSmooth   :: Bool
  , vtEnableSoftBand :: Bool
  , vtEnableBlend    :: Bool
  , vtEnableWarp     :: Bool
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

-- | Default "PreDoll-0" style toggles
defaultVocoderToggles :: VocoderToggles
defaultVocoderToggles = VocoderToggles
  { vtEnableSmooth   = True
  , vtEnableSoftBand = True
  , vtEnableBlend    = True
  , vtEnableWarp     = True
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
applyHarmonicVocoder sampleRate fundamentalHz
    VocoderToggles
      { vtEnableSmooth = enableSmooth
      , vtEnableSoftBand = enableSoftBand
      , vtEnableBlend = enableBlend
      , vtEnableWarp = enableWarp
      , vtEnableMix = enableMix
    }
    samples =
  let
    VocoderNumericParams
      { vnpSmoothRadius = smoothRadius
      , vnpTolerance = tolerance
      , vnpBlendCutoff = blendCutoff
      , vnpBlendAmount = blendAmount
      , vnpWarpAlpha = warpAlpha
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
    -- Optional spectral warping
    warpIndex i alpha =
      let x = fromIntegral i / fromIntegral n
          warped = x ** alpha
      in floor (warped * fromIntegral n)

    spectrumWarped =
      if enableWarp
        then V.imap (\i _ -> spectrum0 V.! warpIndex i warpAlpha) spectrum0
        else spectrum0

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
    -- Spectral smoothing
    smoothSpectrum radius spectrum =
      let avg i =
            let start = max 0 (i - radius)
                end   = min (V.length spectrum - 1) (i + radius)
                window = V.slice start (end - start + 1) spectrum
                s = V.foldl' (+) 0 window
            in s / (fromIntegral (V.length window) :+ 0)
      in if enableSmooth
          then V.imap (\i _ -> avg i) spectrum
          else spectrum

    smoothed = smoothSpectrum smoothRadius filtered

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

    vocodedEnvelope = extractEnvelope sampleRate analysisParams vocoded
    envelopeApplied = applyEnvelopeRatio sampleRate originalEnvelope vocodedEnvelope vocoded

    mixDryWet = V.zipWith (\d w -> (1 - wetRatio) * d + wetRatio * w)

  in if enableMix
       then mixDryWet samples envelopeApplied
       else envelopeApplied


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
