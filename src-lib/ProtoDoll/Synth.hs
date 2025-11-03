{-# LANGUAGE RecordWildCards #-}
module ProtoDoll.Synth (phonemeToRealization) where

import LambdaSound hiding (I)
import LambdaSound qualified as Sound
import ProtoDoll.SynthTypes
import ProtoDoll.ParseResult hiding (I)
import ProtoDoll.ParseResult as PR
import Control.Monad.State.Strict (State, get, put)
import Data.Word (Word16)
import Data.List.NonEmpty (NonEmpty(..))
import Data.IntSet

vowelToFormants :: VowelName -> IntSet
vowelToFormants A = fromListIntSet [220, 440, 660]
vowelToFormants E = fromListIntSet [330, 660, 990]
vowelToFormants I = fromListIntSet [440, 880, 1320]
vowelToFormants O = fromListIntSet [260, 520, 780]
vowelToFormants U = fromListIntSet [300, 600, 900]

phonemeToRealization :: Phoneme -> State VowelName Realization
phonemeToRealization (Chord v) = do
  set (last v)
  let (start, middle, end) = startMiddleEnd v
  pure Realization
    { dur = EnvTime {min = 32, weight = 2}
    , form = Tones
        { start = vowelToFormants <$> start
        , middle = union (vowelToFormants <$> middle)
        , end = vowelToFormants <$> end
        }
    }
  where
    startMiddleEnd :: NonEmpty VowelName -> (VowelName, [VowelName], VowelName)
    startMiddleEnd (x :| [])  = ([], [x], [])
    startMiddleEnd (x :| ys)  = (x, drop 1 (reverse ys), take 1 (reverse ys))

phonemeToRealization NeutralVowel = do
  v <- get
  pure Realization
    { dur = EnvTime {min = 16, weight = 1}
    , form = Tones
        { start = Nothing
        , middle = vowelToFormants v
        , end = Nothing
        }
    }

phonemeToRealization (Consonant c) = pure $ consonantToRealization c
phonemeToRealization (Liminal l) = pure $ liminalToRealization l
phonemeToRealization (Silence s) = pure $ silenceToRealization s

liminalToRealization :: Liminal -> Realization
liminalToRealization l =
  Realization
    { dur = 16
    , form = Noise
        { noiseEnvelope = liminalEnvelope l
        , noiseFilter = liminalFilter l
        , noiseSubharmonic = 0
        , noiseReverb = liminalReverb l
        }
    }

liminalEnvelope :: Liminal -> ADSR
liminalEnvelope T0 = ADSR
  { attackTime  = constTime 2
  , decayTime   = constTime 2
  , sustainTime = constTime 0
  , releaseTime = constTime 3
  , sustainLevel = 0.8
  }

liminalEnvelope H2W = consonantEnvelope S

liminalFilter :: Liminal -> Float
liminalFilter T0 = 1.5 -- slightly blued noise
liminalFilter H2W = 0.5 -- reddish noise

consonantToRealization :: Consonant -> Realization
consonantToRealization (Consonant c) =
  Realization
    { dur = mannerToTicks (manner c)
    , form = Noise
        { noiseEnvelope = consonantEnvelope (manner c) (voice c)
        , noiseFilter = consonantFilter (place c) (voice c)
        , noiseSubharmonic = consonantSubharmonic (manner c)
        , noiseReverb = consonantReverb (manner c)
        }
    }

mannerToTicks :: Manner -> Ticks
mannerToTicks P = 2 * 16
mannerToTicks S = 5 * 16
mannerToTicks C = 6 * 16

constTime :: Int -> EnvTime
constTime x = EnvTime {min = fromIntegral x, weight = 0}

relativeTime :: Int -> EnvTime
relativeTime x = EnvTime {min = 0, weight = fromIntegral x}

consonantEnvelope :: Manner -> ADSR
consonantEnvelope P = ADSR
  { attackTime  = constTime 3
  , decayTime   = constTime 9
  , sustainTime = constTime 0
  , releaseTime = constTime 9
  , sustainLevel = 0.0
  }

consonantEnvelope S = ADSR
  { attackTime  = EnvTime 3 1
  , decayTime   = EnvTime 9 2
  , sustainTime = EnvTime 0 5
  , releaseTime = EnvTime 9 1
  , sustainLevel = 0.7
  }

consonantEnvelope C = ADSR
  { attackTime  = EnvTime 3 0
  , decayTime   = EnvTime 9 2
  , sustainTime = EnvTime 0 3
  , releaseTime = EnvTime 9 1
  , sustainLevel = 0.5
  }

ticksToDuration :: Word16 -> Duration
ticksToDuration x = Duration $ (x :: Double) / 256.0 * 0.5 -- assuming 120 bpm

toSound :: Int -> Realization -> Sound Sound.T Double
toSound timeAvailable Realization {dur, form} =
  toSound' form

toSound' :: Realization' -> Sound Sound.I Double
toSound' Tones {..} =
  parallel
  [ cutAfter (1/3) triangle <$> start
  , parallel $ triangle <$> middle
  , cutBefore (2/3) triangle <$> end
  ]

toSound' Noise {..} =
  parallel
  [ triangle subharmonic
  , simpleReverb noiseReverb
    . applyEnvelopeAndDuration noiseEnvelope
    . tintNoise noiseFilter
    $ noise 1337
  ]

tintNoise :: Float -> Sound Sound.I Double -> Sound Sound.I Double
tintNoise alpha inputNoise = parallel $
  -- Approximate 1/f^alpha coloring by splitting the band into a few
  -- log-ish spaced bandpass bands, weighting each band by 1/f^(alpha/2),
  -- and summing with a gentle global lowpass.  This is cheap and
  -- gives a perceptually reasonable white->pink->brown transition.
  let centers :: [Double]
      centers = [200.0, 800.0, 2000.0, 5000.0] -- Hz, low -> high

      -- Q for the bandpass filters (tweak to taste)
      qForm = 1.3

      -- compute raw weights proportional to amplitude scaling:
      -- power ∝ 1/f^alpha => amplitude ∝ 1/f^(alpha/2)
      alphaD = realToFrac alpha :: Double
      rawWeights = (\f -> 1.0 / (f ** (alphaD / 2.0))) <$> centers
      weightSum  = sum rawWeights
      weights    = (/ weightSum) <$> rawWeights

      -- build each weighted band
      mkBand f w =
        let bp = applyIIRFilter (bandPassFilter (Hz f) qForm) inputNoise
        in amplify (realToFrac w) bp

      bands = zipWith mkBand centers weights

      -- global gentle lowpass to shape overall high-frequency content as alpha increases
      -- map alpha in [0..2] -> lp cutoff in [8000 .. 1000] Hz (tighter for browner noise)
      lpCutHz = 8000.0 * (1.0 - (alphaD / 2.0)) + 1000.0 * (alphaD / 2.0)
      base   = applyIIRFilter (lowPassFilter (Hz lpCutHz) 0.9) inputNoise
  in parallel (base : bands)

cutAfter :: Portion -> (Int -> Sound Sound.I Double) -> IntSet -> Sound Sound.I Double
cutAfter p = zipSoundWith (\ p x -> if p <= p then x else silence) progress

cutBefore :: Portion -> (Int -> Sound Sound.I Double) -> IntSet -> Sound Sound.I Double
cutBefore p = zipSoundWith (\ p x -> if p >= p then x else silence) progress
