module SynthTypes (toRealization) where

newtype Ticks = Ticks Int

oneBeat :: Ticks
oneBeat = Ticks 256

toRealization :: [Phoneme] -> [Realization]
toRealization = map (phonemeToRealization PreDoll0)

data Realization = Realization
  { dur :: Ticks -- in 256ths of a foot.
  , form :: Realization'
  }

data Realization'
  = Tones {formants :: IntSet}
  | Noise
    { noiseEnvelope :: Envelope
    , noiseFilter :: _
    , noiseSubharmonic :: Word16
    , noiseReverb :: _
    }

data LANGUAGE = PreDoll0

vowelTicks :: VowelLength -> Ticks
vowelTicks Long = 6 * 16
vowelTicks Full = 4 * 16
vowelTicks SemiReduced = 3 * 16
vowelTicks UltraReduced = 2 * 16

phonemeToRealization :: LANGUAGE -> Phoneme -> Realization
phonemeToRealization PreDoll0 (Vowel v) =
  Realization
    { dur = vowelTicks (vowelLength v)
    , form = Tones
        { formants = vowelFormants v
        }
    }

phonemeToRealization PreDoll0 (Consonant c) =
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

consonantEnvelope :: Manner -> MyEnvelope
consonantEnvelope P =
  Envelope
    { totalTime = 2 * 16
    , attackTime = 0.02
    , decayTime = 0.05
    , sustainLevel = if voice == Unvoiced then 0.0 else 0.2
    , releaseTime = 0.03
    }

consonantEnvelope S =

consonantEnvelope C = clickEnvelope

phonemeToRealization PreDoll0 (Liminal l) =
  Realization
    { dur = 16
    , form = Noise
        { noiseEnvelope = liminalEnvelope l
        , noiseFilter = liminalFilter l
        , noiseSubharmonic = 0
        , noiseReverb = liminalReverb l
        }
    }

ticksToDuration :: Word16 -> Duration
ticksToDuration x = Duration $ (x :: Double) / 256.0 * 0.5 -- assuming 120 bpm

toSound :: Realization -> Sound T Double
toSound Realization {dur, form} =
  scaleDuration (ticksToDuration dur) . toSound' form

toSound' :: Realization' -> Sound I Double
toSound' Tones {formants} = parallel $ triangle <$> toDescList formants

toSound' Noise {..} =
  parallel
  [ triangle subharmonic
  , simpleReverb noiseReverb
    . applyEnvelopeAndDuration noiseEnvelope
    . tintNoise noiseFilter
    $ whiteNoise
  ]

tintNoise :: Float -> Sound I Double -> Sound I Double
tintNoise alpha inputNoise =
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
      rawWeights = map (\f -> 1.0 / (f ** (alphaD / 2.0))) centers
      weightSum  = sum rawWeights
      weights    = map (/ weightSum) rawWeights

      -- build each weighted band
      mkBand f w =
        let bp = applyIIRFilter (bandPassFilter (Hz f) qForm) inputNoise
        in scale (realToFrac w) bp

      bands = zipWith mkBand centers weights

      -- global gentle lowpass to shape overall high-frequency content as alpha increases
      -- map alpha in [0..2] -> lp cutoff in [8000 .. 1000] Hz (tighter for browner noise)
      lpCutHz = 8000.0 * (1.0 - (alphaD / 2.0)) + 1000.0 * (alphaD / 2.0)
      base   = applyIIRFilter (lowPassFilter (Hz lpCutHz) 0.9) inputNoise
  in parallel (base : bands)

