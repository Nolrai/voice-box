{-# LANGUAGE RecordWildCards #-}
module ProtoDoll.Synth (footToSound, chainSynthisis, feetToSound) where

import LambdaSound hiding (I)
import LambdaSound qualified as Sound
import ProtoDoll.SynthTypes as ST
import ProtoDoll.ParseResult hiding (I)
import ProtoDoll.ParseResult as PR
import Control.Monad.State.Strict (State, get, put, evalState)
import Data.Word (Word16)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.IntSet
import Data.List as List


feetToSound :: [Foot] -> Sound T Pulse
feetToSound feet = evalState (chainSynthisis footToSound feet) A

chainSynthisis :: Monad m => (a -> m (Sound T Pulse)) -> [a] -> m (Sound T Pulse)
chainSynthisis f = List.foldr (\ p -> (<*>) ((>>>) <$> f p)) (pure (0 |-> silence))

footToSound :: Foot -> State VowelName (Sound T Pulse)
footToSound = chainSynthisis (fmap toSound . phonemeToRealization)

vowelToFormants :: VowelName -> IntSet
vowelToFormants A = fromList [220, 440, 660]
vowelToFormants E = fromList [330, 660, 990]
vowelToFormants I = fromList [440, 880, 1320]
vowelToFormants O = fromList [260, 520, 780]
vowelToFormants U = fromList [300, 600, 900]

phonemeToRealization :: Phoneme -> State VowelName Realization
phonemeToRealization (Chord v) = do
  put (NE.last v)
  let (start, middle, end) = startMiddleEnd v
  pure Realization
    { dur = 64
    , form = Tones
        { start = unions (vowelToFormants <$> start)
        , middle = unions (vowelToFormants <$> middle)
        , end = unions (vowelToFormants <$> end)
        }
    }
  where
    startMiddleEnd :: NonEmpty VowelName -> ([VowelName], [VowelName], [VowelName])
    startMiddleEnd (x :| [])  = ([], [x], [])
    startMiddleEnd (x :| ys)  = ([x], drop 1 (reverse ys), take 1 (reverse ys))

phonemeToRealization NeutralVowel = do
  v <- get
  pure Realization
    { dur = 16
    , form = Tones
        { start = mempty
        , middle = vowelToFormants v
        , end = mempty
        }
    }

phonemeToRealization (Consonant c) = pure $ consonantToRealization c
phonemeToRealization (Liminal l) = pure $ liminalToRealization l
phonemeToRealization (PR.Silence s) = pure $ silenceToRealization s

liminalToRealization :: Liminal -> Realization
liminalToRealization l =
  Realization
    { dur = 16
    , form = Noise
        { noiseEnvelope = liminalEnvelope l
        , noiseFilter = liminalFilter l
        , noiseSubharmonic = 0
        , noiseReverb = 4
        }
    }

liminalEnvelope :: Liminal -> ADSR
liminalEnvelope T0 = ADSR
  { attackTime  = 2
  , decayTime   = 2
  , sustainTime = 0
  , releaseTime = 3
  , sustainLevel = 0.8
  }

liminalEnvelope H2W = consonantEnvelope S

liminalFilter :: Liminal -> Float
liminalFilter T0 = 1.5 -- slightly blued noise
liminalFilter H2W = 0.5 -- reddish noise

consonantToRealization :: Consonant -> Realization
consonantToRealization c =
  Realization
    { dur = mannerToTicks (manner c)
    , form = Noise
        { noiseEnvelope = consonantEnvelope (manner c)
        , noiseFilter = consonantFilter (place c) (voice c)
        , noiseSubharmonic = consonantSubharmonic (voice c)
        , noiseReverb = consonantReverb (manner c) (voice c)
        }
    }

mannerToTicks :: Manner -> Ticks
mannerToTicks P = 2 * 16
mannerToTicks S = 5 * 16
mannerToTicks C = 6 * 16

consonantEnvelope :: Manner -> ADSR
consonantEnvelope P = ADSR
  { attackTime  = 3
  , decayTime   = 9
  , sustainTime = 0
  , releaseTime = 9
  , sustainLevel = 0.0
  }

consonantEnvelope S = ADSR
  { attackTime  = 3
  , decayTime   = 9
  , sustainTime = 5
  , releaseTime = 9
  , sustainLevel = 0.7
  }

consonantEnvelope C = ADSR
  { attackTime  = 3
  , decayTime   = 9
  , sustainTime = 3
  , releaseTime = 9
  , sustainLevel = 0.5
  }

consonantFilter :: Place -> Voicing -> Float
consonantFilter Front White = 1.5
consonantFilter Front Brown = 1.0
consonantFilter Front Nasal = 0.8
consonantFilter Mid   White = 1.2
consonantFilter Mid   Brown = 0.9
consonantFilter Mid   Nasal = 0.7
consonantFilter Back  White = 1.0
consonantFilter Back  Brown = 0.7
consonantFilter Back  Nasal = 0.5

consonantSubharmonic :: Voicing -> Portion
consonantSubharmonic Nasal = 0.3
consonantSubharmonic _     = 0.0

consonantReverb :: Manner -> Voicing -> Ticks
consonantReverb m v =
  round $ fromIntegral (mannerReverb m) * voiceReverb v

mannerReverb :: Manner -> Ticks
mannerReverb P = 4
mannerReverb S = 12
mannerReverb C = 10

voiceReverb :: Voicing -> Double
voiceReverb White = 1
voiceReverb Brown = 1 + 1/4
voiceReverb Nasal = 1 + 5/8

ticksToDuration :: Word16 -> Duration
ticksToDuration x = Duration $ fromIntegral x / 256.0 * 0.5 -- assuming 120 bpm

-- | Convert an ADSR whose times are expressed in "ticks" into an envelope
-- whose total time will be exactly 'totalTicks'. The ADSR fields are scaled
-- proportionally so the relative shape is preserved but the sum of the parts
-- matches the provided total.
fitADSR :: Word16 -> ADSR -> ADSR
fitADSR totalTicks ADSR{..} =
  let sumTicks = attackTime + decayTime + sustainTime + releaseTime
      scale :: Double
      scale = if sumTicks == 0 then 0 else fromIntegral totalTicks / fromIntegral sumTicks
      scaleField :: Word16 -> Word16
      scaleField t = round (fromIntegral t * scale)
  in ADSR
      { attackTime  = scaleField attackTime
      , decayTime   = scaleField decayTime
      , sustainTime = scaleField sustainTime
      , releaseTime = scaleField releaseTime
      , sustainLevel = sustainLevel
      }

overlay :: Sound Sound.I Pulse -> Sound Sound.T Pulse -> Sound Sound.T Pulse
overlay continuous timed =
  parallel2 (adoptDuration timed continuous) timed

intToHz :: Int -> Hz
intToHz = Hz . fromIntegral

mkTone :: Int -> Sound Sound.I Pulse
mkTone f = triangleWave (intToHz f)

mkChord :: IntSet -> Sound Sound.I Pulse
mkChord tones = parallel $ mkTone <$> toList tones

-- Pass the phoneme duration (ticks) down into the raw generator so envelopes
-- can be fitted to the phoneme duration reliably.
toSound :: Realization -> Sound Sound.T Pulse
toSound r0 =
  let r = ensureADSRFits r0
  in case form r of
    Tones{..} ->
      ticksToDuration (dur r) |->
      parallel
      [ cutAfter (1/3) $ mkChord start
      , mkChord middle
      , cutBefore (2/3) $ mkChord end
      ]

    Noise{..} ->
      let subtone = amplify noiseSubharmonic $ triangleWave 250
          baseNoise = noise seed
          coloredNoise = tintNoise noiseFilter baseNoise
          shapedNoise = applyASDR (dur r) noiseEnvelope coloredNoise
          mainPart = simpleReverb (ticksToDuration noiseReverb) shapedNoise
      in subtone `overlay` mainPart
    ST.Silence -> ticksToDuration (dur r) |-> silence

seed :: Int
seed = 12345 -- fixed seed for reproducibility

-- | Apply an ADSR to a sound so that the ADSR exactly spans 'totalTicks'.
-- The ADSR passed in should have times in ticks; we assume caller may have
-- provided an ADSR shape whose absolute sum differs from totalTicks, so we
-- scale (or fit) beforehand. Convenience: callers should use 'fitADSR' to
-- pre-scale as needed; here we accept already-fitted ADSR.
applyASDR :: Word16 -> ADSR -> Sound Sound.I Pulse -> Sound Sound.T Pulse
applyASDR totalTicks ADSR {..} inputSound =
  let -- ensure the envelope spans the phoneme duration we were given
      timedInput = ticksToDuration totalTicks |-> inputSound
      env = Envelope
        { attack  = ticksToDuration attackTime
        , decay   = ticksToDuration decayTime
        , sustain = realToFrac sustainLevel
        , release = ticksToDuration releaseTime
        }
  in applyEnvelope env timedInput

tintNoise :: Float -> Sound Sound.I Pulse -> Sound Sound.I Pulse
tintNoise alpha inputNoise =
  -- Approximate 1/f^alpha coloring by splitting the band into a few
  -- log-ish spaced bandpass bands, weighting each band by 1/f^(alpha/2),
  -- and summing with a gentle global lowpass.  This is cheap and
  -- gives a perceptually reasonable white->pink->brown transition.
  let centers :: [Float]
      centers = [200.0, 800.0, 2000.0, 5000.0] -- Hz, low -> high

      -- Q for the bandpass filters (tweak to taste)
      qForm = 1.3

      -- compute raw weights proportional to amplitude scaling:
      -- power ∝ 1/f^alpha => amplitude ∝ 1/f^(alpha/2)
      alphaD = realToFrac alpha :: Float
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
      (lpCutHz :: Float) = realToFrac $ 8000.0 * (1.0 - (alphaD / 2.0)) + 1000.0 * (alphaD / 2.0)
      base   = applyIIRFilter (lowPassFilter (Hz lpCutHz) 0.9) inputNoise
  in parallel (base : bands)

cutAfter :: Progress -> Sound Sound.I Pulse -> Sound Sound.I Pulse
cutAfter threshold = zipSoundWith (\ p x -> if p <= threshold then x else 0) progress

cutBefore :: Progress -> Sound Sound.I Pulse -> Sound Sound.I Pulse
cutBefore threshold = zipSoundWith (\ p x -> if p >= threshold then x else 0) progress

silenceToRealization :: Silence -> Realization
silenceToRealization x =
  Realization {
    dur = case x of
      UtteranceBoundary -> 32
      PhraseBoundary    -> 16
      Gap               -> 8,
    form = ST.Silence
  }

-- | Ensure that any ADSR in the Realization's Noise form is scaled to match
-- the Realization's dur. Returns the Realization unchanged for non-Noise forms.
ensureADSRFits :: Realization -> Realization
ensureADSRFits r@Realization{ dur = totalTicks, form = Noise{..} } =
  r { form = Noise
            { noiseEnvelope    = fitADSR totalTicks noiseEnvelope
            , noiseFilter      = noiseFilter
            , noiseSubharmonic = noiseSubharmonic
            , noiseReverb      = noiseReverb
            }
    }
ensureADSRFits r = r

