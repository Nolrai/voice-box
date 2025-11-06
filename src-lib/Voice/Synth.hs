{-# LANGUAGE RecordWildCards #-}

module Voice.Synth (footToSound, chainSynthisis, paragraphsToSound) where

import Control.Monad.State.Strict (State, evalState, get, put)
import Data.IntSet
import Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Word (Word16)
import LambdaSound hiding (I, f1, f2)
import LambdaSound qualified as Sound
import Voice.IPA.Types hiding (I)
import Voice.IPA.Types as PR
import Voice.Types as ST

paragraphsToSound :: [[Foot]] -> Sound T Pulse
paragraphsToSound paragraphs =
  mconcat (paragraphToSound <$> paragraphs)

paragraphToSound :: [Foot] -> Sound T Pulse
paragraphToSound feet =
  evalState (chainSynthisis footToSound feet) A >>> utteranceBoundarySound

chainSynthisis :: (Monad m) => (a -> m (Sound T Pulse)) -> [a] -> m (Sound T Pulse)
chainSynthisis f = List.foldr (\p -> (<*>) ((>>>) <$> f p)) (pure (0 |-> silence))

footToSound :: Foot -> State VowelName (Sound T Pulse)
footToSound = chainSynthisis (fmap toSound . phonemeToRealization)

-- realistic-ish formant centres (Hz) for each vowel
vowelToFormants :: VowelName -> IntSet
vowelToFormants A = fromList [730, 1090, 2440]  -- as in "father"
vowelToFormants E = fromList [530, 1840, 2480]  -- as in "bed"
vowelToFormants I = fromList [270, 2290, 3010]  -- as in "see"
vowelToFormants O = fromList [570, 840, 2410]   -- as in "ought"
vowelToFormants U = fromList [300, 870, 2240]   -- as in "boot"

phonemeToRealization :: Phoneme -> State VowelName Realization
phonemeToRealization (Chord v) = do
  put (NE.last v)
  let (start, middle, end) = startMiddleEnd v
  pure
    Realization
      { dur = 64,
        form =
          Tones
            { start = unions (vowelToFormants <$> start),
              middle = unions (vowelToFormants <$> middle),
              end = unions (vowelToFormants <$> end)
            }
      }
  where
    startMiddleEnd :: NonEmpty VowelName -> ([VowelName], [VowelName], [VowelName])
    startMiddleEnd (x :| []) = ([], [x], [])
    startMiddleEnd (x :| ys) = ([x], drop 1 (reverse ys), take 1 (reverse ys))
phonemeToRealization NeutralVowel = do
  v <- get
  pure
    Realization
      { dur = 16,
        form =
          Tones
            { start = mempty,
              middle = vowelToFormants v,
              end = mempty
            }
      }
phonemeToRealization (Consonant c) = pure $ consonantToRealization c
phonemeToRealization (Liminal l) = pure $ liminalToRealization l
phonemeToRealization (PR.Silence s) = pure $ silenceToRealization s

liminalToRealization :: Liminal -> Realization
liminalToRealization l =
  Realization
    { dur = 16,
      form =
        Noise
          { noiseEnvelope = liminalEnvelope l,
            noiseFilter = liminalFilter l,
            noiseSubharmonic = 0,
            noiseReverb = 4
          }
    }

liminalEnvelope :: Liminal -> ADSR
liminalEnvelope T0 =
  ADSR
    { attackTime = 2,
      decayTime = 2,
      sustainTime = 0,
      releaseTime = 3,
      sustainLevel = 0.8
    }
liminalEnvelope H2W = consonantEnvelope S

liminalFilter :: Liminal -> Float
liminalFilter T0 = 1.5 -- slightly blued noise
liminalFilter H2W = 0.5 -- reddish noise

consonantToRealization :: Consonant -> Realization
consonantToRealization c =
  Realization
    { dur = mannerToTicks (manner c),
      form =
        Noise
          { noiseEnvelope = consonantEnvelope (manner c),
            noiseFilter = consonantFilter (place c) (voice c),
            noiseSubharmonic = consonantSubharmonic (voice c),
            noiseReverb = consonantReverb (manner c) (voice c)
          }
    }

mannerToTicks :: Manner -> Ticks
mannerToTicks P = 2 * 16
mannerToTicks S = 5 * 16
mannerToTicks C = 6 * 16

consonantEnvelope :: Manner -> ADSR
consonantEnvelope P =
  ADSR
    { attackTime = 3,
      decayTime = 9,
      sustainTime = 0,
      releaseTime = 9,
      sustainLevel = 0.0
    }
consonantEnvelope S =
  ADSR
    { attackTime = 3,
      decayTime = 9,
      sustainTime = 5,
      releaseTime = 9,
      sustainLevel = 0.7
    }
consonantEnvelope C =
  ADSR
    { attackTime = 3,
      decayTime = 9,
      sustainTime = 3,
      releaseTime = 9,
      sustainLevel = 0.5
    }

consonantFilter :: Place -> Voicing -> Float
consonantFilter Front White = 1.5
consonantFilter Front Brown = 1.0
consonantFilter Front Nasal = 0.8
consonantFilter Mid White = 1.2
consonantFilter Mid Brown = 0.9
consonantFilter Mid Nasal = 0.7
consonantFilter Back White = 1.0
consonantFilter Back Brown = 0.7
consonantFilter Back Nasal = 0.5

consonantSubharmonic :: Voicing -> Portion
consonantSubharmonic Nasal = 0.3
consonantSubharmonic _ = 0.0

consonantReverb :: Manner -> Voicing -> Ticks
consonantReverb m v =
  round $ fromIntegral (mannerReverb m) * voiceReverb v

mannerReverb :: Manner -> Ticks
mannerReverb P = 4
mannerReverb S = 12
mannerReverb C = 10

voiceReverb :: Voicing -> Double
voiceReverb White = 1
voiceReverb Brown = 1 + 1 / 4
voiceReverb Nasal = 1 + 5 / 8

ticksToDuration :: Word16 -> Duration
ticksToDuration x = Duration $ fromIntegral x / 256.0 * 0.5 -- assuming 120 bpm

-- | Convert an ADSR whose times are expressed in "ticks" into an envelope
-- whose total time will be exactly 'totalTicks'. The ADSR fields are scaled
-- proportionally so the relative shape is preserved but the sum of the parts
-- matches the provided total.
fitADSR :: Word16 -> ADSR -> ADSR
fitADSR totalTicks ADSR {..} =
  let sumTicks = attackTime + decayTime + sustainTime + releaseTime
      scale :: Double
      scale = if sumTicks == 0 then 0 else fromIntegral totalTicks / fromIntegral sumTicks
      scaleField :: Word16 -> Word16
      scaleField t = round (fromIntegral t * scale)
  in ADSR
        { attackTime = scaleField attackTime,
          decayTime = scaleField decayTime,
          sustainTime = scaleField sustainTime,
          releaseTime = scaleField releaseTime,
          sustainLevel = sustainLevel
        }

overlay :: Sound Sound.I Pulse -> Sound Sound.T Pulse -> Sound Sound.T Pulse
overlay continuous timed =
  parallel2 (adoptDuration timed continuous) timed

intToHz :: Int -> Hz
intToHz n = Hz (fromIntegral n)

-- build a band-limited pulsetrain at f0 by summing harmonics with 1/n rolloff
bandLimitedPulse :: Hz -> Sound Sound.I Pulse
bandLimitedPulse = harmonic sineWave

-- Pass the phoneme duration (ticks) down into the raw generator so envelopes
-- can be fitted to the phoneme duration reliably.
toSound :: Realization -> Sound Sound.T Pulse
toSound r0 =
  let r = ensureADSRFits r0
  in case form r of
        Tones {..} ->
          -- voiced excitation passed through formant bandpass filters
          let f0 :: Hz
              -- default fundamental; later you can make this part of Realization
              f0 = Hz 110

              -- narrow-ish Q for formant bandpasses
              qForm = 6.0

              -- build a bandpassed version of the voiced source for a given formant set
              formantBand :: IntSet -> Sound Sound.I Pulse
              formantBand tones =
                let excitation = bandLimitedPulse f0 -- basic periodic source
                    mkBand fc =
                      let bp = applyIIRFilter (bandPassFilter (intToHz fc) qForm) excitation
                      in amplify 1 bp
                in parallel (mkBand <$> toList tones)

              -- progress window helpers: produce three windows that sum to 1
              time1, time2 :: Float
              time1 = 1 / 3
              time2 = 2 / 3

              startWindow :: Float -> Float
              startWindow p =
                if p <= time1 then max 0 (1 - 3 * p) else 0

              coreWindow :: Float -> Float
              coreWindow p
                | p <= time1 = 3 * p
                | p <= time2 = 1
                | otherwise = max 0 (3 * (1 - p))

              endWindow :: Float -> Float
              endWindow p = if p >= time2 then max 0 (3 * p - 2) else 0

              -- apply an arbitrary float window (function of normalized progress in [0..1]) to a sound
              applyWindow :: (Float -> Float) -> Sound Sound.I Pulse -> Sound Sound.I Pulse
              applyWindow w =
                zipSoundWith (\p x -> x * realToFrac (w (realToFrac p))) progress

              -- bandpassed + windowed segments
              startBand = applyWindow startWindow (formantBand start)
              coreBand  = applyWindow coreWindow  (formantBand middle) -- core runs whole duration but windowed
              endBand   = applyWindow endWindow   (formantBand end)
          in ticksToDuration (dur r) |-> parallel [startBand, coreBand, endBand]
        Noise {..} ->
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
      env =
        Envelope
          { attack = ticksToDuration attackTime,
            decay = ticksToDuration decayTime,
            sustain = realToFrac sustainLevel,
            release = ticksToDuration releaseTime
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
      weightSum = sum rawWeights
      weights = (/ weightSum) <$> rawWeights

      -- build each weighted band
      mkBand f w =
        let bp = applyIIRFilter (bandPassFilter (Hz f) qForm) inputNoise
        in amplify (realToFrac w) bp

      bands = zipWith mkBand centers weights

      -- global gentle lowpass to shape overall high-frequency content as alpha increases
      -- map alpha in [0..2] -> lp cutoff in [8000 .. 1000] Hz (tighter for browner noise)
      (lpCutHz :: Float) = realToFrac $ 8000.0 * (1.0 - (alphaD / 2.0)) + 1000.0 * (alphaD / 2.0)
      base = applyIIRFilter (lowPassFilter (Hz lpCutHz) 0.9) inputNoise
  in parallel (base : bands)

utteranceBoundarySound :: Sound T Pulse
utteranceBoundarySound = toSound (silenceToRealization UtteranceBoundary)

silenceToRealization :: Silence -> Realization
silenceToRealization x =
  Realization
    { dur = case x of
        UtteranceBoundary -> 32
        PhraseBoundary -> 16
        Gap -> 8,
      form = ST.Silence
    }

-- | Ensure that any ADSR in the Realization's Noise form is scaled to match
-- the Realization's dur. Returns the Realization unchanged for non-Noise forms.
ensureADSRFits :: Realization -> Realization
ensureADSRFits r@Realization {dur = totalTicks, form = Noise {..}} =
  r
    { form =
        Noise
          { noiseEnvelope = fitADSR totalTicks noiseEnvelope,
            noiseFilter = noiseFilter,
            noiseSubharmonic = noiseSubharmonic,
            noiseReverb = noiseReverb
          }
    }
ensureADSRFits r = r
