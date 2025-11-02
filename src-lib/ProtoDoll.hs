module ProtoDoll where
import LambdaSound
import LambdaSound.Filter (applyIIRFilter, lowPassFilter, highPassFilter, bandPassFilter)

data Voicing = White | Brown | Nasal
  deriving (Enum, Show, Eq, Ord, Read)

data Manner = P | S | C
  deriving (Enum, Show, Eq, Ord, Read)

data Place = Front | Mid | Back

data Liminal = T0 | H2W

data Consonant = Consonant
  { manner :: Manner
  , voice  :: Voice
  , place  :: Place
  , dur    :: Double
  }

data Phoneme
  = Vowel Vowel
  | Consonant Consonant
  | Liminal Liminal

data Realization = Realization {dur :: Word16, form :: Realization'}

data Realization'
  = Tones {formants :: IntSet}
  | Noise
    { noiseEnvelope :: Envelope
    , noiseFilter :: _
    , noiseSubharmonic :: Word16
    , noiseReverb :: _
    }

toSeconds :: Word16 -> Duration
toSeconds x = Duration $ (x :: Double) / ((maxBound :: Word16) :: Double)

toSound :: Realization -> Sound T Double
toSound Realization {dur, form} =
  setDuration (toSeconds dur) . toSound' form

toSound' :: Realization' -> Sound I Double
toSound' Tones {formants} = parallel $ triangle <$> toDescList formants

toSound' Noise {..} =
  parallel
  [ triangle subharmonic
  , simpleReverb noiseReverb . applyEnvelope noiseEnvelope $ tintNoise noiseFilter whiteNoise
  ]

-- Choose implementation by changing this alias (toggle per-branch).
-- Filtered IIR-based tinting: cheap, reliable, intelligible consonants.
tintNoiseFiltered :: _ -> Sound I Double -> Sound I Double
tintNoiseFiltered inputNoise =
  let hpCut    = Hz 60
      lpCut    = Hz 8000
      formant1 = Hz 800
      formant2 = Hz 2000
      qBase    = 0.9
      qForm    = 1.3
      bandlimited = applyIIRFilter (highPassFilter hpCut qBase) .
                    applyIIRFilter (lowPassFilter  lpCut qBase) $
                    inputNoise
  in applyIIRFilter (bandPassFilter formant1 qForm) .