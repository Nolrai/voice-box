{-# LANGUAGE NoImplicitPrelude #-}

module VoiceBox.Audio.Ear.Types where

import Data.Int (Int, Int8)
import Data.Map (Map)
import Data.Vector (Vector)
import LambdaSound (Hz)

  -- | Pulse counts are stored as Int8 to minimize memory usage and match the expected range of quantized phase differences. This is sufficient for the biological model and avoids unnecessary overhead.
data EarResult = EarResult
  { earSampleRate :: Hz,
    earPulseCounts :: Vector (Map Hz (Map Int Int8))
  }
