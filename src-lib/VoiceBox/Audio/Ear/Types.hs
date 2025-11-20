{-# LANGUAGE NoImplicitPrelude #-}

module VoiceBox.Audio.Ear.Types where

import Data.Int (Int, Int8)
import Data.Map (Map)
import Data.Vector (Vector)
import LambdaSound (Hz)

data EarResult = EarResult
  { earSampleRate :: Hz,
    earPulseCounts :: Vector (Map Hz (Map Int Int8))
  }
