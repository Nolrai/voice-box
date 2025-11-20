module Main where

import Test.Tasty
import VoiceBox.Audio.EarTest qualified as EarTest
import VoiceBox.Language.ParseGoldenTest qualified as LanguageTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "VoiceBox Tests"
      [ EarTest.tests,
        LanguageTest.tests
      ]
