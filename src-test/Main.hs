module Main where

import Test.Tasty
import qualified VoiceBox.Audio.EarTest as EarTest
import qualified VoiceBox.Language.ParseGoldenTest as LanguageTest

main :: IO ()
main = defaultMain $
  testGroup "VoiceBox Tests"
    [ EarTest.tests
    , LanguageTest.tests
    ]