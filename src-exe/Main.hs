{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import LambdaSound
import Options.Applicative
import Paths_voice_box (version)
import System.Exit (exitFailure, exitSuccess)
import Control.Exception (catch, SomeException, displayException)
import System.FilePath (dropExtension, takeExtension, (<.>))

-- VoiceBox internal modules
import VoiceBox.Language.IPA qualified as IPA
import VoiceBox.Language.Util (writeFileUtf8, errorIO)
import VoiceBox.Audio.Analyze (analyzeAudio, readWaveFile, writeWaveFile)
import VoiceBox.Audio.Transform qualified as Transform
import VoiceBox.Types qualified as VB
import VoiceBox.Language.Synth qualified as Synth

-- | Command-line options
data Options = Options
  { optInputFile :: FilePath,
    optOutputFile :: Maybe FilePath,
    optSampleRate :: Int,
    optDebug :: Bool,
    optAnalyze :: Bool,
    optSynthesize :: Bool,
    optTransform :: Bool,
    optBatchToggles :: Bool
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument
      str
      ( metavar "INPUT"
          <> help "Input file (text for synthesis, or .wav for analysis)"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "Output WAV file (default: INPUT.wav)"
          )
      )
    <*> option
      auto
      ( long "sample-rate"
          <> short 'r'
          <> metavar "HZ"
          <> value 44100
          <> showDefault
          <> help "Sample rate for output audio"
      )
    <*> switch (long "debug" <> short 'd' <> help "Write debug output")
    <*> switch (long "analyze" <> short 'a' <> help "Analyze WAV file")
    <*> switch (long "synthesize" <> short 's' <> help "Synthesize audio from text")
    <*> switch (long "transform" <> short 't' <> help "Apply PreDoll-0 transformation to WAV input")
    <*> switch (long "batch-toggles" <> short 'b' <> help "Batch render multiple toggle combinations")

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper <**> versionOption)
    ( fullDesc
        <> progDesc "Synthesize or analyze speech audio"
        <> header "voice-box — a hybrid speech synthesizer and analyzer"
    )
  where
    versionOption =
      infoOption
        ("voice-box version " <> show version)
        (long "version" <> short 'v' <> help "Show version information")



main :: IO ()
main = (execParser opts >>= processFile)
  `catch` \(e :: SomeException) -> do
    putStrLn $ "\n[ERROR] " ++ displayException e
    exitFailure


processFile :: Options -> IO ()
processFile optsValues = do
  let path = optInputFile optsValues
      ext = takeExtension path

  -- Transform takes precedence and always exits after running.
  when (optTransform optsValues) $ do
    transformWavFile optsValues path
    exitSuccess

  -- Only one of analyze/synthesize may be set.
  case (optAnalyze optsValues, optSynthesize optsValues) of
    (True, True) -> errorIO "Cannot specify both --analyze and --synthesize"
    (True, False) -> analyzeWavFile optsValues path
    (False, True) -> synthesizeFromText optsValues path
    (False, False) ->
      if ext == ".wav"
        then analyzeWavFile optsValues path
        else synthesizeFromText optsValues path

-- | Analyze a WAV file and print extracted features

analyzeWavFile :: Options -> FilePath -> IO ()
analyzeWavFile optsValues path = do
  putStrLn $ "Analyzing WAV file: " ++ path
  features <- analyzeAudio path

  putStrLn "\n=== Analysis Results ==="
  putStrLn $ "Pitch samples: " ++ show (length $ VB.afPitch features)
  putStrLn $ "Envelope samples: " ++ show (length $ VB.afEnvelope features)
  putStrLn $ "Formant frames: " ++ show (length $ VB.afFormants features)
  putStrLn $ "Segments: " ++ show (length $ VB.afSegments features)

  when (optDebug optsValues) $ do
    putStrLn "\nFirst 10 pitch detections:"
    mapM_
      (\p -> putStrLn $ "  Time: " ++ show (VB.pitchTime p) ++ "s, Freq: " ++ show (VB.pitchFreq p) ++ " Hz")
      (take 10 $ VB.afPitch features)

    putStrLn "\nFirst 10 envelope samples:"
    mapM_
      (\e -> putStrLn $ "  Time: " ++ show (VB.ampTime e) ++ "s, Amp: " ++ show (VB.ampMagnitude e))
      (take 10 $ VB.afEnvelope features)

    putStrLn "\nDetected segments:"
    mapM_
      (\s -> putStrLn $ "  " ++ VB.segLabel s ++ ": " ++ show (VB.segStart s) ++ "s - " ++ show (VB.segEnd s) ++ "s")
      (VB.afSegments features)

-- | Apply the PreDoll-0 transformation to a WAV file

transformWavFile :: Options -> FilePath -> IO ()
transformWavFile optsValues path = do
  putStrLn $ "Applying PreDoll-0 transformation to: " ++ path
  let outputFile = fromMaybe (dropExtension path ++ "_predoll0.wav") (optOutputFile optsValues)

  if optBatchToggles optsValues
    then do
      audioResult <- readWaveFile path
      case audioResult of
        Left err -> errorIO ("Error reading WAV file: " <> Text.pack err)
        Right (samples, sr) -> do
          let base = dropExtension outputFile
              tags =
                [ ("Y", Transform.VocoderToggles True),
                  ("N", Transform.VocoderToggles False)
                ]
              fundamental = fromIntegral $ Transform.tpAutotuneSteps Transform.preDoll0Params

          -- Render and write output for each toggle combination.
          forM_ tags $ \(tag, toggles) -> do
            let vocoded = Transform.applyHarmonicVocoder sr fundamental toggles samples
                final = Transform.applySimpleSpeedup 2.0 vocoded
                outPath = base ++ "__" ++ tag ++ "_final.wav"
            writeWaveFile outPath sr final
            putStrLn $ "  -> Wrote " ++ outPath
          putStrLn "\nBatch render complete!"
          return ()
    else do
      maybeErr <- Transform.transformWavFile path outputFile
      case maybeErr of
        Just err -> errorIO ("Error: " <> Text.pack err)
        Nothing -> putStrLn ("Saved transformed audio to: " ++ outputFile)

-- | Synthesize audio from a text file

synthesizeFromText :: Options -> FilePath -> IO ()
synthesizeFromText optsValues path = do
  result <- IPA.parseFile path
  putStrLn $ "Parsed " ++ show (length result) ++ " utterances."

  -- Write debug output if requested.
  when (optDebug optsValues) $
    writeFileUtf8 (path <.> "debug") (Text.pack (show result))

  putStrLn "Beginning synthesis..."
  let sound = Synth.paragraphsToSound result
      soundFile = fromMaybe (dropExtension path <.> "wav") (optOutputFile optsValues)
      sampleRate = Hz (fromIntegral $ optSampleRate optsValues)

  putStrLn $ "Saving " <> soundFile <> " ..."
  saveWav soundFile sampleRate sound
  return ()
-- | Synthesize audio from a text file: parses, optionally writes debug output, and saves the WAV.
