-- Test program for VoiceBox audio analysis

module Main where

import Data.Vector.Storable qualified as V
import Data.WAVE qualified as WAVE
import System.Exit (exitSuccess)
import VoiceBox.Audio.Analyze qualified as VB
import VoiceBox.Types qualified as VB

main :: IO ()
main = do
  putStrLn "=== VoiceBox Audio Analysis Test ==="

  -- Generate a simple test tone (440 Hz sine wave)
  let sampleRate = 44100
  let duration = 0.5 -- 0.5 seconds
  let freq = 440.0 -- A4
  let samples = generateSineTone sampleRate duration freq

  -- Save as WAV file
  let wavePath = "test_tone.wav"
  putStrLn $ "Generating test tone at " ++ show freq ++ " Hz..."
  saveWave wavePath sampleRate samples

  -- Analyze the file
  putStrLn "\nAnalyzing audio..."
  features <- VB.analyzeAudio wavePath

  -- Print results
  putStrLn "\n=== Analysis Results ==="
  putStrLn $ "Pitch samples: " ++ show (length $ VB.afPitch features)
  putStrLn $ "Envelope samples: " ++ show (length $ VB.afEnvelope features)
  putStrLn $ "Formant frames: " ++ show (length $ VB.afFormants features)
  putStrLn $ "Segments: " ++ show (length $ VB.afSegments features)

  -- Show first few pitch values
  putStrLn "\nFirst 5 pitch detections:"
  mapM_
    (\p -> putStrLn $ "  Time: " ++ show (VB.pitchTime p) ++ "s, Freq: " ++ show (VB.pitchFreq p) ++ " Hz")
    (take 5 $ VB.afPitch features)

  -- Show first few envelope values
  putStrLn "\nFirst 5 envelope samples:"
  mapM_
    (\e -> putStrLn $ "  Time: " ++ show (VB.ampTime e) ++ "s, Amplitude: " ++ show (VB.ampMagnitude e))
    (take 5 $ VB.afEnvelope features)

  -- Show segments
  putStrLn "\nDetected segments:"
  mapM_
    (\s -> putStrLn $ "  " ++ VB.segLabel s ++ ": " ++ show (VB.segStart s) ++ "s - " ++ show (VB.segEnd s) ++ "s")
    (VB.afSegments features)

  putStrLn "\n✓ Analysis complete!"
  exitSuccess

-- | Generate a sine wave tone
generateSineTone :: Int -> Double -> Double -> V.Vector Double
generateSineTone sampleRate duration freq =
  let (numSamples :: Int) = floor (fromIntegral sampleRate * duration)
      sr = fromIntegral sampleRate
      genSample i = sin (2 * pi * freq * fromIntegral i / sr)
  in V.fromList $ map genSample [0 .. numSamples - 1]

-- | Save samples as a WAVE file
saveWave :: FilePath -> Int -> V.Vector Double -> IO ()
saveWave path sampleRate samples =
  let header =
        WAVE.WAVEHeader
          { WAVE.waveNumChannels = 1,
            WAVE.waveFrameRate = fromIntegral sampleRate,
            WAVE.waveBitsPerSample = 16,
            WAVE.waveFrames = Just (V.length samples)
          }
      -- Convert normalized Double [-1,1] to Int32 (WAVE sample type)
      toInt32 :: Double -> WAVE.WAVESample
      toInt32 x = round (x * 32767.0)
      samplesList = V.toList $ V.map toInt32 samples
      wave = WAVE.WAVE header [[s] | s <- samplesList]
   in WAVE.putWAVEFile path wave
