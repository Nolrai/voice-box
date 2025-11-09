#!/usr/bin/env runghc
-- Batch render specific toggle combinations for focused testing
-- Usage: runghc batch-render-tags.hs input.wav output_prefix
--
-- NOTE: Smooth (spectral smoothing) is now always enabled, so tags are 4 characters

import System.Environment (getArgs)
import System.Exit (exitFailure)
import VoiceBox.Transform qualified as Transform
import VoiceBox.Analyze (readWaveFile, writeWaveFile)
import Control.Monad (forM_)
import qualified Data.Vector.Storable as VS

-- | Parse a tag string into VocoderToggles (4 chars: SoftBand, Blend, Warp, Mix)
parseTag :: String -> Transform.VocoderToggles
parseTag tag
  | length tag /= 4 = error $ "Tag must be 4 characters: " ++ tag
  | not (all (`elem` "YN") tag) = error $ "Tag must only contain Y/N: " ++ tag
  | otherwise =
      let [sb, bl, w, m] = map (== 'Y') tag
      in Transform.VocoderToggles
           { Transform.vtEnableSoftBand = sb
           , Transform.vtEnableBlend = bl
           , Transform.vtEnableWarp = w
           , Transform.vtEnableMix = m
           }

-- | Promising tags from the rating session (converted from old 5-char to new 4-char)
-- Old tags with Smooth=Y: YYNYN, YYNNY, YNNYY, NYNYY, YYNNN, YNNNY, YNNNN, NNNNY
promisingTags :: [String]
promisingTags =
  [ "YNYN"  -- was YYNYN: SoftBand, Warp
  , "YNNY"  -- was YYNNY: SoftBand, Mix
  , "NNYY"  -- was YNNYY: Warp, Mix
  , "YNYY"  -- was NYNYY: SoftBand, Warp, Mix
  , "YNNN"  -- was YYNNN: SoftBand only
  , "NNNY"  -- was YNNNY: Mix only
  , "NNNN"  -- was YNNNN: No toggles (only Smooth, which is now always on)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath, outputPrefix] -> do
      putStrLn $ "Reading input: " ++ inputPath
      audioResult <- readWaveFile inputPath
      case audioResult of
        Left err -> do
          putStrLn $ "Error reading WAV: " ++ err
          exitFailure
        Right (samples, sampleRate) -> do
          putStrLn $ "Sample rate: " ++ show sampleRate
          putStrLn $ "Samples: " ++ show (VS.length samples)
          putStrLn $ "\nRendering " ++ show (length promisingTags) ++ " promising combinations...\n"

          forM_ promisingTags $ \tag -> do
            let toggles = parseTag tag
                fundamental = fromIntegral $ Transform.tpAutotuneSteps Transform.preDoll0Params
                vocoded = Transform.applyHarmonicVocoder sampleRate fundamental toggles samples
                speedupFactor = 2.0
                final = Transform.applySimpleSpeedup speedupFactor vocoded
                outputPath = outputPrefix ++ "_" ++ tag ++ "_focused.wav"

            putStrLn $ "  Rendering " ++ tag ++ " → " ++ outputPath
            writeWaveFile outputPath sampleRate final

          putStrLn "\nDone! Run rate-vocoder-files.hs to evaluate these renders."

    _ -> do
      putStrLn "Usage: runghc batch-render-tags.hs INPUT.wav OUTPUT_PREFIX"
      putStrLn "Example: runghc batch-render-tags.hs story.wav predoll0_focused"
      exitFailure
