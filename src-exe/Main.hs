module Main where

import ProtoDoll.Parse
import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  args <- getArgs
  case args of
    [path] -> processFile path
    _ -> do
      print =<< getArgs
      putStrLn "Usage: voice-box <input-file>"

processFile :: FilePath -> IO ()
processFile path = do
  result <- ProtoDoll.Parse.parseFile path
  putStrLn $ "Parsed " ++ show (length result) ++ " utterances."

  let feetCount = length <$> result
  putStrLn $ "Utterance lengths (in feet): " ++ show feetCount

  let feetLengths = (length <$>) <$> result
  putStrLn $ "Feet length (in phonemes): " ++ show feetLengths

  let utterancePhonemes = fmap (fmap length) result
  putStrLn $ "Utterance lengths (in phonemes): " ++ show utterancePhonemes

  let totalPhonemes = sum (fmap sum utterancePhonemes)
  putStrLn $ "Total phonemes: " ++ show totalPhonemes

  -- putStrLn "Beginning synthesis..."
  -- let sound = feetToSound result
  -- let soundFile = dropExtension path <.> "wav"
  -- putStrLn $ "Saving " <> soundFile <> " ..."
  -- saveWav soundFile (Hz 44100) sound

  -- putStrLn "Playng sound."
  -- play 44100 1.0 sound

  exitSuccess