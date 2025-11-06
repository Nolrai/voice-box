module Main where

import ProtoDoll.Parse
import ProtoDoll.Synth
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import LambdaSound
import System.FilePath (dropExtension, (<.>))
import GHC.IO.StdHandles (stderr)
import System.IO (hPrint)
import Utils.IO (writeFileUtf8)
import qualified Data.Text as Text

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

  -- Debug: print the parsed result to stderr
  writeFileUtf8 (path <.> "debug") (Text.show result)

  putStrLn "Beginning synthesis..."
  let sound = paragraphsToSound result
  let soundFile = dropExtension path <.> "wav"
  putStrLn $ "Saving " <> soundFile <> " ..."
  saveWav soundFile (Hz 44100) sound

  -- putStrLn "Playing sound."
  -- play 44100 1.0 sound

  exitSuccess