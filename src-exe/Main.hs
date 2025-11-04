module Main where

import ProtoDoll.Parse
import ProtoDoll.Synth
import System.Environment (getArgs)
import LambdaSound
import System.Exit (exitSuccess)
import System.FilePath

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  [path] <- getArgs

  result <- ProtoDoll.Parse.parseFile path
  putStrLn $ "Parsed " ++ show (length result) ++ " feet."
  putStrLn $ "Phonemes per foot: " ++ show (length <$> result)
  putStrLn $ "Total phonemes: " ++ show (sum (length <$> result))

  putStrLn "Beginning synthesis..."
  let sound = feetToSound result
  let soundFile = dropExtension path <.> "wav"
  putStrLn $ "Saving " <> soundFile <> " ..."
  saveWav soundFile (Hz 44100) sound

  putStrLn "Playng sound."
  play 44100 1.0 sound

  exitSuccess
