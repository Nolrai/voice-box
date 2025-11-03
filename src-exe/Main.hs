module Main where

import ProtoDoll.Parse
import System.Environment (getArgs)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  [path] <- getArgs
  result <- ProtoDoll.Parse.parseFile path
  putStrLn $ "Parsed " ++ show (length result) ++ " feet."
  print $ length <$> result
