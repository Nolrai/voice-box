module Main where

import ProtoDoll.Parse

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  [path] <- getArgs
  result <- ProtoDoll.Parse.parseFile path
  MyLib.someFunc
