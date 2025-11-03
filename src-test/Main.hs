module Main (main) where

import qualified ProtoDoll.Parse as PD
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Exit (exitFailure)
import Control.Monad (when)
import Data.Function ((&))

-- Golden test:
-- - reads test input at ../story_predoll-0.txt (project root)
-- - runs the parser from the library
-- - compares the `show` output against test/golden/parse_golden.txt
-- If the golden file does not exist it is created and the test fails so you can
-- inspect and accept the new golden.
main :: IO ()
main = do
  let inputPath  = "../story_predoll-0.txt"
      goldenPath = "test/golden/parse_golden.txt"

  parsed <- PD.parseFile inputPath
  let actual = show parsed

  exists <- doesFileExist goldenPath
  if not exists
    then do
      createDirectoryIfMissing True (takeDirectory goldenPath)
      writeFile goldenPath actual
      putStrLn $ "Golden file created at " ++ goldenPath ++ ". Please verify and re-run tests."
      exitFailure
    else do
      expected <- readFile goldenPath
      if expected == actual
        then putStrLn "Golden matched."
        else do
          putStrLn "Golden mismatch!"
          putStrLn "---- Expected ----"
          putStrLn expected
          putStrLn "---- Actual ----"
          putStrLn actual
          exitFailure
