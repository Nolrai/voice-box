#!/usr/bin/env runghc
-- Play vocoder output files in order of toggle count (fewest Y's to most Y's)
-- Usage: runghc play-vocoder-files.hs [directory]
--
-- NOTE: Smooth (spectral smoothing) is now always enabled, so tags are 4 characters

import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.Process (callCommand)
import Data.List (sort, isPrefixOf, isSuffixOf)
import Control.Monad (filterM, forM_)

-- | Extract the toggle tag from a filename like "predoll0__YNYNN_02_vocoder.wav"
-- Returns the tag string (e.g., "YNYNN") or Nothing if not found
extractTag :: String -> Maybe String
extractTag filename =
  case dropWhile (/= '_') filename of
    ('_':'_':rest) ->
      let tag = takeWhile (/= '_') rest
      in if all (`elem` "YN") tag && not (null tag)
         then Just tag
         else Nothing
    _ -> Nothing

-- | Count how many 'Y' characters are in a tag
countYs :: String -> Int
countYs = length . filter (== 'Y')

-- | Parse a filename into (toggleCount, tag, filepath)
parseFile :: FilePath -> String -> Maybe (Int, String, FilePath)
parseFile dir filename
  | "predoll0__" `isPrefixOf` filename &&
    "_03_vocoder_speedup.wav" `isSuffixOf` filename =
      case extractTag filename of
        Just tag -> Just (countYs tag, tag, dir ++ "/" ++ filename)
        Nothing -> Nothing
  | otherwise = Nothing

main :: IO ()
main = do
  args <- getArgs
  let dir = if null args then "." else head args

  -- List all files in directory
  files <- listDirectory dir

  -- Parse and filter vocoder files
  let parsed = [ p | f <- files, Just p <- [parseFile dir f] ]

  -- Sort by toggle count (fewest Y's first)
  let sorted = sort parsed

  if null sorted
    then putStrLn $ "No vocoder files found in " ++ dir
    else do
      putStrLn $ "Found " ++ show (length sorted) ++ " vocoder files"
      putStrLn "Playing in order (fewest toggles → most toggles):\n"

      -- Play each file in order
      forM_ sorted $ \(count, tag, path) -> do
        exists <- doesFileExist path
        if exists
          then do
            putStrLn $ "Playing: " ++ tag ++ " (" ++ show count ++ " toggles enabled)"
            callCommand $ "aplay " ++ show path  -- show adds quotes for shell safety
          else
            putStrLn $ "Skipping missing file: " ++ path
