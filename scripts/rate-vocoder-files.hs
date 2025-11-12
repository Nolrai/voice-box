#!/usr/bin/env runghc
-- Interactive vocoder file rating system
-- Usage: runghc rate-vocoder-files.hs [directory] [tag1] [tag2] ...
--
-- NOTE: Smooth (spectral smoothing) is now always enabled, so tags are 4 characters
--
-- If no tags specified, uses wildcard ???? (all files)
-- If tags specified, only files matching those exact tags are rated
-- Tags can use wildcards: Y/N/? where:
--   Y = must be enabled
--   N = must be disabled
--   ? = either (wildcard)
--
-- Examples:
--   ./rate-vocoder-files.hs .                    # all files
--   ./rate-vocoder-files.hs . YNYN YNNY NNYY     # only these 3 tags
--   ./rate-vocoder-files.hs . ?N??               # pattern: Blend=N
--   ./rate-vocoder-files.hs . YNYN ?N??          # mix exact tags and patterns

import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.Process (callCommand)
import System.IO (hFlush, stdout)
import Data.List (sort, isPrefixOf, isSuffixOf, intercalate)
import Control.Monad (unless, forM_)

-- | Toggle names in order (Smooth is now always enabled)
toggleNames :: [String]
toggleNames =
  [ "SoftBand (Gaussian bandpass)"
  , "Blend (low-freq mix)"
  , "Warp (spectral compression)"
  , "Mix (dry/wet blend)"
  ]

-- | Extract the toggle tag from a filename
extractTag :: String -> Maybe String
extractTag filename =
  case dropWhile (/= '_') filename of
    ('_':'_':rest) ->
      let tag = takeWhile (/= '_') rest
      in if all (`elem` "YN") tag && length tag == 4  -- Changed from 5 to 4
         then Just tag
         else Nothing
    _ -> Nothing

-- | Count 'Y' toggles
countYs :: String -> Int
countYs = length . filter (== 'Y')

-- | Check if a tag matches a filter pattern (Y/N/? wildcards)
matchesPattern :: String -> String -> Bool
matchesPattern filterPattern tag
  | length filterPattern /= length tag = False
  | otherwise = all matchChar (zip filterPattern tag)
  where
    matchChar ('?', _) = True
    matchChar (p, t) = p == t

-- | Parse a filename into (toggleCount, tag, filepath)
parseFile :: FilePath -> String -> Maybe (Int, String, FilePath)
parseFile dir filename
  | "predoll0__" `isPrefixOf` filename &&
    "_03_vocoder_speedup.wav" `isSuffixOf` filename =
      case extractTag filename of
        Just tag -> Just (countYs tag, tag, dir ++ "/" ++ filename)
        Nothing -> Nothing
  | otherwise = Nothing

-- | Describe which toggles are enabled
describeToggles :: String -> String
describeToggles tag =
  let enabled = [ name | (name, c) <- zip toggleNames tag, c == 'Y' ]
  in if null enabled
     then "No toggles enabled (pure robotic)"
     else intercalate ", " enabled

-- | Get user rating
getRating :: IO (Maybe String)
getRating = do
  putStr "Rate (h=too human, r=too robotic, p=perfect, u=unintelligible, s=skip, q=quit, x=replay): "
  hFlush stdout
  input <- getLine
  return $ case input of
    "h" -> Just "too-human"
    "r" -> Just "too-robotic"
    "p" -> Just "perfect"
    "u" -> Just "unintelligible"
    "s" -> Just "skip"
    "q" -> Nothing
    "x" -> Just "replay"
    _   -> Just "skip"

main :: IO ()
main = do
  args <- getArgs
  let (dir, patterns) = case args of
        [] -> (".", ["????"])  -- default: all files (4 chars now)
        [d] -> (d, ["????"])   -- directory only, all files
        (d:ps) -> (d, ps)      -- directory + list of patterns/tags

  -- List and parse files
  files <- listDirectory dir
  let parsed = [ p | f <- files, Just p <- [parseFile dir f] ]
      -- Filter: match if tag matches ANY of the patterns
      filtered = [ (c, t, p) | (c, t, p) <- parsed, any (`matchesPattern` t) patterns ]
      sorted = sort filtered

  if null sorted
    then putStrLn $ "No vocoder files found matching patterns: " ++ show patterns ++ " in " ++ dir
    else do
      putStrLn "\n=== Vocoder Rating Session ==="
      putStrLn $ "Filter patterns: " ++ show patterns
      putStrLn $ "Found " ++ show (length sorted) ++ " matching vocoder files\n"
      putStrLn "Controls:"
      putStrLn "  p = perfect (hits the sweet spot!)"
      putStrLn "  h = too human (too smooth/natural)"
      putStrLn "  r = too robotic (too synthetic)"
      putStrLn "  u = unintelligible (broken/garbled)"
      putStrLn "  s = skip (not sure)"
      putStrLn "  x = replay current file"
      putStrLn "  q = quit and show results\n"

      results <- rateFiles sorted []

      putStrLn "\n=== Results ==="

      let perfectOnes = [ (tag, count) | (tag, count, "perfect") <- results ]
          tooHuman = [ (tag, count) | (tag, count, "too-human") <- results ]
          tooRobotic = [ (tag, count) | (tag, count, "too-robotic") <- results ]
          unintelligible = [ (tag, count) | (tag, count, "unintelligible") <- results ]

      unless (null perfectOnes) $ do
        putStrLn "\n🎯 PERFECT (Sweet spot!):"
        forM_ perfectOnes $ \(tag, count) -> do
          putStrLn $ "  " ++ tag ++ " (" ++ show count ++ " toggles): " ++ describeToggles tag

      unless (null tooHuman) $ do
        putStrLn "\n👤 TOO HUMAN (Need more processing):"
        forM_ tooHuman $ \(tag, count) -> do
          putStrLn $ "  " ++ tag ++ " (" ++ show count ++ " toggles): " ++ describeToggles tag

      unless (null tooRobotic) $ do
        putStrLn "\n🤖 TOO ROBOTIC (Need to soften):"
        forM_ tooRobotic $ \(tag, count) -> do
          putStrLn $ "  " ++ tag ++ " (" ++ show count ++ " toggles): " ++ describeToggles tag

      unless (null unintelligible) $ do
        putStrLn "\n❌ UNINTELLIGIBLE (Broken):"
        forM_ unintelligible $ \(tag, count) -> do
          putStrLn $ "  " ++ tag ++ " (" ++ show count ++ " toggles): " ++ describeToggles tag

      -- Analysis
      unless (null perfectOnes && null tooHuman && null tooRobotic) $ do
        putStrLn "\n📊 Analysis:"
        let avgPerfect = if null perfectOnes then 0 else fromIntegral (sum $ map snd perfectOnes) / fromIntegral (length perfectOnes) :: Double
            avgHuman = if null tooHuman then 0 else fromIntegral (sum $ map snd tooHuman) / fromIntegral (length tooHuman) :: Double
            avgRobotic = if null tooRobotic then 0 else fromIntegral (sum $ map snd tooRobotic) / fromIntegral (length tooRobotic) :: Double

        unless (null perfectOnes) $
          putStrLn $ "  Perfect files average: " ++ show avgPerfect ++ " toggles enabled"
        unless (null tooHuman) $
          putStrLn $ "  Too-human files average: " ++ show avgHuman ++ " toggles enabled"
        unless (null tooRobotic) $
          putStrLn $ "  Too-robotic files average: " ++ show avgRobotic ++ " toggles enabled"

-- | Rate each file interactively
rateFiles :: [(Int, String, FilePath)] -> [(String, Int, String)] -> IO [(String, Int, String)]
rateFiles [] results = return results
rateFiles ((count, tag, path):rest) results = do
  exists <- doesFileExist path
  if not exists
    then do
      putStrLn $ "Skipping missing file: " ++ path
      rateFiles rest results
    else do
      putStrLn "\\n----------------------------------------"
      putStrLn $ "Tag: " ++ tag ++ " (" ++ show count ++ "/4 toggles enabled)"  -- Changed from 5 to 4
      putStrLn $ "Enabled: " ++ describeToggles tag
      putStrLn $ "File: " ++ path
      putStrLn ""

      playAndRate path tag count rest results

-- | Play file and get rating
playAndRate :: FilePath -> String -> Int -> [(Int, String, FilePath)] -> [(String, Int, String)] -> IO [(String, Int, String)]
playAndRate path tag count rest results = do
  callCommand $ "aplay -q " ++ show path
  rating <- getRating
  case rating of
    Nothing -> return results  -- quit
    Just "replay" -> playAndRate path tag count rest results
    Just r -> rateFiles rest ((tag, count, r) : results)
