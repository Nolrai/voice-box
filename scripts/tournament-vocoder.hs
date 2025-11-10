#!/usr/bin/env runghc
-- Round-robin tournament to compare vocoder configurations
-- Usage: runghc tournament-vocoder.hs [directory] [prefix] [tag1] [tag2] ...
--        or: runghc tournament-vocoder.hs [directory] [tag1] [tag2] ... (prefix defaults to "predoll0")
--
-- Each pair of tags will be played back-to-back and you choose the winner
-- At the end, shows final rankings based on win/loss records
--
-- NOTE: Tags are now 3 characters (SoftBand, Blend, Mix)
--       Smooth is always enabled, Warp is always disabled

import System.Directory (listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.Process (callCommand)
import System.IO (hFlush, stdout)
import Data.List (sort, sortBy, isPrefixOf, isSuffixOf, nub)
import Data.Ord (comparing, Down(..))
import Control.Monad (forM_, when, unless)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

-- | Extract the toggle tag from a filename
extractTag :: String -> Maybe String
extractTag filename =
  case dropWhile (/= '_') filename of
    ('_':'_':rest) ->
      let tag = takeWhile (/= '_') rest
      in if all (`elem` "YN") tag && (length tag == 3 || length tag == 4 || length tag == 5)
        then case tag of
          -- 5-char old format: strip leading Y (Smooth) and drop 3rd char (Warp)
          ['Y', sb, bl, _, mx] -> Just [sb, bl, mx]
          -- 4-char format: drop 3rd char (Warp)
          [sb, bl, _, mx] | length tag == 4 -> Just [sb, bl, mx]
          -- 3-char new format: use as-is
          tag3 | length tag3 == 3 -> Just tag3
          _ -> Nothing
        else Nothing
    _ -> Nothing

-- | Parse a filename into (tag, filepath) - now accepts a prefix parameter
parseFile :: FilePath -> String -> String -> Maybe (String, FilePath)
parseFile dir prefix filename
  | (prefix ++ "__") `isPrefixOf` filename &&
    "_03_vocoder_speedup.wav" `isSuffixOf` filename =
      case extractTag filename of
        Just tag -> Just (tag, dir ++ "/" ++ filename)
        Nothing -> Nothing
  | otherwise = Nothing

-- | Generate all pairs for round-robin
allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x:xs) = [(x, y) | y <- xs] ++ allPairs xs

-- | Get user's choice between two competitors
getChoice :: String -> String -> IO (Maybe String)
getChoice tag1 tag2 = do
  putStr $ "Winner? (1=" ++ tag1 ++ ", 2=" ++ tag2 ++ ", t=tie, r=replay, q=quit): "
  hFlush stdout
  input <- getLine
  case input of
    "1" -> return $ Just tag1
    "2" -> return $ Just tag2
    "t" -> return $ Just "tie"
    "r" -> return $ Just "replay"
    "q" -> return Nothing
    _   -> do
      putStrLn "Invalid input. Please enter 1, 2, t, r, or q."
      getChoice tag1 tag2

-- | Record for win/loss/tie stats
data Record = Record
  { wins :: Int
  , losses :: Int
  , ties :: Int
  } deriving (Show)

emptyRecord :: Record
emptyRecord = Record 0 0 0

-- | Add a win
addWin :: Record -> Record
addWin r = r { wins = wins r + 1 }

-- | Add a loss
addLoss :: Record -> Record
addLoss r = r { losses = losses r + 1 }

-- | Add a tie
addTie :: Record -> Record
addTie r = r { ties = ties r + 1 }

-- | Calculate win percentage (ties count as 0.5 wins)
winPct :: Record -> Double
winPct r =
  let totalGames = fromIntegral (wins r + losses r + ties r)
      effectiveWins = fromIntegral (wins r) + 0.5 * fromIntegral (ties r)
  in if totalGames == 0 then 0 else effectiveWins / totalGames

-- | Run the tournament
runTournament :: [(String, FilePath)] -> IO (Map.Map String Record)
runTournament competitors = do
  let tags = map fst competitors
      pairs = allPairs competitors
      totalMatches = length pairs

  putStrLn "\n=== Round-Robin Tournament ==="
  putStrLn $ "Competitors: " ++ show (length tags)
  putStrLn $ "Total matches: " ++ show totalMatches
  putStrLn "\nControls:"
  putStrLn "  1 = first file wins"
  putStrLn "  2 = second file wins"
  putStrLn "  t = tie (both equally good/bad)"
  putStrLn "  r = replay both files"
  putStrLn "  q = quit tournament\n"

  runMatches pairs 1 totalMatches Map.empty

-- | Run all matches
runMatches :: [((String, FilePath), (String, FilePath))] -> Int -> Int -> Map.Map String Record -> IO (Map.Map String Record)
runMatches [] _ _ records = return records
runMatches (((tag1, path1), (tag2, path2)):rest) matchNum total records = do
  putStrLn "\n========================================"
  putStrLn $ "Match " ++ show matchNum ++ "/" ++ show total
  putStrLn $ tag1 ++ " vs " ++ tag2
  putStrLn "========================================\n"

  result <- playMatch path1 path2 tag1 tag2
  case result of
    Nothing -> return records  -- quit
    Just winner -> do
      let records' = updateRecords tag1 tag2 winner records
      runMatches rest (matchNum + 1) total records'

-- | Play a match and get the result
playMatch :: FilePath -> FilePath -> String -> String -> IO (Maybe String)
playMatch path1 path2 tag1 tag2 = do
  exists1 <- doesFileExist path1
  exists2 <- doesFileExist path2

  unless exists1 $ putStrLn $ "Warning: Missing file " ++ path1
  unless exists2 $ putStrLn $ "Warning: Missing file " ++ path2

  if not exists1 || not exists2
    then do
      putStrLn "Skipping match due to missing files"
      return $ Just "tie"
    else do
      putStrLn $ "Playing: " ++ tag1
      callCommand $ "aplay -q " ++ show path1
      putStrLn ""
      putStrLn $ "Playing: " ++ tag2
      callCommand $ "aplay -q " ++ show path2
      putStrLn ""

      choice <- getChoice tag1 tag2
      case choice of
        Just "replay" -> playMatch path1 path2 tag1 tag2
        other -> return other

-- | Update records based on match result
updateRecords :: String -> String -> String -> Map.Map String Record -> Map.Map String Record
updateRecords tag1 tag2 winner records
  | winner == tag1 =
      let records1 = Map.alter (Just . addWin . fromMaybe emptyRecord) tag1 records
          records2 = Map.alter (Just . addLoss . fromMaybe emptyRecord) tag2 records1
      in records2
  | winner == tag2 =
      let records1 = Map.alter (Just . addLoss . fromMaybe emptyRecord) tag1 records
          records2 = Map.alter (Just . addWin . fromMaybe emptyRecord) tag2 records1
      in records2
  | winner == "tie" =
      let records1 = Map.alter (Just . addTie . fromMaybe emptyRecord) tag1 records
          records2 = Map.alter (Just . addTie . fromMaybe emptyRecord) tag2 records1
      in records2
  | otherwise = records

-- | Display final standings
displayStandings :: Map.Map String Record -> IO ()
displayStandings records = do
  putStrLn "\n========================================"
  putStrLn "FINAL STANDINGS"
  putStrLn "========================================"

  let standings = sortBy (comparing (Down . winPct . snd)) $ Map.toList records

  putStrLn $ "\n" ++ pad 6 "Rank" ++ pad 8 "Tag" ++ pad 6 "W" ++ pad 6 "L" ++ pad 6 "T" ++ "Win%"
  putStrLn $ replicate 40 '-'

  forM_ (zip [1..] standings) $ \(rank, (tag, record)) -> do
    let pct = winPct record * 100
    putStrLn $ pad 6 (show rank)
            ++ pad 8 tag
            ++ pad 6 (show $ wins record)
            ++ pad 6 (show $ losses record)
            ++ pad 6 (show $ ties record)
            ++ printf "%.1f%%" pct

  where
    pad n s = take n (s ++ repeat ' ')
    printf fmt val = take 6 (show (round val :: Int) ++ "%")

main :: IO ()
main = do
  args <- getArgs
  let (dir, prefix, tags) = case args of
        [] -> (".", "predoll0", [])
        [d] -> (d, "predoll0", [])
        (d:p:ts) ->
          -- If second arg looks like a tag (all Y/N and 3-5 chars), treat it as a tag not prefix
          if all (`elem` "YN") p && length p >= 3 && length p <= 5
          then (d, "predoll0", p:ts)
          else (d, p, ts)

  -- List and parse files
  files <- listDirectory dir
  let parsed = [ p | f <- files, Just p <- [parseFile dir prefix f] ]
      filtered =
        if null tags
        then parsed
        else [ p | p@(t, _) <- parsed, t `elem` tags ]

  if length filtered < 2
    then putStrLn $ "Need at least 2 files to run tournament. Found: " ++ show (length filtered)
    else do
      records <- runTournament filtered
      displayStandings records

