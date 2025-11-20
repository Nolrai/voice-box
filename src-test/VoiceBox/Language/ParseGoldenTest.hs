module VoiceBox.Language.ParseGoldenTest (tests) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import VoiceBox.Language.IPA qualified as PD
import VoiceBox.Language.IPA.Types qualified as PR

-- Golden test:
-- - reads test input at ../story_predoll-0.txt (project root)
-- - runs the parser from the library
-- - compares the `show` output against test-data/golden/parse_golden.txt
-- If the golden file does not exist it is created and the test fails so you can
-- inspect and accept the new golden.
tests :: TestTree
tests =
  testGroup
    "ParseGoldenTest"
    [ testCase "IPA parser golden" $ do
        let inputPath = "test-data/story_predoll-0.txt"
            goldenPath = "test-data/golden/parse_golden.txt"
        parsed <- PD.parseFile inputPath
        let actual = prettyPrint parsed
        exists <- doesFileExist goldenPath
        if not exists
          then do
            createDirectoryIfMissing True (takeDirectory goldenPath)
            writeFile goldenPath actual
            assertFailure $ "Golden file created at " ++ goldenPath ++ ". Please verify and re-run tests."
          else do
            expected <- readFile goldenPath
            actual @?= expected
    ]

prettyPrint :: [[PR.Foot]] -> String
prettyPrint paragraphs = unlines $ concatMap prettyPrintParagraph paragraphs

prettyPrintParagraph :: [PR.Foot] -> [String]
prettyPrintParagraph feet = prettyPrintFoot <$> feet

prettyPrintFoot :: PR.Foot -> String
prettyPrintFoot foot = unlines $ zipWith prettyPrintPhoneme [0 ..] foot

prettyPrintPhoneme :: Int -> PR.Phoneme -> String
prettyPrintPhoneme n p = replicate n '\t' ++ show p
