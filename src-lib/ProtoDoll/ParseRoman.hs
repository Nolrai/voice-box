{-# LANGUAGE OverloadedStrings #-}

-- | ProtoDoll.Parse
-- Parsers for the ProtoDoll phonetic description format.
--
-- This module exposes a small Megaparsec-based parser that consumes a
-- Text input (expected UTF-8) and produces a list of 'Foot' values as
-- defined in 'ProtoDoll.ParseResult'.
--
-- Notes:
-- * The parser accepts either the Unicode primary-stress mark U+02C8 (ˈ)
--   or the ASCII apostrophe (') as the top-level stress separator.
-- * Input may contain concatenated phoneme tokens (no required separators)
--   so the token parsers are written to parse adjacent tokens.
-- * Silence tokens recognized: ".\n" (utterance boundary), ". " (phrase
--   boundary), "\n" (utterance boundary), and a gap when a stress marker
--   follows immediately.

module ProtoDoll.ParseRoman where

import ProtoDoll.ParseResult
import ProtoDoll.ParsePhonoCode
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString qualified as BS
import Data.Text.Encoding (decodeUtf8')
import Control.Exception (throwIO)
import Data.List (sortBy)
import Data.Ord (comparing)

type Parser = Parsec Void Text

-- | Read and parse a file containing ProtoDoll text.
--
-- This function:
-- * reads the file as a strict ByteString,
-- * decodes UTF-8 explicitly with 'decodeUtf8'' (so invalid UTF-8 is reported),
-- * runs the romanization normalizer ('deromanize') and then the top-level parser.
parseFile :: FilePath -> IO [Foot]
parseFile path = do
  bs <- BS.readFile path
  case decodeUtf8' bs of
    Left ue -> throwIO $ userError ("invalid UTF-8 in " ++ path ++ ": " ++ show ue)
    Right content ->
      case deromanize content of
        Left err -> throwIO $ userError ("deromanization error in " ++ path ++ ": " ++ err)
        Right deromContent ->
          case runParser (parseText <* hspace <* eof) path deromContent of
            Left err -> throwIO $ userError (errorBundlePretty err)
            Right feet -> pure feet


-- | Deromanize/normalize a romanized ProtoDoll text into the compact tokens
-- accepted by the main parser.  This is strict: it rejects any character or
-- token sequence that doesn't map to the phonocode alphabet.
deromanize :: Text -> Either String Text
deromanize txt =
  let base = T.toLower txt

      -- mapping table: roman graphemes -> phonocode tokens
      -- longest-first ordering is important (e.g. "ng" before "g").
      table :: [(Text, Text)]
      table =
        [ -- affricates / multigraphs -> c# (affricate) codes
          ("pf", "c1w"), ("bv", "c1b"), ("pv", "c1n")
        , ("ts", "c2w"), ("dz", "c2b"), ("tz", "c2n")
        , ("ch", "c3w"), ("jz", "c3b"), ("cj", "c3n")
          -- sibilants / fricatives
        , ("sh", "s3w"), ("zh", "s3b"), ("jh", "s3n")
        , ("th", "s2w"), ("dh", "s2b")
        , ("ph", "s1w"), ("bh", "s1b")
          -- velars/plosives and nasals
        , ("ng", "p3n")
        , ("gh", "p3b"), ("kh", "p3w")
        , ("ngg", "p3n") -- safety
          -- simple consonants
        , ("p", "p1w"), ("b", "p1b"), ("m", "p1n")
        , ("t", "p2w"), ("d", "p2b"), ("n", "p2n")
        , ("k", "p3w"), ("g", "p3b")
          -- fricative single-letters (map to sensible places)
        , ("f", "s1w"), ("v", "s1b")
        , ("s", "s2w"), ("z", "s2b")
          -- fallback sh/zh mapped above, keep h as h2w liminal
        , ("h", "h2w")
          -- vowels / long vowels (convert macrons to doubled letters => chord)
        , ("ā", "aa"), ("ē", "ee"), ("ī", "ii"), ("ō", "oo"), ("ū", "uu")
        , ("aa", "aa"), ("ee", "ee"), ("ii", "ii"), ("oo", "oo"), ("uu", "uu")
          -- schwa and stress
        , ("ə", "q")
        , ("'", "ˈ"), ("ʹ", "ˈ")
          -- common diphthongs left intact so parser will make Chords: ai, au, oi, ei, etc.
          -- ensure accidental uppercase/diacritics handled by lowercasing above.
        ]

      -- sort by pattern length descending to ensure longest-first replacement
      tblSorted = sortBy (comparing (negate . T.length . fst)) table

      applyAll t [] = t
      applyAll t ((a,b):rest) = applyAll (T.replace a b t) rest

      out = applyAll base tblSorted

  in case validateDeromanized out of
      Nothing -> Right out
      Just (idx, snippet) ->
        Left $ "unexpected token starting at index " ++ show idx ++ ": " ++ T.unpack snippet

-- | Validate the deromanized text is composed only of tokens the main parser expects.
-- Acceptable tokens:
--   - stress mark U+02C8 (ˈ)
--   - horizontal whitespace or newlines
--   - '.' used for silence markers
--   - vowel sequences: one or more of [a e i o u q]
--   - consonant phonocodes: [psc][123][wbn] (e.g. p1w, s2b, c3n)
--   - liminals: "t0" and "h2w"
validateDeromanized :: Text -> Maybe (Int, Text)
validateDeromanized t0 = go 0 t0
  where
    go :: Int -> Text -> Maybe (Int, Text)
    go _ txt | T.null txt = Nothing
    go idx txt =
      case T.uncons txt of
        Just (c, rest)
          | c == 'ˈ' -> go (idx + 1) rest
          | c == '.' ->
              -- allow '.' optionally followed by space/newline
              let rest' = if not (T.null rest) && (T.head rest == ' ' || T.head rest == '\n') then T.tail rest else rest
              in go (idx + 1) rest'
          | c == '\n' -> go (idx + 1) rest
          | isHorizontalSpace c -> go (idx + 1) rest
          | isVowel c ->
              -- consume a run of vowels (Chord)
              let (vrun, rest') = T.span isVowel txt
              in go (idx + T.length vrun) rest'
          | T.isPrefixOf "t0" txt -> go (idx + 2) (T.drop 2 txt)
          | T.isPrefixOf "h2w" txt -> go (idx + 3) (T.drop 3 txt)
          | Just consLen <- matchConsonant txt -> go (idx + consLen) (T.drop consLen txt)
          | otherwise ->
              -- return problem snippet (up to 10 chars) for diagnostics
              let snippet = T.take 10 txt
              in Just (idx, snippet)
        Nothing -> Nothing

    isHorizontalSpace ch = ch == ' ' || ch == '\t' || ch == '\r'
    isVowel ch = ch `elem` ("aeiouq" :: String)

    -- match a consonant phonocode of the form [psc][123][wbn]
    matchConsonant :: Text -> Maybe Int
    matchConsonant txt =
      case T.unpack txt of
        (m:p:v:_) | m `elem` ("psc" :: String) && p `elem` ("123" :: String) && v `elem` ("wbn" :: String) -> Just 3
        _ -> Nothing