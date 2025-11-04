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
module ProtoDoll.ParsePhonoCode where

import ProtoDoll.ParseResult
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor
import Data.Void
import Data.Text (Text)
import Data.ByteString qualified as BS
import Data.Text.Encoding (decodeUtf8')
import Control.Exception (throwIO)
import Data.List.NonEmpty (NonEmpty(..))

type Parser = Parsec Void Text

-- | Read and parse a file containing ProtoDoll text.
--
-- This function:
-- * reads the file as a strict ByteString,
-- * decodes UTF-8 explicitly with 'decodeUtf8'' (so invalid UTF-8 is reported),
-- * runs the top-level parser and throws a userError on parse failure.
parseFile :: FilePath -> IO [Foot]
parseFile path = do
  bs <- BS.readFile path
  case decodeUtf8' bs of
    Left ue -> throwIO $ userError ("invalid UTF-8 in " ++ path ++ ": " ++ show ue)
    Right content ->
      case runParser (parseText <* hspace <* eof) path content of
        Left err -> throwIO $ userError (errorBundlePretty err)
        Right feet -> pure feet

-- | Top-level parser: a sequence of 'Foot' values separated by a stress marker.
--
-- Uses 'sepBy' with 'stressSep' so the separator is consistently interpreted
-- everywhere in the parser.
parseText :: Parser [Foot]
parseText = sepBy parseFeet stressSep

-- | Top-level stress separator parser.
--
-- Accepts either the Unicode primary-stress mark 'ˈ' (U+02C8) or the ASCII
-- apostrophe '\''.  If you need to treat ASCII apostrophe differently (for
-- example, as a glottal stop) change callers to use only 'ˈ' and add a
-- separate parser for glottal stop.
stressSep :: Parser Char
stressSep = char 'ˈ' <|> char '\''

-- | Parse one foot: a sequence of phoneme tokens.
--
-- A 'Foot' is represented as many phonemes parsed by 'parsePhoneme'.
parseFeet :: Parser Foot
parseFeet =
  many parsePhoneme

-- | Parse a single phoneme.
--
-- This will try to parse silence tokens first (so boundaries are recognized),
-- otherwise it attempts to parse vowel, consonant or liminal tokens. Token
-- parsers accept adjacent tokens (no mandatory whitespace).
parsePhoneme :: Parser Phoneme
parsePhoneme = try parseSilence <|>
  -- hspace1 is not required here intentionally: input often omits separators.
  -- space1 would consume newlines which are significant for utterance boundaries.
  (hspace >> (try parseVowel <|> try parseConsonant <|> parseLiminal <|> parseNeutralVowel))

parseNeutralVowel :: Parser Phoneme
parseNeutralVowel = char 'q' $> NeutralVowel

-- | Parse a chord of one or more vowel letters.
--
-- A chord is a sequence of 'parseSingleVowel' values and is wrapped into a
-- 'Chord' Phoneme.
parseVowel :: Parser Phoneme
parseVowel = Chord <$> nonEmptySome parseSingleVowel

nonEmptySome :: Parser a -> Parser (NonEmpty a)
nonEmptySome p = (:|) <$> p <*> many p

-- | Parse a single vowel letter into a 'VowelName'.
parseSingleVowel :: Parser VowelName
parseSingleVowel =
      (char 'a' $> A)
  <|> (char 'e' $> E)
  <|> (char 'i' $> I)
  <|> (char 'o' $> O)
  <|> (char 'u' $> U)

-- | Parse a consonant: manner + place + voice.
parseConsonant :: Parser Phoneme
parseConsonant = do
  m <- parseManner
  p <- parsePlace
  v <- parseVoice
  return $ Consonant (MkConsonant m v p)

-- | Consonant manner parser (p = plosive, s = fricative, c = approximant/other).
parseManner :: Parser Manner
parseManner =
      (char 'p' $> P)
  <|> (char 's' $> S)
  <|> (char 'c' $> C)

-- | Voicing parser (w = white/voiced, b = brown, n = nasal) — maps to 'Voicing'.
parseVoice :: Parser Voicing
parseVoice =
      (char 'w' $> White)
  <|> (char 'b' $> Brown)
  <|> (char 'n' $> Nasal)

-- | Place parser (1 = front, 2 = mid, 3 = back).
parsePlace :: Parser Place
parsePlace =
      (char '1' $> Front)
  <|> (char '2' $> Mid)
  <|> (char '3' $> Back)

-- | Parse liminal (special) tokens.
--
-- Recognizes "t0" and "h2w" and wraps them in the 'Liminal' Phoneme.
parseLiminal :: Parser Phoneme
parseLiminal =
  Liminal <$> ((string "t0" $> T0) <|> (string "h2w" $> H2W))

-- | Parse silence / boundary tokens.
--
-- Recognized forms:
-- * ".\n"   => 'Silence UtteranceBoundary'
-- * ". "    => 'Silence PhraseBoundary'
-- * "\n"    => 'Silence UtteranceBoundary'
-- * (hspace followed by stress marker) => 'Silence Gap'
--
-- The order places the longest/more-specific matches first so that
-- prefixes like "." do not accidentally intercept ".\n".
parseSilence :: Parser Phoneme
parseSilence =
  (try (string ".\n") $> Silence UtteranceBoundary)
    <|> (try (string ". ") $> Silence PhraseBoundary)
    <|> (string "\n" $> Silence UtteranceBoundary)
    <|> (hspace1 >> lookAhead stressSep $> Silence Gap)