{-# LANGUAGE OverloadedStrings #-}

module ProtoDoll.Parse.PhonoCode where

import Control.Exception (throwIO)
import qualified Data.ByteString as BS
import Data.Functor (($>))
import Data.Text.Encoding (decodeUtf8')
import ProtoDoll.Parse.Types
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified ProtoDoll.Parse.Common as C

-- | Read and parse a file containing ProtoDoll text.
--
-- This function:
-- * reads the file as a strict ByteString,
-- * decodes UTF-8 explicitly with 'decodeUtf8'' (so invalid UTF-8 is reported),
-- * runs the top-level parser and throws a userError on parse failure.
parseFile :: FilePath -> IO [[Foot]]
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
-- top-level (same shape as Roman parser)
parseText :: Parser [[Foot]]
parseText = sepBy (sepBy parseFoot C.stressSep) eol

-- | Parse one foot: a sequence of phoneme tokens.
--
-- A 'Foot' is represented as many phonemes parsed by 'parsePhoneme'.
parseFoot :: Parser Foot
parseFoot = many parsePhoneme

-- | Parse a single phoneme.
--
-- This will try to parse silence tokens first (so boundaries are recognized),
-- otherwise it attempts to parse vowel, consonant or liminal tokens. Token
-- parsers accept adjacent tokens (no mandatory whitespace).
parsePhoneme :: Parser Phoneme
parsePhoneme =
  try C.silenceParser
    <|>
    -- hspace1 is not required here intentionally: input often omits separators.
    (hspace >> (try C.vowelChord <|> try parseConsonant <|> C.parseLiminal <|> parseNeutralVowel))

parseNeutralVowel :: Parser Phoneme
parseNeutralVowel = char 'q' $> NeutralVowel

-- | Parse a consonant: manner + place + voice.
parseConsonant :: Parser Phoneme
parseConsonant = do
  m <- C.parseManner
  p <- C.parsePlace
  v <- C.parseVoice
  return $ Consonant (MkConsonant m v p)

-- | Parse liminal (special) tokens.
--
-- Recognizes "t0" and "h2w" and wraps them in the 'Liminal' Phoneme.
parseLiminal :: Parser Phoneme
parseLiminal = C.parseLiminal

-- | Parse silence / boundary tokens.
parseSilence :: Parser Phoneme
parseSilence = C.silenceParser
