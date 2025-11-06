{-# LANGUAGE OverloadedStrings #-}
module ProtoDoll.Parse.Common
  ( Parser
  , longestChoice
  , vowelName
  , vowelChord
  , silenceParser
  , stressSep
  , nonEmptySome
  , parseManner
  , parsePlace
  , parseVoice
  , parseLiminal
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.List.NonEmpty (NonEmpty(..))
import ProtoDoll.Parse.Types
import Data.Functor (($>))

-- Build a choice parser from (text -> value) table using longest-first matching.
longestChoice :: [(Text, a)] -> Parser a
longestChoice table =
  let sorted = sortBy (comparing (negate . T.length . fst)) table
      mk (s, v) = try (string s $> v)
  in choice (map mk sorted)

-- vowel parser (single roman vowel to VowelName)
-- does not include "q" or ""
vowelName :: Parser VowelName
vowelName =
      (char 'a' $> A)
  <|> (char 'e' $> E)
  <|> (char 'i' $> I)
  <|> (char 'o' $> O)
  <|> (char 'u' $> U)

-- one-or-more vowels -> Chord
vowelChord :: Parser Phoneme
vowelChord =
  Chord <$> try (nonEmptySome vowelName)

-- stress separator: primary-stress mark or ASCII apostrophe (returns unit)
stressSep :: Parser ()
stressSep = (char 'ˈ' $> ()) <|> (char '\'' $> ())

-- non-empty some, NonEmpty helper
nonEmptySome :: Parser a -> Parser (NonEmpty a)
nonEmptySome p = (:|) <$> p <*> many p

-- manner / place / voice parsers (shared with PhonoCode)
parseManner :: Parser Manner
parseManner =
      (char 'p' $> P)
  <|> (char 's' $> S)
  <|> (char 'c' $> C)

parseVoice :: Parser Voicing
parseVoice =
      (char 'w' $> White)
  <|> (char 'b' $> Brown)
  <|> (char 'n' $> Nasal)

parsePlace :: Parser Place
parsePlace =
      (char '1' $> Front)
  <|> (char '2' $> Mid)
  <|> (char '3' $> Back)

-- liminal tokens shared by both parsers
parseLiminal :: Parser Phoneme
parseLiminal =
      (string "t0" $> Liminal T0)
  <|> (char '?'   $> Liminal T0)  -- alternate symbol for t0
  <|> (string "h2w" $> Liminal H2W)
  <|> (char 'h'    $> Liminal H2W)

-- silence tokens shared by both parsers
silenceParser :: Parser Phoneme
silenceParser =
      try (string ".\n" $> Silence UtteranceBoundary)
  <|> try (string ". " $> Silence PhraseBoundary)
  <|> (char '\n' $> Silence UtteranceBoundary)
  <|> (hspace1 >> lookAhead (char '\'' <|> char 'ˈ') $> Silence Gap)
  <|> (char ',' >> hspace1 $> Silence Gap)