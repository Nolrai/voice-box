{-# LANGUAGE OverloadedStrings #-}
module Voice.IPA.Roman
  ( parseText
  ) where
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor (($>))

import Voice.IPA.Types
import qualified Voice.IPA.Common as C

-- top-level (same shape as Phono parser)
parseText :: Parser [[Foot]]
parseText = sepBy (sepBy parseFoot C.stressSep) eol

parseFoot :: Parser Foot
parseFoot = many parsePhoneme

parsePhoneme :: Parser Phoneme
parsePhoneme =
      try C.silenceParser
  <|> (hspace >> choice [try parseNeutralVowel, try C.vowelChord, try parseConsonant, parseLiminal])

-- parse neutral vowel (rotated e / schwa) in romanization — not a Chord
parseNeutralVowel :: Parser Phoneme
parseNeutralVowel = char 'ə' $> NeutralVowel

-- build consonant parser from a table (longest-first handled by longestChoice)
parseConsonant :: Parser Phoneme
parseConsonant = C.longestChoice consonantTable
  where
    consonantTable :: [(Text, Phoneme)]
    consonantTable =
      [ ("pf", Consonant (MkConsonant C White Front))
      , ("bv", Consonant (MkConsonant C Brown Front))
      , ("pv", Consonant (MkConsonant C Nasal Front))
      , ("ts", Consonant (MkConsonant C White Mid))
      , ("dz", Consonant (MkConsonant C Brown Mid))
      , ("tz", Consonant (MkConsonant C Nasal Mid))
      , ("ch", Consonant (MkConsonant C White Back))
      , ("jz", Consonant (MkConsonant C Brown Back))
      , ("cj", Consonant (MkConsonant C Nasal Back))
      , ("sh", Consonant (MkConsonant S White Back))
      , ("zh", Consonant (MkConsonant S Brown Back))
      , ("jh", Consonant (MkConsonant S Nasal Back))
      , ("th", Consonant (MkConsonant S White Mid))
      , ("dh", Consonant (MkConsonant S Brown Mid))
      , ("ph", Consonant (MkConsonant S White Front))
      , ("bh", Consonant (MkConsonant S Brown Front))
      , ("ng", Consonant (MkConsonant P Nasal Back))
      , ("gh", Consonant (MkConsonant P Brown Back))
      , ("kh", Consonant (MkConsonant P White Back))
      , ("p",  Consonant (MkConsonant P White Front))
      , ("b",  Consonant (MkConsonant P Brown Front))
      , ("m",  Consonant (MkConsonant P Nasal Front))
      , ("t",  Consonant (MkConsonant P White Mid))
      , ("d",  Consonant (MkConsonant P Brown Mid))
      , ("n",  Consonant (MkConsonant P Nasal Mid))
      , ("k",  Consonant (MkConsonant P White Back))
      , ("g",  Consonant (MkConsonant P Brown Back))
      , ("f",  Consonant (MkConsonant S White Front))
      , ("v",  Consonant (MkConsonant S Brown Front))
      , ("s",  Consonant (MkConsonant S White Mid))
      , ("z",  Consonant (MkConsonant S Brown Mid))
      ]

parseLiminal :: Parser Phoneme
parseLiminal =
      try (string "t0" $> Liminal T0)
  <|> try (char '?' $> Liminal T0)
  <|> try (string "h2w" $> Liminal H2W)
  <|> (char 'h' $> Liminal H2W)
