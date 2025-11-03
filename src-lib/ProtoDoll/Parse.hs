module ProtoDoll.Parse where
import ProtoDoll.ParseResult
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor

parseFile path = do
  content <- readFile path
  case runParser parseText path content of
    Left err -> putStrLn (errorBundlePretty err)
    Right feet -> print feet

parseText :: Parser [Feet]
parseText = do
  sepBy parseFeet (char '\'')

parseFeet :: Parser Feet
parseFeet =
  many parsePhoneme

parsePhoneme :: Parser Phoneme
parsePhoneme = try parseSilence <|>
  (optional spaces >> (try parseVowel <|> try parseConsonant <|> parseLiminal))

parseVowel :: Parser Phoneme
parseVowel = someOf ["a","e","i","o","u","q"] >>= \vowelNames ->
  return $ Chord (map charToVowelName vowelNames)
  where
    charToVowelName 'a' = A
    charToVowelName 'e' = E
    charToVowelName 'i' = I
    charToVowelName 'o' = O
    charToVowelName 'u' = U
    charToVowelName 'q' = Q

parseConsonant :: Parser Phoneme
parseConsonant = do
  m <- parseManner
  p <- parsePlace
  v <- parseVoice
  return $ Consonant (Consonant m v p)

parseManner :: Parser Manner
parseManner =
      (char 'p' $> P)
  <|> (char 's' $> S)
  <|> (char 'c' $> C)

parseVoice :: Parser Voice
parseVoice =
      (char 'w' $> White)
  <|> (char 'b' $> Brown)
  <|> (char 'n' $> Nasal)

parsePlace :: Parser Place
parsePlace =
      (char '1' $> Front)
  <|> (char '2' $> Mid)
  <|> (char '3' $> Back)

parseLiminal :: Parser Phoneme
parseLiminal =
  Liminal
    <$> (string "t0" $> T0)
    <|> (string "h2w" $> H2W)

parseSilence :: Parser Phoneme
parseSilence =
  try ((string "\n"  <|> string ". \n") $> Silence UtteranceBoundary)
    <|> (string ". " $> Silence PhraseBoundary)
    <|> (string " " >> lookAhead (char '\'') $> Silence Gap)