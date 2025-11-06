{-# LANGUAGE OverloadedStrings #-}

module Voice.IPA (parseFile) where

import Control.Exception (throwIO)
import Control.Monad.Except
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Void (Void)
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Char (hspace)
import Text.Megaparsec.Error
import Voice.IPA.PhonoCode qualified as Phono
import Voice.IPA.Roman qualified as Roman
import Voice.IPA.Types

-- | Run an ExceptT computation and throw a user-error on failure.
liftToUserError :: ExceptT String IO a -> IO a
liftToUserError action = do
  result <- runExceptT action
  either (throwIO . userError) pure result

wrapEither :: (Show e) => String -> Either e a -> ExceptT String IO a
wrapEither context action = case action of
  Left err -> throwError (context ++ ": " ++ show err)
  Right val -> pure val

-- fallback helpers: prefer left; on failure try right, or combine both errors
-- orElse :: ExceptT String IO a -> ExceptT String IO a -> ExceptT String IO a
-- orElse left right = left `catchError` const right

-- infixr 1 <||>
-- (<||>) :: ExceptT String IO a -> ExceptT String IO a -> ExceptT String IO a
-- (<||>) = orElse

orElseCombine :: ExceptT String IO a -> ExceptT String IO a -> ExceptT String IO a
orElseCombine left right =
  left `catchError` \e1 ->
    right `catchError` \e2 -> throwError (e1 ++ "\n" ++ e2)

infixr 1 <|||>

(<|||>) :: ExceptT String IO a -> ExceptT String IO a -> ExceptT String IO a
(<|||>) = orElseCombine

-- | Top-level parsing: try phonocode, then romanized parse. Uses ExceptT to
-- keep the error flow linear and readable.
parseFile :: FilePath -> IO [[Foot]]
parseFile path = do
  bs <- BS.readFile path
  liftToUserError $ do
    content <- wrapEither "invalid UTF-8" (decodeUtf8' bs)
    -- try raw phonocode; if it fails try romanized parse, and combine errors if both fail
    tryParse "phonocode parse" path Phono.parseText content
      <|||> tryParse "romanized parse" path Roman.parseText content

tryParse :: String -> FilePath -> Parser a -> Text -> ExceptT String IO a
tryParse context filePath p txt =
  let parseAction = runParser (p <* hspace <* eof) filePath txt
   in wrapParser context parseAction

wrapParser :: String -> Either (ParseErrorBundle Text Void) a -> ExceptT String IO a
wrapParser context (Left err) = throwError (context ++ ": " ++ errorBundlePretty err)
wrapParser _ (Right val) = pure val
