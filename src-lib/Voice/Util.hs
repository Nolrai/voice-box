{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Small IO helpers.
-- | This module hosts utilities for invoking external tools; RunLexurgy was
-- | moved here to centralize IO helpers used by multiple executables.
module Voice.Util
  ( annotateIO,
    errorIO,
    readFileUtf8,
    writeFileUtf8
  )
where

import Control.Exception (Exception, SomeException, catch, displayException, throwIO)
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Show (Show (..))
import System.IO (IO, FilePath)
import Control.Category ((.))
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import GHC.IO.Exception (userError)
import Data.Either (Either(..))
import Control.Applicative (pure)

-- | Wrapper exception carrying additional textual context and the original exception.
-- |
-- | Fields:
-- |  * 'context' — a short, human-readable description of the operation that failed
-- |    (prefer a single line and avoid including secrets).
-- |  * 'originalException' — the caught 'SomeException' that triggered this wrapper.
-- |
-- | Use this type to annotate exceptions at IO boundaries so higher-level code can
-- | report contextual information while preserving the original cause.
data AnnotatedException
  = AnnotatedException
  { context :: Text,
    originalException :: SomeException
  }
  deriving (Show)

instance Exception AnnotatedException where
  displayException AnnotatedException {..} =
    "Error during: " <> T.unpack context <> "\n" <> displayException originalException

-- | Run an IO action and, on exception, rethrow it wrapped with a short context message.
-- |
-- | Preconditions:
-- |  * 'ctx' should describe the operation (e.g. \"reading config file <path>\")
-- |    and must NOT contain sensitive data.
-- |
-- | Behaviour:
-- |  * Catches all exceptions of type 'SomeException' and rethrows an
-- |    'AnnotatedException' that includes both the provided context and the
-- |    original exception. The original exception is preserved in
-- |    'originalException' for programmatic inspection.
-- |
-- | Recommended usage:
-- |  * Use at top-level IO boundaries (file reads, process invocation) to add
-- |    human-friendly diagnostics before propagating errors to error-reporting
-- |    layers or test assertions.
annotateIO :: Text -> IO a -> IO a
annotateIO ctx action =
  action `catch` \(e :: SomeException) ->
    throwIO (AnnotatedException {context = ctx, originalException = e})

errorIO :: Text -> IO a
errorIO = throwIO . userError . T.unpack

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 fp = do
  bs <- BS.readFile fp
  case decodeUtf8' bs of
    Left err -> throwIO (userError $ "Invalid UTF-8 in " <> fp <> ": " <> show err)
    Right t  -> pure t

writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 fp txt = BS.writeFile fp (encodeUtf8 txt)
