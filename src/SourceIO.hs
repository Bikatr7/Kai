module SourceIO (readSourceFile) where

import Control.Exception (IOException, evaluate, try)

-- Force lazy decoding while the exception handler is still installed.
readSourceFile :: FilePath -> IO (Either IOException String)
readSourceFile path = try $ do
  content <- readFile path
  _ <- evaluate (length content)
  return content
