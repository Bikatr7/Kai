module SourceIO (readSourceFile) where

import Control.Exception (IOException, try)
import qualified UTF8

-- Force lazy decoding while the exception handler is still installed.
readSourceFile :: FilePath -> IO (Either IOException String)
readSourceFile = try . UTF8.readFile
