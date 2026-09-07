module UTF8 (readFile, writeFile, appendFile) where

import Prelude hiding (readFile, writeFile, appendFile)
import Control.Exception (evaluate)
import System.IO (IOMode(..), hGetContents, hPutStr, hSetEncoding, utf8, withFile)

readFile :: FilePath -> IO String
readFile path = withFile path ReadMode $ \handle -> do
  hSetEncoding handle utf8
  contents <- hGetContents handle
  _ <- evaluate (length contents)
  pure contents

writeFile :: FilePath -> String -> IO ()
writeFile = writeWithMode WriteMode

appendFile :: FilePath -> String -> IO ()
appendFile = writeWithMode AppendMode

writeWithMode :: IOMode -> FilePath -> String -> IO ()
writeWithMode mode path contents = withFile path mode $ \handle -> do
  hSetEncoding handle utf8
  hPutStr handle contents
