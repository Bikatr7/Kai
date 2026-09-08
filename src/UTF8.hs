module UTF8 (readFile, writeFile, appendFile) where

import Prelude hiding (readFile, writeFile, appendFile)
import Control.Exception (evaluate, try, IOException)
import System.IO (IOMode(..), hGetContents, hPutStr, hSetEncoding, utf8, withFile)
import System.IO.Error (ioeSetLocation)

readFile :: FilePath -> IO String
readFile path = do
  -- Carry decoding failures out as values so withFile cannot overwrite their
  -- location while closing the handle. Opening/closing failures stay ordinary
  -- file errors; callers can distinguish invalid UTF-8 from an invalid path.
  result <- withFile path ReadMode $ \handle -> tryRead $ do
    hSetEncoding handle utf8
    contents <- hGetContents handle
    _ <- evaluate (length contents)
    pure contents
  either (ioError . (`ioeSetLocation` "Kai.UTF8.decode")) pure result
  where
    tryRead :: IO String -> IO (Either IOException String)
    tryRead = try

writeFile :: FilePath -> String -> IO ()
writeFile = writeWithMode WriteMode

appendFile :: FilePath -> String -> IO ()
appendFile = writeWithMode AppendMode

writeWithMode :: IOMode -> FilePath -> String -> IO ()
writeWithMode mode path contents = withFile path mode $ \handle -> do
  hSetEncoding handle utf8
  hPutStr handle contents
