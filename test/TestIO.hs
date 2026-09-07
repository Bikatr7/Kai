module TestIO (captureOutput, withStdin, withReadOnlyStdout) where

import Control.Exception (bracket, evaluate)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO

withTempHandle :: ((FilePath, Handle) -> IO a) -> IO a
withTempHandle action = do
  dir <- getTemporaryDirectory
  bracket (openTempFile dir "kai-test-io")
    (\(path, handle) -> hClose handle >> removeFile path) action

-- Handles, rather than raw descriptors, restore buffering and EOF state too.
-- A temporary file avoids pipe-buffer deadlocks on large output and input.
captureOutput :: IO a -> IO (a, String)
captureOutput action = withTempHandle $ \(_, target) -> do
  hFlush stdout
  result <- bracket (hDuplicate stdout)
    (\saved -> hFlush stdout >> hDuplicateTo saved stdout >> hClose saved)
    (\_ -> hDuplicateTo target stdout >> action)
  hSeek target AbsoluteSeek 0
  output <- hGetContents target
  _ <- evaluate (length output)
  return (result, output)

withStdin :: String -> IO a -> IO a
withStdin input action = withTempHandle $ \(_, source) -> do
  hPutStr source input
  hSeek source AbsoluteSeek 0
  bracket (hDuplicate stdin)
    (\saved -> hDuplicateTo saved stdin >> hClose saved)
    (\_ -> hDuplicateTo source stdin >> action)

withReadOnlyStdout :: IO a -> IO a
withReadOnlyStdout action = withTempHandle $ \(path, original) -> do
  hClose original
  withFile path ReadMode $ \target -> do
    hFlush stdout
    bracket (hDuplicate stdout)
      (\saved -> hDuplicateTo saved stdout >> hClose saved)
      (\_ -> hDuplicateTo target stdout >> action)
