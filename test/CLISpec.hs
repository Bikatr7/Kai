module CLISpec where

import Test.Hspec
import Control.Exception (bracket, evaluate)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents, hPutStr, openTempFile)

import CLI (runCLI)

-- POSIX-specific stdout capture, consistent with the existing input tests.
import System.Posix.IO

captureOutput :: IO a -> IO (a, String)
captureOutput action = do
    (readFd, writeFd) <- createPipe
    oldStdout <- dup stdOutput
    dupTo writeFd stdOutput
    closeFd writeFd
    result <- action
    dupTo oldStdout stdOutput
    closeFd oldStdout
    readHandle <- fdToHandle readFd
    hGetContents readHandle >>= \out -> evaluate (length out) >> return (result, out)

withTempKaiFile :: String -> (FilePath -> IO a) -> IO a
withTempKaiFile content action = do
  tempDir <- getTemporaryDirectory
  bracket
    (do
        (path, handle) <- openTempFile tempDir "kai-cli-test.kai"
        hPutStr handle content
        hClose handle
        return path)
    removeFile
    action

spec :: Spec
spec = describe "CLI" $ do
  it "returns a non-zero exit code for runtime errors" $ do
    (exitCode, output) <- captureOutput $ runCLI ["-e", "10 / 0"]
    exitCode `shouldBe` ExitFailure 1
    output `shouldContain` "Runtime error: DivByZero"

  it "returns a non-zero exit code for parse errors" $ do
    (exitCode, output) <- captureOutput $ runCLI ["-e", "let x ="]
    exitCode `shouldBe` ExitFailure 1
    output `shouldContain` "Parse error:"

  it "passes command-line arguments through program-mode execution" $ do
    withTempKaiFile "let x = 1\nprint (length args)\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path, "foo", "bar"]
      exitCode `shouldBe` ExitSuccess
      output `shouldContain` "2\n"

  it "runs files with a shebang line" $ do
    withTempKaiFile "#!/usr/bin/env kai\nprint \"hello\"\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess
      output `shouldContain` "hello\n"

  it "accepts top-level let expressions in program files" $ do
    withTempKaiFile "let x = 1\nlet y = x in print y\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess
      output `shouldContain` "1\n"
