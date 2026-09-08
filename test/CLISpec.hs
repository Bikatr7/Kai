module CLISpec where

import Test.Hspec
import Control.Exception (bracket, evaluate)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, hGetContents, hPutStr, openTempFile)

import CLI (runCLI, versionString)

import TestIO (captureOutput, withStdin)


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
  it "prints the exact version for --version and -V" $ do
    mapM_ (assertVersionOutput . (: [])) ["--version", "-V"]

  it "keeps version output clean in debug mode" $ do
    mapM_ (assertVersionOutput . ("--debug" :) . (: [])) ["--version", "-V"]

  it "documents both version flags in help output" $ do
    (exitCode, output) <- captureOutput $ runCLI ["--help"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "kai --version"
    output `shouldContain` "kai -V"

  it "passes --version through as a script argument after the filename" $ do
    withTempKaiFile "print (head args)\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path, "--version"]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "--version\n"

  it "returns a non-zero exit code for runtime errors" $ do
    (exitCode, output) <- captureOutput $ runCLI ["-e", "10 / 0"]
    exitCode `shouldBe` ExitFailure 1
    output `shouldBe` "<expression>:1:1: Runtime error: Division by zero.\n1 | 10 / 0\n  | ^\n"

  it "returns a non-zero exit code for parse errors" $ do
    (exitCode, output) <- captureOutput $ runCLI ["-e", "let x ="]
    exitCode `shouldBe` ExitFailure 1
    output `shouldContain` "Parse error:"

  it "passes command-line arguments through program-mode execution" $ do
    withTempKaiFile "let x = 1\nprint (length args)\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path, "foo", "bar"]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "2\n"

  it "runs files with a shebang line" $ do
    withTempKaiFile "#!/usr/bin/env kai\nprint \"hello\"\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "hello\n"

  it "accepts top-level let expressions in program files" $ do
    withTempKaiFile "let x = 1\nlet y = x in print y\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "1\n"

  it "type checks record field access in program files" $ do
    withTempKaiFile "print ({a = 1, b = true}.a)\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "1\n"

  it "returns a non-zero exit code for missing record fields in program files" $ do
    withTempKaiFile "print ({a = 1}.b)\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitFailure 1
      output `shouldBe` path ++ ":1:8: Type error: Missing record field 'b'.\n1 | print ({a = 1}.b)\n  |        ^\n"

  it "runs wildcard-pattern scripts through the real CLI path" $ do
    withTempKaiFile
      "let _ = print (case Just 42 of _ -> \"matched\" | Nothing -> \"not matched\")\n\
      \let _ = print (case [1, 2, 3] of _ -> \"list matched\" | [] -> \"empty\")\n\
      \let _ = print (case (1, \"hello\") of _ -> \"tuple matched\")\n\
      \let _ = print (case Just 42 of _ -> \"bound\" | Nothing -> \"none\")\n\
      \case Nothing of _ -> print \"wildcard works\" | Just x -> print \"should not match\"\n"
      $ \path -> do
          (exitCode, output) <- captureOutput $ runCLI [path]
          exitCode `shouldBe` ExitSuccess
          output `shouldBe` "matched\nlist matched\ntuple matched\nbound\nwildcard works\n"

  it "runs nested record-access scripts through the real CLI path" $ do
    withTempKaiFile "let r = {outer = {inner = 7}, ok = true}\nprint (r.outer.inner)\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "7\n"

  it "runs multiline do-block scripts through the real CLI path" $ do
    withTempKaiFile "let result = do {\n  print \"hello from block\";\n  7\n}\nprint result\n" $ \path -> do
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` "hello from block\n7\n"

  it "runs annotated top-level letrec files through the real CLI path" $ do
    withTempKaiFile
      "letrec nestedLayers : Int -> [a] -> Int = \\depth -> \\xs -> if depth == 0 then length xs else 1 + nestedLayers (depth - 1) [xs]\n\
      \print (nestedLayers 2 [1, 2, 3])\n"
      $ \path -> do
          (exitCode, output) <- captureOutput $ runCLI [path]
          exitCode `shouldBe` ExitSuccess
          output `shouldBe` "3\n"
  where
    assertVersionOutput args = do
      (exitCode, output) <- captureOutput $ runCLI args
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` versionString ++ "\n"
