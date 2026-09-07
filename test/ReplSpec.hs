module ReplSpec where

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
        (path, handle) <- openTempFile tempDir "kai-repl.kai"
        hPutStr handle content
        hClose handle
        return path)
    removeFile
    action

countOccurrences :: String -> String -> Int
countOccurrences needle haystack
  | null needle = 0
  | otherwise = go haystack
  where
    go [] = 0
    go rest
      | needle `prefixOf` rest = 1 + go (drop (length needle) rest)
      | otherwise = go (drop 1 rest)
    prefixOf prefix str = take (length prefix) str == prefix

spec :: Spec
spec = describe "REPL" $ do
  it "starts by default and handles :type" $ do
    (exitCode, output) <- captureOutput $ withStdin ":type \\x -> x\n:quit\n" $ runCLI []
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` (versionString ++ " REPL")
    output `shouldContain` "Session args come from: kai repl arg1 arg2"
    output `shouldBe` replTranscript "kai> t0 -> t0\nkai> "

  it "keeps definitions across inputs" $ do
    (exitCode, output) <- captureOutput $ withStdin "let x = 41\nx + 1\n:quit\n" $ runCLI []
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` replTranscript "kai> x : Int\nkai> 42\nkai> "

  it "makes repl args available through args" $ do
    (exitCode, output) <- captureOutput $ withStdin "print (length args)\n:quit\n" $ runCLI ["repl", "foo", "bar"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` replTranscript "kai> 2\n()\nkai> "

  it "supports multiline let-in input with inline comments and repl args" $ do
    let input =
          "let firstArg = head args in  // \"foo\"\n\
          \let numArgs = length args in  // 3\n\
          \do { print firstArg; print numArgs; print (show args) }  // [\"foo\", \"bar\", \"baz\"]\n\
          \:quit\n"
    (exitCode, output) <- captureOutput $ withStdin input $ runCLI ["repl", "foo", "bar", "baz"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` replTranscript "kai> .... .... foo\n3\n[foo, bar, baz]\n()\nkai> "

  it "supports annotated polymorphic recursion in repl definitions" $ do
    let input =
          "letrec nestedLayers : Int -> [a] -> Int = \\depth -> \\xs -> if depth == 0 then length xs else 1 + nestedLayers (depth - 1) [xs]\n\
          \nestedLayers 2 [1, 2, 3]\n\
          \:quit\n"
    (exitCode, output) <- captureOutput $ withStdin input $ runCLI []
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "nestedLayers : Int -> ["
    output `shouldContain` "\nkai> 3\nkai> "
    output `shouldNotContain` "error:"

  it "continues data declarations when a line ends with a constructor separator" $ do
    let input =
          "data Flag = Off |\n\
          \  On\n\
          \case On of Off -> 0 | On -> 1\n\
          \:quit\n"
    (exitCode, output) <- captureOutput $ withStdin input $ runCLI []
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` replTranscript "kai> .... Off : Flag\nOn : Flag\nkai> 1\nkai> "

  it "continues case alternatives when a branch line ends with a separator" $ do
    let input =
          "data Flag = Off | On\n\
          \case On of Off -> 0 |\n\
          \  On -> 1\n\
          \:quit\n"
    (exitCode, output) <- captureOutput $ withStdin input $ runCLI []
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` replTranscript "kai> Off : Flag\nOn : Flag\nkai> .... 1\nkai> "

  it "still evaluates complete single-line data declarations immediately" $ do
    let input = "data Flag = Off\nOff\n:quit\n"
    (exitCode, output) <- captureOutput $ withStdin input $ runCLI []
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` replTranscript "kai> Off : Flag\nkai> Off\nkai> "

  it "explains how repl args work in :help" $ do
    (exitCode, output) <- captureOutput $ withStdin ":help\n:quit\n" $ runCLI []
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "Start with `kai repl foo bar` or `kai --repl foo bar` to populate args."
    output `shouldContain` "End a data declaration or case branch with `|` to continue it on the next line."

  it "loads files with data declarations and reuses their definitions" $ do
    withTempKaiFile
      "data Option a = None | Some a\n\
      \let unwrap = \\value -> case value of None -> 0 | Some x -> x\n"
      $ \path -> do
          let input = ":load " ++ path ++ "\nunwrap (Some 7)\n:quit\n"
          (exitCode, output) <- captureOutput $ withStdin input $ runCLI []
          exitCode `shouldBe` ExitSuccess
          output `shouldBe` replTranscript ("kai> Loaded " ++ path ++ "\nkai> 7\nkai> ")

  it "reloads the most recently loaded file" $ do
    withTempKaiFile "let value = 123456\nvalue\n" $ \path -> do
      let input = ":load " ++ path ++ "\n:reload\n:quit\n"
      (exitCode, output) <- captureOutput $ withStdin input $ runCLI []
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` replTranscript
        ("kai> 123456\nLoaded " ++ path ++ "\nkai> 123456\nLoaded " ++ path ++ "\nkai> ")

replTranscript :: String -> String
replTranscript body = unlines
  [ versionString ++ " REPL"
  , "Commands: :type EXPR, :load FILE, :reload, :quit, :help"
  , "Session args come from: kai repl arg1 arg2  (or: kai --repl arg1 arg2)"
  ] ++ body
