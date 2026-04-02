module ExampleSpec where

import Test.Hspec
import Control.Exception (bracket, evaluate)
import Control.Monad (forM_)
import System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, hGetContents, hPutStr, openTempFile)

import CLI (runCLI)
import Parser (parseProgram)
import TypeChecker (typeCheckProgramWithDirIO)
import qualified ModuleSystem

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

withStdin :: String -> IO a -> IO a
withStdin input action = do
  (readFd, writeFd) <- createPipe
  writeHandle <- fdToHandle writeFd
  hPutStr writeHandle input
  hClose writeHandle
  oldStdin <- dup stdInput
  dupTo readFd stdInput
  closeFd readFd
  result <- action
  dupTo oldStdin stdInput
  closeFd oldStdin
  return result

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  tempDir <- getTemporaryDirectory
  bracket
    (do
        (path, handle) <- openTempFile tempDir "kai-example-dir"
        hClose handle
        removeFile path
        createDirectory path
        return path)
    removeDirectoryRecursive
    action

withTempTextFile :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempTextFile dir content =
  bracket
    (do
        (path, handle) <- openTempFile dir "kai-example.txt"
        hPutStr handle content
        hClose handle
        return path)
    removeFile

spec :: Spec
spec = describe "Examples" $ do
  it "runs the calculator example interactively" $ do
    (exitCode, output) <- captureOutput $ withStdin "1\n7\n8\n5\n" $ runCLI ["examples/calculator.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "add => 15"
    output `shouldContain` "Goodbye."

  it "runs the discard demo example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/discard_demo.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "[log] starting example"
    output `shouldContain` "Final report:"

  it "runs the file counter example with a real file" $ do
    withTempDir $ \dir ->
      withTempTextFile dir "Kai examples should stay practical and typed.\n" $ \path -> do
        (exitCode, output) <- captureOutput $ runCLI ["examples/file_counter.kai", path]
        exitCode `shouldBe` ExitSuccess
        output `shouldContain` path
        output `shouldContain` "7 words"

  it "runs the file IO example and writes the expected contents" $ do
    withTempDir $ \dir -> do
      let outputPath = dir </> "kai-output.txt"
      (exitCode, output) <- captureOutput $ runCLI ["examples/file_io.kai", outputPath]
      exitCode `shouldBe` ExitSuccess
      output `shouldContain` "Wrote "
      output `shouldContain` "Read back:"
      exists <- doesFileExist outputPath
      exists `shouldBe` True
      contents <- readFile outputPath
      contents `shouldBe` "Kai writes files\nKai reads them back\nKai keeps scripts typed"

  it "runs the fizzbuzz example with a configured upper bound" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/fizzbuzz.kai", "15"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "FizzBuzz"
    output `shouldContain` "done"

  it "runs the greet example with command-line names" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/greet.kai", "Alice", "Bob"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "Hello, Alice!"
    output `shouldContain` "All greetings sent."

  it "runs the guessing game example interactively" $ do
    (exitCode, output) <- captureOutput $ withStdin "5\n7\n" $ runCLI ["examples/guess_the_number.kai", "7"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "Too low."
    output `shouldContain` "Correct in 2 tries!"

  it "runs the list processing example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/list_processing.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "Report:"
    output `shouldContain` "Tagged status:"

  it "runs the text analysis example with a real input file" $ do
    withTempDir $ \dir ->
      withTempTextFile dir "Kai examples stay practical, typed, and honest.\n" $ \path -> do
        (exitCode, output) <- captureOutput $ runCLI ["examples/text_analysis.kai", path]
        exitCode `shouldBe` ExitSuccess
        output `shouldContain` "Preview:"
        output `shouldContain` "First long word:"

  it "runs the text processing example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/text_processing.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "Fields:"
    output `shouldContain` "First non-empty: focus"

  it "runs the wildcard patterns example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/wildcard_patterns.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldContain` "status: success"
    output `shouldContain` "list has values"

  it "parses and type checks reusable example modules" $ do
    let files =
          [ "examples/MathUtils.kai"
          , "examples/StringUtils.kai"
          , "examples/TextAnalysis.kai"
          , "examples/modules/MathUtils.kai"
          , "examples/modules/StringUtils.kai"
          , "examples/modules/TextAnalysis.kai"
          , "examples/modules/MathUtils/MathUtils.kai"
          , "examples/modules/StringUtils/StringUtils.kai"
          , "examples/modules/TextAnalysis/MathUtils.kai"
          , "examples/modules/TextAnalysis/StringUtils.kai"
          , "examples/modules/TextAnalysis/TextAnalysis.kai"
          ]
    forM_ files $ \path -> do
      content <- readFile path
      case parseProgram content of
        Left err -> expectationFailure $ path ++ " parse error: " ++ show err
        Right program -> do
          result <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO (takeDirectory path) program
          case result of
            Left err -> expectationFailure $ path ++ " type error: " ++ show err
            Right _ -> return ()

  it "runs reusable example modules directly" $ do
    let files =
          [ "examples/MathUtils.kai"
          , "examples/StringUtils.kai"
          , "examples/TextAnalysis.kai"
          , "examples/modules/MathUtils.kai"
          , "examples/modules/StringUtils.kai"
          , "examples/modules/TextAnalysis.kai"
          , "examples/modules/MathUtils/MathUtils.kai"
          , "examples/modules/StringUtils/StringUtils.kai"
          , "examples/modules/TextAnalysis/MathUtils.kai"
          , "examples/modules/TextAnalysis/StringUtils.kai"
          , "examples/modules/TextAnalysis/TextAnalysis.kai"
          ]
    forM_ files $ \path -> do
      (exitCode, _) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess

  it "keeps duplicate example module copies in sync" $ do
    let groups =
          [ [ "examples/MathUtils.kai", "examples/modules/MathUtils.kai", "examples/modules/MathUtils/MathUtils.kai", "examples/modules/TextAnalysis/MathUtils.kai" ]
          , [ "examples/StringUtils.kai", "examples/modules/StringUtils.kai", "examples/modules/StringUtils/StringUtils.kai", "examples/modules/TextAnalysis/StringUtils.kai" ]
          , [ "examples/TextAnalysis.kai", "examples/modules/TextAnalysis.kai", "examples/modules/TextAnalysis/TextAnalysis.kai" ]
          ]
    forM_ groups $ \group -> do
      contents <- mapM readFile group
      contents `shouldSatisfy` all (== head contents)
