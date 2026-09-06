module ExampleSpec where

import Test.Hspec
import Control.Exception (IOException, bracket, bracket_, evaluate, try)
import Control.Monad (forM_, when)
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, setCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, hGetContents, hPutStr, openTempFile)

import CLI (runCLI)
import Parser (parseProgram)
import TypeChecker (typeCheckProgramWithDirIO)
import qualified ModuleSystem

import TestIO (captureOutput, withStdin)



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
  forM_ [False, True] $ \fails ->
    it ("restores directory and environment after example failure = " ++ show fails) $
      withTempDir $ \dir -> do
        originalDir <- getCurrentDirectory
        originalMode <- lookupEnv "KAI_EXAMPLE_MODE"
        result <- try (withRestoredExampleState $ do
          setCurrentDirectory dir
          setEnv "KAI_EXAMPLE_MODE" "temporary-test-value"
          when fails $ ioError (userError "example fixture failure")) :: IO (Either IOException ())
        case result of
          Left err -> do
            fails `shouldBe` True
            show err `shouldContain` "example fixture failure"
          Right () -> fails `shouldBe` False
        getCurrentDirectory `shouldReturn` originalDir
        lookupEnv "KAI_EXAMPLE_MODE" `shouldReturn` originalMode

  it "runs the calculator example interactively" $ do
    (exitCode, output) <- captureOutput $ withStdin "1\n7\n8\n5\n" $ runCLI ["examples/calculator.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` unlines
      [ "=== Kai Calculator ==="
      , "1) add  2) subtract  3) multiply  4) divide  5) exit"
      , "Choose an option:"
      , "First number:"
      , "Second number:"
      , "add => 15"
      , "=== Kai Calculator ==="
      , "1) add  2) subtract  3) multiply  4) divide  5) exit"
      , "Choose an option:"
      , "Goodbye."
      ]

  it "runs the discard demo example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/discard_demo.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` unlines
      [ "[log] starting example"
      , "[log] pipeline: read, type-check, run"
      , "Final report: {steps: [read, type-check, run], user: Kai}"
      ]

  it "runs the custom data types example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/custom_data_types.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` customDataOutput

  it "runs the file counter example with a real file" $ do
    withTempDir $ \dir ->
      withTempTextFile dir "Kai examples should stay practical and typed.\n" $ \path -> do
        (exitCode, output) <- captureOutput $ runCLI ["examples/file_counter.kai", path]
        exitCode `shouldBe` ExitSuccess
        output `shouldBe` path ++ ": 7 words, longest word length 9\n"

  it "runs the file IO example and writes the expected contents" $ do
    withTempDir $ \dir -> withRestoredExampleState $ do
      originalDir <- getCurrentDirectory
      let workspacePath = dir </> "workspace"
      let outputPath = workspacePath </> "report.txt"
      (exitCode, output) <- captureOutput $ runCLI ["examples/file_io.kai", workspacePath]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` unlines
        [ "Workspace: " ++ workspacePath
        , "Current dir: " ++ originalDir
        , "Mode: workspace-demo"
        , "Entries here: [report.txt]"
        , "Exists after write: True"
        , "Read back: Kai writes files | Kai can hop between directories | Kai keeps scripts typed"
        ]
      exists <- doesFileExist outputPath
      exists `shouldBe` True
      contents <- readFile outputPath
      contents `shouldBe` "Kai writes files\nKai can hop between directories\nKai keeps scripts typed"

  it "runs the fizzbuzz example with a configured upper bound" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/fizzbuzz.kai", "15"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` unlines
      ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "FizzBuzz", "done"]

  it "runs the greet example with command-line names" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/greet.kai", "Alice", "Bob"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` "Hello, Alice!\nHello, Bob!\nAll greetings sent.\n"

  it "runs the guessing game example interactively" $ do
    (exitCode, output) <- captureOutput $ withStdin "5\n7\n" $ runCLI ["examples/guess_the_number.kai", "7"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` unlines
      [ "Guess a number between 1 and 100."
      , "Attempt 1: enter a guess"
      , "Too low."
      , "Attempt 2: enter a guess"
      , "Correct in 2 tries!"
      ]

  it "runs the list processing example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/list_processing.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` unlines
      [ "Report: {count: 6, evenCount: 2, labels: [(5, steady), (8, steady), (13, steady), (21, high), (34, high), (55, high)], total: 136}"
      , "Tagged total: {label: total, value: 136}"
      , "Tagged status: {label: status, value: ready}"
      ]

  it "runs the text analysis example with a real input file" $ do
    withTempDir $ \dir ->
      withTempTextFile dir "Kai examples stay practical, typed, and honest.\n" $ \path -> do
        (exitCode, output) <- captureOutput $ runCLI ["examples/text_analysis.kai", path]
        exitCode `shouldBe` ExitSuccess
        output `shouldBe` unlines
          [ "Preview: Kai | examples | stay | practical, | typed,"
          , "Words: 7"
          , "Characters: 48"
          , "Average word length: 5"
          , "Longest word length: 10"
          , "First long word: examples"
          ]

  it "runs the text processing example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/text_processing.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` unlines
      ["Fields: [apples, bananas, pears]", "Preview: Kai | examples | should | stay", "First non-empty: focus"]

  it "runs the wildcard patterns example" $ do
    (exitCode, output) <- captureOutput $ runCLI ["examples/wildcard_patterns.kai"]
    exitCode `shouldBe` ExitSuccess
    output `shouldBe` "status: success\ntuple for kai\nlist has values\n"

  it "parses and type checks example modules and custom data type examples" $ do
    let files =
          [ "examples/MathUtils.kai"
          , "examples/StringUtils.kai"
          , "examples/TextAnalysis.kai"
          , "examples/custom_data_types.kai"
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

  it "runs module-style examples and direct custom data type examples" $ do
    let files =
          [ "examples/MathUtils.kai"
          , "examples/StringUtils.kai"
          , "examples/TextAnalysis.kai"
          , "examples/custom_data_types.kai"
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
      (exitCode, output) <- captureOutput $ runCLI [path]
      exitCode `shouldBe` ExitSuccess
      output `shouldBe` if path == "examples/custom_data_types.kai" then customDataOutput else ""

  it "keeps duplicate example module copies in sync" $ do
    let groups =
          [ [ "examples/MathUtils.kai", "examples/modules/MathUtils.kai", "examples/modules/MathUtils/MathUtils.kai", "examples/modules/TextAnalysis/MathUtils.kai" ]
          , [ "examples/StringUtils.kai", "examples/modules/StringUtils.kai", "examples/modules/StringUtils/StringUtils.kai", "examples/modules/TextAnalysis/StringUtils.kai" ]
          , [ "examples/TextAnalysis.kai", "examples/modules/TextAnalysis.kai", "examples/modules/TextAnalysis/TextAnalysis.kai" ]
          ]
    forM_ groups $ \group -> do
      contents <- mapM readFile group
      contents `shouldSatisfy` all (== head contents)

customDataOutput :: String
customDataOutput = unlines
  [ "AST: Mul(Add(Lit(5), Lit(3)), Neg(Lit(2)))"
  , "Pretty: ((5 + 3) * (-2))"
  , "Simplified: ((5 + 3) * -2)"
  , "Value: -16"
  ]

withRestoredExampleState :: IO a -> IO a
withRestoredExampleState action = do
  originalDir <- getCurrentDirectory
  originalMode <- lookupEnv "KAI_EXAMPLE_MODE"
  bracket_ (pure ()) (do
      setCurrentDirectory originalDir
      maybe (unsetEnv "KAI_EXAMPLE_MODE") (setEnv "KAI_EXAMPLE_MODE") originalMode) action
