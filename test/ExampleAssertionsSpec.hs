module ExampleAssertionsSpec where

import Control.Monad (forM_)
import CLI (runCLI)
import Data.List (isPrefixOf, isSuffixOf, nub, sort)
import qualified Data.Map as Map
import Evaluator (evalProgramWithEnv)
import ExampleSpec (withRestoredExampleState, withTempDir)
import qualified ModuleSystem
import Parser (parseProgram)
import ScriptSpec (allKaiFilesIn)
import System.Directory (copyFile, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>), makeRelative, takeBaseName, takeDirectory)
import System.Exit (ExitCode(..))
import System.Timeout (timeout)
import Syntax (Program(..), TopLevel(..))
import Test.Hspec
import TestIO (captureOutput, withStdin)
import TestSupport (evaluateCheckedSource, readFixture)
import TypeChecker (typeCheckProgramWithDirIO)

spec :: Spec
spec = describe "Example assertions" $ do
  root <- runIO getCurrentDirectory
  files <- runIO $ allKaiFilesIn "examples"
  it "discovers example files" $ files `shouldSatisfy` not . null
  forM_ files $ \path -> describe path $ do
    it "checks the declared result and type" $
      withExampleCopy root files path $ \copy -> do
        result <- runCheck copy
        case result of
          Just (ExitSuccess, output) -> output `shouldSatisfy` isSuffixOf "Script checks passed\n"
          other -> expectationFailure $ "Example directives failed: " ++ show other
    forM_ [("expect", "1", "Expected 1"), ("expect-type", "TInt", "Expected type")] $
      \(directive, wrong, diagnostic) ->
        it ("rejects an incorrect " ++ directive) $
          withExampleCopy root files path $ \copy -> do
            source <- readFixture copy
            let prefix = "// " ++ directive ++ ":"
            writeFile copy $ unlines
              [ if prefix `isPrefixOf` line then prefix ++ " " ++ wrong else line
              | line <- lines source
              ]
            result <- runCheck copy
            case result of
              Just (ExitFailure 1, output) -> output `shouldContain` diagnostic
              other -> expectationFailure $ "Incorrect directive was not rejected: " ++ show other

    source <- runIO $ readFixture path
    case parseProgram source of
      Left err -> it "parses" $ expectationFailure $ show err
      Right (Program declarations) -> do
        let exports = concat [names | TLExport names <- declarations]
            cases = libraryCases (takeBaseName path)
        if null exports then pure () else do
          it "has behavior cases for every export" $
            sort (nub [name | (name, _) <- cases]) `shouldBe` sort exports
          forM_ cases $ \(name, examples) ->
            forM_ examples $ \(arguments, expected) ->
              it ("evaluates imported " ++ name ++ " " ++ arguments) $ do
                let sourceText = "import " ++ takeBaseName path ++ "\n" ++ name ++ " " ++ arguments
                    directory = root </> takeDirectory path
                case parseProgram sourceText of
                  Left err -> expectationFailure $ show err
                  Right program -> do
                    inferred <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO directory program
                    case inferred of
                      Left err -> expectationFailure $ show err
                      Right _ -> do
                        actual <- evalProgramWithEnv Map.empty directory program
                        actual `shouldBe` evaluateCheckedSource expected

-- Each invocation gets its own copies and filesystem, including negative controls.
-- The original CLI integration tests separately assert stdout and file contents.
withExampleCopy :: FilePath -> [FilePath] -> FilePath -> (FilePath -> IO a) -> IO a
withExampleCopy root files path action = withTempDir $ \directory ->
  withRestoredExampleState $ do
    forM_ files $ \file -> do
      let destination = directory </> makeRelative "examples" file
      createDirectoryIfMissing True (takeDirectory destination)
      copyFile (root </> file) destination
    setCurrentDirectory directory
    action (directory </> makeRelative "examples" path)

runCheck :: FilePath -> IO (Maybe (ExitCode, String))
runCheck path = timeout 5000000 $ captureOutput $ withStdin input $ runCLI ["--check", path]
  where
    input = case takeBaseName path of
      "calculator" -> "5\n"
      "guess_the_number" -> "42\n"
      _ -> ""

-- Expected results are literals, independent of the implementations under test.
libraryCases :: String -> [(String, [(String, String)])]
libraryCases "MathUtils" =
  [ ("sum", [("[]", "0"), ("[7]", "7"), ("[-4, 0, 9, -2]", "3")])
  , ("average", [("[]", "0"), ("[7]", "7"), ("[1, 2]", "1"), ("[-3, 0]", "-2")])
  , ("maximum", [("[]", "Nothing"), ("[-7]", "Just (-7)"), ("[-9, -2, -2, -5]", "Just (-2)")])
  , ("minimum", [("[]", "Nothing"), ("[7]", "Just 7"), ("[9, -2, -2, 5]", "Just (-2)")])
  , ("countWhere", [("(\\n -> n > 0) []", "0"), ("(\\n -> n > 0) [-1, 0, 2, 3]", "2"), ("(\\n -> n > 0) [-2, -1]", "0"), ("(\\n -> n > 0) [1, 2]", "2")])
  ]
libraryCases "StringUtils" =
  [ ("words", [("\"\"", "[]"), ("\"   \"", "[]"), ("\"  Kai   雪  \"", "[\"Kai\", \"雪\"]"), ("\"a\tb\"", "[\"a\tb\"]")])
  , ("csvFields", [("\"\"", "[]"), ("\", ,\"", "[]"), ("\" a, , 雪 ,b, \"", "[\"a\", \"雪\", \"b\"]")])
  , ("previewWords", [("0 \"one two\"", "\"\""), ("(-1) \"one two\"", "\"\""), ("2 \"one two three\"", "\"one | two\""), ("9 \"one two\"", "\"one | two\""), ("3 \"\"", "\"\"")])
  , ("firstNonEmpty", [("[]", "Nothing"), ("[\"\", \"  \" ]", "Nothing"), ("[\"  \", \" 雪 \", \"later\"]", "Just \"雪\"")])
  ]
libraryCases "TextAnalysis" =
  [ ("wordCount", [("\"\"", "0"), ("\"   \"", "0"), ("\"  Kai   雪  \"", "2")])
  , ("characterCount", [("\"\"", "0"), ("\"a 雪\\n\"", "4")])
  , ("averageWordLength", [("\"\"", "0"), ("\"a bbbb\"", "2"), ("\"雪\"", "1")])
  , ("longestWordLength", [("\"\"", "0"), ("\"a bbbb cc\"", "4")])
  , ("firstLongWord", [("4 \"\"", "Nothing"), ("4 \"four tiny\"", "Nothing"), ("4 \"a first longer\"", "Just \"first\""), ("(-1) \"a\"", "Just \"a\"")])
  , ("preview", [("\"\"", "\"\""), ("\"a b c d e f\"", "\"a | b | c | d | e\"")])
  , ("summarize",
      [("\"\"", "{words = 0, characters = 0, averageWordLength = 0, longestWordLength = 0, preview = \"\", firstLongWord = Nothing}")
      ,("\"Kai loves 雪\"", "{words = 3, characters = 11, averageWordLength = 3, longestWordLength = 5, preview = \"Kai | loves | 雪\", firstLongWord = Just \"loves\"}")])
  , ("formatSummary",
      [("\"\"", "\"Words: 0\\nCharacters: 0\\nAverage word length: 0\\nLongest word length: 0\\nPreview: \\nFirst long word: none\"")
      ,("\"Kai loves 雪\"", "\"Words: 3\\nCharacters: 11\\nAverage word length: 3\\nLongest word length: 5\\nPreview: Kai | loves | 雪\\nFirst long word: loves\"")])
  ]
libraryCases _ = []
