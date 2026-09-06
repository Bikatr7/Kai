module ScriptSpec where

import Test.Hspec
import System.Directory (doesDirectoryExist, listDirectory, findExecutable)
import System.FilePath ((</>), takeExtension)
import System.Exit (ExitCode(..))
import Data.List (sort, stripPrefix)
import Data.Char (isSpace)
import Control.Monad (forM, forM_, when)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Process (readProcessWithExitCode)
import CLI (runCLI)
import TestIO (captureOutput, withStdin)
import TestSupport (readFixture)

spec :: Spec
spec = do
  describe "Shared script fixture conventions" $ do
    fixtures <- runIO $ do
      bytes <- BS.readFile "test/fixtures/script_directives.json"
      either (ioError . userError) pure (eitherDecode bytes :: Either String [(String, String, Bool)])
    it "discovers shared directive cases" $ fixtures `shouldSatisfy` not . null
    forM_ fixtures $ \(name, source, success) ->
      it name $ case validateFixtureDirectives source of
        Right () -> success `shouldBe` True
        Left _ -> success `shouldBe` False

  describe "Script output assertions" $ do
    forM_ [
        ("absent means silence", "", "Script checks passed\n", True),
        ("unexpected output", "", "unexpected\nScript checks passed\n", False),
        ("exact output", "// stdout: \"Hello, World!\\n\"", "Hello, World!\nScript checks passed\n", True),
        ("wrong output", "// stdout: \"Hello, World!\\n\"", "Hello, somebody!\nScript checks passed\n", False),
        ("missing output", "// stdout: \"hello\\n\"", "Script checks passed\n", False),
        ("extra output", "// stdout: \"hello\\n\"", "hello\nextra\nScript checks passed\n", False),
        ("wrong newline", "// stdout: \"hello\\n\"", "helloScript checks passed\n", False),
        ("Unicode", "// stdout: \"é雪\\n\"", "é雪\nScript checks passed\n", True),
        ("blank lines and spaces", "// stdout: \"  \\n\\n\"", "  \n\nScript checks passed\n", True),
        ("missing success marker", "// stdout: \"hello\\n\"", "hello\n", False),
        ("duplicate directive", "// stdout: \"\"\n// stdout: \"\"", "Script checks passed\n", False),
        ("malformed JSON", "// stdout: hello", "Script checks passed\n", False),
        ("non-string JSON", "// stdout: 42", "Script checks passed\n", False)
      ] $ \(label, source, output, success) ->
        it label $ case checkCapturedOutput source output of
          Right () -> success `shouldBe` True
          Left _ -> success `shouldBe` False
    it "decodes Unicode stdin fixtures" $
      stringFixture "stdin" "// stdin: \"é雪\\n\"" `shouldBe` Right "é雪\n"
    it "checks the packaged CLI corpus harness against real programs and incorrect expectations" $ do
      executable <- findExecutable "kai"
      case executable of
        Nothing -> expectationFailure "Built kai executable missing from PATH"
        Just kai -> do
          (code, out, err) <- readProcessWithExitCode "python3" ["test/script_corpus_test.py", kai] ""
          if code == ExitSuccess then err `shouldContain` "OK"
            else expectationFailure (out ++ err)

  describe "Script expectation coverage" $ do
    files <- runIO $ allKaiFilesIn "."
    it "requires valid, unambiguous expectation directives in every repository .kai file" $ do
      forM_ files $ \path -> do
        source <- readFixture path
        case validateFixtureDirectives source of
          Right () -> pure ()
          Left err -> expectationFailure (path ++ ": " ++ err)
    it "checks script sensitivity reporting against passing, failing, and ineffective fixtures" $ do
      executable <- findExecutable "kai"
      case executable of
        Nothing -> expectationFailure "Built kai executable missing from PATH"
        Just kai -> do
          (code, out, err) <- readProcessWithExitCode "python3" ["test/script_audit_test.py", kai] ""
          if code == ExitSuccess then err `shouldContain` "OK"
            else expectationFailure (out ++ err)

  forM_ ["tests", "test"] $ \directory ->
    describe ("Script files in " ++ directory ++ "/") $ do
      files <- runIO $ allKaiFilesIn directory
      when (directory == "tests") $
        it "discovers executable fixtures" $ files `shouldSatisfy` not . null
      forM_ files $ \path -> it path $ do
        content <- readFixture path
        input <- case stringFixture "stdin" content of
          Right value -> return value
          Left err -> expectationFailure err >> return ""
        (exitCode, output) <- captureOutput $ withStdin input $ runCLI ["--check", path]
        if exitCode == ExitSuccess
          then checkCapturedOutput content output `shouldBe` Right ()
          else expectationFailure output

stringFixture :: String -> String -> Either String String
stringFixture name source = case [value | line <- lines source, Just value <- [stripPrefix ("// " ++ name ++ ":") line]] of
  [] -> Right ""
  [value] -> case eitherDecode (BS.fromStrict $ TextEncoding.encodeUtf8 $ Text.pack value) of
    Right text -> Right text
    Left err -> Left $ "Bad " ++ name ++ " fixture: " ++ err
  _ -> Left $ "Duplicate " ++ name ++ " fixture"

validateFixtureDirectives :: String -> Either String ()
validateFixtureDirectives source = do
  required "expect" True
  required "expect-type" False
  _ <- stringFixture "stdin" source
  _ <- stringFixture "stdout" source
  pure ()
  where
    required name mandatory = case [value | line <- lines source, Just value <- [stripPrefix ("// " ++ name ++ ":") line]] of
      [] | not mandatory -> Right ()
      [value] | not (all isSpace value) -> Right ()
      _ -> Left $ "Missing, empty, or duplicate " ++ name ++ " fixture"

checkCapturedOutput :: String -> String -> Either String ()
checkCapturedOutput source output = do
  expected <- (++ "Script checks passed\n") <$> stringFixture "stdout" source
  if output == expected then Right ()
    else Left $ "Expected stdout " ++ show expected ++ ", got " ++ show output

allKaiFilesIn :: FilePath -> IO [FilePath]
allKaiFilesIn dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    nested <- forM entries $ \entry -> do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then if entry `elem` ignoredDirectories then pure [] else allKaiFilesIn path
        else pure [path | takeExtension path == ".kai"]
    pure $ sort (concat nested)
  where
    ignoredDirectories = [".git", ".stack-work", "dist-site", "dist-newstyle"]
