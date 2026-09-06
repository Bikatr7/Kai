module DocumentationSpec where

import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Documentation examples" $ do
  it "executes README/spec examples, checks stated results and signatures, and runs rendered website examples" $ do
    kai <- requireExecutable "kai"
    html <- lookupEnv "KAI_TEST_WEBSITE_HTML"
    websiteArgs <- case html of
      Just path -> pure ["--html",path]
      Nothing -> (:[]) <$> requireExecutable "kai-website"
    (code, out, err) <- readProcessWithExitCode "python3" (["scripts/check-doc-examples.py",kai] ++ websiteArgs) ""
    err `shouldBe` ""
    if code == ExitSuccess then out `shouldContain` "Documentation examples:"
      else expectationFailure out
  it "rejects false documentation claims and malformed rendered examples" $ do
    kai <- requireExecutable "kai"
    (code, out, err) <- readProcessWithExitCode "python3" ["test/doc_examples_test.py",kai] ""
    if code == ExitSuccess then err `shouldContain` "OK"
      else expectationFailure (out ++ err)

requireExecutable :: String -> IO FilePath
requireExecutable name = do
  found <- findExecutable name
  case found of
    Just path -> pure path
    Nothing -> expectationFailure ("Built executable missing from PATH: " ++ name) >> pure ""
