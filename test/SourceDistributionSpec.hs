module SourceDistributionSpec where

import Control.Monad (when)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), normalise)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec
import ScriptSpec (allKaiFilesIn)
import ExampleSpec (withTempDir)

spec :: Spec
spec = describe "Source distribution" $
  it "includes executable scripts, every Kai fixture, documentation, and website assets" $
    withTempDir $ \directory -> do
      (code, out, err) <- readProcessWithExitCode "cabal" ["sdist","--output-directory=" ++ directory] ""
      when (code /= ExitSuccess) $ expectationFailure (out ++ err)
      archives <- filter (".tar.gz" `isSuffixOf`) <$> listDirectory directory
      length archives `shouldBe` 1
      let archive = directory </> head archives
      doesFileExist archive `shouldReturn` True
      (tarCode, listing, tarErr) <- readProcessWithExitCode "tar" ["-tzf",archive] ""
      (tarCode,tarErr) `shouldBe` (ExitSuccess,"")
      let files = map (normalise . drop 1 . dropWhile (/= '/')) (lines listing)
          required = ["README.md","DEVELOPING.md","FEATURES.md","SPEC.md","AGENTS.md",
                      "package.yaml","stack.yaml","stack.yaml.lock","Makefile",
                      "scripts/kai","scripts/check-script-corpus.py","scripts/check-doc-examples.py",
                      "scripts/export-site.sh","scripts/test-release-binary.sh",
                      "test/doc_examples_test.py","test/fixtures/legacy-kai-runner.sh",
                      ".github/workflows/release.yml",".github/workflows/deploy.yml",
                      "website/static/style.css","website/static/favicon.svg"]
      kaiFiles <- concat <$> mapM allKaiFilesIn ["test","tests","examples"]
      filter (`notElem` files) (map normalise (required ++ kaiFiles)) `shouldBe` []
      filter (isPrefixOf (normalise ".stack-work/")) files `shouldBe` []
