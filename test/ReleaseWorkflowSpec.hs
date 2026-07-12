module ReleaseWorkflowSpec where

import Data.List (isInfixOf)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

runAssetNameScript :: String -> String -> IO (ExitCode, String, String)
runAssetNameScript os arch =
  readProcessWithExitCode "bash" ["scripts/release-asset-name.sh", os, arch] ""

spec :: Spec
spec = describe "Release workflow asset naming" $ do
  it "names Linux x64 binaries as amd64" $ do
    (exitCode, stdout, stderr) <- runAssetNameScript "Linux" "X64"
    exitCode `shouldBe` ExitSuccess
    stdout `shouldBe` "kai-linux-amd64\n"
    stderr `shouldBe` ""

  it "names macOS arm64 binaries correctly" $ do
    (exitCode, stdout, stderr) <- runAssetNameScript "macOS" "ARM64"
    exitCode `shouldBe` ExitSuccess
    stdout `shouldBe` "kai-macos-arm64\n"
    stderr `shouldBe` ""

  it "names Windows x64 binaries with the .exe suffix" $ do
    (exitCode, stdout, stderr) <- runAssetNameScript "Windows" "X64"
    exitCode `shouldBe` ExitSuccess
    stdout `shouldBe` "kai-windows-amd64.exe\n"
    stderr `shouldBe` ""

  it "rejects unsupported architectures" $ do
    (exitCode, stdout, stderr) <- runAssetNameScript "macOS" "sparc"
    exitCode `shouldBe` ExitFailure 1
    stdout `shouldBe` ""
    stderr `shouldContain` "Unsupported arch: sparc"

  it "uses the asset naming helper in the release workflow" $ do
    workflow <- readFile ".github/workflows/release.yml"
    workflow `shouldSatisfy` isInfixOf "scripts/release-asset-name.sh"
    workflow `shouldSatisfy` not . isInfixOf "mv dist/kai dist/kai-macos-amd64"

  it "only starts a release through explicit workflow dispatch" $ do
    workflow <- readFile ".github/workflows/release.yml"
    let triggerSection = unlines $ takeWhile (/= "permissions:") $ dropWhile (/= "on:") $ lines workflow
    triggerSection `shouldSatisfy` isInfixOf "workflow_dispatch:"
    triggerSection `shouldSatisfy` not . isInfixOf "push:"
    workflow `shouldSatisfy` isInfixOf "if: github.ref == 'refs/heads/master'"

  it "gates release builds on tests and benchmark validation" $ do
    workflow <- readFile ".github/workflows/release.yml"
    workflow `shouldSatisfy` isInfixOf "stack test --fast"
    workflow `shouldSatisfy` isInfixOf "stack bench --benchmark-arguments=\"--iters 1\""
    workflow `shouldSatisfy` isInfixOf "needs: [check-version, validate]"
    workflow `shouldSatisfy` isInfixOf "gh release view"
    workflow `shouldSatisfy` isInfixOf "^kai-windows-"
    workflow `shouldSatisfy` isInfixOf "overwrite_files: true"
    workflow `shouldSatisfy` isInfixOf "git rev-list -n 1"
