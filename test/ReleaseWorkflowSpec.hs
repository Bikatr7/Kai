module ReleaseWorkflowSpec where

import Control.Exception (bracket)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , executable
  , getCurrentDirectory
  , getPermissions
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  , setPermissions
  , withCurrentDirectory
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec

runAssetNameScript :: String -> String -> IO (ExitCode, String, String)
runAssetNameScript os arch =
  readProcessWithExitCode "bash" ["scripts/release-asset-name.sh", os, arch] ""

runPackageNameScript :: String -> String -> IO (ExitCode, String, String)
runPackageNameScript os arch =
  readProcessWithExitCode "bash" ["scripts/release-package-name.sh", os, arch] ""

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory action = do
  base <- getTemporaryDirectory
  bracket
    (do
      (path, handle) <- openTempFile base "kai-release-workflow-spec"
      hClose handle
      removeFile path
      createDirectory path
      return path)
    removePathForcibly
    action

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

  it "uses permission-preserving release packages on every platform" $ do
    let expectedPackages =
          [ ("Linux", "X64", "kai-linux-amd64.tar.gz\n")
          , ("macOS", "ARM64", "kai-macos-arm64.zip\n")
          , ("Windows", "X64", "kai-windows-amd64.zip\n")
          ]
    mapM_ assertPackageName expectedPackages

  it "round-trips a Linux tarball with its executable mode" $
    assertPackageRoundTrip "Linux" "X64" "kai-linux-amd64.tar.gz" "kai" True

  it "round-trips a macOS ZIP with its executable mode" $
    assertPackageRoundTrip "macOS" "ARM64" "kai-macos-arm64.zip" "kai" True

  it "round-trips a real Windows ZIP with kai.exe at its root" $
    assertPackageRoundTrip "Windows" "X64" "kai-windows-amd64.zip" "kai.exe" False

  it "rejects a tar archive mislabeled as a Windows ZIP" $
    withTempDirectory $ \tempDir -> do
      let binary = tempDir </> "kai.exe"
          packagePath = tempDir </> "fake.zip"
          extractedDir = tempDir </> "extracted"
      writeFile binary "not actually an executable\n"
      (tarExit, _, tarStderr) <-
        readProcessWithExitCode "tar" ["-cf", packagePath, "-C", tempDir, "kai.exe"] ""
      tarExit `shouldBe` ExitSuccess
      tarStderr `shouldBe` ""

      (extractExit, extractStdout, extractStderr) <-
        readProcessWithExitCode
          "bash"
          ["scripts/extract-release-package.sh", "Windows", packagePath, extractedDir]
          ""
      extractExit `shouldBe` ExitFailure 1
      extractStdout `shouldBe` ""
      extractStderr `shouldContain` "not a valid non-empty ZIP archive"

  it "rejects unsupported architectures" $ do
    (exitCode, stdout, stderr) <- runAssetNameScript "macOS" "sparc"
    exitCode `shouldBe` ExitFailure 1
    stdout `shouldBe` ""
    stderr `shouldContain` "Unsupported arch: sparc"

  it "uses the asset naming helper in the release workflow" $ do
    workflow <- readFile ".github/workflows/release.yml"
    workflow `shouldSatisfy` isInfixOf "scripts/release-asset-name.sh"
    workflow `shouldSatisfy` not . isInfixOf "mv dist/kai dist/kai-macos-amd64"

  it "starts for package changes on master and supports manual retries" $ do
    workflow <- readFile ".github/workflows/release.yml"
    let triggerSection = unlines $ takeWhile (/= "permissions:") $ dropWhile (/= "on:") $ lines workflow
    triggerSection `shouldSatisfy` isInfixOf "push:"
    triggerSection `shouldSatisfy` isInfixOf "branches:"
    triggerSection `shouldSatisfy` isInfixOf "- master"
    triggerSection `shouldSatisfy` isInfixOf "paths:"
    triggerSection `shouldSatisfy` isInfixOf "- package.yaml"
    triggerSection `shouldSatisfy` isInfixOf "workflow_dispatch:"
    workflow `shouldSatisfy` isInfixOf "if: github.ref == 'refs/heads/master'"

  it "gates release builds on tests and benchmark validation" $ do
    workflow <- readFile ".github/workflows/release.yml"
    workflow `shouldSatisfy` isInfixOf "stack test --fast"
    workflow `shouldSatisfy` isInfixOf "stack bench --benchmark-arguments=\"--iters 1\""
    workflow `shouldSatisfy` isInfixOf "needs: [check-version, validate]"
    workflow `shouldSatisfy` isInfixOf "gh release view"
    workflow `shouldSatisfy` isInfixOf "kai-linux-amd64.tar.gz"
    workflow `shouldSatisfy` isInfixOf "kai-macos-arm64.zip"
    workflow `shouldSatisfy` isInfixOf "kai-windows-amd64.zip"
    workflow `shouldSatisfy` isInfixOf "SHA256SUMS"
    workflow `shouldSatisfy` isInfixOf "overwrite_files: true"
    workflow `shouldSatisfy` isInfixOf "git rev-list -n 1"

  it "pins release runners and verifies exact downloaded packages before publication" $ do
    workflow <- readFile ".github/workflows/release.yml"
    workflow `shouldSatisfy` isInfixOf "ubuntu-22.04"
    workflow `shouldSatisfy` isInfixOf "macos-15"
    workflow `shouldSatisfy` isInfixOf "windows-2022"
    workflow `shouldSatisfy` isInfixOf "MACOSX_DEPLOYMENT_TARGET"
    workflow `shouldSatisfy` isInfixOf "macos-target: '11.3'"
    workflow `shouldSatisfy` isInfixOf "draft: true"
    workflow `shouldSatisfy` isInfixOf "group: kai-release"
    workflow `shouldSatisfy` isInfixOf "cancel-in-progress: false"
    workflow `shouldSatisfy` isInfixOf "reuse-draft:"
    workflow `shouldSatisfy` isInfixOf "if: needs.check-version.outputs.reuse-draft != 'true'"
    let checkVersionSection = unlines $ takeWhile (/= "  validate:") $ dropWhile (/= "  check-version:") $ lines workflow
    checkVersionSection `shouldSatisfy` isInfixOf "contents: write"
    checkVersionSection `shouldSatisfy` isInfixOf "persist-credentials: false"
    workflow `shouldSatisfy` isInfixOf "verify-release:"
    workflow `shouldSatisfy` isInfixOf "scripts/test-release-binary.sh"
    workflow `shouldSatisfy` isInfixOf "--repo \"$GITHUB_REPOSITORY\" --draft=false --latest"
    let verifySection = unlines $ takeWhile (/= "  publish-release:") $ dropWhile (/= "  verify-release:") $ lines workflow
    verifySection `shouldSatisfy` isInfixOf "contents: write"
    verifySection `shouldSatisfy` isInfixOf "persist-credentials: false"
    verifySection `shouldSatisfy` isInfixOf "Download draft release package"

  it "supports fail-closed macOS and Windows signing when explicitly enabled" $ do
    workflow <- readFile ".github/workflows/release.yml"
    workflow `shouldSatisfy` isInfixOf "vars.APPLE_SIGNING_ENABLED == 'true'"
    workflow `shouldSatisfy` isInfixOf "codesign"
    workflow `shouldSatisfy` isInfixOf "notarytool submit"
    workflow `shouldSatisfy` isInfixOf "vars.WINDOWS_SIGNING_ENABLED == 'true'"
    workflow `shouldSatisfy` isInfixOf "signtool.exe"
    workflow `shouldSatisfy` isInfixOf "Get-AuthenticodeSignature"

  it "contains no malformed patch markers in shell command continuations" $ do
    workflow <- readFile ".github/workflows/release.yml"
    workflow `shouldSatisfy` not . isInfixOf "+            "

  it "syntax-checks every release helper script" $ do
    let scripts =
          [ "scripts/release-asset-name.sh"
          , "scripts/release-package-name.sh"
          , "scripts/package-release-binary.sh"
          , "scripts/extract-release-package.sh"
          , "scripts/test-release-binary.sh"
          ]
    mapM_ assertBashSyntax scripts
  where
    assertPackageName (os, arch, expected) = do
      (exitCode, stdout, stderr) <- runPackageNameScript os arch
      exitCode `shouldBe` ExitSuccess
      stdout `shouldBe` expected
      stderr `shouldBe` ""

    assertPackageRoundTrip os arch expectedPackage expectedBinary shouldBeExecutable =
      withTempDirectory $ \tempDir -> do
        repoDir <- getCurrentDirectory
        let binary = "fake-kai"
            packageDir = "packages"
            extractedDir = "extracted"
            contents = "#!/usr/bin/env bash\nexit 0\n"
        withCurrentDirectory tempDir $ do
          writeFile binary contents
          permissions <- getPermissions binary
          setPermissions binary permissions { executable = True }
          createDirectoryIfMissing True packageDir

          (packageExit, packageStdout, packageStderr) <-
            readProcessWithExitCode
              "bash"
              [repoDir </> "scripts/package-release-binary.sh", os, arch, binary, packageDir]
              ""
          packageExit `shouldBe` ExitSuccess
          packageStderr `shouldBe` ""
          let packagePath = trimNewline packageStdout
          packagePath `shouldBe` packageDir </> expectedPackage
          when (os /= "Linux") $ do
            header <- BS.take 4 <$> BS.readFile packagePath
            header `shouldBe` BS.pack [0x50, 0x4b, 0x03, 0x04]

          (extractExit, extractStdout, extractStderr) <-
            readProcessWithExitCode
              "bash"
              [repoDir </> "scripts/extract-release-package.sh", os, packagePath, extractedDir]
              ""
          extractExit `shouldBe` ExitSuccess
          extractStderr `shouldBe` ""
          let extractedBinary = trimNewline extractStdout
          extractedBinary `shouldBe` extractedDir </> expectedBinary
          readFile extractedBinary `shouldReturn` contents
          when shouldBeExecutable $ do
            extractedPermissions <- getPermissions extractedBinary
            executable extractedPermissions `shouldBe` True

    assertBashSyntax script = do
      (exitCode, stdout, stderr) <- readProcessWithExitCode "bash" ["-n", script] ""
      exitCode `shouldBe` ExitSuccess
      stdout `shouldBe` ""
      stderr `shouldBe` ""

    trimNewline = reverse . dropWhile (`elem` ['\n', '\r']) . reverse
