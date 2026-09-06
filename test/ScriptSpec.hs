module ScriptSpec where

import Test.Hspec
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Exit (ExitCode(..))
import Data.List (sort, isPrefixOf, stripPrefix)
import Control.Monad (filterM, forM, forM_)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BS
import CLI (runCLI)
import TestIO (captureOutput, withStdin)

spec :: Spec
spec = do
  describe "Script expectation coverage" $ do
    files <- runIO $ allKaiFilesIn "."
    it "requires an expectation directive in every repository .kai file" $ do
      missing <- filterM (fmap (not . any ("// expect:" `isPrefixOf`) . lines) . readFile) files
      missing `shouldBe` []

  forM_ ["tests", "test"] $ \directory ->
    describe ("Script files in " ++ directory ++ "/") $ do
      files <- runIO $ allKaiFilesIn directory
      forM_ files $ \path -> it path $ do
        content <- readFile path
        input <- case [value | line <- lines content, Just value <- [stripPrefix "// stdin:" line]] of
          [] -> return ""
          [value] -> case eitherDecode (BS.pack value) of
            Right text -> return text
            Left err -> expectationFailure ("Bad stdin fixture: " ++ err) >> return ""
          _ -> expectationFailure "Duplicate stdin fixture" >> return ""
        (exitCode, output) <- captureOutput $ withStdin input $ runCLI ["--check", path]
        if exitCode == ExitSuccess
          then output `shouldContain` "Script checks passed\n"
          else expectationFailure output

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
