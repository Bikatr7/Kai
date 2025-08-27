module ScriptSpec where

import Test.Hspec
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (sort)
import Control.Monad (forM, forM_)

import Parser
import Evaluator

spec :: Spec
spec = do
  describe "Script files in tests/" $ do
    results <- runIO $ loadResults "tests"
    forM_ results $ \(fp, res) -> case res of
      Left err -> it (fp ++ " => " ++ err) $ expectationFailure err
      Right val -> it (fp ++ " => " ++ show val) $ True `shouldBe` True

  describe "Script files in test/" $ do
    results <- runIO $ loadResults "test"
    forM_ results $ \(fp, res) -> case res of
      Left err -> it (fp ++ " => " ++ err) $ expectationFailure err
      Right val -> it (fp ++ " => " ++ show val) $ True `shouldBe` True

-- Utilities
kaiFilesIn :: FilePath -> IO [FilePath]
kaiFilesIn dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    pure $ sort [ dir </> e | e <- entries, takeExtension e == ".kai" ]

loadResults :: FilePath -> IO [(FilePath, Either String Value)]
loadResults dir = do
  files <- kaiFilesIn dir
  forM files $ \fp -> do
    content <- readFile fp
    case parseFile fp content of
      Left perr -> pure (fp, Left ("Parse error: " ++ show perr))
      Right expr -> case eval expr of
        Left rerr -> pure (fp, Left ("Runtime error: " ++ show rerr))
        Right val -> pure (fp, Right val)
