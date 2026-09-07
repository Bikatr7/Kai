{-# LANGUAGE OverloadedStrings #-}
module TestReportSpec (spec) where

import Control.Exception (toException)
import Data.Aeson (Value(..), eitherDecode, object, (.=), toJSON)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson.KeyMap as KM
import ExampleSpec (withTempDir)
import System.Environment (withArgs)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), withFile)
import Test.Hspec
import qualified Test.Hspec.Core.Formatters.V2 as F
import Test.Hspec.Core.Runner (Config(..), defaultConfig, hspecWithResult, summaryExamples, summaryFailures)
import TestReport

spec :: Spec
spec = describe "Per-test result reporting" $ do
  it "records the full path, source location, duration, and successful result" $ do
    itemRecord (["group", "nested"], "é雪")
      (F.Item (Just (F.Location "test/Example.hs" 12 7)) (F.Seconds 0.25) "1000 generated cases" F.Success)
      `shouldBe` object
        [ "groups" .= (["group", "nested"] :: [String]), "name" .= ("é雪" :: String)
        , "source" .= object ["file" .= ("test/Example.hs" :: String), "line" .= (12 :: Int), "column" .= (7 :: Int)]
        , "seconds" .= (0.25 :: Double), "info" .= ("1000 generated cases" :: String)
        , "result" .= ("PASS" :: String), "detail" .= ("" :: String) ]
  it "preserves pending status and missing locations" $ do
    let Object fields = itemRecord ([], "pending") (F.Item Nothing 0 "" (F.Pending Nothing (Just "unsupported")))
    KM.lookup "source" fields `shouldBe` Just Null
    KM.lookup "result" fields `shouldBe` Just (String "PENDING")
    KM.lookup "detail" fields `shouldBe` Just (String "unsupported")
  it "does not call an assertion failure or exception a pass" $ do
    mapM_ (\reason -> do
      let Object fields = itemRecord ([], "failure") (F.Item Nothing 0 "" (F.Failure Nothing reason))
      KM.lookup "result" fields `shouldBe` Just (String "FAIL")
      KM.lookup "detail" fields `shouldBe` Just (toJsonString (show reason)))
      [F.NoReason, F.Reason "wrong", F.ExpectedButGot Nothing "42" "0", F.Error Nothing (toException (userError "failure"))]
  it "writes every actual success, failure, and pending result as independent JSON" $ withTempDir $ \directory -> do
    let path = directory </> "results.jsonl"
    summary <- withFile path WriteMode $ \handle -> withArgs [] $
      hspecWithResult defaultConfig
        { configIgnoreConfigFile = True
        , configFormat = Just (F.formatterToFormat (reportFormatter handle F.silent)) } $ do
          it "good" $ (42 :: Int) `shouldBe` 42
          it "bad" $ (0 :: Int) `shouldBe` 42
          it "waiting" $ pendingWith "unsupported"
    (summaryExamples summary, summaryFailures summary) `shouldBe` (3,1)
    rows <- mapM (either (\err -> expectationFailure err >> pure Null) pure . eitherDecode) . BS.lines =<< BS.readFile path
    [KM.lookup "result" fields | Object fields <- rows]
      `shouldBe` map (Just . String) ["PASS","FAIL","PENDING"]

toJsonString :: String -> Value
toJsonString = toJSON
