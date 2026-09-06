{-# LANGUAGE OverloadedStrings #-}
module TestReport (runReportedSpec, reportFormatter, itemRecord) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=), encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (Handle, IOMode(WriteMode), hFlush, withFile)
import Test.Hspec (Spec, hspec)
import Test.Hspec.Core.Formatters.V2
import Test.Hspec.Core.Runner (Config(..), defaultConfig, hspecWith)

-- Optional machine-readable results alongside the ordinary Hspec output.
runReportedSpec :: Spec -> IO ()
runReportedSpec tests = do
  report <- lookupEnv "KAI_TEST_REPORT"
  case report of
    Nothing -> hspec tests
    Just path -> withFile path WriteMode $ \handle ->
      hspecWith defaultConfig
        { configFormat = Just (formatterToFormat (reportFormatter handle specdoc)) } tests

reportFormatter :: Handle -> Formatter -> Formatter
reportFormatter handle base = base
  { formatterItemDone = \path item -> do
      liftIO $ BS.hPutStrLn handle (encode (itemRecord path item)) >> hFlush handle
      formatterItemDone base path item
  }

itemRecord :: Path -> Item -> Value
itemRecord (groups, name) item = object
  [ "groups" .= groups
  , "name" .= name
  , "source" .= fmap locationRecord (itemLocation item)
  , "seconds" .= duration
  , "info" .= itemInfo item
  , "result" .= status
  , "detail" .= detail
  ]
  where
    Seconds duration = itemDuration item
    (status, detail) = case itemResult item of
      Success -> ("PASS" :: String, "")
      Pending _ reason -> ("PENDING", fromMaybe "" reason)
      Failure _ reason -> ("FAIL", show reason)
    locationRecord loc = object
      [ "file" .= locationFile loc, "line" .= locationLine loc, "column" .= locationColumn loc ]
