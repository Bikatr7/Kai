module SiteExportSpec where

import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.List (isInfixOf)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , executable
  , getCurrentDirectory
  , getPermissions
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  , setPermissions
  )
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Process
  ( CreateProcess(cwd, env)
  , proc
  , readCreateProcessWithExitCode
  , readProcessWithExitCode
  )
import Test.Hspec

data ExportHarness = ExportHarness
  { harnessRoot :: FilePath
  , harnessOutput :: FilePath
  , harnessStack :: FilePath
  , harnessCurl :: FilePath
  , harnessPidFile :: FilePath
  , harnessStartCount :: FilePath
  , harnessServerLog :: FilePath
  , harnessUrlLog :: FilePath
  }

spec :: Spec
spec = describe "Static site exporter" $ do
  it "selects its own port, validates Kai output, rewrites assets, and stops its child" $
    withExportHarness $ \harness -> do
      (exitCode, _, stderr) <- runExporter harness []

      exitCode `shouldBe` ExitSuccess
      stderr `shouldBe` ""
      exportedHtml <- readFile (harnessOutput harness </> "index.html")
      exportedHtml `shouldContain` "<meta name=\"kai-site\" content=\"kai-language\">"
      exportedHtml `shouldContain` "Kai Language"
      exportedHtml `shouldContain` "href=\"static/style.css\""
      exportedHtml `shouldSatisfy` not . isInfixOf "href=\"/static/"
      doesFileExist (harnessOutput harness </> "static" </> "style.css") `shouldReturn` True

      requestedUrls <- lines <$> readFile (harnessUrlLog harness)
      requestedUrls `shouldSatisfy` not . null
      requestedUrls `shouldSatisfy` (not . any (isInfixOf ":3000/"))
      startedServers <- lines <$> readFile (harnessServerLog harness)
      length startedServers `shouldBe` 1
      readPort (head startedServers) `shouldSatisfy` \port -> port >= 49152 && port <= 65535
      assertRecordedServerStopped harness

  it "refuses a response without its token and preserves the previous export" $
    withExportHarness $ \harness -> do
      createDirectoryIfMissing True (harnessOutput harness)
      writeFile (harnessOutput harness </> "index.html") "previous export"

      (exitCode, _, stderr) <-
        runExporter
          harness
          [ ("KAI_SITE_EXPORT_PORT", "52123")
          , ("MOCK_CURL_MODE", "foreign")
          ]

      exitCode `shouldBe` ExitFailure 1
      stderr `shouldContain` "response not owned by this exporter"
      readFile (harnessOutput harness </> "index.html") `shouldReturn` "previous export"
      assertRecordedServerStopped harness

  it "retries another selected port when its first child exits during startup" $
    withExportHarness $ \harness -> do
      (exitCode, _, stderr) <-
        runExporter harness [("MOCK_STACK_MODE", "fail-first")]

      exitCode `shouldBe` ExitSuccess
      stderr `shouldBe` ""
      readFile (harnessStartCount harness) `shouldReturn` "2\n"
      startedServers <- lines <$> readFile (harnessServerLog harness)
      length startedServers `shouldBe` 2
      readPort (head startedServers) `shouldNotBe` readPort (startedServers !! 1)
      assertRecordedServerStopped harness

  it "rejects an invalid fixed port before starting the website" $
    forM_ ["not-a-port", "0", "65536", "99999999999999999999"] $ \invalidPort ->
      withExportHarness $ \harness -> do
        (exitCode, _, stderr) <-
          runExporter harness [("KAI_SITE_EXPORT_PORT", invalidPort)]

        exitCode `shouldBe` ExitFailure 1
        stderr `shouldContain` "must be an integer between 1 and 65535"
        doesFileExist (harnessStartCount harness) `shouldReturn` False
        doesDirectoryExist (harnessOutput harness) `shouldReturn` False

withExportHarness :: (ExportHarness -> IO a) -> IO a
withExportHarness = bracket createExportHarness removeExportHarness

createExportHarness :: IO ExportHarness
createExportHarness = do
  tempRoot <- createTempDirectory "kai-site-export-spec"
  let harness =
        ExportHarness
          { harnessRoot = tempRoot
          , harnessOutput = tempRoot </> "dist-site"
          , harnessStack = tempRoot </> "mock-stack"
          , harnessCurl = tempRoot </> "mock-curl"
          , harnessPidFile = tempRoot </> "server.pid"
          , harnessStartCount = tempRoot </> "start-count"
          , harnessServerLog = tempRoot </> "servers.log"
          , harnessUrlLog = tempRoot </> "urls.log"
          }
  writeExecutable (harnessStack harness) mockStack
  writeExecutable (harnessCurl harness) mockCurl
  pure harness

removeExportHarness :: ExportHarness -> IO ()
removeExportHarness harness = do
  exists <- doesDirectoryExist (harnessRoot harness)
  when exists $ removeDirectoryRecursive (harnessRoot harness)

createTempDirectory :: String -> IO FilePath
createTempDirectory template = do
  tempRoot <- getTemporaryDirectory
  (path, handle) <- openTempFile tempRoot template
  hClose handle
  removeFile path
  createDirectory path
  pure path

writeExecutable :: FilePath -> String -> IO ()
writeExecutable path contents = do
  writeFile path contents
  permissions <- getPermissions path
  setPermissions path permissions {executable = True}

runExporter :: ExportHarness -> [(String, String)] -> IO (ExitCode, String, String)
runExporter harness overrides = do
  repositoryRoot <- getCurrentDirectory
  inheritedEnvironment <- getEnvironment
  let testEnvironment =
        [ ("KAI_SITE_EXPORT_OUT_DIR", harnessOutput harness)
        , ("KAI_SITE_EXPORT_STACK_BIN", harnessStack harness)
        , ("KAI_SITE_EXPORT_CURL_BIN", harnessCurl harness)
        , ("MOCK_SERVER_PID", harnessPidFile harness)
        , ("MOCK_START_COUNT", harnessStartCount harness)
        , ("MOCK_SERVER_LOG", harnessServerLog harness)
        , ("MOCK_URL_LOG", harnessUrlLog harness)
        ]
      processEnvironment = foldr setEnvironmentVariable inheritedEnvironment (overrides ++ testEnvironment)
      exporter =
        (proc "bash" ["scripts/export-site.sh"])
          { cwd = Just repositoryRoot
          , env = Just processEnvironment
          }
  readCreateProcessWithExitCode exporter ""

setEnvironmentVariable :: (String, String) -> [(String, String)] -> [(String, String)]
setEnvironmentVariable pair@(name, _) environment =
  pair : filter ((/= name) . fst) environment

readPort :: String -> Int
readPort entry = read (head (words entry))

assertRecordedServerStopped :: ExportHarness -> Expectation
assertRecordedServerStopped harness = do
  pid <- head . lines <$> readFile (harnessPidFile harness)
  (exitCode, _, _) <-
    readProcessWithExitCode
      "bash"
      ["-c", "if kill -0 " ++ pid ++ " 2>/dev/null; then exit 1; else exit 0; fi"]
      ""
  exitCode `shouldBe` ExitSuccess

mockStack :: String
mockStack = unlines
  [ "#!/usr/bin/env bash"
  , "set -euo pipefail"
  , "if [[ \"${1:-}\" == \"build\" ]]; then exit 0; fi"
  , "if [[ \"${1:-}\" != \"exec\" || \"${2:-}\" != \"kai-website\" ]]; then exit 64; fi"
  , "count=0"
  , "if [[ -f \"${MOCK_START_COUNT}\" ]]; then count=$(cat \"${MOCK_START_COUNT}\"); fi"
  , "count=$((count + 1))"
  , "printf '%s\\n' \"${count}\" > \"${MOCK_START_COUNT}\""
  , "printf '%s %s\\n' \"${PORT}\" \"${KAI_SITE_EXPORT_TOKEN}\" >> \"${MOCK_SERVER_LOG}\""
  , "if [[ \"${MOCK_STACK_MODE:-serve}\" == \"exit\" ]]; then exit 1; fi"
  , "if [[ \"${MOCK_STACK_MODE:-serve}\" == \"fail-first\" && \"${count}\" -eq 1 ]]; then exit 1; fi"
  , "printf '%s\\n' \"$$\" > \"${MOCK_SERVER_PID}\""
  , "trap 'exit 0' TERM INT"
  , "while true; do sleep 1; done"
  ]

mockCurl :: String
mockCurl = unlines
  [ "#!/usr/bin/env bash"
  , "set -euo pipefail"
  , "headers=''"
  , "output=''"
  , "url=''"
  , "while (( $# > 0 )); do"
  , "  case \"$1\" in"
  , "    -D) headers=$2; shift 2 ;;"
  , "    -o) output=$2; shift 2 ;;"
  , "    --connect-timeout|--max-time) shift 2 ;;"
  , "    -sS|--fail) shift ;;"
  , "    *) url=$1; shift ;;"
  , "  esac"
  , "done"
  , "printf '%s\\n' \"${url}\" >> \"${MOCK_URL_LOG}\""
  , "token=${KAI_SITE_EXPORT_TOKEN}"
  , "body='<html><head><meta name=\"kai-site\" content=\"kai-language\"><link href=\"/static/style.css\"></head><body>Kai Language</body></html>'"
  , "if [[ \"${url}\" == *\":3000/\"* || \"${MOCK_CURL_MODE:-owned}\" == \"foreign\" ]]; then"
  , "  token='foreign-token'"
  , "  body='<html><head><meta name=\"kai-site\" content=\"kai-language\"></head><body>Foreign Kai page</body></html>'"
  , "fi"
  , "printf 'HTTP/1.1 200 OK\\r\\nX-Kai-Site: kai-language\\r\\nX-Kai-Export-Token: %s\\r\\n\\r\\n' \"${token}\" > \"${headers}\""
  , "printf '%s\\n' \"${body}\" > \"${output}\""
  ]
