module UTF8Spec where

import Control.Exception (bracket)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.Char (ord)
import qualified Data.Map as Map
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hGetEncoding, hSetBinaryMode, hSetEncoding, latin1, stdin, stdout)
import Test.Hspec

import CLI (runCLI)
import Evaluator (Value(..), RuntimeError(..), IOErrorKind(..), evalWithEnv)
import ExampleSpec (withTempDir)
import Syntax
import TestIO (captureOutput, withStdin)
import ReplSpec (replTranscript)

sample :: String
sample = "é雪🚀"

sampleBytes :: BS.ByteString
sampleBytes = BS.pack [0xc3,0xa9,0xe9,0x9b,0xaa,0xf0,0x9f,0x9a,0x80]

ascii :: String -> BS.ByteString
ascii = BS.pack . map (fromIntegral . ord)

withLatin1 :: IO a -> IO a
withLatin1 action = bracket
  ((,,) <$> getLocaleEncoding <*> hGetEncoding stdin <*> hGetEncoding stdout)
  (\(locale, inputEncoding, outputEncoding) -> do
    setLocaleEncoding locale
    restoreEncoding stdin inputEncoding
    restoreEncoding stdout outputEncoding)
  (\_ -> setLocaleEncoding latin1 >> action)
  where
    restoreEncoding handle = maybe (hSetBinaryMode handle True) (hSetEncoding handle)

spec :: Spec
spec = describe "UTF-8 files" $ around_ withLatin1 $ do
  it "decodes source independently of the host locale" $ withTempDir $ \dir -> do
    let path = dir </> "source.kai"
    BS.writeFile path (ascii "// expect: 3\nstrLength \"" <> sampleBytes <> ascii "\"\n")
    (code, output) <- captureOutput $ runCLI ["--check",path]
    code `shouldBe` ExitSuccess
    output `shouldBe` "Script checks passed\n"

  it "reads empty source files" $ withTempDir $ \dir -> do
    let path = dir </> "empty.kai"
    BS.writeFile path BS.empty
    result <- captureOutput $ runCLI [path]
    result `shouldBe` (ExitSuccess, "")

  it "preserves missing-source error types" $ withTempDir $ \dir -> do
    (code, output) <- captureOutput $ runCLI [dir </> "missing.kai"]
    code `shouldBe` ExitFailure 1
    output `shouldContain` "IO error:"
    output `shouldContain` "does not exist"

  forM_ [("invalid leading byte",[0xff]), ("truncated sequence",[0xc3]),
         ("overlong sequence",[0xc0,0xaf]), ("surrogate",[0xed,0xa0,0x80]),
         ("out-of-range code point",[0xf4,0x90,0x80,0x80]),
         ("invalid byte after valid text",[0x61,0x62,0xff])] $ \(label, bytes) ->
    it ("rejects " ++ label ++ " and closes the source handle") $ withTempDir $ \dir -> do
      let path = dir </> "invalid.kai"
      BS.writeFile path (BS.pack bytes)
      (code, output) <- captureOutput $ runCLI [path]
      code `shouldBe` ExitFailure 1
      output `shouldContain` "IO error:"
      BS.writeFile path sampleBytes

  it "executes UTF-8 scripts through the CLI" $ withTempDir $ \dir -> do
    let path = dir </> "unicode.kai"
    BS.writeFile path (ascii "// expect: 3\nstrLength \"" <> sampleBytes <> ascii "\"\n")
    (code, output) <- captureOutput $ runCLI ["--check",path]
    code `shouldBe` ExitSuccess
    output `shouldBe` "Script checks passed\n"

  it "decodes imported modules as UTF-8" $ withTempDir $ \dir -> do
    let mainPath = dir </> "main.kai"
    BS.writeFile (dir </> "Unicode.kai")
      (ascii "// expect: ()\nlet count = strLength \"" <> sampleBytes <> ascii "\"\n")
    BS.writeFile mainPath (ascii "// expect: 3\nimport Unicode\ncount\n")
    (code, output) <- captureOutput $ runCLI ["--check",mainPath]
    code `shouldBe` ExitSuccess
    output `shouldBe` "Script checks passed\n"

  it "decodes REPL loads as UTF-8" $ withTempDir $ \dir -> do
    let path = dir </> "repl.kai"
    BS.writeFile path (ascii "// expect: 3\nstrLength \"" <> sampleBytes <> ascii "\"\n")
    (code, output) <- captureOutput $ withStdin (":load " ++ path ++ "\n:quit\n") $ runCLI []
    code `shouldBe` ExitSuccess
    output `shouldBe` replTranscript ("kai> 3\nLoaded " ++ path ++ "\nkai> ")

  it "reads UTF-8 text through the standard library" $ withTempDir $ \dir -> do
    let path = dir </> "text.txt"
    BS.writeFile path sampleBytes
    evalWithEnv Map.empty (ReadFile (StrLit path)) `shouldReturn` Right (VStr sample)

  forM_ [("creates",False,sample,sampleBytes), ("overwrites",True,sample,sampleBytes),
         ("truncates to empty",True,"",BS.empty)] $ \(label, exists, contents, bytes) ->
    it (label ++ " UTF-8 text files") $ withTempDir $ \dir -> do
      let path = dir </> "written.txt"
      when exists $ BS.writeFile path (sampleBytes <> sampleBytes)
      evalWithEnv Map.empty (WriteFile (StrLit path) (StrLit contents)) `shouldReturn` Right VUnit
      BS.readFile path `shouldReturn` bytes

  forM_ [False,True] $ \exists ->
    it ("appends UTF-8 text with existing file = " ++ show exists) $ withTempDir $ \dir -> do
      let path = dir </> "appended.txt"
          prefix = if exists then ascii "prefix:" else BS.empty
      when exists $ BS.writeFile path prefix
      evalWithEnv Map.empty (AppendFile (StrLit path) (StrLit sample)) `shouldReturn` Right VUnit
      BS.readFile path `shouldReturn` (prefix <> sampleBytes)

  it "does not alter a file when appending empty text" $ withTempDir $ \dir -> do
    let path = dir </> "unchanged.txt"
    BS.writeFile path sampleBytes
    evalWithEnv Map.empty (AppendFile (StrLit path) (StrLit "")) `shouldReturn` Right VUnit
    BS.readFile path `shouldReturn` sampleBytes

  it "returns a typed error and stops effects on invalid UTF-8 file contents" $ withTempDir $ \dir -> do
    let path = dir </> "invalid.txt"
        marker = dir </> "later.txt"
        expression = Seq (ReadFile (StrLit path)) (WriteFile (StrLit marker) (StrLit "unexpected"))
    BS.writeFile path (BS.pack [0xff])
    result <- evalWithEnv Map.empty expression
    case result of
      Left (IOFailure InvalidEncoding "readFile" (Just actual) detail) -> do
        actual `shouldBe` path
        detail `shouldSatisfy` (not . null)
      other -> expectationFailure (show other)
    doesFileExist marker `shouldReturn` False

  it "fully decodes files spanning multiple input buffers" $ withTempDir $ \dir -> do
    let path = dir </> "large.txt"
    BS.writeFile path (BS.concat (replicate 10000 sampleBytes))
    evalWithEnv Map.empty (ReadFile (StrLit path)) `shouldReturn` Right (VStr (concat (replicate 10000 sample)))
