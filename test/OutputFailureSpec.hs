module OutputFailureSpec where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), withFile, hGetContents)
import System.Process (CreateProcess(..), StdStream(..), proc, withCreateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Hspec
import ExampleSpec (withTempDir)

spec :: Spec
spec = describe "Output failures" $ do
  forM_ [(["--version"],"IO error:"), (["--help"],"IO error:"),
         (["-e","print 42"],"Runtime error: print: I/O operation failed."),
         (["-e","1/0"],"Runtime error: Division by zero."),
         (["-e","1+true"],"Type error:"),
         (["-e","let x ="],"Parse error:"),
         (["--debug","-e","42"],"IO error:")] $ \(arguments, diagnostic) ->
    it ("fails with a stderr diagnostic for " ++ show arguments) $
      withTempDir $ \directory -> do
        (code, err) <- runWithReadOnlyOutput directory arguments False
        code `shouldBe` ExitFailure 1
        err `shouldContain` diagnostic

  it "stops effects after buffered stdout fails" $ withTempDir $ \directory -> do
    let marker = directory </> "later.txt"
    (code, err) <- runWithReadOnlyOutput directory
      ["-e","print \"hello\"; writeFile " ++ show marker ++ " \"unexpected\""] False
    code `shouldBe` ExitFailure 1
    err `shouldContain` "print: I/O operation failed."
    doesFileExist marker `shouldReturn` False

  it "returns failure even when both output streams are unwritable" $ withTempDir $ \directory -> do
    (code, err) <- runWithReadOnlyOutput directory ["-e","print 42"] True
    (code,err) `shouldBe` (ExitFailure 1,"")

  it "allows computations that do not write stdout" $ withTempDir $ \directory -> do
    result <- runWithReadOnlyOutput directory ["-e","6*7"] False
    result `shouldBe` (ExitSuccess,"")

  it "does not report successful script checks when the result cannot be written" $ withTempDir $ \directory -> do
    let script = directory </> "fixture.kai"
    writeFile script "// expect: 42\n42\n"
    (code, err) <- runWithReadOnlyOutput directory ["--check",script] False
    code `shouldBe` ExitFailure 1
    err `shouldContain` "IO error:"

runWithReadOnlyOutput :: FilePath -> [String] -> Bool -> IO (ExitCode,String)
runWithReadOnlyOutput directory arguments brokenStderr = do
  found <- findExecutable "kai"
  binary <- case found of
    Just path -> pure path
    Nothing -> expectationFailure "Built kai executable missing from PATH" >> pure ""
  let target = directory </> "readonly"
  writeFile target ""
  result <- timeout 5000000 $ withFile target ReadMode $ \handle ->
    withCreateProcess ((proc binary arguments)
      { std_out = UseHandle handle, std_err = if brokenStderr then UseHandle handle else CreatePipe }) $
      \_ _ errorHandle process -> do
        err <- case errorHandle of
          Nothing -> pure ""
          Just stream -> do
            contents <- hGetContents stream
            _ <- evaluate (length contents)
            pure contents
        code <- waitForProcess process
        pure (code,err)
  case result of
    Just output -> pure output
    Nothing -> expectationFailure "CLI hung after an output failure" >> pure (ExitFailure 124,"")
