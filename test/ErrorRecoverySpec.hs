module ErrorRecoverySpec where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT, liftIO)
import Control.Exception (AsyncException(ThreadKilled), IOException, bracket, try, throwIO)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Evaluator (Value(..), RuntimeError(..), IOErrorKind(..), eval, evalPure)
import Evaluator.Errors (evalRecoveryWith, runtimeErrorValue, valueRuntimeError)
import ExampleSpec (withTempDir)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory, getPermissions, setPermissions, Permissions(..), getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), withFile)
import System.IO.Error (isPermissionError)
import Test.Hspec
import TestSupport
import TestIO (captureOutput, withStdin, withReadOnlyStdout)
import qualified TypeChecker as T
import Syntax

spec :: Spec
spec = describe "Structured error recovery" $ do
  forM_
    [ ("attempt (\\unit -> 42)", VRight (VInt 42))
    , ("attempt (\\unit -> 1 / 0)", VLeft (VData "DivisionByZero" []))
    , ("attempt (\\unit -> 2147483647 + 1)", VLeft (VData "ArithmeticOverflow" []))
    , ("attempt (\\unit -> head ([] : [Int]))", VLeft (VData "EmptyList" [VStr "head"]))
    , ("attempt (\\unit -> tail ([] : [Int]))", VLeft (VData "EmptyList" [VStr "tail"]))
    , ("attempt (\\unit -> raise (UserError \"bad\"))", VLeft (VData "UserError" [VStr "bad"]))
    , ("case attempt (\\unit -> 1 / 0) of Left DivisionByZero -> 7 | Left other -> 8 | Right n -> n", VInt 7)
    , ("attempt (\\unit -> attempt (\\inner -> 1 / 0))", VRight (VLeft (VData "DivisionByZero" [])))
    , ("attempt (\\unit -> case attempt (\\inner -> 1 / 0) of Left err -> raise err | Right n -> n)", VLeft (VData "DivisionByZero" []))
    , ("attempt (\\unit -> case attempt (\\inner -> 1 / 0) of Left err -> raise (UserError \"handler\") | Right n -> n)", VLeft (VData "UserError" [VStr "handler"]))
    , ("headMaybe ([] : [Int])", VNothing)
    , ("headMaybe [1,2]", VJust (VInt 1))
    , ("tailMaybe ([] : [Int])", VNothing)
    , ("tailMaybe [1]", VJust (VList []))
    , ("tailMaybe [1,2]", VJust (VList [VInt 2]))
    , ("let recover = attempt in map recover [\\unit -> 1, \\unit -> 1 / 0]", VList [VRight (VInt 1),VLeft (VData "DivisionByZero" [])])
    ] $ \(source,expected) -> it source $ do
      evaluateCheckedSource source `shouldBe` Right expected
      eval (parseExpression source) `shouldReturn` Right expected

  it "checks qualified Error types in annotations" $
    shouldInfer "\\e : Error -> raise e" (T.TFun (T.TCustom "Error" []) (T.TVar "a"))

  it "requires an action instead of an eagerly evaluated result" $
    case inferSource "attempt (readFile \"settings.txt\")" of
      Left (T.UnificationError (T.TFun T.TUnit _) T.TString) -> pure ()
      other -> expectationFailure (show other)

  it "cannot turn static mismatches into recoverable runtime values" $
    inferSource "attempt (\\unit -> 1 + true)" `shouldBe` Left (T.UnificationError T.TBool T.TInt)

  it "rejects raising a non-Error value" $
    case inferSource "raise 1" of
      Left (T.UnificationError (T.TCustom "Error" []) T.TInt) -> pure ()
      other -> expectationFailure (show other)

  it "retains prior effects, skips later effects, and continues outside the action" $ do
    let source = "let result = attempt (\\unit -> do { print \"before\"; discard (1 / 0); print \"after\" }) in do { print \"continued\"; result }"
    inferSource source `shouldBe` Right (T.TEither (T.TCustom "Error" []) T.TUnit)
    captureOutput (eval (parseExpression source)) `shouldReturn`
      (Right (VLeft (VData "DivisionByZero" [])),"before\ncontinued\n")

  it "invokes a successful action exactly once" $ do
    let source = "attempt (\\unit -> do { print \"once\"; 7 })"
    captureOutput (eval (parseExpression source)) `shouldReturn`
      (Right (VRight (VInt 7)),"once\n")

  it "does not retry a failing handler" $ do
    let source = "attempt (\\unit -> case attempt (\\inner -> 1 / 0) of Left err -> do { print \"handler\"; raise err } | Right n -> n)"
    captureOutput (eval (parseExpression source)) `shouldReturn`
      (Right (VLeft (VData "DivisionByZero" [])),"handler\n")

  it "does not catch errors while constructing the action argument" $ do
    let source = "attempt (do { discard (1 / 0); \\unit -> 42 })"
    evaluateCheckedSource source `shouldBe` Left DivByZero
    eval (parseExpression source) `shouldReturn` Left DivByZero

  it "does not keep the boundary active for a returned function" $ do
    let source = "case attempt (\\unit -> \\n -> 1 / n) of Right f -> f 0 | Left err -> 99"
    evaluateCheckedSource source `shouldBe` Left DivByZero
    eval (parseExpression source) `shouldReturn` Left DivByZero

  it "does not confuse an ordinary Left result with a raised failure" $ do
    let source = "attempt (\\unit -> Left \"ordinary result\")"
        expected = Right (VRight (VLeft (VStr "ordinary result")))
    evaluateCheckedSource source `shouldBe` expected
    eval (parseExpression source) `shouldReturn` expected

  forM_ [0,7] $ \code -> it ("preserves exit " ++ show code) $ do
    let source = "attempt (\\unit -> exit " ++ show code ++ ")"
    evaluateCheckedSource source `shouldBe` Left (ExitRequested code)
    eval (parseExpression source) `shouldReturn` Left (ExitRequested code)

  it "does not catch uninitialized recursion" $ do
    let source = "attempt (\\unit -> letrec x = x in x)"
    evaluateCheckedSource source `shouldBe` Left (UninitializedRecursion "x")
    eval (parseExpression source) `shouldReturn` Left (UninitializedRecursion "x")

  it "does not catch defensive evaluator type failures" $ do
    let expression = parseExpression "attempt (\\unit -> 1 true)"
    evalPure expression `shouldBe` Left (TypeError "Cannot apply non-callable value: 1")
    eval expression `shouldReturn` Left (TypeError "Cannot apply non-callable value: 1")

  it "does not catch host cancellation inside the action" $ do
    let evaluate :: Map.Map String Value -> Expr -> ExceptT RuntimeError IO Value
        evaluate _ (Var "action") = pure $ VFun "unit" (Var "body") Map.empty
        evaluate _ _ = liftIO $ throwIO ThreadKilled
    result <- try (runExceptT $ evalRecoveryWith pure evaluate Map.empty (Attempt (Var "action")))
      :: IO (Either AsyncException (Either RuntimeError Value))
    result `shouldBe` Left ThreadKilled

  it "does not claim external I/O is available to the pure evaluator" $
    evaluateCheckedSource "attempt (\\unit -> readLine ())" `shouldBe`
      Left (TypeError "readLine not available in pure evaluation")

  forM_
    [ (DivByZero, VData "DivisionByZero" [])
    , (IntegerOverflow, VData "ArithmeticOverflow" [])
    , (EmptyListError "head", VData "EmptyList" [VStr "head"])
    , (EndOfInputError, VData "EndOfInput" [])
    , (UserFailure "detail", VData "UserError" [VStr "detail"])
    ] $ \(failure,value) -> it ("round-trips " ++ show failure) $ do
      runtimeErrorValue failure `shouldBe` Just value
      valueRuntimeError value `shouldBe` Right failure

  forM_ [minBound .. maxBound] $ \category ->
    forM_ [Nothing,Just "雪 file"] $ \path ->
      it ("preserves I/O category and payload " ++ show (category,path)) $ do
        let failure = IOFailure category "readFile" path "host detail"
            value = VData "IOError" [VData (show category) [],VStr "readFile",maybe VNothing (VJust . VStr) path,VStr "host detail"]
        runtimeErrorValue failure `shouldBe` Just value
        valueRuntimeError value `shouldBe` Right failure

  forM_ [VInt 1,VData "UserError" [],VData "IOError" [],
         VData "IOError" [VData "Unknown" [],VStr "x",VNothing,VStr "detail"],
         VData "IOError" [VData "OtherIO" [],VStr "x",VJust (VInt 1),VStr "detail"]] $ \value ->
    it ("rejects malformed internal Error value " ++ show value) $
      valueRuntimeError value `shouldBe` Left (TypeError "raise expects a valid Error value")

  it "distinguishes EOF, a blank line, and preserved whitespace" $ do
    let source = "(readLine (), readLine (), readLine (), readLine ())"
    inferSource source `shouldBe` Right (T.TTuple (replicate 4 (T.TMaybe T.TString)))
    withStdin "\n  雪  \nlast" (eval (parseExpression source)) `shouldReturn`
      Right (VTuple [VJust (VStr ""),VJust (VStr "  雪  "),VJust (VStr "last"),VNothing])

  it "makes legacy input EOF recoverable" $
    withStdin "" (eval (parseExpression "attempt (\\unit -> input)")) `shouldReturn`
      Right (VLeft (VData "EndOfInput" []))

  it "recovers from a missing file and executes the fallback" $ withTempDir $ \dir -> do
    let path = dir </> "missing 雪.txt"
        source = "case attempt (\\unit -> readFile " ++ kaiString path ++ ") of Left (IOError NotFound op path detail) -> (op, path, strLength detail > 0) | Left other -> (\"wrong\", Nothing, false) | Right text -> (\"unexpected\", Nothing, false)"
    eval (parseExpression source) `shouldReturn`
      Right (VTuple [VStr "readFile",VJust (VStr path),VBool True])

  it "preserves an earlier write and skips the write after failure" $ withTempDir $ \dir -> do
    let before = dir </> "before.txt"
        after = dir </> "after.txt"
        source = "attempt (\\unit -> do { writeFile " ++ show before ++ " \"saved\"; discard (1 / 0); writeFile " ++ show after ++ " \"unexpected\" })"
    eval (parseExpression source) `shouldReturn` Right (VLeft (VData "DivisionByZero" []))
    readFile before `shouldReturn` "saved"
    doesFileExist after `shouldReturn` False

  it "classifies invalid UTF-8 as InvalidEncoding" $ withTempDir $ \dir -> do
    let path = dir </> "invalid.txt"
    BS.writeFile path (BS.pack [255])
    result <- eval (parseExpression ("attempt (\\unit -> readFile " ++ show path ++ ")"))
    case result of
      Right (VLeft (VData "IOError" [VData "InvalidEncoding" [],VStr "readFile",VJust (VStr actual),VStr detail])) -> do
        actual `shouldBe` path
        detail `shouldSatisfy` (not . null)
      other -> expectationFailure (show other)

  it "classifies a failed write and preserves its path" $ withTempDir $ \dir -> do
    let path = dir </> "missing" </> "out.txt"
    result <- eval (parseExpression ("attempt (\\unit -> writeFile " ++ show path ++ " \"data\")"))
    case result of
      Right (VLeft (VData "IOError" [VData "NotFound" [],VStr "writeFile",VJust (VStr actual),VStr detail])) -> do
        actual `shouldBe` path
        detail `shouldSatisfy` (not . null)
      other -> expectationFailure (show other)

  it "allows a failed print to be handled without running the following action" $ do
    result <- withReadOnlyStdout $ eval (parseExpression "attempt (\\unit -> do { print \"fail\"; exit 7 })")
    case result of
      Right (VLeft (VData "IOError" [VData _ [],VStr "print",VNothing,VStr detail])) ->
        detail `shouldSatisfy` (not . null)
      other -> expectationFailure (show other)

  it "classifies an existing directory as AlreadyExists" $ withTempDir $ \dir -> do
    let path = dir </> "existing directory"
    createDirectory path
    assertIOFailure "AlreadyExists" "createDirectory" path
      ("createDirectory " ++ kaiString path)

  it "classifies an invalid NUL-containing path as InvalidPath" $ withTempDir $ \dir -> do
    let path = dir </> "invalid\0path"
    assertIOFailure "InvalidPath" "readFile" path ("readFile " ++ kaiString path)

  forM_ ["readFile","writeFile","appendFile","fileExists","listDirectory",
         "createDirectory","removeDirectory","setCurrentDirectory"] $ \operation ->
    it ("rejects NUL paths before " ++ operation ++ " can use a truncated prefix") $ withTempDir $ \dir ->
      bracket getCurrentDirectory setCurrentDirectory $ \originalDirectory -> do
        let prefix = dir </> "existing"
            directoryOperation = operation `elem` ["listDirectory","createDirectory","removeDirectory","setCurrentDirectory"]
            path = prefix ++ "\0suffix"
            payload = if operation `elem` ["writeFile","appendFile"] then " \"changed\"" else ""
        if directoryOperation then createDirectory prefix else writeFile prefix "unchanged"
        assertIOFailure "InvalidPath" operation path (operation ++ " " ++ kaiString path ++ payload)
        getCurrentDirectory `shouldReturn` originalDirectory
        if directoryOperation then doesDirectoryExist prefix `shouldReturn` True
          else readFile prefix `shouldReturn` "unchanged"

  it "evaluates a supplied write argument once before rejecting the path" $ withTempDir $ \dir -> do
    let path = dir </> "invalid\0path"
    (_,output) <- captureOutput $ assertIOFailure "InvalidPath" "writeFile" path
      ("writeFile " ++ kaiString path ++ " (do { print \"supplied\"; \"data\" })")
    output `shouldBe` "supplied\n"

  it "classifies a file locked by an open writer as ResourceBusy" $ withTempDir $ \dir -> do
    let path = dir </> "busy.txt"
    withFile path WriteMode $ \_ ->
      assertIOFailure "ResourceBusy" "readFile" path ("readFile " ++ kaiString path)

  it "recovers from a real permission denial when file permissions are enforced" $ withTempDir $ \dir -> do
    let path = dir </> "private.txt"
    writeFile path "private"
    bracket (getPermissions path) (setPermissions path) $ \original -> do
      setPermissions path original {readable = False, writable = False, executable = False}
      probe <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      case probe of
        Right _ -> pendingWith "This account/platform can read files despite removed permissions"
        Left failure -> do
          isPermissionError failure `shouldBe` True
          assertIOFailure "PermissionDenied" "readFile" path ("readFile " ++ kaiString path)

assertIOFailure :: String -> String -> FilePath -> String -> Expectation
assertIOFailure category operation path action = do
  let source = "attempt (\\unit -> " ++ action ++ ")"
  case inferSource source of
    Left failure -> expectationFailure (show failure)
    Right _ -> pure ()
  result <- eval (parseExpression source)
  case result of
    Right (VLeft (VData "IOError" [VData actualCategory [],VStr actualOperation,VJust (VStr actualPath),VStr detail])) -> do
      (actualCategory,actualOperation,actualPath) `shouldBe` (category,operation,path)
      detail `shouldSatisfy` (not . null)
    other -> expectationFailure (show other)

kaiString :: String -> String
kaiString value = "\"" ++ concatMap escape value ++ "\""
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape char = [char]
