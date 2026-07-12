module IOSpec where

import Test.Hspec
import qualified Data.Map as Map
import Data.List (sort)
import Control.Exception (bracket, bracket_)
import Control.Monad (forM_)
import System.Directory
  ( createDirectory
  , canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  , setCurrentDirectory
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (hClose, openTempFile)
import System.Posix.IO (closeFd, createPipe, dup, dupTo, stdInput)

import Evaluator (RuntimeError(..), Value(..), evalWithEnv)
import Parser (parseExpr)
import TypeChecker (Type(..), typeCheck)
import Syntax

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  tempDir <- getTemporaryDirectory
  bracket
    (do
        (path, handle) <- openTempFile tempDir "kai-io"
        hClose handle
        removeFile path
        createDirectory path
        return path)
    removeDirectoryRecursive
    action

parseOrFail :: String -> IO Expr
parseOrFail source =
  case parseExpr source of
    Left err -> expectationFailure ("Parse error: " ++ show err) >> fail "unreachable"
    Right expr -> return expr

evalSource :: String -> IO (Either RuntimeError Value)
evalSource source = parseOrFail source >>= evalWithEnv Map.empty

withEofStdin :: IO a -> IO a
withEofStdin action = bracket setup restore (const action)
  where
    setup = do
      (readFd, writeFd) <- createPipe
      closeFd writeFd
      oldStdin <- dup stdInput
      dupTo readFd stdInput
      closeFd readFd
      return oldStdin
    restore oldStdin = do
      dupTo oldStdin stdInput
      closeFd oldStdin

commandWithExitCode :: Int -> String
commandWithExitCode code
  | os == "mingw32" = "cmd /c exit " ++ show code
  | otherwise = "sh -c 'exit " ++ show code ++ "'"

spec :: Spec
spec = describe "Extended IO Stdlib" $ do
  it "converts stdin EOF into a Kai runtime error" $ do
    withEofStdin $ evalSource "input"
      `shouldReturn` Left (TypeError "input: could not read from stdin")

  it "writes, appends, and reads files" $ do
    withTempDir $ \dir -> do
      let path = dir </> "note.txt"
      result <- evalSource $
        "writeFile " ++ show path ++ " \"hello\"; appendFile " ++ show path ++ " \" world\"; readFile " ++ show path
      result `shouldBe` Right (VStr "hello world")

  it "stops evaluating later effectful operands after the first runtime error" $ do
    withTempDir $ \dir -> do
      let missingPath = dir </> "missing.txt"
          failure = ReadFile (StrLit missingPath)
          expectedError = TypeError $ "readFile: could not read file '" ++ missingPath ++ "'"
          later marker = Seq (WriteFile (StrLit marker) (StrLit "ran"))
          mapFunction marker resultValue =
            Lambda "item" Nothing $
              If (Eq (Var "item") (IntLit 0)) failure (later marker resultValue)
          foldFunction marker =
            Lambda "acc" Nothing $
              Lambda "item" Nothing $
                If (Eq (Var "item") (IntLit 0)) failure (later marker (Var "acc"))
          cases :: [(String, FilePath -> Expr)]
          cases =
            [ ("sequence", \marker -> Seq failure (later marker UnitLit))
            , ("add", \marker -> Add failure (later marker (IntLit 1)))
            , ("subtract", \marker -> Sub failure (later marker (IntLit 1)))
            , ("multiply", \marker -> Mul failure (later marker (IntLit 1)))
            , ("divide", \marker -> Div failure (later marker (IntLit 1)))
            , ("concatenate", \marker -> Concat failure (later marker (StrLit "value")))
            , ("and", \marker -> And failure (later marker (BoolLit True)))
            , ("or", \marker -> Or failure (later marker (BoolLit True)))
            , ("equality", \marker -> Eq failure (later marker (StrLit "value")))
            , ("less-than", \marker -> Lt failure (later marker (IntLit 1)))
            , ("greater-than", \marker -> Gt failure (later marker (IntLit 1)))
            , ("application", \marker -> App failure (later marker (IntLit 1)))
            , ("cons", \marker -> Cons failure (later marker (ListLit [])))
            , ("list-literal", \marker -> ListLit [failure, later marker (StrLit "value")])
            , ("record-literal", \marker -> RecordLit [("first", failure), ("second", later marker UnitLit)])
            , ("tuple-literal", \marker -> TupleLit [failure, later marker UnitLit])
            , ("map-arguments", \marker -> Syntax.Map failure (later marker (ListLit [])))
            , ("filter-arguments", \marker -> Filter failure (later marker (ListLit [])))
            , ("fold-arguments", \marker -> Foldl failure (later marker (IntLit 0)) (ListLit []))
            , ("take", \marker -> Take failure (later marker (ListLit [])))
            , ("drop", \marker -> Drop failure (later marker (ListLit [])))
            , ("zip", \marker -> Zip failure (later marker (ListLit [])))
            , ("split", \marker -> Split failure (later marker (StrLit "value")))
            , ("join", \marker -> Join failure (later marker (ListLit [])))
            , ("replace", \marker -> Replace failure (later marker (StrLit "new")) (StrLit "value"))
            , ("write-file", \marker -> WriteFile failure (later marker (StrLit "value")))
            , ("append-file", \marker -> AppendFile failure (later marker (StrLit "value")))
            , ("set-env", \marker -> SetEnv failure (later marker (StrLit "value")))
            , ("map-elements", \marker -> Syntax.Map (mapFunction marker (Var "item")) (ListLit [IntLit 0, IntLit 1]))
            , ("filter-elements", \marker -> Filter (mapFunction marker (BoolLit True)) (ListLit [IntLit 0, IntLit 1]))
            , ("fold-elements", \marker -> Foldl (foldFunction marker) (IntLit 0) (ListLit [IntLit 0, IntLit 1]))
            ]
      forM_ cases $ \(label, makeExpression) -> do
        let marker = dir </> (label ++ ".marker")
        evalWithEnv Map.empty (makeExpression marker) `shouldReturn` Left expectedError
        doesFileExist marker `shouldReturn` False

  it "reports file existence for present and missing files" $ do
    withTempDir $ \dir -> do
      let present = dir </> "present.txt"
      writeFile present "ok"
      evalSource ("fileExists " ++ show present) `shouldReturn` Right (VBool True)
      evalSource ("fileExists " ++ show (dir </> "missing.txt")) `shouldReturn` Right (VBool False)

  it "lists directory contents as strings" $ do
    withTempDir $ \dir -> do
      writeFile (dir </> "a.txt") "a"
      writeFile (dir </> "b.txt") "b"
      result <- evalSource ("listDirectory " ++ show dir)
      case result of
        Right (VList values) ->
          sort [name | VStr name <- values] `shouldBe` ["a.txt", "b.txt"]
        Right other -> expectationFailure $ "Expected directory listing, got " ++ show other
        Left err -> expectationFailure $ "Runtime error: " ++ show err

  it "creates and removes directories" $ do
    withTempDir $ \dir -> do
      let created = dir </> "created"
      evalSource ("createDirectory " ++ show created) `shouldReturn` Right VUnit
      doesDirectoryExist created `shouldReturn` True
      evalSource ("removeDirectory " ++ show created) `shouldReturn` Right VUnit
      doesDirectoryExist created `shouldReturn` False

  it "gets and sets the current directory" $ do
    withTempDir $ \dir -> do
      originalDir <- getCurrentDirectory
      bracket_ (return ()) (setCurrentDirectory originalDir) $ do
        expectedDir <- canonicalizePath dir
        result <- evalSource ("setCurrentDirectory " ++ show dir ++ "; getCurrentDirectory")
        case result of
          Right (VStr actualDir) -> canonicalizePath actualDir `shouldReturn` expectedDir
          Right other -> expectationFailure $ "Expected current directory string, got " ++ show other
          Left err -> expectationFailure $ "Runtime error: " ++ show err

  it "sets and reads environment variables" $ do
    let name = "KAI_TEST_ENV_STD_LIB"
    original <- lookupEnv name
    bracket_
      (return ())
      (case original of
          Just value -> setEnv name value
          Nothing -> unsetEnv name)
      $ do
          evalSource ("setEnv " ++ show name ++ " \"configured\"; getEnv " ++ show name)
            `shouldReturn` Right (VJust (VStr "configured"))

  it "converts invalid environment names into Kai runtime errors" $ do
    let invalidEnvName = "KAI_INVALID=NAME"
    evalSource ("setEnv " ++ show invalidEnvName ++ " \"value\"")
      `shouldReturn` Left (TypeError $ "setEnv: could not set environment variable '" ++ invalidEnvName ++ "'")

  it "reports missing record fields consistently in IO evaluation" $ do
    evalWithEnv Map.empty (RecordAccess (RecordLit [("present", IntLit 1)]) "missing")
      `shouldReturn` Left (RecordFieldNotFound "missing")

  it "returns shell exit codes from system" $ do
    evalSource ("system " ++ show (commandWithExitCode 7)) `shouldReturn` Right (VInt 7)

  it "propagates exit as an explicit runtime control flow signal" $ do
    expr <- parseOrFail "exit 3"
    evalWithEnv Map.empty expr `shouldReturn` Left (ExitRequested 3)

  it "type checks the new builtins" $ do
    expr1 <- parseOrFail "listDirectory \".\""
    typeCheck expr1 `shouldBe` Right (TList TString)
    expr2 <- parseOrFail "getEnv \"HOME\""
    typeCheck expr2 `shouldBe` Right (TMaybe TString)
    expr3 <- parseOrFail "system \"echo hi\""
    typeCheck expr3 `shouldBe` Right TInt
    expr4 <- parseOrFail "if true then 1 else exit 2"
    typeCheck expr4 `shouldBe` Right TInt
