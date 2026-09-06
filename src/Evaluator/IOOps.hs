module Evaluator.IOOps where

import Evaluator.Types
import Evaluator.Helpers (evalInIO, showValue)
import Control.Monad.Except (ExceptT(..), throwError)
import Syntax
import qualified System.IO as IO
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, removeDirectory, setCurrentDirectory)
import qualified System.Directory as Directory
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode(..))
import qualified System.Process as Process
import Control.Exception (IOException, try)
import qualified Data.Map as Map
import qualified UTF8

cliArgsEnv :: [String] -> Env
cliArgsEnv scriptArgs =
  let argValues = VList (map VStr scriptArgs)
  in Map.fromList [("__args__", argValues), ("args", argValues)]

lookupArgs :: Env -> Value
lookupArgs env =
  case Map.lookup "__args__" env of
    Just v -> v
    Nothing -> case Map.lookup "args" env of
      Just v -> v
      Nothing -> VList []

evalIOPure :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalIOPure _ _ Input = Left $ TypeError "input not available in pure evaluation"
evalIOPure _ env Args = Right $ lookupArgs env
evalIOPure eval env (Print e) = do
  _ <- eval env e
  Right VUnit
evalIOPure _ _ (ReadFile _) = Left $ TypeError "readFile not available in pure evaluation"
evalIOPure _ _ (WriteFile _ _) = Left $ TypeError "writeFile not available in pure evaluation"
evalIOPure _ _ (AppendFile _ _) = Left $ TypeError "appendFile not available in pure evaluation"
evalIOPure _ _ (FileExists _) = Left $ TypeError "fileExists not available in pure evaluation"
evalIOPure _ _ (ListDirectory _) = Left $ TypeError "listDirectory not available in pure evaluation"
evalIOPure _ _ (CreateDirectory _) = Left $ TypeError "createDirectory not available in pure evaluation"
evalIOPure _ _ (RemoveDirectory _) = Left $ TypeError "removeDirectory not available in pure evaluation"
evalIOPure _ _ GetCurrentDirectory = Left $ TypeError "getCurrentDirectory not available in pure evaluation"
evalIOPure _ _ (SetCurrentDirectory _) = Left $ TypeError "setCurrentDirectory not available in pure evaluation"
evalIOPure _ _ (System _) = Left $ TypeError "system not available in pure evaluation"
evalIOPure _ _ (GetEnv _) = Left $ TypeError "getEnv not available in pure evaluation"
evalIOPure _ _ (SetEnv _ _) = Left $ TypeError "setEnv not available in pure evaluation"
evalIOPure eval env (Exit e) = do
  result <- eval env e
  case result of
    VInt code -> Left $ ExitRequested code
    _ -> Left $ TypeError "exit: code must be an integer"
evalIOPure _ _ _ = error "evalIOPure called on non-IO expression"

-- All real I/O uses the same language-error adapter as ordinary operations.
evalIOWithEnv :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalIOWithEnv = evalInIO evalIO

evalIO :: Eval (ExceptT RuntimeError IO) -> Eval (ExceptT RuntimeError IO)
evalIO _ _ Input = attempt "input: could not read from stdin" VStr getLine
evalIO _ env Args = pure $ lookupArgs env
evalIO eval env (Print expression) = do
  value <- eval env expression
  attempt "print: could not write to stdout" (const VUnit) $
    putStrLn (showValue value) >> IO.hFlush IO.stdout
evalIO eval env (ReadFile path) =
  withString eval env path "readFile: path must be a string" $ \p ->
    attempt ("readFile: could not read file '" ++ p ++ "'") VStr (UTF8.readFile p)
evalIO eval env (WriteFile path content) =
  withStrings eval env path content "writeFile: path must be a string" "writeFile: content must be a string" $ \p c ->
    attempt ("writeFile: could not write to file '" ++ p ++ "'") (const VUnit) (UTF8.writeFile p c)
evalIO eval env (AppendFile path content) =
  withStrings eval env path content "appendFile: path must be a string" "appendFile: content must be a string" $ \p c ->
    attempt ("appendFile: could not append to file '" ++ p ++ "'") (const VUnit) (UTF8.appendFile p c)
evalIO eval env (FileExists path) =
  withString eval env path "fileExists: path must be a string" $ \p ->
    attempt ("fileExists: could not inspect path '" ++ p ++ "'") VBool (doesFileExist p)
evalIO eval env (ListDirectory path) =
  withString eval env path "listDirectory: path must be a string" $ \p ->
    attempt ("listDirectory: could not list directory '" ++ p ++ "'") (VList . map VStr) (Directory.listDirectory p)
evalIO eval env (CreateDirectory path) =
  withString eval env path "createDirectory: path must be a string" $ \p ->
    attempt ("createDirectory: could not create directory '" ++ p ++ "'") (const VUnit) (createDirectory p)
evalIO eval env (RemoveDirectory path) =
  withString eval env path "removeDirectory: path must be a string" $ \p ->
    attempt ("removeDirectory: could not remove directory '" ++ p ++ "'") (const VUnit) (removeDirectory p)
evalIO _ _ GetCurrentDirectory =
  attempt "getCurrentDirectory: could not get current directory" VStr getCurrentDirectory
evalIO eval env (SetCurrentDirectory path) =
  withString eval env path "setCurrentDirectory: path must be a string" $ \p ->
    attempt ("setCurrentDirectory: could not change directory to '" ++ p ++ "'") (const VUnit) (setCurrentDirectory p)
evalIO eval env (System commandExpr) =
  withString eval env commandExpr "system: command must be a string" $ \command ->
    attempt "system: could not execute command" exitValue (Process.system command)
  where
    exitValue ExitSuccess = VInt 0
    exitValue (ExitFailure code) = VInt code
evalIO eval env (GetEnv nameExpr) =
  withString eval env nameExpr "getEnv: name must be a string" $ \name ->
    attempt ("getEnv: could not read environment variable '" ++ name ++ "'") (maybe VNothing (VJust . VStr)) (lookupEnv name)
evalIO eval env (SetEnv nameExpr valueExpr) =
  withStrings eval env nameExpr valueExpr "setEnv: name must be a string" "setEnv: value must be a string" $ \name value ->
    attempt ("setEnv: could not set environment variable '" ++ name ++ "'") (const VUnit) (setEnv name value)
evalIO eval env (Exit codeExpr) = do
  value <- eval env codeExpr
  case value of
    VInt code -> throwError $ ExitRequested code
    _ -> throwError $ TypeError "exit: code must be an integer"
evalIO _ _ _ = error "evalIO called on non-IO expression"

attempt :: String -> (a -> Value) -> IO a -> ExceptT RuntimeError IO Value
attempt message wrap action = ExceptT $ do
  result <- tryIO action
  pure $ either (const $ Left $ TypeError message) (Right . wrap) result
  where
    tryIO :: IO a -> IO (Either IOException a)
    tryIO = try

withString :: Eval (ExceptT RuntimeError IO) -> Env -> Expr -> String -> (String -> ExceptT RuntimeError IO Value) -> ExceptT RuntimeError IO Value
withString eval env expression message next = do
  value <- eval env expression
  case value of
    VStr text -> next text
    _ -> throwError $ TypeError message

-- Evaluate both operands before validating their types, matching strict application.
withStrings :: Eval (ExceptT RuntimeError IO) -> Env -> Expr -> Expr -> String -> String -> (String -> String -> ExceptT RuntimeError IO Value) -> ExceptT RuntimeError IO Value
withStrings eval env first second firstError secondError next = do
  firstValue <- eval env first
  secondValue <- eval env second
  case (firstValue, secondValue) of
    (VStr a, VStr b) -> next a b
    (VStr _, _) -> throwError $ TypeError secondError
    _ -> throwError $ TypeError firstError
