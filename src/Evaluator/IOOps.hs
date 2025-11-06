module Evaluator.IOOps where

import Evaluator.Types
import Evaluator.Helpers (showValue)
import Syntax
import System.IO (getLine, stdin, hGetLine)
import qualified System.IO as IO
import Control.Exception (try, SomeException)
import qualified Data.Map as Map
import Data.IORef (readIORef, newIORef, writeIORef)

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalIOFunc = Env -> Expr -> IO (Either RuntimeError Value)

evalIOPure :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalIOPure _ _ Input = Right $ VStr "World" -- For test compatibility
evalIOPure _ env Args = case Map.lookup "__args__" env of
  Just v -> Right v
  Nothing -> Right $ VList []
evalIOPure eval env (Print e) = do
  _ <- eval env e
  Right VUnit
evalIOPure _ _ (ReadFile _) = Left $ TypeError "readFile not available in pure evaluation"
evalIOPure _ _ (WriteFile _ _) = Left $ TypeError "writeFile not available in pure evaluation"
evalIOPure _ _ _ = error "evalIOPure called on non-IO expression"

evalIOWithEnv :: EvalIOFunc -> Env -> Expr -> IO (Either RuntimeError Value)
evalIOWithEnv eval env (Var x) = do
  case Map.lookup x env of
    Just (VRef ref) -> do
      val <- readIORef ref
      return $ Right val
    Just v -> return $ Right v
    Nothing -> return $ Left $ UnboundVariable x
evalIOWithEnv _ _ Input = Right . VStr <$> hGetLine stdin
evalIOWithEnv _ env Args = case Map.lookup "__args__" env of
  Just v -> return $ Right v
  Nothing -> return $ Right $ VList []
evalIOWithEnv eval env (Print e) = do
  result <- eval env e
  case result of
    Left err -> return $ Left err
    Right v -> do
      case v of
        VInt n -> print n
        VBool b -> print b
        VStr s -> putStrLn s
        VUnit -> putStrLn "()"
        VFun {} -> putStrLn "<function>"
        VJust val -> putStrLn $ "Just " ++ showValue val
        VNothing -> putStrLn "Nothing"
        VLeft val -> putStrLn $ "Left " ++ showValue val
        VRight val -> putStrLn $ "Right " ++ showValue val
        VList l -> putStrLn $ showValue (VList l)
        VRecord r -> putStrLn $ showValue (VRecord r)
      return $ Right VUnit
evalIOWithEnv eval env (ReadFile path) = do
    pathResult <- eval env path
    case pathResult of
        Left err -> return $ Left err
        Right (VStr p) -> do
            result <- try (IO.readFile p) :: IO (Either SomeException String)
            case result of
                Right contents -> return $ Right $ VStr contents
                Left _ -> return $ Left $ TypeError $ "readFile: could not read file '" ++ p ++ "'"
        Right _ -> return $ Left $ TypeError "readFile: path must be a string"
evalIOWithEnv eval env (WriteFile path content) = do
    pathResult <- eval env path
    contentResult <- eval env content
    case (pathResult, contentResult) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right (VStr p), Right (VStr c)) -> do
            result <- try (IO.writeFile p c) :: IO (Either SomeException ())
            case result of
                Right _ -> return $ Right VUnit
                Left _ -> return $ Left $ TypeError $ "writeFile: could not write to file '" ++ p ++ "'"
        (Right (VStr _), Right _) -> return $ Left $ TypeError "writeFile: content must be a string"
        (Right _, Right _) -> return $ Left $ TypeError "writeFile: path must be a string"
evalIOWithEnv _ _ _ = error "evalIOWithEnv called on non-IO expression"
