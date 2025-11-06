module Evaluator.Bindings where

import Evaluator.Types
import Syntax
import qualified Data.Map as Map
import Data.IORef

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalBindings :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalBindings eval env (Let var _maybeType val body) = do
  valValue <- eval env val
  let env' = if var == "_" then env else Map.insert var valValue env
  eval env' body
evalBindings eval env (LetRec var _maybeType val body) = do
  let testEnv = Map.insert var (VFun "_placeholder" (IntLit 0) env) env
  case eval testEnv val of
    Left err -> Left $ TypeError $ "LetRec definition failed: " ++ show err
    Right _ -> do
      let env' = Map.insert var recValue env
          recValue = case eval env' val of
                       Right v -> v
                       Left err -> VFun "_error" (IntLit 0) env
      eval env' body
evalBindings eval env (TypeAnnotation e _type) = eval env e
evalBindings _ _ _ = error "evalBindings called on non-binding expression"

evalBindingsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalBindingsIO eval env (Let var _maybeType val body) = do
  valResult <- eval env val
  case valResult of
    Left err -> return $ Left err
    Right valValue -> do
      let env' = if var == "_" then env else Map.insert var valValue env
      eval env' body
evalBindingsIO eval env (LetRec var _maybeType val body) = do
  let testEnv = Map.insert var (VFun "_placeholder" (IntLit 0) env) env
  testResult <- eval testEnv val
  case testResult of
    Left err -> return $ Left $ TypeError $ "LetRec definition failed: " ++ show err
    Right _ -> do
      recValueRef <- newIORef (VFun "_placeholder" (IntLit 0) env)
      let env' = Map.insert var (VRef recValueRef) env
      valResult <- eval env' val
      case valResult of
        Right recValue -> do
          writeIORef recValueRef recValue
          eval env' body
        Left err -> return $ Left err
evalBindingsIO eval env (TypeAnnotation e _type) = eval env e
evalBindingsIO _ _ _ = error "evalBindingsIO called on non-binding expression"
