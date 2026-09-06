module Evaluator.Bindings where

import Evaluator.Types
import Data.Either (fromRight)
import Syntax
import qualified Data.Map as Map
import Evaluator.Recursion (initializeRecursiveBindings)
import Evaluator.Helpers (bindResult)


evalBindings :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalBindings eval env (Let var _maybeType val body) = do
  valValue <- eval env val
  let env' = if var == "_" then env else Map.insert var valValue env
  eval env' body
evalBindings eval env (LetRec var _maybeType val body) = do
  _ <- eval (Map.insert var (VUninitialized var) env) val
  let env' = Map.insert var recValue env
      recResult = eval env' val
      recValue = fromRight (VUninitialized var) recResult
  _ <- recResult
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
  bindResult (initializeRecursiveBindings eval env [(var, val)]) $ \recursive ->
    eval recursive body
evalBindingsIO eval env (TypeAnnotation e _type) = eval env e
evalBindingsIO _ _ _ = error "evalBindingsIO called on non-binding expression"
