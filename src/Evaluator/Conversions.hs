module Evaluator.Conversions where

import Evaluator.Types
import Evaluator.Helpers
import Syntax

type EvalFunc = Env -> Expr -> Either RuntimeError Value

evalConversions :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalConversions eval env (ParseInt e) = do
  result <- eval env e
  case result of
    VStr s -> case parseIntString s of
      Just n -> Right $ VJust (VInt n)
      Nothing -> Right VNothing
    _ -> Left $ TypeError "parseInt requires string argument"
evalConversions eval env (ToString e) = do
  result <- eval env e
  case result of
    VInt n -> Right $ VStr (show n)
    _ -> Left $ TypeError "toString requires integer argument"
evalConversions eval env (Show e) = do
  result <- eval env e
  Right $ VStr (showValue result)
evalConversions eval env (MJust e) = do
  result <- eval env e
  Right $ VJust result
evalConversions _ _ MNothing = Right VNothing
evalConversions eval env (ELeft e) = do
  result <- eval env e
  Right $ VLeft result
evalConversions eval env (ERight e) = do
  result <- eval env e
  Right $ VRight result
evalConversions _ _ _ = error "evalConversions called on non-conversion expression"
