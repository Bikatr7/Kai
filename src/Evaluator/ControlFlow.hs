module Evaluator.ControlFlow where

import Evaluator.Types
import Syntax

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalControlFlow :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalControlFlow eval env (Seq e1 e2) = do
  _ <- eval env e1
  eval env e2
evalControlFlow _ _ _ = error "evalControlFlow called on non-control-flow expression"

evalControlFlowIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalControlFlowIO eval env (Seq e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ do
    _ <- r1
    r2
evalControlFlowIO _ _ _ = error "evalControlFlowIO called on non-control-flow expression"
