module Evaluator.ControlFlow where

import Evaluator.Types
import Evaluator.Helpers (bindResult)
import Syntax

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalControlFlow :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalControlFlow eval env (Seq e1 e2) = do
  _ <- eval env e1
  eval env e2
evalControlFlow _ _ _ = error "evalControlFlow called on non-control-flow expression"

evalControlFlowIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalControlFlowIO eval env (Seq e1 e2) =
  bindResult (eval env e1) $ \_ -> eval env e2
evalControlFlowIO _ _ _ = error "evalControlFlowIO called on non-control-flow expression"
