{-# LANGUAGE FlexibleContexts #-}
module Evaluator.ControlFlow where

import Evaluator.Types
import Control.Monad.Except (MonadError)
import Evaluator.Helpers (evalInIO)
import Syntax

evalControlFlow :: MonadError RuntimeError m => Eval m -> Eval m
evalControlFlow eval env (Seq e1 e2) = do
  _ <- eval env e1
  eval env e2
evalControlFlow _ _ _ = error "evalControlFlow called on non-control-flow expression"

evalControlFlowIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalControlFlowIO = evalInIO evalControlFlow
