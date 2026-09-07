{-# LANGUAGE FlexibleContexts #-}
module Evaluator.Conversions where

import Evaluator.Types
import Control.Monad.Except (MonadError, throwError)
import Evaluator.Helpers
import Syntax

evalConversions :: MonadError RuntimeError m => Eval m -> Eval m
evalConversions eval env (ParseInt e) = do
  result <- eval env e
  case result of
    VStr s -> case parseIntString s of
      Just n -> pure $ VJust (VInt n)
      Nothing -> pure VNothing
    _ -> throwError $ TypeError "parseInt requires string argument"
evalConversions eval env (ToString e) = do
  result <- eval env e
  case result of
    VInt n -> pure $ VStr (show n)
    _ -> throwError $ TypeError "toString requires integer argument"
evalConversions eval env (Show e) = do
  result <- eval env e
  pure $ VStr (showValue result)
evalConversions eval env (Discard e) = do
  _ <- eval env e  -- Evaluate but ignore result
  pure VUnit
evalConversions eval env (MJust e) = do
  result <- eval env e
  pure $ VJust result
evalConversions _ _ MNothing = pure VNothing
evalConversions eval env (ELeft e) = do
  result <- eval env e
  pure $ VLeft result
evalConversions eval env (ERight e) = do
  result <- eval env e
  pure $ VRight result
evalConversions _ _ _ = error "evalConversions called on non-conversion expression"

evalConversionsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalConversionsIO = evalInIO evalConversions
