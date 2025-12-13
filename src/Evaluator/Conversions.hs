module Evaluator.Conversions where

import Evaluator.Types
import Evaluator.Helpers
import Syntax

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

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
evalConversions eval env (Discard e) = do
  _ <- eval env e  -- Evaluate but ignore result
  Right VUnit
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

evalConversionsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalConversionsIO eval env (ParseInt e) = do
  result <- eval env e
  case result of
    Left err -> return $ Left err
    Right (VStr s) -> case parseIntString s of
      Just n -> return $ Right $ VJust (VInt n)
      Nothing -> return $ Right VNothing
    Right _ -> return $ Left $ TypeError "parseInt requires string argument"
evalConversionsIO eval env (ToString e) = do
  result <- eval env e
  case result of
    Left err -> return $ Left err
    Right (VInt n) -> return $ Right $ VStr (show n)
    Right _ -> return $ Left $ TypeError "toString requires integer argument"
evalConversionsIO eval env (Show e) = do
  result <- eval env e
  case result of
    Left err -> return $ Left err
    Right val -> return $ Right $ VStr (showValue val)
evalConversionsIO eval env (Discard e) = do
  result <- eval env e
  case result of
    Left err -> return $ Left err
    Right _ -> return $ Right VUnit
evalConversionsIO eval env (MJust e) = do
  result <- eval env e
  case result of
    Left err -> return $ Left err
    Right val -> return $ Right $ VJust val
evalConversionsIO _ _ MNothing = return $ Right VNothing
evalConversionsIO eval env (ELeft e) = do
  result <- eval env e
  case result of
    Left err -> return $ Left err
    Right val -> return $ Right $ VLeft val
evalConversionsIO eval env (ERight e) = do
  result <- eval env e
  case result of
    Left err -> return $ Left err
    Right val -> return $ Right $ VRight val
evalConversionsIO _ _ _ = error "evalConversionsIO called on non-conversion expression"
