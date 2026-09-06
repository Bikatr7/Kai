{-# LANGUAGE FlexibleContexts #-}
module Evaluator.Arithmetic where

import Evaluator.Types
import Control.Monad.Except (MonadError, throwError, liftEither)
import Evaluator.Helpers (evalInIO)
import Syntax

evalArithmetic :: MonadError RuntimeError m => Eval m -> Eval m
evalArithmetic eval env (Add e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> liftEither $ checkedInt (toInteger n1 + toInteger n2)
    _ -> throwError $ TypeError "Addition requires integer operands"
evalArithmetic eval env (Sub e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> liftEither $ checkedInt (toInteger n1 - toInteger n2)
    _ -> throwError $ TypeError "Subtraction requires integer operands"
evalArithmetic eval env (Mul e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> liftEither $ checkedInt (toInteger n1 * toInteger n2)
    _ -> throwError $ TypeError "Multiplication requires integer operands"
evalArithmetic eval env (Div e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt _, VInt 0) -> throwError DivByZero
    (VInt n1, VInt n2) -> liftEither $ checkedInt (toInteger n1 `div` toInteger n2)
    _ -> throwError $ TypeError "Division requires integer operands"
evalArithmetic eval env (Concat e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VStr s1, VStr s2) -> pure $ VStr (s1 ++ s2)
    (VList l1, VList l2) -> pure $ VList (l1 ++ l2)
    _ -> throwError $ TypeError "Concatenation requires string or list operands"
evalArithmetic _ _ _ = error "evalArithmetic called on non-arithmetic expression"

evalArithmeticIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalArithmeticIO = evalInIO evalArithmetic

checkedInt :: Integer -> Either RuntimeError Value
checkedInt result
  | isKaiInt result = Right $ VInt (fromInteger result)
  | otherwise = Left IntegerOverflow
