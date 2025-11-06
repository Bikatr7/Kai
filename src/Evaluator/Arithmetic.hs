module Evaluator.Arithmetic where

import Evaluator.Types
import Syntax

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalArithmetic :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalArithmetic eval env (Add e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
    _ -> Left $ TypeError "Addition requires integer operands"
evalArithmetic eval env (Sub e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
    _ -> Left $ TypeError "Subtraction requires integer operands"
evalArithmetic eval env (Mul e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 * n2)
    _ -> Left $ TypeError "Multiplication requires integer operands"
evalArithmetic eval env (Div e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt _, VInt 0) -> Left DivByZero
    (VInt n1, VInt n2) -> Right $ VInt (n1 `div` n2)
    _ -> Left $ TypeError "Division requires integer operands"
evalArithmetic eval env (Concat e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VStr s1, VStr s2) -> Right $ VStr (s1 ++ s2)
    (VList l1, VList l2) -> Right $ VList (l1 ++ l2)
    _ -> Left $ TypeError "Concatenation requires string or list operands"
evalArithmetic _ _ _ = error "evalArithmetic called on non-arithmetic expression"

evalArithmeticIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalArithmeticIO eval env (Add e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
      _ -> Left $ TypeError "Addition requires integer operands"
evalArithmeticIO eval env (Sub e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
      _ -> Left $ TypeError "Subtraction requires integer operands"
evalArithmeticIO eval env (Mul e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 * n2)
      _ -> Left $ TypeError "Multiplication requires integer operands"
evalArithmeticIO eval env (Div e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt _, VInt 0) -> Left DivByZero
      (VInt n1, VInt n2) -> Right $ VInt (n1 `div` n2)
      _ -> Left $ TypeError "Division requires integer operands"
evalArithmeticIO eval env (Concat e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VStr s1, VStr s2) -> Right $ VStr (s1 ++ s2)
      (VList l1, VList l2) -> Right $ VList (l1 ++ l2)
      _ -> Left $ TypeError "Concatenation requires string or list operands"
evalArithmeticIO _ _ _ = error "evalArithmeticIO called on non-arithmetic expression"
