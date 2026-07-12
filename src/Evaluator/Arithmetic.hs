module Evaluator.Arithmetic where

import Evaluator.Types
import Evaluator.Helpers (bindResult)
import Syntax

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalArithmetic :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalArithmetic eval env (Add e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> checkedInt (toInteger n1 + toInteger n2)
    _ -> Left $ TypeError "Addition requires integer operands"
evalArithmetic eval env (Sub e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> checkedInt (toInteger n1 - toInteger n2)
    _ -> Left $ TypeError "Subtraction requires integer operands"
evalArithmetic eval env (Mul e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> checkedInt (toInteger n1 * toInteger n2)
    _ -> Left $ TypeError "Multiplication requires integer operands"
evalArithmetic eval env (Div e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt _, VInt 0) -> Left DivByZero
    (VInt n1, VInt n2) -> checkedInt (toInteger n1 `div` toInteger n2)
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
evalArithmeticIO eval env (Add e1 e2) =
  evalBinary e1 e2 $ \v1 v2 -> case (v1, v2) of
    (VInt n1, VInt n2) -> checkedInt (toInteger n1 + toInteger n2)
    _ -> Left $ TypeError "Addition requires integer operands"
  where evalBinary = evalBinaryIO eval env
evalArithmeticIO eval env (Sub e1 e2) =
  evalBinary e1 e2 $ \v1 v2 -> case (v1, v2) of
    (VInt n1, VInt n2) -> checkedInt (toInteger n1 - toInteger n2)
    _ -> Left $ TypeError "Subtraction requires integer operands"
  where evalBinary = evalBinaryIO eval env
evalArithmeticIO eval env (Mul e1 e2) =
  evalBinary e1 e2 $ \v1 v2 -> case (v1, v2) of
    (VInt n1, VInt n2) -> checkedInt (toInteger n1 * toInteger n2)
    _ -> Left $ TypeError "Multiplication requires integer operands"
  where evalBinary = evalBinaryIO eval env
evalArithmeticIO eval env (Div e1 e2) =
  evalBinary e1 e2 $ \v1 v2 -> case (v1, v2) of
    (VInt _, VInt 0) -> Left DivByZero
    (VInt n1, VInt n2) -> checkedInt (toInteger n1 `div` toInteger n2)
    _ -> Left $ TypeError "Division requires integer operands"
  where evalBinary = evalBinaryIO eval env
evalArithmeticIO eval env (Concat e1 e2) =
  evalBinary e1 e2 $ \v1 v2 -> case (v1, v2) of
    (VStr s1, VStr s2) -> Right $ VStr (s1 ++ s2)
    (VList l1, VList l2) -> Right $ VList (l1 ++ l2)
    _ -> Left $ TypeError "Concatenation requires string or list operands"
  where evalBinary = evalBinaryIO eval env
evalArithmeticIO _ _ _ = error "evalArithmeticIO called on non-arithmetic expression"

evalBinaryIO
  :: EvalFuncIO
  -> Env
  -> Expr
  -> Expr
  -> (Value -> Value -> Either RuntimeError Value)
  -> IO (Either RuntimeError Value)
evalBinaryIO eval env e1 e2 combine =
  bindResult (eval env e1) $ \v1 ->
    bindResult (eval env e2) $ \v2 ->
      return $ combine v1 v2

checkedInt :: Integer -> Either RuntimeError Value
checkedInt result
  | isKaiInt result = Right $ VInt (fromInteger result)
  | otherwise = Left IntegerOverflow
