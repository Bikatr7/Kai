module Evaluator where

import Syntax
import TypeChecker

data Value
  = VInt Int
  | VBool Bool
  deriving (Show, Eq)

data RuntimeError
  = DivByZero
  | TypeError String
  deriving (Show, Eq)

eval :: Expr -> Either RuntimeError Value
eval (IntLit n) = Right $ VInt n
eval (BoolLit b) = Right $ VBool b

eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
    _ -> Left $ TypeError "Addition requires integer operands"

eval (Sub e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
    _ -> Left $ TypeError "Subtraction requires integer operands"

eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 * n2)
    _ -> Left $ TypeError "Multiplication requires integer operands"

eval (Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt _, VInt 0) -> Left DivByZero
    (VInt n1, VInt n2) -> Right $ VInt (n1 `div` n2)
    _ -> Left $ TypeError "Division requires integer operands"

eval (And e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> Right $ VBool (b1 && b2)
    _ -> Left $ TypeError "AND requires boolean operands"

eval (Or e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> Right $ VBool (b1 || b2)
    _ -> Left $ TypeError "OR requires boolean operands"

eval (Not e) = do
  v <- eval e
  case v of
    VBool b -> Right $ VBool (not b)
    _ -> Left $ TypeError "NOT requires a boolean operand"

eval (Eq e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 == n2)
    (VBool b1, VBool b2) -> Right $ VBool (b1 == b2)
    _ -> Left $ TypeError "Equality comparison requires operands of the same type"

eval (Lt e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 < n2)
    _ -> Left $ TypeError "Less than comparison requires integer operands"

eval (Gt e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 > n2)
    _ -> Left $ TypeError "Greater than comparison requires integer operands"

eval (If c t e) = do
  vc <- eval c
  case vc of
    VBool True -> eval t
    VBool False -> eval e
    _ -> Left $ TypeError "If condition must be a boolean" 