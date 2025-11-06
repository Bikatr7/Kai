module Evaluator.BooleanOps where

import Evaluator.Types
import Syntax

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalBooleanOps :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalBooleanOps eval env (And e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> Right $ VBool (b1 && b2)
    _ -> Left $ TypeError "AND requires boolean operands"
evalBooleanOps eval env (Or e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> Right $ VBool (b1 || b2)
    _ -> Left $ TypeError "OR requires boolean operands"
evalBooleanOps eval env (Not e) = do
  v <- eval env e
  case v of
    VBool b -> Right $ VBool (not b)
    _ -> Left $ TypeError "NOT requires a boolean operand"
evalBooleanOps eval env (Eq e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 == n2)
    (VBool b1, VBool b2) -> Right $ VBool (b1 == b2)
    (VStr s1, VStr s2) -> Right $ VBool (s1 == s2)
    (VUnit, VUnit) -> Right $ VBool True
    (VNothing, VNothing) -> Right $ VBool True
    (VJust v1', VJust v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
    (VLeft v1', VLeft v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
    (VRight v1', VRight v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
    (VList l1, VList l2) -> Right $ VBool (l1 == l2)
    (VRecord r1, VRecord r2) -> Right $ VBool (r1 == r2)
    (VTuple vs1, VTuple vs2) -> Right $ VBool (vs1 == vs2)
    _ -> Left $ TypeError "Equality comparison requires operands of the same type"
evalBooleanOps eval env (Lt e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 < n2)
    _ -> Left $ TypeError "Less than comparison requires integer operands"
evalBooleanOps eval env (Gt e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 > n2)
    _ -> Left $ TypeError "Greater than comparison requires integer operands"
evalBooleanOps eval env (If c t e) = do
  vc <- eval env c
  case vc of
    VBool True -> eval env t
    VBool False -> eval env e
    _ -> Left $ TypeError "If condition must be a boolean"
evalBooleanOps _ _ _ = error "evalBooleanOps called on non-boolean operation"

evalBooleanOpsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalBooleanOpsIO eval env (And e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ case (r1, r2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> case (v1, v2) of
      (VBool b1, VBool b2) -> Right $ VBool (b1 && b2)
      _ -> Left $ TypeError "AND requires boolean operands"
evalBooleanOpsIO eval env (Or e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ case (r1, r2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> case (v1, v2) of
      (VBool b1, VBool b2) -> Right $ VBool (b1 || b2)
      _ -> Left $ TypeError "OR requires boolean operands"
evalBooleanOpsIO eval env (Not e) = do
  r <- eval env e
  return $ case r of
    Left err -> Left err
    Right v -> case v of
      VBool b -> Right $ VBool (not b)
      _ -> Left $ TypeError "NOT requires a boolean operand"
evalBooleanOpsIO eval env (Eq e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ case (r1, r2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 == n2)
      (VBool b1, VBool b2) -> Right $ VBool (b1 == b2)
      (VStr s1, VStr s2) -> Right $ VBool (s1 == s2)
      (VUnit, VUnit) -> Right $ VBool True
      (VNothing, VNothing) -> Right $ VBool True
      (VJust v1', VJust v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
      (VLeft v1', VLeft v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
      (VRight v1', VRight v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
      (VList l1, VList l2) -> Right $ VBool (l1 == l2)
      (VRecord r1, VRecord r2) -> Right $ VBool (r1 == r2)
      (VTuple vs1, VTuple vs2) -> Right $ VBool (vs1 == vs2)
      _ -> Left $ TypeError "Equality comparison requires operands of the same type"
evalBooleanOpsIO eval env (Lt e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ case (r1, r2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 < n2)
      _ -> Left $ TypeError "Less than comparison requires integer operands"
evalBooleanOpsIO eval env (Gt e1 e2) = do
  r1 <- eval env e1
  r2 <- eval env e2
  return $ case (r1, r2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 > n2)
      _ -> Left $ TypeError "Greater than comparison requires integer operands"
evalBooleanOpsIO eval env (If c t e) = do
  rc <- eval env c
  case rc of
    Left err -> return $ Left err
    Right (VBool True) -> eval env t
    Right (VBool False) -> eval env e
    Right _ -> return $ Left $ TypeError "If condition must be a boolean"
evalBooleanOpsIO _ _ _ = error "evalBooleanOpsIO called on non-boolean operation"
