module Evaluator.BooleanOps where

import Evaluator.Types
import Evaluator.Helpers (bindResult)
import Syntax
import qualified Data.Map as Map

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
  VBool <$> valuesEqual v1 v2
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
evalBooleanOpsIO eval env (And e1 e2) =
  evalBooleanBinaryIO eval env e1 e2 $ \v1 v2 -> case (v1, v2) of
      (VBool b1, VBool b2) -> Right $ VBool (b1 && b2)
      _ -> Left $ TypeError "AND requires boolean operands"
evalBooleanOpsIO eval env (Or e1 e2) =
  evalBooleanBinaryIO eval env e1 e2 $ \v1 v2 -> case (v1, v2) of
      (VBool b1, VBool b2) -> Right $ VBool (b1 || b2)
      _ -> Left $ TypeError "OR requires boolean operands"
evalBooleanOpsIO eval env (Not e) = do
  r <- eval env e
  return $ case r of
    Left err -> Left err
    Right v -> case v of
      VBool b -> Right $ VBool (not b)
      _ -> Left $ TypeError "NOT requires a boolean operand"
evalBooleanOpsIO eval env (Eq e1 e2) =
  evalBooleanBinaryIO eval env e1 e2 $ \v1 v2 -> VBool <$> valuesEqual v1 v2
evalBooleanOpsIO eval env (Lt e1 e2) =
  evalBooleanBinaryIO eval env e1 e2 $ \v1 v2 -> case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 < n2)
      _ -> Left $ TypeError "Less than comparison requires integer operands"
evalBooleanOpsIO eval env (Gt e1 e2) =
  evalBooleanBinaryIO eval env e1 e2 $ \v1 v2 -> case (v1, v2) of
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

evalBooleanBinaryIO
  :: EvalFuncIO
  -> Env
  -> Expr
  -> Expr
  -> (Value -> Value -> Either RuntimeError Value)
  -> IO (Either RuntimeError Value)
evalBooleanBinaryIO eval env e1 e2 combine =
  bindResult (eval env e1) $ \v1 ->
    bindResult (eval env e2) $ \v2 ->
      return $ combine v1 v2

valuesEqual :: Value -> Value -> Either RuntimeError Bool
valuesEqual left right = do
  ensureComparableValue left
  ensureComparableValue right
  compareComparableValues left right

ensureComparableValue :: Value -> Either RuntimeError ()
ensureComparableValue value = case value of
  VFun {} -> Left unsupportedEqualityError
  VConstructor {} -> Left unsupportedEqualityError
  VRef _ -> Left unsupportedEqualityError
  VData _ values -> mapM_ ensureComparableValue values
  VJust inner -> ensureComparableValue inner
  VNothing -> Right ()
  VLeft inner -> ensureComparableValue inner
  VRight inner -> ensureComparableValue inner
  VList values -> mapM_ ensureComparableValue values
  VRecord fields -> mapM_ ensureComparableValue (Map.elems fields)
  VTuple values -> mapM_ ensureComparableValue values
  _ -> Right ()

compareComparableValues :: Value -> Value -> Either RuntimeError Bool
compareComparableValues left right = case (left, right) of
  (VInt n1, VInt n2) -> Right $ n1 == n2
  (VBool b1, VBool b2) -> Right $ b1 == b2
  (VStr s1, VStr s2) -> Right $ s1 == s2
  (VUnit, VUnit) -> Right True
  (VData name1 values1, VData name2 values2)
    | name1 /= name2 -> Right False
    | otherwise -> compareValueLists values1 values2
  (VNothing, VNothing) -> Right True
  (VNothing, VJust _) -> Right False
  (VJust _, VNothing) -> Right False
  (VJust value1, VJust value2) -> compareComparableValues value1 value2
  (VLeft _, VRight _) -> Right False
  (VRight _, VLeft _) -> Right False
  (VLeft value1, VLeft value2) -> compareComparableValues value1 value2
  (VRight value1, VRight value2) -> compareComparableValues value1 value2
  (VList values1, VList values2) -> compareValueLists values1 values2
  (VRecord fields1, VRecord fields2)
    | Map.keys fields1 /= Map.keys fields2 -> Right False
    | otherwise -> compareValueLists (Map.elems fields1) (Map.elems fields2)
  (VTuple values1, VTuple values2) -> compareValueLists values1 values2
  (VFun {}, _) -> Left unsupportedEqualityError
  (_, VFun {}) -> Left unsupportedEqualityError
  (VConstructor {}, _) -> Left unsupportedEqualityError
  (_, VConstructor {}) -> Left unsupportedEqualityError
  (VRef _, _) -> Left unsupportedEqualityError
  (_, VRef _) -> Left unsupportedEqualityError
  _ -> Left $ TypeError "Equality comparison requires operands of the same type"

compareValueLists :: [Value] -> [Value] -> Either RuntimeError Bool
compareValueLists [] [] = Right True
compareValueLists (left:lefts) (right:rights) = do
  equal <- compareComparableValues left right
  if equal then compareValueLists lefts rights else Right False
compareValueLists _ _ = Right False

unsupportedEqualityError :: RuntimeError
unsupportedEqualityError =
  TypeError "Equality is not defined for callable or recursive reference values"
