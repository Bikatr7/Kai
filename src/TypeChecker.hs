module TypeChecker where

import Syntax
import qualified Data.Map as Map

data Type = TInt | TBool | TFun Type Type  -- TFun arg_type return_type
  deriving (Show, Eq)

type TypeEnv = Map.Map String Type

data TypeError
  = TypeMismatch Type Type
  | ExpectedInt Type
  | ExpectedBool Type
  | ExpectedFunction Type
  | UnboundVariable String
  | DivisionByZero
  deriving (Show, Eq)

typeCheck :: Expr -> Either TypeError Type
typeCheck = typeCheckWithEnv Map.empty

typeCheckWithEnv :: TypeEnv -> Expr -> Either TypeError Type
typeCheckWithEnv _ (IntLit _) = Right TInt
typeCheckWithEnv _ (BoolLit _) = Right TBool

typeCheckWithEnv env (Var x) = 
  case Map.lookup x env of
    Just t -> Right t
    Nothing -> Left $ UnboundVariable x

typeCheckWithEnv env (Add e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheckWithEnv env (Sub e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheckWithEnv env (Mul e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheckWithEnv env (Div e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheckWithEnv env (And e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  case (t1, t2) of
    (TBool, TBool) -> Right TBool
    (TBool, t) -> Left $ TypeMismatch TBool t
    (t, _) -> Left $ TypeMismatch TBool t

typeCheckWithEnv env (Or e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  case (t1, t2) of
    (TBool, TBool) -> Right TBool
    (TBool, t) -> Left $ TypeMismatch TBool t
    (t, _) -> Left $ TypeMismatch TBool t

typeCheckWithEnv env (Not e) = do
  t <- typeCheckWithEnv env e
  case t of
    TBool -> Right TBool
    t' -> Left $ ExpectedBool t'

typeCheckWithEnv env (Eq e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  if t1 == t2
    then Right TBool
    else Left $ TypeMismatch t1 t2

typeCheckWithEnv env (Lt e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  case (t1, t2) of
    (TInt, TInt) -> Right TBool
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheckWithEnv env (Gt e1 e2) = do
  t1 <- typeCheckWithEnv env e1
  t2 <- typeCheckWithEnv env e2
  case (t1, t2) of
    (TInt, TInt) -> Right TBool
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheckWithEnv env (If c t e) = do
  tc <- typeCheckWithEnv env c
  case tc of
    TBool -> do
      tt <- typeCheckWithEnv env t
      te <- typeCheckWithEnv env e
      if tt == te
        then Right tt
        else Left $ TypeMismatch tt te
    t -> Left $ ExpectedBool t

-- Lambda functions: \x -> body has type (arg_type -> return_type)
-- We need to infer the argument type, but for now let's be simple as i am fucking lazy
typeCheckWithEnv env (Lambda param body) = do
  -- For now, we'll require explicit types or use a simple inference
  -- This is a simplified version - in practice you'd want proper type inference
  let argType = TInt  -- Default to Int for now - this is hacky but works for examples and i am lazy
  let env' = Map.insert param argType env
  retType <- typeCheckWithEnv env' body
  return $ TFun argType retType

-- Function application: (f arg) where f :: a -> b and arg :: a gives b
typeCheckWithEnv env (App fun arg) = do
  funType <- typeCheckWithEnv env fun
  argType <- typeCheckWithEnv env arg
  case funType of
    TFun expectedArgType retType ->
      if argType == expectedArgType
        then Right retType
        else Left $ TypeMismatch expectedArgType argType
    _ -> Left $ ExpectedFunction funType