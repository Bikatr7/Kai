module TypeChecker where

import Syntax

-- Kai types, not gonna pretend I know what I'm doing here
data Type = TInt | TBool
  deriving (Show, Eq)

-- Type checking errors (allegedly)
data TypeError
  = TypeMismatch Type Type
  | ExpectedInt Type
  | ExpectedBool Type
  | DivisionByZero
  deriving (Show, Eq)

-- Type check an expression
typeCheck :: Expr -> Either TypeError Type
typeCheck (IntLit _) = Right TInt
typeCheck (BoolLit _) = Right TBool

typeCheck (Add e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheck (Sub e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheck (Mul e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheck (Div e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  case (t1, t2) of
    (TInt, TInt) -> Right TInt
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheck (And e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  case (t1, t2) of
    (TBool, TBool) -> Right TBool
    (TBool, t) -> Left $ TypeMismatch TBool t
    (t, _) -> Left $ TypeMismatch TBool t

typeCheck (Or e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  case (t1, t2) of
    (TBool, TBool) -> Right TBool
    (TBool, t) -> Left $ TypeMismatch TBool t
    (t, _) -> Left $ TypeMismatch TBool t

typeCheck (Not e) = do
  t <- typeCheck e
  case t of
    TBool -> Right TBool
    t' -> Left $ ExpectedBool t'

typeCheck (Eq e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  if t1 == t2
    then Right TBool
    else Left $ TypeMismatch t1 t2

typeCheck (Lt e1 e2) = do
  t1 <- typeCheck e1
  t2 <- typeCheck e2
  case (t1, t2) of
    (TInt, TInt) -> Right TBool
    (TInt, t) -> Left $ TypeMismatch TInt t
    (t, _) -> Left $ TypeMismatch TInt t

typeCheck (If c t e) = do
  tc <- typeCheck c
  case tc of
    TBool -> do
      tt <- typeCheck t
      te <- typeCheck e
      if tt == te
        then Right tt
        else Left $ TypeMismatch tt te
    t -> Left $ ExpectedBool t 