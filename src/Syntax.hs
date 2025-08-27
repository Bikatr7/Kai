module Syntax where

data Expr
  = IntLit Int            -- Integer literals
  | BoolLit Bool          -- Boolean literals
  | Add Expr Expr         -- Addition
  | Sub Expr Expr         -- Subtraction
  | Mul Expr Expr         -- Multiplication
  | Div Expr Expr         -- Division
  | And Expr Expr         -- Logical AND
  | Or Expr Expr          -- Logical OR
  | Not Expr              -- Logical NOT
  | Eq Expr Expr          -- Equality
  | Lt Expr Expr          -- Less than
  | Gt Expr Expr          -- Greater than
  | If Expr Expr Expr     -- If-then-else
  deriving (Show, Eq) 