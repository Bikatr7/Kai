module Syntax where

data Expr
  = IntLit Int            -- Integer literals
  | BoolLit Bool          -- Boolean literals
  | StrLit String         -- String literals
  | UnitLit               -- Unit literal ()
  | Var String            -- Variables
  | Add Expr Expr         -- Addition
  | Sub Expr Expr         -- Subtraction
  | Mul Expr Expr         -- Multiplication
  | Div Expr Expr         -- Division
  | Concat Expr Expr      -- String concatenation (++)
  | And Expr Expr         -- Logical AND
  | Or Expr Expr          -- Logical OR
  | Not Expr              -- Logical NOT
  | Eq Expr Expr          -- Equality
  | Lt Expr Expr          -- Less than
  | Gt Expr Expr          -- Greater than
  | If Expr Expr Expr     -- If-then-else
  | Print Expr            -- Print expression (prints at runtime, returns ())
  | Lambda String Expr    -- Lambda function (\x -> expr)
  | App Expr Expr         -- Function application
  | Let String Expr Expr  -- Let binding (let x = val in expr)
  | LetRec String Expr Expr -- Recursive let binding (letrec x = val in expr)
  deriving (Show, Eq) 
