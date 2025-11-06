module Evaluator.Literals where

import Evaluator.Types
import Syntax

evalLiteral :: Expr -> Either RuntimeError Value
evalLiteral (IntLit n) = Right $ VInt n
evalLiteral (BoolLit b) = Right $ VBool b
evalLiteral (StrLit s) = Right $ VStr s
evalLiteral UnitLit = Right VUnit
evalLiteral _ = error "evalLiteral called on non-literal expression"
