module Evaluator.Types where

import Syntax
import qualified Data.Map as Map
import Data.IORef
import Data.List (intercalate)
import Control.DeepSeq

data Value
  = VInt Int
  | VBool Bool
  | VStr String
  | VUnit
  | VFun String Expr Env
  | VJust Value
  | VNothing
  | VLeft Value
  | VRight Value
  | VList [Value]
  | VRecord (Map.Map String Value)
  | VTuple [Value]
  | VRef (IORef Value)

instance Show Value where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VStr s) = show s
  show VUnit = "()"
  show (VFun param body env) = "<function " ++ param ++ ">"
  show (VJust v) = "Just (" ++ show v ++ ")"
  show VNothing = "Nothing"
  show (VLeft v) = "Left (" ++ show v ++ ")"
  show (VRight v) = "Right (" ++ show v ++ ")"
  show (VList vs) = show vs
  show (VRecord m) = show m
  show (VTuple vs) = "(" ++ intercalate ", " (map show vs) ++ ")"
  show (VRef _) = "<ref>"

instance Eq Value where
  (VInt n1) == (VInt n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  (VStr s1) == (VStr s2) = s1 == s2
  VUnit == VUnit = True
  (VJust v1) == (VJust v2) = v1 == v2
  VNothing == VNothing = True
  (VLeft v1) == (VLeft v2) = v1 == v2
  (VRight v1) == (VRight v2) = v1 == v2
  (VList vs1) == (VList vs2) = vs1 == vs2
  (VRecord m1) == (VRecord m2) = m1 == m2
  (VTuple vs1) == (VTuple vs2) = vs1 == vs2
  (VRef _) == (VRef _) = False
  (VFun {}) == (VFun {}) = False
  _ == _ = False

instance NFData Value where
  rnf (VInt n) = rnf n
  rnf (VBool b) = rnf b
  rnf (VStr s) = rnf s
  rnf VUnit = ()
  rnf (VJust v) = rnf v
  rnf VNothing = ()
  rnf (VLeft v) = rnf v
  rnf (VRight v) = rnf v
  rnf (VList vs) = rnf vs
  rnf (VRecord m) = rnf m
  rnf (VTuple vs) = rnf vs
  rnf (VRef _) = ()  -- IORef can't be fully evaluated
  rnf (VFun _ _ _) = ()  -- Function can't be fully evaluated

type Env = Map.Map String Value

data RuntimeError
  = DivByZero
  | TypeError String
  | UnboundVariable String
  | RecordFieldNotFound String
  deriving (Show, Eq)
