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
  | VConstructor String Int [Value]
  | VData String [Value]
  | VJust Value
  | VNothing
  | VLeft Value
  | VRight Value
  | VList [Value]
  | VRecord (Map.Map String Value)
  | VTuple [Value]
  | VRef (IORef Value)
  | VUninitialized String

instance Show Value where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VStr s) = show s
  show VUnit = "()"
  show (VFun param _ _) = "<function " ++ param ++ ">"
  show (VConstructor name _ []) = "<constructor " ++ name ++ ">"
  show (VConstructor name _ args) = "<constructor " ++ name ++ " " ++ show args ++ ">"
  show (VData name []) = name
  show (VData name values) = name ++ "(" ++ intercalate ", " (map show values) ++ ")"
  show (VJust v) = "Just (" ++ show v ++ ")"
  show VNothing = "Nothing"
  show (VLeft v) = "Left (" ++ show v ++ ")"
  show (VRight v) = "Right (" ++ show v ++ ")"
  show (VList vs) = show vs
  show (VRecord m) = show m
  show (VTuple vs) = "(" ++ intercalate ", " (map show vs) ++ ")"
  show (VUninitialized name) = "<uninitialized " ++ name ++ ">"
  show (VRef _) = "<ref>"

instance Eq Value where
  (VInt n1) == (VInt n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  (VStr s1) == (VStr s2) = s1 == s2
  VUnit == VUnit = True
  (VData n1 vs1) == (VData n2 vs2) = n1 == n2 && vs1 == vs2
  (VJust v1) == (VJust v2) = v1 == v2
  VNothing == VNothing = True
  (VLeft v1) == (VLeft v2) = v1 == v2
  (VRight v1) == (VRight v2) = v1 == v2
  (VList vs1) == (VList vs2) = vs1 == vs2
  (VRecord m1) == (VRecord m2) = m1 == m2
  (VTuple vs1) == (VTuple vs2) = vs1 == vs2
  (VRef _) == (VRef _) = False
  (VConstructor {}) == (VConstructor {}) = False
  (VFun {}) == (VFun {}) = False
  _ == _ = False

instance NFData Value where
  rnf (VInt n) = rnf n
  rnf (VBool b) = rnf b
  rnf (VStr s) = rnf s
  rnf VUnit = ()
  rnf (VConstructor name arity values) = rnf name `seq` rnf arity `seq` rnf values
  rnf (VData name values) = rnf name `seq` rnf values
  rnf (VJust v) = rnf v
  rnf VNothing = ()
  rnf (VLeft v) = rnf v
  rnf (VRight v) = rnf v
  rnf (VList vs) = rnf vs
  rnf (VRecord m) = rnf m
  rnf (VTuple vs) = rnf vs
  rnf (VUninitialized name) = rnf name
  rnf (VRef _) = ()  -- IORef can't be fully evaluated
  rnf (VFun _ _ _) = ()  -- Function can't be fully evaluated

type Env = Map.Map String Value

type Eval m = Env -> Expr -> m Value
type EvalFunc = Eval (Either RuntimeError)
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

data RuntimeError
  = DivByZero
  | IntegerOverflow
  | UninitializedRecursion String
  | TypeError String
  | UnboundVariable String
  | RecordFieldNotFound String
  | ExitRequested Int
  | EmptyListError String
  | EndOfInputError
  | IOFailure IOErrorKind String (Maybe String) String
  | UserFailure String
  | RuntimeAt SourceSpan RuntimeError
  | RuntimeContext String SourceSpan RuntimeError
  deriving (Show, Eq)

data IOErrorKind
  = NotFound | PermissionDenied | AlreadyExists | InvalidPath
  | InvalidEncoding | ResourceBusy | OtherIO
  deriving (Show, Eq, Enum, Bounded)

stripRuntimeLocation :: RuntimeError -> RuntimeError
stripRuntimeLocation (RuntimeAt _ failure) = stripRuntimeLocation failure
stripRuntimeLocation (RuntimeContext _ _ failure) = stripRuntimeLocation failure
stripRuntimeLocation failure = failure

locateRuntimeError :: SourceSpan -> Expr -> RuntimeError -> RuntimeError
locateRuntimeError _ _ failure@ExitRequested {} = failure
locateRuntimeError location expression failure = case failure of
  RuntimeAt {} -> context
  RuntimeContext {} -> context
  _ -> RuntimeAt location failure
  where
    context = case unlocatedExpr expression of
      App fun _ -> RuntimeContext ("while evaluating " ++ callableName fun) location failure
      _ -> failure
    callableName fun = case unlocatedExpr fun of
      Var name -> "call to " ++ name
      App inner _ -> callableName inner
      _ -> "function application"
