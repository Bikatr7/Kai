module TypeChecker.Inference where

import qualified Data.Map as Map
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Literals
import TypeChecker.Arithmetic
import TypeChecker.ControlFlow
import TypeChecker.Functions
import TypeChecker.Bindings
import TypeChecker.DataStructures
import TypeChecker.Operations

infer :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
infer env expr = case expr of
  IntLit _ -> inferLiteral env expr
  BoolLit _ -> inferLiteral env expr
  StrLit _ -> inferLiteral env expr
  UnitLit -> inferLiteral env expr
  Input -> inferLiteral env expr
  Args -> inferLiteral env expr
  Var x -> inferVariable env expr

  Add _ _ -> inferArithmetic infer env expr
  Sub _ _ -> inferArithmetic infer env expr
  Mul _ _ -> inferArithmetic infer env expr
  Div _ _ -> inferArithmetic infer env expr
  Concat _ _ -> inferArithmetic infer env expr

  Print _ -> inferControlFlow infer env expr
  And _ _ -> inferControlFlow infer env expr
  Or _ _ -> inferControlFlow infer env expr
  Seq _ _ -> inferControlFlow infer env expr
  Not _ -> inferControlFlow infer env expr
  Eq _ _ -> inferControlFlow infer env expr
  Lt _ _ -> inferControlFlow infer env expr
  Gt _ _ -> inferControlFlow infer env expr
  If _ _ _ -> inferControlFlow infer env expr

  Lambda _ _ _ -> inferFunctions infer env expr
  App _ _ -> inferFunctions infer env expr

  Let _ _ _ _ -> inferBindings infer env expr
  LetRec _ _ _ _ -> inferBindings infer env expr
  TypeAnnotation _ _ -> inferBindings infer env expr

  ListLit _ -> inferDataStructures infer env expr
  Cons _ _ -> inferDataStructures infer env expr
  Head _ -> inferDataStructures infer env expr
  Tail _ -> inferDataStructures infer env expr
  Null _ -> inferDataStructures infer env expr
  RecordLit _ -> inferDataStructures infer env expr
  RecordAccess _ _ -> inferDataStructures infer env expr
  TupleLit _ -> inferDataStructures infer env expr
  Fst _ -> inferDataStructures infer env expr
  Snd _ -> inferDataStructures infer env expr

  _ -> inferOperations infer env expr
