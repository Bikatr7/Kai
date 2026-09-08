module TypeChecker.Inference where

import Syntax (Expr(..), unlocatedExpr)
import TypeChecker.Types
import TypeChecker.Literals
import TypeChecker.Arithmetic
import TypeChecker.ControlFlow
import TypeChecker.Functions
import TypeChecker.Bindings
import TypeChecker.DataStructures
import TypeChecker.Operations
import TypeChecker.Substitution (applySubst, capturePredicates, addPredicates)
import Control.Monad.State (gets, modify)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map

infer :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
infer env expr = do
  result@(subst,ty) <- inferCore env expr
  lift $ mapM_ (validateRows . applySubst subst) (ty : Map.elems subst)
  pure result

inferCore :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferCore env expr = case expr of
  Located location expression -> do
    previous <- gets inferredWarnings
    modify $ \state -> state { inferredWarnings = [] }
    (result,predicates) <- capturePredicates $ inferCore env expression
      `catchError` (throwError . locateTypeError location)
    warnings <- gets inferredWarnings
    modify $ \state -> state { inferredWarnings = previous ++ map (locateWarning location) warnings }
    let origin predicate = case (unlocatedExpr expression,predicate) of
          (Var _,PredicateAt _ underlying) -> PredicateAt location underlying
          (_,PredicateAt {}) -> predicate
          _ -> PredicateAt location predicate
    addPredicates (map origin predicates)
    pure result
  IntLit _ -> inferLiteral env expr
  BoolLit _ -> inferLiteral env expr
  StrLit _ -> inferLiteral env expr
  UnitLit -> inferLiteral env expr
  Input -> inferLiteral env expr
  Args -> inferLiteral env expr
  GetCurrentDirectory -> inferLiteral env expr
  Var _ -> inferVariable env expr

  Add _ _ -> inferArithmetic inferCore env expr
  Sub _ _ -> inferArithmetic inferCore env expr
  Mul _ _ -> inferArithmetic inferCore env expr
  Div _ _ -> inferArithmetic inferCore env expr
  Concat _ _ -> inferArithmetic inferCore env expr

  Print _ -> inferControlFlow inferCore env expr
  And _ _ -> inferControlFlow inferCore env expr
  Or _ _ -> inferControlFlow inferCore env expr
  Seq _ _ -> inferControlFlow inferCore env expr
  Not _ -> inferControlFlow inferCore env expr
  Eq _ _ -> inferControlFlow inferCore env expr
  Lt _ _ -> inferControlFlow inferCore env expr
  Gt _ _ -> inferControlFlow inferCore env expr
  If _ _ _ -> inferControlFlow inferCore env expr

  Lambda _ _ _ -> inferFunctions inferCore env expr
  App _ _ -> inferFunctions inferCore env expr
  Fix _ -> inferFunctions inferCore env expr

  Let _ _ _ _ -> inferBindings inferCore env expr
  LetRec _ _ _ _ -> inferBindings inferCore env expr
  TypeAnnotation _ _ -> inferBindings inferCore env expr

  ListLit _ -> inferDataStructures inferCore env expr
  Cons _ _ -> inferDataStructures inferCore env expr
  Head _ -> inferDataStructures inferCore env expr
  Tail _ -> inferDataStructures inferCore env expr
  Null _ -> inferDataStructures inferCore env expr
  RecordLit _ -> inferDataStructures inferCore env expr
  RecordAccess _ _ -> inferDataStructures inferCore env expr
  TupleLit _ -> inferDataStructures inferCore env expr
  Fst _ -> inferDataStructures inferCore env expr
  Snd _ -> inferDataStructures inferCore env expr

  _ -> inferOperations inferCore env expr
