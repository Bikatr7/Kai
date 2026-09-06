module TypeChecker.Arithmetic where

import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Syntax (Expr(..))
import TypeChecker.Helpers (inferBinary)
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

inferArithmetic :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferArithmetic infer env (Add e1 e2) =
  inferBinary infer env e1 e2 TInt TInt TInt

inferArithmetic infer env (Sub e1 e2) =
  inferBinary infer env e1 e2 TInt TInt TInt

inferArithmetic infer env (Mul e1 e2) =
  inferBinary infer env e1 e2 TInt TInt TInt

inferArithmetic infer env (Div e1 e2) =
  inferBinary infer env e1 e2 TInt TInt TInt

inferArithmetic infer env (Concat e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applySubstEnv s1 env) e2
  s3 <- lift $ unify (applySubst s2 t1) (applySubst s2 t2)
  let finalSubst = composeSubst s3 (composeSubst s2 s1)
  case applySubst finalSubst t1 of
    TString -> return (finalSubst, TString)
    TList _ -> return (finalSubst, applySubst finalSubst t1)
    finalType -> lift $ throwError $ UnificationError finalType (applySubst finalSubst t2)

inferArithmetic _ _ _ = error "inferArithmetic called on non-arithmetic expression"
