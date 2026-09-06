module TypeChecker.Helpers (inferTwo, inferThree, inferUnary, inferBinary) where

import Control.Monad.Trans (lift)
import Syntax (Expr)
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

inferTwo :: InferFunc -> TypeEnv -> Expr -> Expr -> TypeInfer (Substitution, Type, Type)
inferTwo infer env e1 e2 = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applySubstEnv s1 env) e2
  let combinedSubst = composeSubst s2 s1
  return (combinedSubst, applySubst s2 t1, t2)

inferThree :: InferFunc -> TypeEnv -> Expr -> Expr -> Expr -> TypeInfer (Substitution, Type, Type, Type)
inferThree infer env e1 e2 e3 = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applySubstEnv s1 env) e2
  let s12 = composeSubst s2 s1
  (s3, t3) <- infer (applySubstEnv s12 env) e3
  let s123 = composeSubst s3 s12
  return (s123, applySubst s3 (applySubst s2 t1), applySubst s3 t2, t3)

-- Preserve source-order inference and carry each constraint into later operands.
inferUnary :: InferFunc -> TypeEnv -> Expr -> Type -> Type -> TypeInfer (Substitution, Type)
inferUnary infer env expression argument result = do
  (subst, actual) <- infer env expression
  constraint <- lift $ unify (applySubst subst actual) argument
  return (composeSubst constraint subst, result)

inferBinary :: InferFunc -> TypeEnv -> Expr -> Expr -> Type -> Type -> Type -> TypeInfer (Substitution, Type)
inferBinary infer env first second firstType secondType result = do
  (subst, actualFirst, actualSecond) <- inferTwo infer env first second
  firstConstraint <- lift $ unify actualFirst firstType
  secondConstraint <- lift $ unify (applySubst firstConstraint actualSecond) secondType
  return (composeSubst secondConstraint (composeSubst firstConstraint subst), result)
