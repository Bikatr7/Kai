module TypeChecker.ControlFlow where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

type InferFunc = TypeEnv -> Expr -> TypeInfer (Substitution, Type)

inferControlFlow :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferControlFlow infer env (Print e) = do
  (s, _) <- infer env e
  return (s, TUnit)

inferControlFlow infer env (And e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TBool
  s4 <- lift $ unify (applySubst s3 t2) TBool
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TBool)

inferControlFlow infer env (Or e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TBool
  s4 <- lift $ unify (applySubst s3 t2) TBool
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TBool)

inferControlFlow infer env (Seq e1 e2) = do
  (s1, _) <- infer env e1
  (s2, t2) <- infer (applySubstEnv s1 env) e2
  let finalSubst = composeSubst s2 s1
  return (finalSubst, t2)

inferControlFlow infer env (Not e) = do
  (s1, t1) <- infer env e
  s2 <- lift $ unify t1 TBool
  return (composeSubst s2 s1, TBool)

inferControlFlow infer env (Eq e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) (applySubst s2 t2)
  let finalSubst = composeSubstList [s1, s2, s3]
  return (finalSubst, TBool)

inferControlFlow infer env (Lt e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TBool)

inferControlFlow infer env (Gt e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TBool)

inferControlFlow infer env (If c t e) = do
  (s1, tc) <- infer env c
  s2 <- lift $ unify tc TBool
  let s12 = composeSubst s2 s1
  (s3, tt) <- infer env t
  (s4, te) <- infer env e
  s5 <- lift $ unify (applySubst s4 tt) te
  let finalSubst = composeSubstList [s12, s3, s4, s5]
  return (finalSubst, applySubst s5 te)

inferControlFlow _ _ _ = error "inferControlFlow called on non-control-flow expression"
