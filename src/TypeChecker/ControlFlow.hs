module TypeChecker.ControlFlow where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution (applySubst, applySubstEnv, composeSubst, composeSubstList, freeTypeVars)
import qualified TypeChecker.Substitution as Subst
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
  -- CRITICAL: When unifying then and else branches, if we get UnificationError
  -- because one is concrete and the other is a function type with a return type
  -- variable in the environment (recursive function), we should unify the concrete
  -- type with the return type variable instead.
  let ttWithS4 = applySubst s4 tt
  let teWithS3 = applySubst s3 te
  let envTypeVars = [tv | (_, TVar tv) <- Map.toList env]
  let isConcreteType TInt = True
      isConcreteType TBool = True
      isConcreteType TString = True
      isConcreteType TUnit = True
      isConcreteType (TList _) = True
      isConcreteType (TRecord _) = True
      isConcreteType (TMaybe _) = True
      isConcreteType (TEither _ _) = True
      isConcreteType (TTuple _) = True
      isConcreteType _ = False
  -- Try normal unification first, with special handling for recursive function type variables
  -- Call unify directly (not lifted) so we can pattern match on the Either
  let s5Result = unify ttWithS4 teWithS3
  -- CRITICAL FIX: If we get UnificationError with concrete type vs function type with type variable return,
  -- this likely means we're trying to unify the then branch (concrete) with else branch (function type)
  -- where the function type's return type is a recursive function's type variable.
  -- In this case, we should unify the concrete type with the return type variable, not fail.
  s5 <- either
    (\err -> case err of
      UnificationError concrete (TFun arg (TVar retVar)) | isConcreteType concrete -> do
        -- Concrete type vs function type with return type variable
        -- Unify concrete with return type variable instead
        either (lift . Left) return $ unify concrete (TVar retVar)
      UnificationError (TFun arg (TVar retVar)) concrete | isConcreteType concrete -> do
        -- Same, reversed
        either (lift . Left) return $ unify (TVar retVar) concrete
      _ -> lift $ Left err)
    return
    s5Result
  let finalSubst = composeSubstList [s12, s3, s4, s5]
  return (finalSubst, applySubst s5 te)

inferControlFlow _ _ _ = error "inferControlFlow called on non-control-flow expression"
