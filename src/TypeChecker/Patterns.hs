module TypeChecker.Patterns where

import qualified Data.Map as Map
import Control.Monad (mapAndUnzipM)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Syntax (Pattern(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

inferPattern :: Pattern -> Type -> TypeInfer (Substitution, TypeEnv)
inferPattern (PVar name) ty = return (Map.empty, Map.singleton name ty)
inferPattern (PInt _) ty = do
  s <- lift $ unify ty TInt
  return (s, Map.empty)
inferPattern (PBool _) ty = do
  s <- lift $ unify ty TBool
  return (s, Map.empty)
inferPattern (PStr _) ty = do
  s <- lift $ unify ty TString
  return (s, Map.empty)
inferPattern PUnit ty = do
  s <- lift $ unify ty TUnit
  return (s, Map.empty)
inferPattern (PJust pat) ty = do
  tyVar <- freshTVar
  s1 <- lift $ unify ty (TMaybe tyVar)
  (s2, env) <- inferPattern pat (applySubst s1 tyVar)
  return (composeSubst s2 s1, env)
inferPattern PNothing ty = do
  tyVar <- freshTVar
  s <- lift $ unify ty (TMaybe tyVar)
  return (s, Map.empty)
inferPattern (PLeft pat) ty = do
  tyVar1 <- freshTVar
  tyVar2 <- freshTVar
  s1 <- lift $ unify ty (TEither tyVar1 tyVar2)
  (s2, env) <- inferPattern pat (applySubst s1 tyVar1)
  return (composeSubst s2 s1, env)
inferPattern (PRight pat) ty = do
  tyVar1 <- freshTVar
  tyVar2 <- freshTVar
  s1 <- lift $ unify ty (TEither tyVar1 tyVar2)
  (s2, env) <- inferPattern pat (applySubst s1 tyVar2)
  return (composeSubst s2 s1, env)
inferPattern (PList []) ty = do
    elemType <- freshTVar
    s <- lift $ unify ty (TList elemType)
    return (s, Map.empty)
inferPattern (PList pats) ty = do
    elemType <- freshTVar
    s1 <- lift $ unify ty (TList elemType)
    let inferPat p = inferPattern p elemType
    (subs, envs) <- mapAndUnzipM inferPat pats
    return (composeSubstList (s1:subs), Map.unions envs)
inferPattern (PCons h t) ty = do
    elemType <- freshTVar
    s1 <- lift $ unify ty (TList elemType)
    (s2, hEnv) <- inferPattern h elemType
    (s3, tEnv) <- inferPattern t (TList elemType)
    return (composeSubstList [s1, s2, s3], Map.union hEnv tEnv)
inferPattern (PRecord fields) ty = do
    let inferField (name, p) = do
            fieldType <- freshTVar
            (s, env) <- inferPattern p fieldType
            return (s, env, (name, fieldType))
    (subs, envs, typedFields) <- unzip3 <$> mapM inferField fields
    s' <- lift $ unify ty (TRecord (Map.fromList typedFields))
    return (composeSubstList (s':subs), Map.unions envs)
inferPattern (PTuple pats) ty = do
    elemTypes <- mapM (const freshTVar) pats
    s1 <- lift $ unify ty (TTuple elemTypes)
    let inferPat (p, t) = inferPattern p t
    (subs, envs) <- mapAndUnzipM inferPat (zip pats elemTypes)
    return (composeSubstList (s1:subs), Map.unions envs)
