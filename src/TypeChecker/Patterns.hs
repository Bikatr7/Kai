module TypeChecker.Patterns where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Syntax (Pattern(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

inferPattern :: TypeEnv -> Pattern -> Type -> TypeInfer (Substitution, TypeEnv)
inferPattern _ (PVar name) ty =
  return (Map.empty, if name == "_" then Map.empty else Map.singleton name (monoScheme ty))
inferPattern _ (PInt _) ty = do
  s <- lift $ unify ty TInt
  return (s, Map.empty)
inferPattern _ (PBool _) ty = do
  s <- lift $ unify ty TBool
  return (s, Map.empty)
inferPattern _ (PStr _) ty = do
  s <- lift $ unify ty TString
  return (s, Map.empty)
inferPattern _ PUnit ty = do
  s <- lift $ unify ty TUnit
  return (s, Map.empty)
inferPattern env (PJust pat) ty = do
  tyVar <- freshTVar
  s1 <- lift $ unify ty (TMaybe tyVar)
  (s2, patEnv) <- inferPattern env pat (applySubst s1 tyVar)
  return (composeSubst s2 s1, patEnv)
inferPattern _ PNothing ty = do
  tyVar <- freshTVar
  s <- lift $ unify ty (TMaybe tyVar)
  return (s, Map.empty)
inferPattern env (PLeft pat) ty = do
  tyVar1 <- freshTVar
  tyVar2 <- freshTVar
  s1 <- lift $ unify ty (TEither tyVar1 tyVar2)
  (s2, patEnv) <- inferPattern env pat (applySubst s1 tyVar1)
  return (composeSubst s2 s1, patEnv)
inferPattern env (PRight pat) ty = do
  tyVar1 <- freshTVar
  tyVar2 <- freshTVar
  s1 <- lift $ unify ty (TEither tyVar1 tyVar2)
  (s2, patEnv) <- inferPattern env pat (applySubst s1 tyVar2)
  return (composeSubst s2 s1, patEnv)
inferPattern env (PList pats) ty = do
    elemType <- freshTVar
    s1 <- lift $ unify ty (TList elemType)
    inferPatternSiblings env s1 [(pat, elemType) | pat <- pats]
inferPattern env (PCons h t) ty = do
    elemType <- freshTVar
    s1 <- lift $ unify ty (TList elemType)
    inferPatternSiblings env s1 [(h, elemType), (t, TList elemType)]
inferPattern env (PRecord fields) ty = do
    fieldTypes <- mapM (const freshTVar) fields
    let typedFields = zipWith (\(name, _) fieldType -> (name, fieldType)) fields fieldTypes
    s1 <- lift $ unify ty (TRecord (Map.fromList typedFields))
    inferPatternSiblings env s1 (zip (map snd fields) fieldTypes)
inferPattern env (PTuple pats) ty = do
    elemTypes <- mapM (const freshTVar) pats
    s1 <- lift $ unify ty (TTuple elemTypes)
    inferPatternSiblings env s1 (zip pats elemTypes)
inferPattern env (PConstructor name pats) ty =
  case Map.lookup name env of
    Nothing -> lift $ Left $ UnboundVariable name
    Just scheme -> do
      ctorType <- instantiate scheme
      (s1, remainingType, patEnv) <- consumeConstructorArgs env ctorType pats
      s2 <- lift $ unify (applySubst s1 remainingType) (applySubst s1 ty)
      let finalSubst = composeSubst s2 s1
      return (finalSubst, applySubstEnv finalSubst patEnv)

inferPatternSiblings :: TypeEnv -> Substitution -> [(Pattern, Type)] -> TypeInfer (Substitution, TypeEnv)
inferPatternSiblings env initialSubst = go initialSubst Map.empty
  where
    go subst patEnv [] = return (subst, applySubstEnv subst patEnv)
    go subst patEnv ((pat, patType) : rest) = do
      (nextSubst, nextEnv) <- inferPattern
        (applySubstEnv subst env)
        pat
        (applySubst subst patType)
      let subst' = composeSubst nextSubst subst
      go subst' (Map.union patEnv nextEnv) rest

consumeConstructorArgs :: TypeEnv -> Type -> [Pattern] -> TypeInfer (Substitution, Type, TypeEnv)
consumeConstructorArgs _ ctorType [] = return (Map.empty, ctorType, Map.empty)
consumeConstructorArgs env ctorType (pat : rest) = do
  argType <- freshTVar
  resultType <- freshTVar
  s1 <- lift $ unify ctorType (TFun argType resultType)
  let appliedEnv = applySubstEnv s1 env
  (s2, patEnv) <- inferPattern appliedEnv pat (applySubst s1 argType)
  let s12 = composeSubst s2 s1
  (s3, remainingType, restEnv) <- consumeConstructorArgs (applySubstEnv s12 env) (applySubst s12 resultType) rest
  let finalSubst = composeSubst s3 s12
  return (finalSubst, applySubst finalSubst remainingType, Map.union (applySubstEnv finalSubst patEnv) restEnv)
