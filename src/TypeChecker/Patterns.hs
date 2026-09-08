module TypeChecker.Patterns where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Control.Monad (when)
import Syntax (Pattern(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import DataDeclarations (constructorPatternScheme)

inferPatternUnchecked :: TypeEnv -> Pattern -> Type -> TypeInfer (Substitution, TypeEnv)
inferPatternUnchecked _ (PVar name) ty =
  return (Map.empty, if name == "_" then Map.empty else Map.singleton name (monoScheme ty))
inferPatternUnchecked _ (PInt _) ty = do
  s <- unifyInfer ty TInt
  return (s, Map.empty)
inferPatternUnchecked _ (PBool _) ty = do
  s <- unifyInfer ty TBool
  return (s, Map.empty)
inferPatternUnchecked _ (PStr _) ty = do
  s <- unifyInfer ty TString
  return (s, Map.empty)
inferPatternUnchecked _ PUnit ty = do
  s <- unifyInfer ty TUnit
  return (s, Map.empty)
inferPatternUnchecked env (PJust pat) ty = do
  tyVar <- freshTVar
  s1 <- unifyInfer ty (TMaybe tyVar)
  (s2, patEnv) <- inferPatternUnchecked env pat (applySubst s1 tyVar)
  return (composeSubst s2 s1, patEnv)
inferPatternUnchecked _ PNothing ty = do
  tyVar <- freshTVar
  s <- unifyInfer ty (TMaybe tyVar)
  return (s, Map.empty)
inferPatternUnchecked env (PLeft pat) ty = do
  tyVar1 <- freshTVar
  tyVar2 <- freshTVar
  s1 <- unifyInfer ty (TEither tyVar1 tyVar2)
  (s2, patEnv) <- inferPatternUnchecked env pat (applySubst s1 tyVar1)
  return (composeSubst s2 s1, patEnv)
inferPatternUnchecked env (PRight pat) ty = do
  tyVar1 <- freshTVar
  tyVar2 <- freshTVar
  s1 <- unifyInfer ty (TEither tyVar1 tyVar2)
  (s2, patEnv) <- inferPatternUnchecked env pat (applySubst s1 tyVar2)
  return (composeSubst s2 s1, patEnv)
inferPatternUnchecked env (PList pats) ty = do
    elemType <- freshTVar
    s1 <- unifyInfer ty (TList elemType)
    inferPatternSiblings env s1 [(pat, elemType) | pat <- pats]
inferPatternUnchecked env (PCons h t) ty = do
    elemType <- freshTVar
    s1 <- unifyInfer ty (TList elemType)
    inferPatternSiblings env s1 [(h, elemType), (t, TList elemType)]
inferPatternUnchecked env (PRecord fields) ty = do
    fieldTypes <- mapM (const freshTVar) fields
    let typedFields = zipWith (\(name, _) fieldType -> (name, fieldType)) fields fieldTypes
    s1 <- unifyInfer ty (TRecord (Map.fromList typedFields))
    inferPatternSiblings env s1 (zip (map snd fields) fieldTypes)
inferPatternUnchecked env (POpenRecord fields rest) ty = do
    fieldTypes <- mapM (const freshTVar) fields
    row <- freshRowVar
    let typedFields = Map.fromList (zip (map fst fields) fieldTypes)
    initial <- unifyInfer ty (TOpenRecord typedFields row)
    (subst, bindings) <- inferPatternSiblings env initial (zip (map snd fields) fieldTypes)
    let remaining = monoScheme (applySubst subst (TOpenRecord Map.empty row))
    pure (subst, if rest == "_" then bindings else Map.insert rest remaining bindings)
inferPatternUnchecked env (PTuple pats) ty = do
    elemTypes <- mapM (const freshTVar) pats
    s1 <- unifyInfer ty (TTuple elemTypes)
    inferPatternSiblings env s1 (zip pats elemTypes)
inferPatternUnchecked env (PConstructor name pats) ty =
  case constructorPatternScheme env name of
    Nothing -> lift $ Left $ UnboundVariable name
    Just scheme -> do
      let arity (TFun _ result) = 1 + arity result
          arity _ = 0
          expected = arity (schemeType scheme)
      when (length pats /= expected) $
        lift $ Left $ ConstructorPatternArity name expected (length pats)
      ctorType <- instantiate scheme
      (s1, remainingType, patEnv) <- consumeConstructorArgs env ctorType pats
      s2 <- unifyInfer (applySubst s1 remainingType) (applySubst s1 ty)
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
  s1 <- unifyInfer ctorType (TFun argType resultType)
  let appliedEnv = applySubstEnv s1 env
  (s2, patEnv) <- inferPattern appliedEnv pat (applySubst s1 argType)
  let s12 = composeSubst s2 s1
  (s3, remainingType, restEnv) <- consumeConstructorArgs (applySubstEnv s12 env) (applySubst s12 resultType) rest
  let finalSubst = composeSubst s3 s12
  return (finalSubst, applySubst finalSubst remainingType, Map.union (applySubstEnv finalSubst patEnv) restEnv)

inferPattern :: TypeEnv -> Pattern -> Type -> TypeInfer (Substitution, TypeEnv)
inferPattern env pat ty = do
  _ <- lift $ bindings pat
  inferPatternUnchecked env pat ty
  where
    bindings (PVar "_") = Right []
    bindings (PVar name) = Right [name]
    bindings (PJust p) = bindings p
    bindings (PLeft p) = bindings p
    bindings (PRight p) = bindings p
    bindings (PList ps) = siblings ps
    bindings (PCons a b) = siblings [a,b]
    bindings (PTuple ps) = siblings ps
    bindings (PConstructor _ ps) = siblings ps
    bindings (PRecord fields) = do
      distinct DuplicateRecordField (map fst fields)
      siblings (map snd fields)
    bindings (POpenRecord fields rest) = do
      distinct DuplicateRecordField (map fst fields)
      siblings (PVar rest : map snd fields)
    bindings _ = Right []
    siblings ps = do
      names <- concat <$> mapM bindings ps
      distinct DuplicatePatternBinding names
      return names
    distinct _ [] = Right ()
    distinct err (name:names)
      | name `elem` names = Left (err name)
      | otherwise = distinct err names
