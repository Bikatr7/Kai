module TypeChecker.Operations where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Patterns

type InferFunc = TypeEnv -> Expr -> TypeInfer (Substitution, Type)

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

inferOperations :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferOperations infer env (ParseInt e) = do
  (s, eType) <- infer env e
  s2 <- lift $ unify (applySubst s eType) TString
  let finalSubst = composeSubst s2 s
  return (finalSubst, TMaybe TInt)

inferOperations infer env (ToString e) = do
  (s, eType) <- infer env e
  s2 <- lift $ unify (applySubst s eType) TInt
  let finalSubst = composeSubst s2 s
  return (finalSubst, TString)

inferOperations infer env (Show e) = do
  (s, _) <- infer env e
  return (s, TString)

inferOperations infer env (Discard e) = do
  (s, _) <- infer env e
  return (s, TUnit)

inferOperations infer env (MJust e) = do
  (s, eType) <- infer env e
  return (s, TMaybe (applySubst s eType))

inferOperations _ _ MNothing = do
  tyVar <- freshTVar
  return (Map.empty, TMaybe tyVar)

inferOperations infer env (ELeft e) = do
  (s, eType) <- infer env e
  tyVar <- freshTVar
  return (s, TEither (applySubst s eType) tyVar)

inferOperations infer env (ERight e) = do
  (s, eType) <- infer env e
  tyVar <- freshTVar
  return (s, TEither tyVar (applySubst s eType))

inferOperations infer env (Map f lst) = do
    (s12, fType, lstType) <- inferTwo infer env f lst
    elemType <- freshTVar
    resultType <- freshTVar
    s3 <- lift $ unify fType (TFun elemType resultType)
    s4 <- lift $ unify (applySubst s3 lstType) (TList (applySubst s3 elemType))
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, TList (applySubst finalSubst resultType))

inferOperations infer env (Filter f lst) = do
    (s12, fType, lstType) <- inferTwo infer env f lst
    elemType <- freshTVar
    s3 <- lift $ unify fType (TFun elemType TBool)
    s4 <- lift $ unify (applySubst s3 lstType) (TList (applySubst s3 elemType))
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Foldl f acc lst) = do
    (s123, fType, accType, lstType) <- inferThree infer env f acc lst
    elemType <- freshTVar
    s4 <- lift $ unify fType (TFun accType (TFun elemType accType))
    s5 <- lift $ unify (applySubst s4 lstType) (TList (applySubst s4 elemType))
    let finalSubst = composeSubst s5 (composeSubst s4 s123)
    return (finalSubst, applySubst finalSubst accType)

inferOperations infer env (Length lst) = do
    (s, lstType) <- infer env lst
    elemType <- freshTVar
    s' <- lift $ unify (applySubst s lstType) (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, TInt)

inferOperations infer env (Reverse lst) = do
    (s, lstType) <- infer env lst
    elemType <- freshTVar
    s' <- lift $ unify (applySubst s lstType) (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Take n lst) = do
    (s12, nType, lstType) <- inferTwo infer env n lst
    s3 <- lift $ unify nType TInt
    elemType <- freshTVar
    s4 <- lift $ unify (applySubst s3 lstType) (TList elemType)
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Drop n lst) = do
    (s12, nType, lstType) <- inferTwo infer env n lst
    s3 <- lift $ unify nType TInt
    elemType <- freshTVar
    s4 <- lift $ unify (applySubst s3 lstType) (TList elemType)
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Zip l1 l2) = do
    (s12, l1Type, l2Type) <- inferTwo infer env l1 l2
    elemType1 <- freshTVar
    elemType2 <- freshTVar
    s3 <- lift $ unify l1Type (TList elemType1)
    s4 <- lift $ unify (applySubst s3 l2Type) (TList elemType2)
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    let finalElemType1 = applySubst finalSubst elemType1
    let finalElemType2 = applySubst finalSubst elemType2
    return (finalSubst, TList (TTuple [finalElemType1, finalElemType2]))

inferOperations infer env (Split delim str) = do
    (s12, delimType, strType) <- inferTwo infer env delim str
    s3 <- lift $ unify delimType TString
    s4 <- lift $ unify (applySubst s3 strType) TString
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, TList TString)

inferOperations infer env (Join delim lst) = do
    (s12, delimType, lstType) <- inferTwo infer env delim lst
    s3 <- lift $ unify delimType TString
    s4 <- lift $ unify (applySubst s3 lstType) (TList TString)
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, TString)

inferOperations infer env (Trim str) = do
    (s, strType) <- infer env str
    s' <- lift $ unify (applySubst s strType) TString
    let finalSubst = composeSubst s' s
    return (finalSubst, TString)

inferOperations infer env (Replace old new str) = do
    (s123, oldType, newType, strType) <- inferThree infer env old new str
    s4 <- lift $ unify oldType TString
    s5 <- lift $ unify (applySubst s4 newType) TString
    s6 <- lift $ unify (applySubst s5 strType) TString
    let finalSubst = composeSubst s6 (composeSubst s5 (composeSubst s4 s123))
    return (finalSubst, TString)

inferOperations infer env (StrLength str) = do
    (s, strType) <- infer env str
    s' <- lift $ unify (applySubst s strType) TString
    let finalSubst = composeSubst s' s
    return (finalSubst, TInt)

inferOperations infer env (ReadFile path) = do
    (s, pathType) <- infer env path
    s' <- lift $ unify (applySubst s pathType) TString
    let finalSubst = composeSubst s' s
    return (finalSubst, TString)

inferOperations infer env (WriteFile path content) = do
    (s12, pathType, contentType) <- inferTwo infer env path content
    s3 <- lift $ unify pathType TString
    s4 <- lift $ unify (applySubst s3 contentType) TString
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, TUnit)

inferOperations infer env (Case scrutinee patterns) = do
  (s1, scrutType) <- infer env scrutinee
  resultType <- freshTVar
  let env' = applySubstEnv s1 env
  (s2, _) <- inferPatterns env' (applySubst s1 scrutType) resultType patterns
  let finalSubst = composeSubst s2 s1
  return (finalSubst, applySubst finalSubst resultType)
  where
    inferPatterns _ _ _ [] = return (Map.empty, TUnit)
    inferPatterns env scrutType resultType ((pat, expr) : rest) = do
      (patSubst, patEnv) <- inferPattern pat scrutType
      let appliedEnv = applySubstEnv patSubst env
      let newEnv = Map.union (applySubstEnv patSubst patEnv) appliedEnv
      (exprSubst, exprType) <- infer newEnv expr
      unifySubst <- lift $ unify (applySubst exprSubst resultType) (applySubst exprSubst exprType)
      let combinedSubst = composeSubstList [patSubst, exprSubst, unifySubst]
      (restSubst, _) <- inferPatterns
        (applySubstEnv combinedSubst env)
        (applySubst combinedSubst scrutType)
        (applySubst combinedSubst resultType)
        rest
      return (composeSubst restSubst combinedSubst, TUnit)

inferOperations _ _ _ = error "inferOperations called on non-operation expression"
