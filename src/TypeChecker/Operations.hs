module TypeChecker.Operations where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Patterns

type InferFunc = TypeEnv -> Expr -> TypeInfer (Substitution, Type)

inferOperations :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferOperations infer env (ParseInt e) = do
  (s, eType) <- infer env e
  s2 <- lift $ unify eType TString
  let finalSubst = composeSubst s2 s
  return (finalSubst, TMaybe TInt)

inferOperations infer env (ToString e) = do
  (s, eType) <- infer env e
  s2 <- lift $ unify eType TInt
  let finalSubst = composeSubst s2 s
  return (finalSubst, TString)

inferOperations infer env (Show e) = do
  (s, _) <- infer env e
  return (s, TString)

inferOperations infer env (MJust e) = do
  (s, eType) <- infer env e
  return (s, TMaybe eType)

inferOperations _ _ MNothing = do
  tyVar <- freshTVar
  return (Map.empty, TMaybe tyVar)

inferOperations infer env (ELeft e) = do
  (s, eType) <- infer env e
  tyVar <- freshTVar
  return (s, TEither eType tyVar)

inferOperations infer env (ERight e) = do
  (s, eType) <- infer env e
  tyVar <- freshTVar
  return (s, TEither tyVar eType)

inferOperations infer env (Map f lst) = do
    (s1, fType) <- infer env f
    (s2, lstType) <- infer env lst
    elemType <- freshTVar
    resultType <- freshTVar
    s3 <- lift $ unify (applySubst s2 fType) (TFun elemType resultType)
    s4 <- lift $ unify (applySubst s3 lstType) (TList (applySubst s3 elemType))
    let finalSubst = composeSubstList [s1, s2, s3, s4]
    return (finalSubst, TList (applySubst finalSubst resultType))

inferOperations infer env (Filter f lst) = do
    (s1, fType) <- infer env f
    (s2, lstType) <- infer env lst
    elemType <- freshTVar
    s3 <- lift $ unify (applySubst s2 fType) (TFun elemType TBool)
    s4 <- lift $ unify (applySubst s3 lstType) (TList (applySubst s3 elemType))
    let finalSubst = composeSubstList [s1, s2, s3, s4]
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Foldl f acc lst) = do
    (s1, fType) <- infer env f
    (s2, accType) <- infer env acc
    (s3, lstType) <- infer env lst
    elemType <- freshTVar
    s4 <- lift $ unify (applySubst s3 (applySubst s2 fType)) (TFun (applySubst s3 accType) (TFun elemType (applySubst s3 accType)))
    s5 <- lift $ unify (applySubst s4 lstType) (TList (applySubst s4 elemType))
    let finalSubst = composeSubstList [s1, s2, s3, s4, s5]
    return (finalSubst, applySubst finalSubst accType)

inferOperations infer env (Length lst) = do
    (s, lstType) <- infer env lst
    elemType <- freshTVar
    s' <- lift $ unify lstType (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, TInt)

inferOperations infer env (Reverse lst) = do
    (s, lstType) <- infer env lst
    elemType <- freshTVar
    s' <- lift $ unify lstType (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Take n lst) = do
    (s1, nType) <- infer env n
    (s2, lstType) <- infer env lst
    s3 <- lift $ unify (applySubst s2 nType) TInt
    elemType <- freshTVar
    s4 <- lift $ unify (applySubst s3 lstType) (TList elemType)
    let finalSubst = composeSubstList [s1, s2, s3, s4]
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Drop n lst) = do
    (s1, nType) <- infer env n
    (s2, lstType) <- infer env lst
    s3 <- lift $ unify (applySubst s2 nType) TInt
    elemType <- freshTVar
    s4 <- lift $ unify (applySubst s3 lstType) (TList elemType)
    let finalSubst = composeSubstList [s1, s2, s3, s4]
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Zip l1 l2) = do
    (s1, l1Type) <- infer env l1
    (s2, l2Type) <- infer env l2
    elemType1 <- freshTVar
    elemType2 <- freshTVar
    s3 <- lift $ unify (applySubst s2 l1Type) (TList elemType1)
    s4 <- lift $ unify (applySubst s3 l2Type) (TList elemType2)
    let finalSubst = composeSubstList [s1, s2, s3, s4]
    let finalElemType1 = applySubst finalSubst elemType1
    let finalElemType2 = applySubst finalSubst elemType2
    return (finalSubst, TList (TTuple [finalElemType1, finalElemType2]))

inferOperations infer env (Split delim str) = do
    (s1, delimType) <- infer env delim
    (s2, strType) <- infer env str
    s3 <- lift $ unify (applySubst s2 delimType) TString
    s4 <- lift $ unify (applySubst s3 strType) TString
    let finalSubst = composeSubstList [s1, s2, s3, s4]
    return (finalSubst, TList TString)

inferOperations infer env (Join delim lst) = do
    (s1, delimType) <- infer env delim
    (s2, lstType) <- infer env lst
    s3 <- lift $ unify (applySubst s2 delimType) TString
    s4 <- lift $ unify (applySubst s3 lstType) (TList TString)
    let finalSubst = composeSubstList [s1, s2, s3, s4]
    return (finalSubst, TString)

inferOperations infer env (Trim str) = do
    (s, strType) <- infer env str
    s' <- lift $ unify strType TString
    let finalSubst = composeSubst s' s
    return (finalSubst, TString)

inferOperations infer env (Replace old new str) = do
    (s1, oldType) <- infer env old
    (s2, newType) <- infer env new
    (s3, strType) <- infer env str
    s4 <- lift $ unify (applySubst s3 (applySubst s2 oldType)) TString
    s5 <- lift $ unify (applySubst s4 newType) TString
    s6 <- lift $ unify (applySubst s5 strType) TString
    let finalSubst = composeSubstList [s1, s2, s3, s4, s5, s6]
    return (finalSubst, TString)

inferOperations infer env (StrLength str) = do
    (s, strType) <- infer env str
    s' <- lift $ unify strType TString
    let finalSubst = composeSubst s' s
    return (finalSubst, TInt)

inferOperations infer env (ReadFile path) = do
    (s, pathType) <- infer env path
    s' <- lift $ unify pathType TString
    let finalSubst = composeSubst s' s
    return (finalSubst, TString)

inferOperations infer env (WriteFile path content) = do
    (s1, pathType) <- infer env path
    (s2, contentType) <- infer env content
    s3 <- lift $ unify (applySubst s2 pathType) TString
    s4 <- lift $ unify (applySubst s3 contentType) TString
    let finalSubst = composeSubstList [s1, s2, s3, s4]
    return (finalSubst, TUnit)

inferOperations infer env (Case scrutinee patterns) = do
  (s1, scrutType) <- infer env scrutinee
  resultType <- freshTVar
  (s2, _) <- inferPatterns env (applySubst s1 scrutType) resultType patterns
  let finalSubst = composeSubst s2 s1
  return (finalSubst, applySubst finalSubst resultType)
  where
    inferPatterns _ _ _ [] = return (Map.empty, TUnit)
    inferPatterns env scrutType resultType ((pat, expr) : rest) = do
      (patSubst, patEnv) <- inferPattern pat scrutType
      let newEnv = Map.union patEnv env
      (exprSubst, exprType) <- infer newEnv expr
      unifySubst <- lift $ unify (applySubst exprSubst resultType) exprType
      let combinedSubst = composeSubstList [patSubst, exprSubst, unifySubst]
      (restSubst, _) <- inferPatterns env (applySubst combinedSubst scrutType) (applySubst combinedSubst resultType) rest
      return (composeSubst restSubst combinedSubst, TUnit)

inferOperations _ _ _ = error "inferOperations called on non-operation expression"
