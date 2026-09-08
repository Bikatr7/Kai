module TypeChecker.Operations where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Syntax (Expr(..), exprSpan)
import TypeChecker.Coverage (checkCoverage)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State (modify)
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Helpers
import TypeChecker.Patterns

inferOperations :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferOperations infer env (Attempt e) = do
  result <- freshTVar
  inferUnary infer env e (TFun TUnit result) (TEither (TCustom "Error" []) result)
inferOperations infer env (Raise e) = do
  result <- freshTVar
  inferUnary infer env e (TCustom "Error" []) result
inferOperations infer env (ReadLine e) = inferUnary infer env e TUnit (TMaybe TString)
inferOperations infer env (HeadMaybe e) = do
  element <- freshTVar
  inferUnary infer env e (TList element) (TMaybe element)
inferOperations infer env (TailMaybe e) = do
  element <- freshTVar
  inferUnary infer env e (TList element) (TMaybe (TList element))

inferOperations infer env (ParseInt e) =
  inferUnary infer env e TString (TMaybe TInt)

inferOperations infer env (ToString e) =
  inferUnary infer env e TInt TString

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
    s3 <- unifyInfer fType (TFun elemType resultType)
    s4 <- unifyInfer (applySubst s3 lstType) (TList (applySubst s3 elemType))
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, TList (applySubst finalSubst resultType))

inferOperations infer env (Filter f lst) = do
    (s12, fType, lstType) <- inferTwo infer env f lst
    elemType <- freshTVar
    s3 <- unifyInfer fType (TFun elemType TBool)
    s4 <- unifyInfer (applySubst s3 lstType) (TList (applySubst s3 elemType))
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Foldl f acc lst) = do
    (s123, fType, accType, lstType) <- inferThree infer env f acc lst
    elemType <- freshTVar
    s4 <- unifyInfer fType (TFun accType (TFun elemType accType))
    s5 <- unifyInfer (applySubst s4 lstType) (TList (applySubst s4 elemType))
    let finalSubst = composeSubst s5 (composeSubst s4 s123)
    return (finalSubst, applySubst finalSubst accType)

inferOperations infer env (Length lst) = do
    (s, lstType) <- infer env lst
    elemType <- freshTVar
    s' <- unifyInfer (applySubst s lstType) (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, TInt)

inferOperations infer env (Reverse lst) = do
    (s, lstType) <- infer env lst
    elemType <- freshTVar
    s' <- unifyInfer (applySubst s lstType) (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst lstType)

inferOperations infer env (Take n lst) = inferSlice infer env n lst
inferOperations infer env (Drop n lst) = inferSlice infer env n lst

inferOperations infer env (Zip l1 l2) = do
    (s12, l1Type, l2Type) <- inferTwo infer env l1 l2
    elemType1 <- freshTVar
    elemType2 <- freshTVar
    s3 <- unifyInfer l1Type (TList elemType1)
    s4 <- unifyInfer (applySubst s3 l2Type) (TList elemType2)
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    let finalElemType1 = applySubst finalSubst elemType1
    let finalElemType2 = applySubst finalSubst elemType2
    return (finalSubst, TList (TTuple [finalElemType1, finalElemType2]))

inferOperations infer env (Split delim str) =
  inferBinary infer env delim str TString TString (TList TString)

inferOperations infer env (Join delim lst) =
  inferBinary infer env delim lst TString (TList TString) TString

inferOperations infer env (Trim str) =
  inferUnary infer env str TString TString

inferOperations infer env (Replace old new str) = do
    (s123, oldType, newType, strType) <- inferThree infer env old new str
    s4 <- unifyInfer oldType TString
    s5 <- unifyInfer (applySubst s4 newType) TString
    s6 <- unifyInfer (applySubst s5 strType) TString
    let finalSubst = composeSubst s6 (composeSubst s5 (composeSubst s4 s123))
    return (finalSubst, TString)

inferOperations infer env (StrLength str) =
  inferUnary infer env str TString TInt

inferOperations infer env (ReadFile path) =
  inferUnary infer env path TString TString

inferOperations infer env (WriteFile path content) =
  inferBinary infer env path content TString TString TUnit

inferOperations infer env (AppendFile path content) =
  inferBinary infer env path content TString TString TUnit

inferOperations infer env (FileExists path) =
  inferUnary infer env path TString TBool

inferOperations infer env (ListDirectory path) =
  inferUnary infer env path TString (TList TString)

inferOperations infer env (CreateDirectory path) =
  inferUnary infer env path TString TUnit

inferOperations infer env (RemoveDirectory path) =
  inferUnary infer env path TString TUnit

inferOperations infer env (SetCurrentDirectory path) =
  inferUnary infer env path TString TUnit

inferOperations infer env (System command) =
  inferUnary infer env command TString TInt

inferOperations infer env (GetEnv name) =
  inferUnary infer env name TString (TMaybe TString)

inferOperations infer env (SetEnv name value) =
  inferBinary infer env name value TString TString TUnit

inferOperations infer env (Exit codeExpr) = do
    (s, codeType) <- infer env codeExpr
    s' <- unifyInfer (applySubst s codeType) TInt
    resultType <- freshTVar
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst resultType)

inferOperations infer env (Case scrutinee patterns) = do
  (s1, scrutType) <- infer env scrutinee
  resultType <- freshTVar
  let env' = applySubstEnv s1 env
  (s2, _) <- inferPatterns env' (applySubst s1 scrutType) resultType patterns
  let finalSubst = composeSubst s2 s1
  warnings <- lift $ checkCoverage (applySubstEnv finalSubst env)
    (applySubst finalSubst scrutType) (map fst patterns)
  modify $ \state -> state { inferredWarnings = inferredWarnings state ++ map warningLocation warnings }
  return (finalSubst, applySubst finalSubst resultType)
  where
    warningLocation warning@(UnreachableAlternative index) = case drop (index-1) patterns of
      (_,branch):_ -> maybe warning (`locateWarning` warning) (exprSpan branch)
      _ -> warning
    warningLocation warning = warning
    inferPatterns _ _ _ [] = return (Map.empty, TUnit)
    inferPatterns scope scrutType resultType ((pat, expr) : rest) = do
      let atBranch failure = maybe failure (`locateTypeError` failure) (exprSpan expr)
      (patSubst, patEnv) <- inferPattern scope pat scrutType `catchError` (throwError . atBranch)
      let appliedEnv = applySubstEnv patSubst scope
      let newEnv = Map.union (applySubstEnv patSubst patEnv) appliedEnv
      (exprSubst, exprType) <- infer newEnv expr
      unifySubst <- unifyInfer (applySubst exprSubst resultType) (applySubst exprSubst exprType)
        `catchError` (throwError . atBranch)
      let combinedSubst = composeSubstList [patSubst, exprSubst, unifySubst]
      (restSubst, _) <- inferPatterns
        (applySubstEnv combinedSubst scope)
        (applySubst combinedSubst scrutType)
        (applySubst combinedSubst resultType)
        rest
      return (composeSubst restSubst combinedSubst, TUnit)

inferOperations _ _ _ = error "inferOperations called on non-operation expression"

inferSlice :: InferFunc -> TypeEnv -> Expr -> Expr -> TypeInfer (Substitution, Type)
inferSlice infer env n lst = do
    (s12, nType, lstType) <- inferTwo infer env n lst
    s3 <- unifyInfer nType TInt
    elemType <- freshTVar
    s4 <- unifyInfer (applySubst s3 lstType) (TList elemType)
    let finalSubst = composeSubst s4 (composeSubst s3 s12)
    return (finalSubst, applySubst finalSubst lstType)
