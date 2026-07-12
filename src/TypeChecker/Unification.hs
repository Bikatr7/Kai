module TypeChecker.Unification where

import qualified Data.Map as Map
import TypeChecker.Types
import TypeChecker.Substitution

occurs :: String -> Type -> Bool
occurs name (TVar name') = name == name'
occurs name (TFun t1 t2) = occurs name t1 || occurs name t2
occurs name (TCustom _ args) = any (occurs name) args
occurs name (TMaybe t) = occurs name t
occurs name (TEither t1 t2) = occurs name t1 || occurs name t2
occurs name (TList t) = occurs name t
occurs name (TRecord fields) = any (occurs name) (Map.elems fields)
occurs name (TTuple ts) = any (occurs name) ts
occurs _ _ = False

unify :: Type -> Type -> Either TypeError Substitution
unify (TVar a) t
  | t == TVar a = Right Map.empty
  | occurs a t = Left $ InfiniteType a t
  | otherwise = Right $ Map.singleton a t
unify t (TVar a) = unify (TVar a) t
unify TInt TInt = Right Map.empty
unify TBool TBool = Right Map.empty
unify TString TString = Right Map.empty
unify TUnit TUnit = Right Map.empty
unify (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  return $ composeSubst s2 s1
unify (TCustom name1 args1) (TCustom name2 args2)
  | name1 /= name2 || length args1 /= length args2 =
      Left $ UnificationError (TCustom name1 args1) (TCustom name2 args2)
  | otherwise = unifyPairs (zip args1 args2)
unify (TMaybe t1) (TMaybe t2) = unify t1 t2
unify (TEither a1 b1) (TEither a2 b2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 b1) (applySubst s1 b2)
  return $ composeSubst s2 s1
unify (TList t1) (TList t2) = unify t1 t2
unify (TRecord f1) (TRecord f2)
  | Map.keys f1 /= Map.keys f2 = Left $ UnificationError (TRecord f1) (TRecord f2)
  | otherwise = unifyPairs (zip (Map.elems f1) (Map.elems f2))
unify (TTuple ts1) (TTuple ts2)
  | length ts1 /= length ts2 = Left $ UnificationError (TTuple ts1) (TTuple ts2)
  | otherwise = unifyPairs (zip ts1 ts2)
unify t1 t2 = Left $ UnificationError t1 t2

unifyPairs :: [(Type, Type)] -> Either TypeError Substitution
unifyPairs = go Map.empty
  where
    go subst [] = Right subst
    go subst ((t1, t2) : rest) = do
      next <- unify (applySubst subst t1) (applySubst subst t2)
      go (composeSubst next subst) rest
