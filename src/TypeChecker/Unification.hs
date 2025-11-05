module TypeChecker.Unification where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Either as Either
import Control.Monad.Except (throwError)
import TypeChecker.Types
import TypeChecker.Substitution

occurs :: String -> Type -> Bool
occurs name (TVar name') = name == name'
occurs name (TFun t1 t2) = occurs name t1 || occurs name t2
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
unify (TMaybe t1) (TMaybe t2) = unify t1 t2
unify (TEither a1 b1) (TEither a2 b2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 b1) (applySubst s1 b2)
  return $ composeSubst s2 s1
unify (TList t1) (TList t2) = unify t1 t2
unify (TRecord f1) (TRecord f2) = do
    if Map.keys f1 /= Map.keys f2
        then Left $ UnificationError (TRecord f1) (TRecord f2)
        else do
            let subs = Map.intersectionWith unify f1 f2
            let errors = Either.lefts (Map.elems subs)
            if not (null errors)
                then Left (head errors)
                else do
                    let successes = Either.rights (Map.elems subs)
                    return $ composeSubstList successes
unify (TTuple ts1) (TTuple ts2)
  | length ts1 /= length ts2 = Left $ UnificationError (TTuple ts1) (TTuple ts2)
  | otherwise = do
      let pairs = zip ts1 ts2
      subs <- sequence [unify t1 t2 | (t1, t2) <- pairs]
      return $ composeSubstList subs
unify t1 t2 = Left $ UnificationError t1 t2
