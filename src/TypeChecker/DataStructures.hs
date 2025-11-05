module TypeChecker.DataStructures where

import qualified Data.Map as Map
import Control.Monad (forM, mapAndUnzipM)
import Data.Bifunctor (second)
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

type InferFunc = TypeEnv -> Expr -> TypeInfer (Substitution, Type)

inferDataStructures :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferDataStructures infer env (ListLit es) = do
    elemType <- freshTVar
    subs <- forM es (\e -> do
        (s, t) <- infer env e
        s' <- lift $ unify t elemType
        return (composeSubst s' s))
    let finalSubst = composeSubstList subs
    return (finalSubst, TList (applySubst finalSubst elemType))

inferDataStructures infer env (Cons h t) = do
    (s1, hType) <- infer env h
    (s2, tType) <- infer env t
    s3 <- lift $ unify (applySubst s2 tType) (TList (applySubst s2 hType))
    let finalSubst = composeSubstList [s1, s2, s3]
    return (finalSubst, applySubst s3 tType)

inferDataStructures infer env (Head e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify eType (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst elemType)

inferDataStructures infer env (Tail e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify eType (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst eType)

inferDataStructures infer env (Null e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify eType (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, TBool)

inferDataStructures infer env (RecordLit fields) = do
    let inferField (name, e) = do
            (s, t) <- infer env e
            return (s, (name, t))
    (subs, typedFields) <- mapAndUnzipM inferField fields
    let finalSubst = composeSubstList subs
    return (finalSubst, TRecord (Map.fromList (map (second (applySubst finalSubst)) typedFields)))

inferDataStructures infer env (RecordAccess r field) = do
    (s, rType) <- infer env r
    fieldType <- freshTVar
    s' <- lift $ unify rType (TRecord (Map.singleton field fieldType))
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst fieldType)

inferDataStructures infer env (TupleLit exprs) = do
    (subs, types) <- mapAndUnzipM (infer env) exprs
    let finalSubst = composeSubstList subs
    let finalTypes = map (applySubst finalSubst) types
    return (finalSubst, TTuple finalTypes)

inferDataStructures infer env (Fst e) = do
    (s, tType) <- infer env e
    t1 <- freshTVar
    t2 <- freshTVar
    s' <- lift $ unify tType (TTuple [t1, t2])
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst t1)

inferDataStructures infer env (Snd e) = do
    (s, tType) <- infer env e
    t1 <- freshTVar
    t2 <- freshTVar
    s' <- lift $ unify tType (TTuple [t1, t2])
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst t2)

inferDataStructures _ _ _ = error "inferDataStructures called on non-data-structure expression"
