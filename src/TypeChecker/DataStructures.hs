module TypeChecker.DataStructures where

import qualified Data.Map as Map
import Control.Monad (foldM)
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
    finalSubst <- foldM (inferListElem elemType) Map.empty es
    return (finalSubst, TList (applySubst finalSubst elemType))
  where
    inferListElem elemType subst e = do
        (s, t) <- infer (applySubstEnv subst env) e
        let subst' = composeSubst s subst
        s' <- lift $ unify (applySubst subst' t) (applySubst subst' elemType)
        return $ composeSubst s' subst'

inferDataStructures infer env (Cons h t) = do
    (s1, hType) <- infer env h
    (s2, tType) <- infer (applySubstEnv s1 env) t
    s3 <- lift $ unify (applySubst s2 tType) (TList (applySubst s2 hType))
    let finalSubst = composeSubstList [s1, s2, s3]
    return (finalSubst, applySubst finalSubst tType)

inferDataStructures infer env (Head e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify (applySubst s eType) (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst elemType)

inferDataStructures infer env (Tail e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify (applySubst s eType) (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst eType)

inferDataStructures infer env (Null e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify (applySubst s eType) (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, TBool)

inferDataStructures infer env (RecordLit fields) = do
    (finalSubst, typedFields) <- foldM inferField (Map.empty, []) fields
    let finalFields = reverse $ map (second (applySubst finalSubst)) typedFields
    return (finalSubst, TRecord (Map.fromList finalFields))
  where
    inferField (subst, acc) (name, e) = do
        (s, t) <- infer (applySubstEnv subst env) e
        let subst' = composeSubst s subst
        return (subst', (name, t) : acc)

inferDataStructures infer env (RecordAccess r field) = do
    (s, rType) <- infer env r
    case applySubst s rType of
        TRecord fields -> case Map.lookup field fields of
            Just fieldType -> return (s, fieldType)
            Nothing -> lift $ Left $ RecordFieldMismatch field
        resolvedType -> do
            fieldType <- freshTVar
            s' <- lift $ unify resolvedType (TRecord (Map.singleton field fieldType))
            let finalSubst = composeSubst s' s
            return (finalSubst, applySubst finalSubst fieldType)

inferDataStructures infer env (TupleLit exprs) = do
    (finalSubst, types) <- foldM inferTupleElem (Map.empty, []) exprs
    let finalTypes = reverse $ map (applySubst finalSubst) types
    return (finalSubst, TTuple finalTypes)
  where
    inferTupleElem (subst, acc) expr = do
        (s, t) <- infer (applySubstEnv subst env) expr
        let subst' = composeSubst s subst
        return (subst', t : acc)

inferDataStructures infer env (Fst e) = do
    (s, tType) <- infer env e
    t1 <- freshTVar
    t2 <- freshTVar
    s' <- lift $ unify (applySubst s tType) (TTuple [t1, t2])
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst t1)

inferDataStructures infer env (Snd e) = do
    (s, tType) <- infer env e
    t1 <- freshTVar
    t2 <- freshTVar
    s' <- lift $ unify (applySubst s tType) (TTuple [t1, t2])
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst t2)

inferDataStructures _ _ _ = error "inferDataStructures called on non-data-structure expression"
