{-# LANGUAGE FlexibleContexts #-}
module Evaluator.DataStructures where

import Evaluator.Types
import Evaluator.Helpers (evalInIO)
import Evaluator.Functions (applyCallableWith, isCallableValue, resolveCallableIO)
import Syntax
import qualified Data.Map as Map
import Control.Monad (foldM)
import Control.Monad.Except (MonadError, throwError, ExceptT(..))


evalDataStructuresWith :: MonadError RuntimeError m => (Value -> m Value) -> Eval m -> Eval m
evalDataStructuresWith _ eval env (ListLit es) = do
    vs <- mapM (eval env) es
    pure $ VList vs
evalDataStructuresWith _ eval env (Cons h t) = do
    vh <- eval env h
    vt <- eval env t
    case vt of
        VList l -> pure $ VList (vh:l)
        _ -> throwError $ TypeError "Cons expects a list as its second argument"
evalDataStructuresWith _ eval env (Head e) = do
    v <- eval env e
    case v of
        VList (h:_) -> pure h
        VList [] -> throwError $ TypeError "Head of an empty list"
        _ -> throwError $ TypeError "Head expects a list"
evalDataStructuresWith _ eval env (Tail e) = do
    v <- eval env e
    case v of
        VList (_:t) -> pure $ VList t
        VList [] -> throwError $ TypeError "Tail of an empty list"
        _ -> throwError $ TypeError "Tail expects a list"
evalDataStructuresWith _ eval env (Null e) = do
    v <- eval env e
    case v of
        VList l -> pure $ VBool (null l)
        _ -> throwError $ TypeError "Null expects a list"
evalDataStructuresWith _ eval env (RecordLit fields) = do
    let evalField (name, e) = do
            v <- eval env e
            return (name, v)
    evaledFields <- mapM evalField fields
    pure $ VRecord (Map.fromList evaledFields)
evalDataStructuresWith _ eval env (RecordAccess r field) = do
    v <- eval env r
    case v of
        VRecord m -> case Map.lookup field m of
            Just fv -> pure fv
            Nothing -> throwError $ RecordFieldNotFound field
        _ -> throwError $ TypeError "Record access expects a record"
evalDataStructuresWith _ eval env (TupleLit exprs) = do
    vals <- mapM (eval env) exprs
    pure $ VTuple vals
evalDataStructuresWith _ eval env (Fst e) = do
    v <- eval env e
    case v of
        VTuple (v1:_) -> pure v1
        VTuple [] -> throwError $ TypeError "fst: empty tuple"
        _ -> throwError $ TypeError "fst: expected a tuple"
evalDataStructuresWith _ eval env (Snd e) = do
    v <- eval env e
    case v of
        VTuple (_:v2:_) -> pure v2
        VTuple _ -> throwError $ TypeError "snd: tuple must have at least 2 elements"
        _ -> throwError $ TypeError "snd: expected a tuple"
evalDataStructuresWith resolve eval env (Map f lst) = do
    fVal <- eval env f
    lstVal <- eval env lst
    callable <- resolve fVal
    case lstVal of
        VList values
          | isCallableValue callable -> VList <$> mapM (applyCallableWith resolve eval callable) values
          | otherwise -> throwError $ TypeError "map: first argument must be callable"
        _
          | isCallableValue callable -> throwError $ TypeError "map: second argument must be a list"
          | otherwise -> throwError $ TypeError "map: invalid arguments"
evalDataStructuresWith resolve eval env (Filter f lst) = do
    fVal <- eval env f
    lstVal <- eval env lst
    callable <- resolve fVal
    case lstVal of
        VList values
          | isCallableValue callable -> do
              results <- mapM (evalPredicate callable) values
              pure $ VList [value | (value, True) <- results]
          | otherwise -> throwError $ TypeError "filter: first argument must be callable"
        _
          | isCallableValue callable -> throwError $ TypeError "filter: second argument must be a list"
          | otherwise -> throwError $ TypeError "filter: invalid arguments"
  where
    evalPredicate callable value = do
      result <- applyCallableWith resolve eval callable value
      case result of
        VBool keep -> pure (value, keep)
        _ -> throwError $ TypeError "filter: predicate must return a boolean"
evalDataStructuresWith resolve eval env (Foldl f acc lst) = do
    fVal <- eval env f
    accVal <- eval env acc
    lstVal <- eval env lst
    callable <- resolve fVal
    case lstVal of
        VList values
          | isCallableValue callable -> foldM (foldStep callable) accVal values
          | otherwise -> throwError $ TypeError "foldl: first argument must be callable"
        _
          | isCallableValue callable -> throwError $ TypeError "foldl: third argument must be a list"
          | otherwise -> throwError $ TypeError "foldl: invalid arguments"
  where
    foldStep callable current value = do
      partiallyApplied <- applyCallableWith resolve eval callable current
      applyCallableWith resolve eval partiallyApplied value
evalDataStructuresWith _ eval env (Length lst) = do
    lstVal <- eval env lst
    case lstVal of
        VList vs -> pure $ VInt (length vs)
        _ -> throwError $ TypeError "length: argument must be a list"
evalDataStructuresWith _ eval env (Reverse lst) = do
    lstVal <- eval env lst
    case lstVal of
        VList vs -> pure $ VList (reverse vs)
        _ -> throwError $ TypeError "reverse: argument must be a list"
evalDataStructuresWith _ eval env (Take n lst) = do
    nVal <- eval env n
    lstVal <- eval env lst
    case (nVal, lstVal) of
        (VInt count, VList vs) -> pure $ VList (take count vs)
        (VInt _, _) -> throwError $ TypeError "take: second argument must be a list"
        (_, VList _) -> throwError $ TypeError "take: first argument must be an integer"
        _ -> throwError $ TypeError "take: invalid arguments"
evalDataStructuresWith _ eval env (Drop n lst) = do
    nVal <- eval env n
    lstVal <- eval env lst
    case (nVal, lstVal) of
        (VInt count, VList vs) -> pure $ VList (drop count vs)
        (VInt _, _) -> throwError $ TypeError "drop: second argument must be a list"
        (_, VList _) -> throwError $ TypeError "drop: first argument must be an integer"
        _ -> throwError $ TypeError "drop: invalid arguments"
evalDataStructuresWith _ eval env (Zip l1 l2) = do
    l1Val <- eval env l1
    l2Val <- eval env l2
    case (l1Val, l2Val) of
        (VList vs1, VList vs2) -> pure $ VList [VTuple [v1, v2] | (v1, v2) <- zip vs1 vs2]
        (VList _, _) -> throwError $ TypeError "zip: second argument must be a list"
        (_, VList _) -> throwError $ TypeError "zip: first argument must be a list"
        _ -> throwError $ TypeError "zip: invalid arguments"
evalDataStructuresWith _ _ _ _ = error "evalDataStructures called on non-data-structure expression"

evalDataStructures :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalDataStructures = evalDataStructuresWith pure

evalDataStructuresIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalDataStructuresIO = evalInIO (evalDataStructuresWith (ExceptT . resolveCallableIO))
