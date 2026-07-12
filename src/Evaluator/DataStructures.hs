module Evaluator.DataStructures where

import Evaluator.Types
import Evaluator.Helpers (bindResult, traverseResults)
import Evaluator.Functions (applyCallable, applyCallableIO, isCallableValue, resolveCallableIO)
import Syntax
import qualified Data.Map as Map
import Control.Monad (foldM)

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalDataStructures :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalDataStructures eval env (ListLit es) = do
    vs <- mapM (eval env) es
    Right $ VList vs
evalDataStructures eval env (Cons h t) = do
    vh <- eval env h
    vt <- eval env t
    case vt of
        VList l -> Right $ VList (vh:l)
        _ -> Left $ TypeError "Cons expects a list as its second argument"
evalDataStructures eval env (Head e) = do
    v <- eval env e
    case v of
        VList (h:_) -> Right h
        VList [] -> Left $ TypeError "Head of an empty list"
        _ -> Left $ TypeError "Head expects a list"
evalDataStructures eval env (Tail e) = do
    v <- eval env e
    case v of
        VList (_:t) -> Right $ VList t
        VList [] -> Left $ TypeError "Tail of an empty list"
        _ -> Left $ TypeError "Tail expects a list"
evalDataStructures eval env (Null e) = do
    v <- eval env e
    case v of
        VList l -> Right $ VBool (null l)
        _ -> Left $ TypeError "Null expects a list"
evalDataStructures eval env (RecordLit fields) = do
    let evalField (name, e) = do
            v <- eval env e
            return (name, v)
    evaledFields <- mapM evalField fields
    Right $ VRecord (Map.fromList evaledFields)
evalDataStructures eval env (RecordAccess r field) = do
    v <- eval env r
    case v of
        VRecord m -> case Map.lookup field m of
            Just fv -> Right fv
            Nothing -> Left $ RecordFieldNotFound field
        _ -> Left $ TypeError "Record access expects a record"
evalDataStructures eval env (TupleLit exprs) = do
    vals <- mapM (eval env) exprs
    Right $ VTuple vals
evalDataStructures eval env (Fst e) = do
    v <- eval env e
    case v of
        VTuple (v1:_) -> Right v1
        VTuple [] -> Left $ TypeError "fst: empty tuple"
        _ -> Left $ TypeError "fst: expected a tuple"
evalDataStructures eval env (Snd e) = do
    v <- eval env e
    case v of
        VTuple (_:v2:_) -> Right v2
        VTuple _ -> Left $ TypeError "snd: tuple must have at least 2 elements"
        _ -> Left $ TypeError "snd: expected a tuple"
evalDataStructures eval env (Map f lst) = do
    fVal <- eval env f
    lstVal <- eval env lst
    case lstVal of
        VList values
          | isCallableValue fVal -> VList <$> mapM (applyCallable eval fVal) values
          | otherwise -> Left $ TypeError "map: first argument must be callable"
        _
          | isCallableValue fVal -> Left $ TypeError "map: second argument must be a list"
          | otherwise -> Left $ TypeError "map: invalid arguments"
evalDataStructures eval env (Filter f lst) = do
    fVal <- eval env f
    lstVal <- eval env lst
    case lstVal of
        VList values
          | isCallableValue fVal -> do
              results <- mapM (evalPredicate fVal) values
              Right $ VList [value | (value, True) <- results]
          | otherwise -> Left $ TypeError "filter: first argument must be callable"
        _
          | isCallableValue fVal -> Left $ TypeError "filter: second argument must be a list"
          | otherwise -> Left $ TypeError "filter: invalid arguments"
  where
    evalPredicate callable value = do
      result <- applyCallable eval callable value
      case result of
        VBool keep -> Right (value, keep)
        _ -> Left $ TypeError "filter: predicate must return a boolean"
evalDataStructures eval env (Foldl f acc lst) = do
    fVal <- eval env f
    accVal <- eval env acc
    lstVal <- eval env lst
    case lstVal of
        VList values
          | isCallableValue fVal -> foldM (foldStep fVal) accVal values
          | otherwise -> Left $ TypeError "foldl: first argument must be callable"
        _
          | isCallableValue fVal -> Left $ TypeError "foldl: third argument must be a list"
          | otherwise -> Left $ TypeError "foldl: invalid arguments"
  where
    foldStep callable current value = do
      partiallyApplied <- applyCallable eval callable current
      applyCallable eval partiallyApplied value
evalDataStructures eval env (Length lst) = do
    lstVal <- eval env lst
    case lstVal of
        VList vs -> Right $ VInt (length vs)
        _ -> Left $ TypeError "length: argument must be a list"
evalDataStructures eval env (Reverse lst) = do
    lstVal <- eval env lst
    case lstVal of
        VList vs -> Right $ VList (reverse vs)
        _ -> Left $ TypeError "reverse: argument must be a list"
evalDataStructures eval env (Take n lst) = do
    nVal <- eval env n
    lstVal <- eval env lst
    case (nVal, lstVal) of
        (VInt count, VList vs) -> Right $ VList (take count vs)
        (VInt _, _) -> Left $ TypeError "take: second argument must be a list"
        (_, VList _) -> Left $ TypeError "take: first argument must be an integer"
        _ -> Left $ TypeError "take: invalid arguments"
evalDataStructures eval env (Drop n lst) = do
    nVal <- eval env n
    lstVal <- eval env lst
    case (nVal, lstVal) of
        (VInt count, VList vs) -> Right $ VList (drop count vs)
        (VInt _, _) -> Left $ TypeError "drop: second argument must be a list"
        (_, VList _) -> Left $ TypeError "drop: first argument must be an integer"
        _ -> Left $ TypeError "drop: invalid arguments"
evalDataStructures eval env (Zip l1 l2) = do
    l1Val <- eval env l1
    l2Val <- eval env l2
    case (l1Val, l2Val) of
        (VList vs1, VList vs2) -> Right $ VList [VTuple [v1, v2] | (v1, v2) <- zip vs1 vs2]
        (VList _, _) -> Left $ TypeError "zip: second argument must be a list"
        (_, VList _) -> Left $ TypeError "zip: first argument must be a list"
        _ -> Left $ TypeError "zip: invalid arguments"
evalDataStructures _ _ _ = error "evalDataStructures called on non-data-structure expression"

evalDataStructuresIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalDataStructuresIO eval env (ListLit es) =
    bindResult (traverseResults (eval env) es) $ \values ->
      return $ Right $ VList values
evalDataStructuresIO eval env (Cons h t) =
    bindResult (eval env h) $ \headValue ->
      bindResult (eval env t) $ \tailValue ->
        return $ case tailValue of
          VList values -> Right $ VList (headValue:values)
          _ -> Left $ TypeError "Cons expects a list as its second argument"
evalDataStructuresIO eval env (Head e) = do
    result <- eval env e
    case result of
        Right (VList (h:_)) -> return $ Right h
        Right (VList []) -> return $ Left $ TypeError "Head of an empty list"
        Right _ -> return $ Left $ TypeError "Head expects a list"
        Left err -> return $ Left err
evalDataStructuresIO eval env (Tail e) = do
    result <- eval env e
    case result of
        Right (VList (_:t)) -> return $ Right $ VList t
        Right (VList []) -> return $ Left $ TypeError "Tail of an empty list"
        Right _ -> return $ Left $ TypeError "Tail expects a list"
        Left err -> return $ Left err
evalDataStructuresIO eval env (Null e) = do
    result <- eval env e
    case result of
        Right (VList l) -> return $ Right $ VBool (null l)
        Right _ -> return $ Left $ TypeError "Null expects a list"
        Left err -> return $ Left err
evalDataStructuresIO eval env (RecordLit fields) =
    bindResult (traverseResults evalField fields) $ \evaluatedFields ->
      return $ Right $ VRecord (Map.fromList evaluatedFields)
  where
    evalField (name, expression) =
      bindResult (eval env expression) $ \value -> return $ Right (name, value)
evalDataStructuresIO eval env (RecordAccess r field) = do
    result <- eval env r
    case result of
        Right (VRecord m) -> return $ case Map.lookup field m of
            Just fv -> Right fv
            Nothing -> Left $ RecordFieldNotFound field
        Right _ -> return $ Left $ TypeError "Record access expects a record"
        Left err -> return $ Left err
evalDataStructuresIO eval env (TupleLit exprs) =
    bindResult (traverseResults (eval env) exprs) $ \values ->
      return $ Right $ VTuple values
evalDataStructuresIO eval env (Fst e) = do
    result <- eval env e
    case result of
        Right (VTuple (v1:_)) -> return $ Right v1
        Right (VTuple []) -> return $ Left $ TypeError "fst: empty tuple"
        Right _ -> return $ Left $ TypeError "fst: expected a tuple"
        Left err -> return $ Left err
evalDataStructuresIO eval env (Snd e) = do
    result <- eval env e
    case result of
        Right (VTuple (_:v2:_)) -> return $ Right v2
        Right (VTuple _) -> return $ Left $ TypeError "snd: tuple must have at least 2 elements"
        Right _ -> return $ Left $ TypeError "snd: expected a tuple"
        Left err -> return $ Left err
evalDataStructuresIO eval env (Map f lst) =
    bindResult (eval env f) $ \functionValue ->
      bindResult (eval env lst) $ \listValue ->
        bindResult (resolveCallableIO functionValue) $ \callable ->
          case listValue of
            VList values
              | isCallableValue callable ->
                  bindResult
                    (traverseResults (applyCallableIO eval callable) values)
                    (return . Right . VList)
              | otherwise -> return $ Left $ TypeError "map: first argument must be callable"
            _
              | isCallableValue callable -> return $ Left $ TypeError "map: second argument must be a list"
              | otherwise -> return $ Left $ TypeError "map: invalid arguments"
evalDataStructuresIO eval env (Filter f lst) =
    bindResult (eval env f) $ \functionValue ->
      bindResult (eval env lst) $ \listValue ->
        bindResult (resolveCallableIO functionValue) $ \callable ->
          case listValue of
            VList values
              | isCallableValue callable ->
                  bindResult
                    (traverseResults (evalPredicate callable) values)
                    (return . Right . VList . map fst . filter snd)
              | otherwise -> return $ Left $ TypeError "filter: first argument must be callable"
            _
              | isCallableValue callable -> return $ Left $ TypeError "filter: second argument must be a list"
              | otherwise -> return $ Left $ TypeError "filter: invalid arguments"
  where
    evalPredicate callable value =
      bindResult (applyCallableIO eval callable value) $ \predicateValue ->
        case predicateValue of
          VBool keep -> return $ Right (value, keep)
          _ -> return $ Left $ TypeError "filter: predicate must return a boolean"
evalDataStructuresIO eval env (Foldl f acc lst) =
    bindResult (eval env f) $ \functionValue ->
      bindResult (eval env acc) $ \initialValue ->
        bindResult (eval env lst) $ \listValue ->
          bindResult (resolveCallableIO functionValue) $ \callable ->
            case listValue of
              VList values
                | isCallableValue callable ->
                    foldM (foldStep callable) (Right initialValue) values
                | otherwise -> return $ Left $ TypeError "foldl: first argument must be callable"
              _
                | isCallableValue callable -> return $ Left $ TypeError "foldl: third argument must be a list"
                | otherwise -> return $ Left $ TypeError "foldl: invalid arguments"
  where
    foldStep callable currentResult value =
      bindResult (return currentResult) $ \current ->
        bindResult (applyCallableIO eval callable current) $ \partiallyApplied ->
          applyCallableIO eval partiallyApplied value
evalDataStructuresIO eval env (Length lst) = do
    result <- eval env lst
    case result of
        Right (VList vs) -> return $ Right $ VInt (length vs)
        Right _ -> return $ Left $ TypeError "length: argument must be a list"
        Left err -> return $ Left err
evalDataStructuresIO eval env (Reverse lst) = do
    result <- eval env lst
    case result of
        Right (VList vs) -> return $ Right $ VList (reverse vs)
        Right _ -> return $ Left $ TypeError "reverse: argument must be a list"
        Left err -> return $ Left err
evalDataStructuresIO eval env (Take n lst) =
    bindResult (eval env n) $ \countValue ->
      bindResult (eval env lst) $ \listValue ->
        return $ case (countValue, listValue) of
          (VInt count, VList values) -> Right $ VList (take count values)
          (VInt _, _) -> Left $ TypeError "take: second argument must be a list"
          (_, VList _) -> Left $ TypeError "take: first argument must be an integer"
          _ -> Left $ TypeError "take: invalid arguments"
evalDataStructuresIO eval env (Drop n lst) =
    bindResult (eval env n) $ \countValue ->
      bindResult (eval env lst) $ \listValue ->
        return $ case (countValue, listValue) of
          (VInt count, VList values) -> Right $ VList (drop count values)
          (VInt _, _) -> Left $ TypeError "drop: second argument must be a list"
          (_, VList _) -> Left $ TypeError "drop: first argument must be an integer"
          _ -> Left $ TypeError "drop: invalid arguments"
evalDataStructuresIO eval env (Zip l1 l2) =
    bindResult (eval env l1) $ \leftValue ->
      bindResult (eval env l2) $ \rightValue ->
        return $ case (leftValue, rightValue) of
          (VList values1, VList values2) ->
            Right $ VList [VTuple [value1, value2] | (value1, value2) <- zip values1 values2]
          (VList _, _) -> Left $ TypeError "zip: second argument must be a list"
          (_, VList _) -> Left $ TypeError "zip: first argument must be a list"
          _ -> Left $ TypeError "zip: invalid arguments"
evalDataStructuresIO _ _ _ = error "evalDataStructuresIO called on non-data-structure expression"
