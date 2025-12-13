module Evaluator.DataStructures where

import Evaluator.Types
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
    case (fVal, lstVal) of
        (VFun param body closureEnv, VList vs) -> do
            results <- mapM (\v -> eval (Map.insert param v closureEnv) body) vs
            Right $ VList results
        (_, VList _) -> Left $ TypeError "map: first argument must be a function"
        (VFun {}, _) -> Left $ TypeError "map: second argument must be a list"
        _ -> Left $ TypeError "map: invalid arguments"
evalDataStructures eval env (Filter f lst) = do
    fVal <- eval env f
    lstVal <- eval env lst
    case (fVal, lstVal) of
        (VFun param body closureEnv, VList vs) -> do
            results <- mapM (\v -> do
                r <- eval (Map.insert param v closureEnv) body
                case r of
                    VBool b -> Right (v, b)
                    _ -> Left $ TypeError "filter: predicate must return a boolean") vs
            Right $ VList [v | (v, True) <- results]
        (_, VList _) -> Left $ TypeError "filter: first argument must be a function"
        (VFun {}, _) -> Left $ TypeError "filter: second argument must be a list"
        _ -> Left $ TypeError "filter: invalid arguments"
evalDataStructures eval env (Foldl f acc lst) = do
    fVal <- eval env f
    accVal <- eval env acc
    lstVal <- eval env lst
    case (fVal, lstVal) of
        (VFun param1 body1 closureEnv, VList vs) -> do
            let foldStep currAcc v = do
                    case eval (Map.insert param1 currAcc closureEnv) body1 of
                        Right (VFun param2 body2 closureEnv2) ->
                            eval (Map.insert param2 v closureEnv2) body2
                        Right _ -> Left $ TypeError "foldl: function must take two arguments"
                        Left err -> Left err
            foldM foldStep accVal vs
        (_, VList _) -> Left $ TypeError "foldl: first argument must be a function"
        (VFun {}, _) -> Left $ TypeError "foldl: third argument must be a list"
        _ -> Left $ TypeError "foldl: invalid arguments"
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
evalDataStructuresIO eval env (ListLit es) = do
    results <- mapM (eval env) es
    case sequence results of
        Right vs -> return $ Right $ VList vs
        Left err -> return $ Left err
evalDataStructuresIO eval env (Cons h t) = do
    hResult <- eval env h
    tResult <- eval env t
    case (hResult, tResult) of
        (Right vh, Right vt) -> case vt of
            VList l -> return $ Right $ VList (vh:l)
            _ -> return $ Left $ TypeError "Cons expects a list as its second argument"
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
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
evalDataStructuresIO eval env (RecordLit fields) = do
    let evalField (name, e) = do
            result <- eval env e
            case result of
                Right v -> return $ Right (name, v)
                Left err -> return $ Left err
    results <- mapM evalField fields
    case sequence results of
        Right evaledFields -> return $ Right $ VRecord (Map.fromList evaledFields)
        Left err -> return $ Left err
evalDataStructuresIO eval env (RecordAccess r field) = do
    result <- eval env r
    case result of
        Right (VRecord m) -> return $ case Map.lookup field m of
            Just fv -> Right fv
            Nothing -> Left $ RecordFieldNotFound field
        Right _ -> return $ Left $ TypeError "Record access expects a record"
        Left err -> return $ Left err
evalDataStructuresIO eval env (TupleLit exprs) = do
    results <- mapM (eval env) exprs
    case sequence results of
        Right vals -> return $ Right $ VTuple vals
        Left err -> return $ Left err
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
evalDataStructuresIO eval env (Map f lst) = do
    fResult <- eval env f
    lstResult <- eval env lst
    case (fResult, lstResult) of
        (Right fVal, Right lstVal) -> case (fVal, lstVal) of
            (VFun param body closureEnv, VList vs) -> do
                results <- mapM (\v -> eval (Map.insert param v closureEnv) body) vs
                case sequence results of
                    Right vs' -> return $ Right $ VList vs'
                    Left err -> return $ Left err
            (_, VList _) -> return $ Left $ TypeError "map: first argument must be a function"
            (VFun {}, _) -> return $ Left $ TypeError "map: second argument must be a list"
            _ -> return $ Left $ TypeError "map: invalid arguments"
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
evalDataStructuresIO eval env (Filter f lst) = do
    fResult <- eval env f
    lstResult <- eval env lst
    case (fResult, lstResult) of
        (Right fVal, Right lstVal) -> case (fVal, lstVal) of
            (VFun param body closureEnv, VList vs) -> do
                results <- mapM (\v -> do
                    r <- eval (Map.insert param v closureEnv) body
                    case r of
                        Right (VBool b) -> return $ Right (v, b)
                        Right _ -> return $ Left $ TypeError "filter: predicate must return a boolean"
                        Left err -> return $ Left err) vs
                case sequence results of
                    Right results' -> return $ Right $ VList [v | (v, True) <- results']
                    Left err -> return $ Left err
            (_, VList _) -> return $ Left $ TypeError "filter: first argument must be a function"
            (VFun {}, _) -> return $ Left $ TypeError "filter: second argument must be a list"
            _ -> return $ Left $ TypeError "filter: invalid arguments"
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
evalDataStructuresIO eval env (Foldl f acc lst) = do
    fResult <- eval env f
    accResult <- eval env acc
    lstResult <- eval env lst
    case (fResult, accResult, lstResult) of
        (Right fVal, Right accVal, Right lstVal) -> case (fVal, lstVal) of
            (VFun param1 body1 closureEnv, VList vs) -> do
                let foldStep currAccEither v = do
                        case currAccEither of
                            Left err -> return $ Left err
                            Right currAcc -> do
                                result <- eval (Map.insert param1 currAcc closureEnv) body1
                                case result of
                                    Right (VFun param2 body2 closureEnv2) -> do
                                        result2 <- eval (Map.insert param2 v closureEnv2) body2
                                        case result2 of
                                            Right val -> return $ Right val
                                            Left err -> return $ Left err
                                    Right _ -> return $ Left $ TypeError "foldl: function must take two arguments"
                                    Left err -> return $ Left err
                foldM foldStep (Right accVal) vs
            (_, VList _) -> return $ Left $ TypeError "foldl: first argument must be a function"
            (VFun {}, _) -> return $ Left $ TypeError "foldl: third argument must be a list"
            _ -> return $ Left $ TypeError "foldl: invalid arguments"
        (Left err, _, _) -> return $ Left err
        (_, Left err, _) -> return $ Left err
        (_, _, Left err) -> return $ Left err
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
evalDataStructuresIO eval env (Take n lst) = do
    nResult <- eval env n
    lstResult <- eval env lst
    case (nResult, lstResult) of
        (Right (VInt count), Right (VList vs)) -> return $ Right $ VList (take count vs)
        (Right (VInt _), Right _) -> return $ Left $ TypeError "take: second argument must be a list"
        (Right _, Right (VList _)) -> return $ Left $ TypeError "take: first argument must be an integer"
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        _ -> return $ Left $ TypeError "take: invalid arguments"
evalDataStructuresIO eval env (Drop n lst) = do
    nResult <- eval env n
    lstResult <- eval env lst
    case (nResult, lstResult) of
        (Right (VInt count), Right (VList vs)) -> return $ Right $ VList (drop count vs)
        (Right (VInt _), Right _) -> return $ Left $ TypeError "drop: second argument must be a list"
        (Right _, Right (VList _)) -> return $ Left $ TypeError "drop: first argument must be an integer"
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        _ -> return $ Left $ TypeError "drop: invalid arguments"
evalDataStructuresIO eval env (Zip l1 l2) = do
    l1Result <- eval env l1
    l2Result <- eval env l2
    case (l1Result, l2Result) of
        (Right (VList vs1), Right (VList vs2)) -> return $ Right $ VList [VTuple [v1, v2] | (v1, v2) <- zip vs1 vs2]
        (Right (VList _), Right _) -> return $ Left $ TypeError "zip: second argument must be a list"
        (Right _, Right (VList _)) -> return $ Left $ TypeError "zip: first argument must be a list"
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        _ -> return $ Left $ TypeError "zip: invalid arguments"
evalDataStructuresIO _ _ _ = error "evalDataStructuresIO called on non-data-structure expression"
