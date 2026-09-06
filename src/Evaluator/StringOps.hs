{-# LANGUAGE FlexibleContexts #-}
module Evaluator.StringOps where

import Evaluator.Types
import Control.Monad.Except (MonadError, throwError, liftEither)
import Evaluator.Helpers (evalInIO, extractString)
import Syntax
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)

evalStringOps :: MonadError RuntimeError m => Eval m -> Eval m
evalStringOps eval env (Split delim str) = do
    delimVal <- eval env delim
    strVal <- eval env str
    case (delimVal, strVal) of
        (VStr d, VStr s) -> pure $ VList (map VStr (splitOn d s))
        (VStr _, _) -> throwError $ TypeError "split: second argument must be a string"
        (_, VStr _) -> throwError $ TypeError "split: first argument must be a string"
        _ -> throwError $ TypeError "split: invalid arguments"
evalStringOps eval env (Join delim lst) = do
    delimVal <- eval env delim
    lstVal <- eval env lst
    case (delimVal, lstVal) of
        (VStr d, VList vs) -> do
            strs <- liftEither $ mapM extractString vs
            pure $ VStr (intercalate d strs)
        (VStr _, _) -> throwError $ TypeError "join: second argument must be a list"
        (_, VList _) -> throwError $ TypeError "join: first argument must be a string"
        _ -> throwError $ TypeError "join: invalid arguments"
evalStringOps eval env (Trim str) = do
    strVal <- eval env str
    case strVal of
        VStr s -> pure $ VStr (dropWhile isSpace $ dropWhileEnd isSpace s)
        _ -> throwError $ TypeError "trim: argument must be a string"
  where
    dropWhileEnd p = reverse . dropWhile p . reverse
evalStringOps eval env (Replace old new str) = do
    oldVal <- eval env old
    newVal <- eval env new
    strVal <- eval env str
    case (oldVal, newVal, strVal) of
        (VStr o, VStr n, VStr s) -> pure $ VStr (intercalate n (splitOn o s))
        (VStr _, VStr _, _) -> throwError $ TypeError "replace: third argument must be a string"
        (VStr _, _, VStr _) -> throwError $ TypeError "replace: second argument must be a string"
        (_, VStr _, VStr _) -> throwError $ TypeError "replace: first argument must be a string"
        _ -> throwError $ TypeError "replace: invalid arguments"
evalStringOps eval env (StrLength str) = do
    strVal <- eval env str
    case strVal of
        VStr s -> pure $ VInt (length s)
        _ -> throwError $ TypeError "strLength: argument must be a string"
evalStringOps _ _ _ = error "evalStringOps called on non-string operation"

evalStringOpsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalStringOpsIO = evalInIO evalStringOps
