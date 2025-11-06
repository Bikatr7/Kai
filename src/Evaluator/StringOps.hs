module Evaluator.StringOps where

import Evaluator.Types
import Evaluator.Helpers (extractString)
import Syntax
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)

type EvalFunc = Env -> Expr -> Either RuntimeError Value

evalStringOps :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalStringOps eval env (Split delim str) = do
    delimVal <- eval env delim
    strVal <- eval env str
    case (delimVal, strVal) of
        (VStr d, VStr s) -> Right $ VList (map VStr (splitOn d s))
        (VStr _, _) -> Left $ TypeError "split: second argument must be a string"
        (_, VStr _) -> Left $ TypeError "split: first argument must be a string"
        _ -> Left $ TypeError "split: invalid arguments"
evalStringOps eval env (Join delim lst) = do
    delimVal <- eval env delim
    lstVal <- eval env lst
    case (delimVal, lstVal) of
        (VStr d, VList vs) -> do
            strs <- mapM extractString vs
            Right $ VStr (intercalate d strs)
        (VStr _, _) -> Left $ TypeError "join: second argument must be a list"
        (_, VList _) -> Left $ TypeError "join: first argument must be a string"
        _ -> Left $ TypeError "join: invalid arguments"
evalStringOps eval env (Trim str) = do
    strVal <- eval env str
    case strVal of
        VStr s -> Right $ VStr (dropWhile isSpace $ dropWhileEnd isSpace s)
        _ -> Left $ TypeError "trim: argument must be a string"
  where
    dropWhileEnd p = reverse . dropWhile p . reverse
evalStringOps eval env (Replace old new str) = do
    oldVal <- eval env old
    newVal <- eval env new
    strVal <- eval env str
    case (oldVal, newVal, strVal) of
        (VStr o, VStr n, VStr s) -> Right $ VStr (replaceAll o n s)
        (VStr _, VStr _, _) -> Left $ TypeError "replace: third argument must be a string"
        (VStr _, _, VStr _) -> Left $ TypeError "replace: second argument must be a string"
        (_, VStr _, VStr _) -> Left $ TypeError "replace: first argument must be a string"
        _ -> Left $ TypeError "replace: invalid arguments"
  where
    replaceAll old new str = intercalate new (splitOn old str)
evalStringOps eval env (StrLength str) = do
    strVal <- eval env str
    case strVal of
        VStr s -> Right $ VInt (length s)
        _ -> Left $ TypeError "strLength: argument must be a string"
evalStringOps _ _ _ = error "evalStringOps called on non-string operation"
