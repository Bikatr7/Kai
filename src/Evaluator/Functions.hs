module Evaluator.Functions where

import Evaluator.Types
import Evaluator.Helpers (bindResult)
import Syntax
import qualified Data.Map as Map
import Data.IORef (readIORef)

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalFunctions :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalFunctions _ env (Lambda param _maybeType body) =
  Right $ VFun param body env
evalFunctions eval env (App fun arg) = do
  funVal <- eval env fun
  argVal <- eval env arg
  applyCallable eval funVal argVal
evalFunctions _ _ _ = error "evalFunctions called on non-function expression"

evalFunctionsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalFunctionsIO _ env (Lambda param _maybeType body) =
  return $ Right $ VFun param body env
evalFunctionsIO eval env (App fun arg) =
  bindResult (eval env fun) $ \funVal ->
    bindResult (eval env arg) $ \argVal -> applyCallableIO eval funVal argVal
evalFunctionsIO _ _ _ = error "evalFunctionsIO called on non-function expression"

isCallableValue :: Value -> Bool
isCallableValue VFun {} = True
isCallableValue VConstructor {} = True
isCallableValue _ = False

applyCallable :: EvalFunc -> Value -> Value -> Either RuntimeError Value
applyCallable eval callable argument = case callable of
  VFun param body closureEnv ->
    eval (Map.insert param argument closureEnv) body
  VConstructor name arity collectedArgs ->
    finishConstructorApplication name arity (collectedArgs ++ [argument])
  _ -> Left $ nonCallableError callable

applyCallableIO :: EvalFuncIO -> Value -> Value -> IO (Either RuntimeError Value)
applyCallableIO eval callable argument =
  bindResult (resolveCallableIO callable) $ \resolved -> case resolved of
    VFun param body closureEnv ->
      eval (Map.insert param argument closureEnv) body
    VConstructor name arity collectedArgs ->
      return $ finishConstructorApplication name arity (collectedArgs ++ [argument])
    _ -> return $ Left $ nonCallableError resolved

resolveCallableIO :: Value -> IO (Either RuntimeError Value)
resolveCallableIO = go []
  where
    go seen (VRef ref)
      | ref `elem` seen =
          return $ Left $ TypeError "Cannot apply cyclic recursive reference"
      | otherwise = readIORef ref >>= go (ref:seen)
    go _ value = return $ Right value

finishConstructorApplication :: String -> Int -> [Value] -> Either RuntimeError Value
finishConstructorApplication name arity arguments
  | length arguments == arity = Right $ VData name arguments
  | length arguments < arity = Right $ VConstructor name arity arguments
  | otherwise = Left $ TypeError $ "Constructor '" ++ name ++ "' received too many arguments"

nonCallableError :: Value -> RuntimeError
nonCallableError value =
  TypeError $ "Cannot apply non-callable value: " ++ take 200 (show value)
