{-# LANGUAGE FlexibleContexts #-}
module Evaluator.Functions where

import Evaluator.Types
import Evaluator.Helpers (evalInIO)
import Control.Monad.Except (MonadError, throwError, liftEither, ExceptT(..), runExceptT)
import Syntax
import qualified Data.Map as Map
import Data.IORef (readIORef)


evalFunctionsWith :: MonadError RuntimeError m => (Value -> m Value) -> Eval m -> Eval m
evalFunctionsWith _ _ env (Lambda param _maybeType body) =
  pure $ VFun param body env
evalFunctionsWith resolve eval env (App fun arg) = do
  funVal <- eval env fun
  argVal <- eval env arg
  applyCallableWith resolve eval funVal argVal
evalFunctionsWith _ _ _ _ = error "evalFunctions called on non-function expression"

evalFunctions :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalFunctions = evalFunctionsWith pure

evalFunctionsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalFunctionsIO = evalInIO (evalFunctionsWith (ExceptT . resolveCallableIO))

isCallableValue :: Value -> Bool
isCallableValue VFun {} = True
isCallableValue VConstructor {} = True
isCallableValue _ = False

applyCallable :: EvalFunc -> Value -> Value -> Either RuntimeError Value
applyCallable = applyCallableWith pure

applyCallableWith :: MonadError RuntimeError m => (Value -> m Value) -> Eval m -> Value -> Value -> m Value
applyCallableWith resolve eval callable argument = do
  resolved <- resolve callable
  case resolved of
    VFun param body closureEnv ->
      eval (Map.insert param argument closureEnv) body
    VConstructor name arity collectedArgs ->
      liftEither $ finishConstructorApplication name arity (collectedArgs ++ [argument])
    _ -> throwError $ nonCallableError resolved

applyCallableIO :: EvalFuncIO -> Value -> Value -> IO (Either RuntimeError Value)
applyCallableIO eval callable argument = runExceptT $
  applyCallableWith (ExceptT . resolveCallableIO)
    (\env expr -> ExceptT $ eval env expr) callable argument

resolveCallableIO :: Value -> IO (Either RuntimeError Value)
resolveCallableIO = go []
  where
    go seen (VRef ref)
      | ref `elem` seen =
          return $ Left $ TypeError "Cannot apply cyclic recursive reference"
      | otherwise = readIORef ref >>= go (ref:seen)
    go _ (VUninitialized name) = return $ Left $ UninitializedRecursion name
    go _ value = return $ Right value

finishConstructorApplication :: String -> Int -> [Value] -> Either RuntimeError Value
finishConstructorApplication name arity arguments
  | length arguments == arity = Right $ VData name arguments
  | length arguments < arity = Right $ VConstructor name arity arguments
  | otherwise = Left $ TypeError $ "Constructor '" ++ name ++ "' received too many arguments"

nonCallableError :: Value -> RuntimeError
nonCallableError value =
  TypeError $ "Cannot apply non-callable value: " ++ take 200 (show value)
