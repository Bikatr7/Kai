module Evaluator.Functions where

import Evaluator.Types
import Syntax
import qualified Data.Map as Map

type EvalFunc = Env -> Expr -> Either RuntimeError Value
type EvalFuncIO = Env -> Expr -> IO (Either RuntimeError Value)

evalFunctions :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalFunctions _ env (Lambda param _maybeType body) =
  Right $ VFun param body env
evalFunctions eval env (App fun arg) = do
  funVal <- eval env fun
  argVal <- eval env arg
  case funVal of
    VFun param body closureEnv ->
      let env' = Map.insert param argVal closureEnv
      in eval env' body
    _ -> Left $ TypeError "Cannot apply non-function value"
evalFunctions _ _ _ = error "evalFunctions called on non-function expression"

evalFunctionsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalFunctionsIO _ env (Lambda param _maybeType body) =
  return $ Right $ VFun param body env
evalFunctionsIO eval env (App fun arg) = do
  funResult <- eval env fun
  argResult <- eval env arg
  case (funResult, argResult) of
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err
    (Right (VFun param body closureEnv), Right argVal) ->
      let env' = Map.insert param argVal closureEnv
      in eval env' body
    (Right _, Right _) -> return $ Left $ TypeError "Cannot apply non-function value"
evalFunctionsIO _ _ _ = error "evalFunctionsIO called on non-function expression"
