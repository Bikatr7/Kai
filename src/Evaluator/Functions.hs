module Evaluator.Functions where

import Evaluator.Types
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
    (Right funVal, Right argVal) -> do
      -- Dereference VRef if needed
      actualFun <- case funVal of
        VRef ref -> do
          val <- readIORef ref
          -- If it's another VRef, dereference recursively
          case val of
            VRef ref2 -> readIORef ref2
            _ -> return val
        _ -> return funVal
      case actualFun of
        VFun param body closureEnv -> do
          let env' = Map.insert param argVal closureEnv
          eval env' body
        _ -> do
          -- Check what's actually in the VRef
          case funVal of
            VRef ref -> do
              val <- readIORef ref
              return $ Left $ TypeError ("Cannot apply non-function value: VRef contains " ++ take 200 (show val))
            _ -> return $ Left $ TypeError ("Cannot apply non-function value: got " ++ take 200 (show actualFun))
evalFunctionsIO _ _ _ = error "evalFunctionsIO called on non-function expression"
