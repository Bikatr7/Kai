module TypeChecker.Functions where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

type InferFunc = TypeEnv -> Expr -> TypeInfer (Substitution, Type)

inferFunctions :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferFunctions infer env (Lambda param maybeType body) = do
  paramType <- case maybeType of
    Just sType -> return $ syntaxTypeToType sType
    Nothing -> freshTVar
  let env' = Map.insert param paramType env
  (s1, bodyType) <- infer env' body
  let finalParamType = applySubst s1 paramType
  return (s1, TFun finalParamType bodyType)

inferFunctions infer env (App fun arg) = do
  resultType <- freshTVar
  (s1, funType) <- infer env fun
  (s2, argType) <- infer env arg
  s3 <- lift $ unify (applySubst s2 funType) (TFun argType resultType)
  let finalSubst = composeSubstList [s1, s2, s3]
  return (finalSubst, applySubst s3 resultType)

inferFunctions _ _ _ = error "inferFunctions called on non-function expression"
