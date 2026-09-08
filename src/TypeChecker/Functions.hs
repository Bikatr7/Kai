module TypeChecker.Functions where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

inferFunctions :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferFunctions infer env (Lambda param maybeType body) = do
  paramType <- case maybeType of
    Just sType -> inferAnnotation env sType
    Nothing -> freshTVar
  let env' = Map.insert param (monoScheme paramType) env
  (s1, bodyType) <- infer env' body
  let finalParamType = applySubst s1 paramType
  return (s1, TFun finalParamType bodyType)

inferFunctions infer env (App fun arg) = do
  resultType <- freshTVar
  (s1, funType) <- infer env fun
  (s2, argType) <- infer (applySubstEnv s1 env) arg
  s3 <- unifyInfer (applySubst s2 funType) (TFun argType resultType)
  let finalSubst = composeSubst s3 (composeSubst s2 s1)
  return (finalSubst, applySubst finalSubst resultType)

inferFunctions infer env (Fix e) = do
  (s1, eType) <- infer env e
  resultType <- freshTVar
  s2 <- unifyInfer (applySubst s1 eType) (TFun resultType resultType)
  let finalSubst = composeSubst s2 s1
  return (finalSubst, applySubst finalSubst resultType)

inferFunctions _ _ _ = error "inferFunctions called on non-function expression"
