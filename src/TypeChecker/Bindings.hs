module TypeChecker.Bindings where

import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification

type InferFunc = TypeEnv -> Expr -> TypeInfer (Substitution, Type)

inferBindings :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferBindings infer env (Let var maybeType val body) = do
  (s1, valType) <- infer env val
  finalValType <- case maybeType of
    Just sType -> do
      let annotatedType = syntaxTypeToType sType
      s2 <- lift $ unify valType annotatedType
      return $ applySubst s2 annotatedType
    Nothing -> return valType
  let env' = if var == "_" then env else Map.insert var finalValType env
  (s2, bodyType) <- infer env' body
  let finalSubst = composeSubst s2 s1
  return (finalSubst, bodyType)

inferBindings infer env (LetRec var maybeType val body) = do
  when (var == "_") $ throwError (InvalidWildcard "Wildcard variables (_) cannot be used in recursive definitions")
  recType <- case maybeType of
    Just sType -> return $ syntaxTypeToType sType
    Nothing -> freshTVar
  let env' = Map.insert var recType env
  (s1, valType) <- infer env' val
  s2 <- lift $ unify (applySubst s1 recType) (applySubst s1 valType)
  let combinedSubst = composeSubst s2 s1
  let finalEnv = Map.insert var (applySubst combinedSubst recType) env
  (s3, bodyType) <- infer finalEnv body
  let finalSubst = composeSubst s3 combinedSubst
  return (finalSubst, bodyType)

inferBindings infer env (TypeAnnotation e sType) = do
  let annotatedType = syntaxTypeToType sType
  (s, exprType) <- infer env e
  s2 <- lift $ unify exprType annotatedType
  let finalSubst = composeSubst s2 s
  return (finalSubst, applySubst finalSubst annotatedType)

inferBindings _ _ _ = error "inferBindings called on non-binding expression"
