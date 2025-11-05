module TypeChecker (
    -- Core types
    Type(..),
    TypeEnv,
    Substitution,
    TypeError(..),
    TypeInfer,

    -- Type conversion
    syntaxTypeToType,

    -- Substitution operations
    freshTVar,
    applySubst,
    applySubstEnv,
    composeSubst,
    composeSubstList,
    freeTypeVars,
    freeTypeVarsEnv,

    -- Unification
    occurs,
    unify,

    -- Type inference
    infer,
    inferPattern,

    -- Public interface
    typeCheck,
    typeCheckWithEnv
) where

import Syntax (Expr)
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Inference
import TypeChecker.Patterns
import qualified Data.Map as Map
import Control.Monad.State (evalStateT)

typeCheck :: Expr -> Either TypeError Type
typeCheck expr = case evalStateT (infer Map.empty expr) 0 of
  Left err -> Left err
  Right (subst, ty) -> Right $ applySubst subst ty

typeCheckWithEnv :: TypeEnv -> Expr -> Either TypeError Type
typeCheckWithEnv env expr = case evalStateT (infer env expr) 0 of
  Left err -> Left err
  Right (subst, ty) -> Right $ applySubst subst ty