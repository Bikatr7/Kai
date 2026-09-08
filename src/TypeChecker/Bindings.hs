module TypeChecker.Bindings where

import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Constraints

inferBindings :: InferFunc -> TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferBindings infer env (Let var annotation value body) = do
  (subst,ty,predicates) <- inferAnnotatedValue infer env annotation value
  let base = applySubstEnv subst env
  scheme <- generalizeConstrained base (applySubst subst ty) (map (applyPredicate subst) predicates)
  let next = if var == "_" then base else Map.insert var scheme base
  (bodySubst,bodyType) <- infer next body
  pure (composeSubst bodySubst subst,bodyType)

inferBindings infer env (LetRec var annotation value body) = do
  when (var == "_") $ throwError (InvalidWildcard "Wildcard variables (_) cannot be used in recursive definitions")
  case annotation of
    Just syntax -> do
      (declared,expected) <- inferQualifiedAnnotation env syntax
      let assumed = generalize env (qualifiedType declared expected)
      ((subst,ty),predicates) <- capturePredicates (infer (Map.insert var assumed env) value)
      let base = applySubstEnv subst env
          annotated = applySubstScheme subst assumed
      actual <- generalizeConstrained base (applySubst subst ty) (map (applyPredicate subst) predicates)
      checkRecursiveScheme base annotated actual
      (bodySubst,bodyType) <- infer (Map.insert var annotated base) body
      pure (composeSubst bodySubst subst,bodyType)
    Nothing -> do
      expected <- freshTVar
      ((subst,ty),predicates) <- capturePredicates (infer (Map.insert var (monoScheme expected) env) value)
      constraint <- unifyInfer (applySubst subst expected) (applySubst subst ty)
      let final = composeSubst constraint subst
          base = applySubstEnv final env
      scheme <- generalizeConstrained base (applySubst final expected) (map (applyPredicate final) predicates)
      (bodySubst,bodyType) <- infer (Map.insert var scheme base) body
      pure (composeSubst bodySubst final,bodyType)

inferBindings infer env (TypeAnnotation expression annotation) = do
  (subst,ty,predicates) <- inferAnnotatedValue infer env (Just annotation) expression
  addPredicates (map (applyPredicate subst) predicates)
  pure (subst,ty)

inferBindings _ _ _ = error "inferBindings called on non-binding expression"
