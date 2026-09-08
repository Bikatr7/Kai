module TypeChecker.Constraints where

import Control.Monad (unless)
import Control.Monad.Trans (lift)
import Data.List (nub, isPrefixOf)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import qualified Data.Set as Set
import DataDeclarations (standardDataTypeEnv)
import Syntax (Expr, SyntaxType)
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification (unifyInfer)

-- These are fixed language capabilities, not an extensible instance registry.
reducePredicates :: TypeEnv -> [Predicate] -> Either TypeError [Predicate]
reducePredicates env predicates = nub . concat <$> mapM reduce predicates
  where
    requirements = equalityRequirements (Map.union env standardDataTypeEnv)
    reduce (PredicateAt location predicate) =
      first (locateTypeError location) $ map (PredicateAt location) <$> reduce predicate
    reduce predicate@(Appendable ty) = case ty of
      TString -> pure []
      TList _ -> pure []
      TVar _ -> pure [predicate]
      _ -> Left (UnsatisfiedConstraint predicate)
    reduce (Equality ty) = equal ty
    equal ty = case ty of
      TInt -> pure []
      TBool -> pure []
      TString -> pure []
      TUnit -> pure []
      TVar _ -> pure [Equality ty]
      TFun _ _ -> Left (UnsatisfiedConstraint (Equality ty))
      TList a -> equal a
      TMaybe a -> equal a
      TEither a b -> both [a,b]
      TTuple values -> both values
      TRecord fields -> both (Map.elems fields)
      TOpenRecord {} -> do
        (fields,row) <- recordRow ty
        known <- both (Map.elems fields)
        case row of
          TRowEmpty -> pure known
          TRowVar _ -> pure (known ++ [Equality (TOpenRecord Map.empty row)])
          _ -> Left (KindMismatch row TRowEmpty)
      TCustom name arguments -> case Map.lookup name requirements of
        Just (positions,False) -> both [argument | (index,argument) <- zip [0..] arguments,
                                                  index `Set.member` positions]
        _ -> Left (UnsatisfiedConstraint (Equality ty))
      _ -> Left (UnsatisfiedConstraint (Equality ty))
    both types = concat <$> mapM equal types

-- Least fixed point over finite parameter sets and a non-comparability flag.
-- This handles phantom parameters and polymorphically recursive fields without
-- expanding recursive types (which may grow without bound).
type EqualityRequirements = Map.Map String (Set.Set Int, Bool)

equalityRequirements :: TypeEnv -> EqualityRequirements
equalityRequirements env = fixed initial
  where
    declarations = Map.fromList [(drop 6 key,(variables,Map.elems constructors)) |
      (key,Forall variables (TRecord constructors)) <- Map.toList env, "@type:" `isPrefixOf` key]
    initial = Map.map (const (Set.empty,False)) declarations
    fixed previous =
      let next = Map.map (analyze previous) declarations
      in if next == previous then next else fixed next
    analyze previous (variables,constructors) = combine
      [needed previous variables field | constructor <- constructors, field <- arguments constructor]
    arguments (TFun argument result) = argument : arguments result
    arguments _ = []
    needed previous variables ty = case ty of
      TVar variable -> (Set.fromList [index | (index,name) <- zip [0..] variables, name == variable],False)
      TFun _ _ -> (Set.empty,True)
      TMaybe a -> needed previous variables a
      TEither a b -> many [a,b]
      TList a -> needed previous variables a
      TTuple values -> many values
      TRecord fields -> many (Map.elems fields)
      TCustom name values -> case Map.lookup name previous of
        Just (positions,bad) -> let (required,nestedBad) = many
                                     [value | (index,value) <- zip [0..] values, index `Set.member` positions]
                               in (required,bad || nestedBad)
        Nothing -> (Set.empty,True)
      TOpenRecord {} -> (Set.empty,True)
      _ -> (Set.empty,False)
      where many = combine . map (needed previous variables)
    combine requirements = (Set.unions (map fst requirements),any snd requirements)

predicateVariables :: [Predicate] -> Set.Set String
predicateVariables = Set.unions . map (freeTypeVars . predicateType)

-- Default only existential Eq variables absent from the environment and result.
-- Append has no default, even if the result of the operation is discarded.
closePredicates :: TypeEnv -> Type -> [Predicate] -> Either TypeError [Predicate]
closePredicates env ty predicates = do
  reduced <- reducePredicates env predicates
  let protected = Set.union (freeTypeVarsEnv env) (freeTypeVars ty)
      equalityVars = predicateVariables [p | p <- reduced, Equality {} <- [unlocatedPredicate p]]
      appendVars = predicateVariables [p | p <- reduced, Appendable {} <- [unlocatedPredicate p]]
      defaults = equalityVars `Set.difference` Set.union protected appendVars
      rows = Set.unions (map (rowVariables . predicateType) reduced)
      substitution = Map.fromList [(name,if name `Set.member` rows then TRowEmpty else TUnit) |
                                   name <- Set.toList defaults]
  final <- reducePredicates env (map (applyPredicate substitution) reduced)
  case [predicate | predicate <- final,
        not (freeTypeVars (predicateType predicate) `Set.isSubsetOf` protected)] of
    predicate:_ -> Left (predicateFailure AmbiguousConstraint predicate)
    [] -> pure final

-- Requirements on monomorphic outer variables must reach their enclosing
-- definition; local generalized variables carry their predicates in the scheme.
generalizeConstrained :: TypeEnv -> Type -> [Predicate] -> TypeInfer Scheme
generalizeConstrained env ty predicates = do
  reduced <- lift $ closePredicates env ty predicates
  let outerVariables = freeTypeVarsEnv env
      floats predicate = freeTypeVars (predicateType predicate) `Set.isSubsetOf` outerVariables
  addPredicates (filter floats reduced)
  pure $ generalize env (qualifiedType (filter (not . floats) reduced) ty)

finishType :: TypeEnv -> Substitution -> Type -> [Predicate] -> TypeInfer Type
finishType env subst ty predicates = do
  let finalType = applySubst subst ty
  reduced <- lift $ closePredicates (applySubstEnv subst env) finalType (map (applyPredicate subst) predicates)
  pure $ qualifiedType reduced finalType

-- Ordinary annotations specialize by unification, as before. Their written
-- context must account for all requirements on newly generalized variables.
inferAnnotatedValue :: InferFunc -> TypeEnv -> Maybe SyntaxType -> Expr
                    -> TypeInfer (Substitution, Type, [Predicate])
inferAnnotatedValue infer env annotation expression = do
  ((subst,ty),predicates) <- capturePredicates (infer env expression)
  case annotation of
    Nothing -> pure (subst,ty,predicates)
    Just syntax -> do
      (declared,expected) <- inferQualifiedAnnotation env syntax
      constraint <- unifyInfer (applySubst subst ty) expected
      let final = composeSubst constraint subst
          base = applySubstEnv final env
          result = applySubst final ty
      actual <- lift $ closePredicates base result (map (applyPredicate final) predicates)
      promised <- lift $ closePredicates base result (map (applyPredicate final) declared)
      requireContext base promised actual
      pure (final,result,nub (actual ++ promised))

requireContext :: TypeEnv -> [Predicate] -> [Predicate] -> TypeInfer ()
requireContext env promised actual = do
  let outer = freeTypeVarsEnv env
      missing = [predicate | predicate <- actual, predicate `notElem` promised,
                  not (freeTypeVars (predicateType predicate) `Set.isSubsetOf` outer)]
  case missing of
    predicate:_ -> lift $ Left (predicateFailure MissingConstraint predicate)
    [] -> pure ()

-- Recursive annotations are checked with rigid quantified variables, preserving
-- their declared polymorphism and requiring their context to imply the body.
checkRecursiveScheme :: TypeEnv -> Scheme -> Scheme -> TypeInfer ()
checkRecursiveScheme env annotated inferred = do
  actualQualified <- instantiateRaw inferred
  let (actualPredicates,actual) = splitQualified actualQualified
      (promisedPredicates,expected) = splitQualified (skolemizeScheme annotated)
  subst <- case matchType actual expected of
    Left _ -> lift $ Left $ GeneralTypeError "Recursive definition does not satisfy its annotated polymorphic type"
    Right matched -> pure matched
  required <- lift $ reducePredicates env (map (applyPredicate subst) actualPredicates)
  promised <- lift $ reducePredicates env promisedPredicates
  unless (all (`elem` promised) required) $ case filter (`notElem` promised) required of
    predicate:_ -> lift $ Left (predicateFailure MissingConstraint predicate)
    [] -> pure ()
