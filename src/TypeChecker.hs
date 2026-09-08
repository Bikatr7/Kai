{-# LANGUAGE TupleSections #-}

module TypeChecker (
    -- Core types
    Type(..),
    Predicate(..),
    TypeEnv,
    Substitution,
    TypeError(..),
    TypeWarning(..),
    stripTypeLocation,
    checkCoverage,
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
    typeCheckWithEnv,
    typeCheckWithWarnings,
    typeCheckProgram,
    typeCheckProgramWithDirIO,
    inferProgramWithEnvIO,
    inferProgramWithWarningsIO,
    inferDefinitionType,
    inferRecursiveDefinitions
) where

import Syntax (Expr(..), SyntaxType, Program(..), TopLevel(..), unlocatedTopLevel)
import Data.Bifunctor (first)
import DataDeclarations (registerDataDeclaration, mergeTypeEnvironments)
import TopLevelRecursion (collectConsecutiveLetrecs, dependencyOrderedLetrecGroups)
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Inference
import TypeChecker.Coverage (checkCoverage)
import TypeChecker.Constraints
import TypeChecker.Patterns
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (runStateT)
import Control.Monad (foldM, unless, when)
import Control.Monad.Trans (lift)
import Text.Read (readMaybe)

-- Start beyond existing free inference variables when entering a saved environment.
runInference :: TypeEnv -> TypeInfer a -> Either TypeError a
runInference env action = fst <$> runInferenceWithWarnings env action

runInferenceWithWarnings :: TypeEnv -> TypeInfer a -> Either TypeError (a,[TypeWarning])
runInferenceWithWarnings env action = do
  (result,state) <- runStateT action (initialInferState seed)
  pure (result,inferredWarnings state)
  where
    seed = maximum (0 : [n + 1 | 't':digits <- Set.toList (freeTypeVarsEnv env),
                                 Just n <- [readMaybe digits]])

typeCheck :: Expr -> Either TypeError Type
typeCheck = typeCheckWithEnv Map.empty

typeCheckWithEnv :: TypeEnv -> Expr -> Either TypeError Type
typeCheckWithEnv env expr = fst <$> typeCheckWithWarnings env expr

typeCheckWithWarnings :: TypeEnv -> Expr -> Either TypeError (Type,[TypeWarning])
typeCheckWithWarnings env expr = runInferenceWithWarnings env $ do
  ((subst, ty), predicates) <- capturePredicates (infer env expr)
  finishType env subst ty predicates

inferDefinitionType :: TypeEnv -> String -> Maybe SyntaxType -> Expr -> Either TypeError (TypeEnv, Type)
inferDefinitionType env name annotation expr = fst <$> inferDefinitionWithWarnings env name annotation expr

inferDefinitionWithWarnings :: TypeEnv -> String -> Maybe SyntaxType -> Expr -> Either TypeError ((TypeEnv,Type),[TypeWarning])
inferDefinitionWithWarnings env name annotation expr = runInferenceWithWarnings env $ do
  (subst,ty,predicates) <- inferAnnotatedValue infer env annotation expr
  let base = applySubstEnv subst env
  scheme <- generalizeConstrained base (applySubst subst ty) (map (applyPredicate subst) predicates)
  let next = if name == "_" then base else Map.insert name scheme base
  return (next,schemeType scheme)

-- One fresh-variable supply and accumulated substitution per recursive block.
-- Independent components are generalized before checking their dependants.
inferRecursiveDefinitions :: TypeEnv -> [TopLevel] -> Either TypeError (TypeEnv, [(String, Type)])
inferRecursiveDefinitions env definitions = fst <$> inferRecursiveWithWarnings env definitions

inferRecursiveWithWarnings :: TypeEnv -> [TopLevel] -> Either TypeError ((TypeEnv,[(String,Type)]),[TypeWarning])
inferRecursiveWithWarnings env locatedDefinitions = runInferenceWithWarnings env $ do
  let definitions = map unlocatedTopLevel locatedDefinitions
  let names = [name | TLDef name _ _ <- definitions]
  unless (length names == Set.size (Set.fromList names)) $
    lift $ Left $ GeneralTypeError "Duplicate name in recursive binding block"
  finalEnv <- foldM inferGroup env (dependencyOrderedLetrecGroups definitions)
  return (finalEnv, [(name, schemeType scheme) | TLDef name _ _ <- definitions,
                    Just scheme <- [Map.lookup name finalEnv]])
  where
    inferGroup outer group = do
      assumed <- mapM (assume outer) group
      let mutual = Map.union (Map.fromList [(name, scheme) | (name, _, scheme, _) <- assumed]) outer
      (subst,bodies) <- foldM (checkBody mutual) (Map.empty,[]) assumed
      let base = applySubstEnv subst outer
          types = [applySubst subst (snd (splitQualified (schemeType scheme))) | (_,_,scheme,_) <- assumed]
          predicates = map (applyPredicate subst) (concat [ps | (_,_,ps) <- bodies])
      reduced <- lift $ closePredicates base (TTuple types) predicates
      bindings <- mapM (finishBinding base subst reduced) bodies
      return $ Map.union (Map.fromList bindings) base
    assume outer (TLDef name annotation (LetRec _ _ value _)) = do
      when (name == "_") $ lift $ Left $ InvalidWildcard "Wildcard variables (_) cannot be used in recursive definitions"
      scheme <- case annotation of
        Nothing -> monoScheme <$> freshTVar
        Just syntax -> do
          (predicates,ty) <- inferQualifiedAnnotation outer syntax
          pure $ generalize outer (qualifiedType predicates ty)
      return (name, annotation, scheme, value)
    assume _ _ = lift $ Left $ GeneralTypeError "Expected recursive definition"
    checkBody mutual (accumulated,bodies) entry@(_,annotation,assumed,value) = do
      ((subst,ty),predicates) <- capturePredicates (infer (applySubstEnv accumulated mutual) value)
      let combined = composeSubst subst accumulated
          actual = applySubst combined ty
          expected = applySubst combined (snd (splitQualified (schemeType assumed)))
      final <- case annotation of
        Nothing -> do
          constraint <- unifyInfer expected actual
          pure $ composeSubst constraint combined
        Just _ -> pure combined
      return (final,bodies ++ [(entry,ty,predicates)])
    finishBinding base subst predicates ((name,annotation,assumed,_),actual,localPredicates) = do
      case annotation of
        Just _ -> do
          inferred <- generalizeConstrained base (applySubst subst actual) (map (applyPredicate subst) localPredicates)
          let annotated = applySubstScheme subst assumed
          checkRecursiveScheme base annotated inferred
          -- Also reject impossible or ambiguous contexts on unused declarations.
          let (declared,ty) = splitQualified (schemeType annotated)
          reduced <- lift $ closePredicates base ty declared
          pure (name,generalize base (qualifiedType reduced ty))
        Nothing -> do
          let ty = applySubst subst (schemeType assumed)
              relevant predicate = not (Set.null (Set.intersection
                (freeTypeVars (predicateType predicate)) (Set.union (freeTypeVars ty) (freeTypeVarsEnv base))))
          scheme <- generalizeConstrained base ty (filter relevant predicates)
          pure (name,scheme)

-- Shared non-import step used by file, module and REPL checking.
checkStep :: TypeEnv -> [TopLevel] -> Either TypeError (TypeEnv, Type, [TopLevel])
checkStep env levels = fst <$> checkStepWithWarnings env levels

checkStepWithWarnings :: TypeEnv -> [TopLevel] -> Either TypeError ((TypeEnv, Type, [TopLevel]),[TypeWarning])
checkStepWithWarnings env (TLAt location level : rest) =
  first (locateTypeError location) (checkStepWithWarnings env (level:rest))
checkStepWithWarnings env (TLExpr expr : rest) = runInferenceWithWarnings env $ do
  ((subst, ty),predicates) <- capturePredicates (infer env expr)
  finalType <- finishType env subst ty predicates
  return (applySubstEnv subst env, finalType, rest)
checkStepWithWarnings env definitions@(TLDef name annotation expr : rest) = do
  case expr of
    LetRec {} -> do
      let (recursive, remaining) = collectConsecutiveLetrecs definitions
      ((next, _),warnings) <- inferRecursiveWithWarnings env recursive
      return ((next, TUnit, remaining),warnings)
    _ -> do
      ((next, _),warnings) <- inferDefinitionWithWarnings env name annotation expr
      return ((next, TUnit, rest),warnings)
checkStepWithWarnings env (TLData name vars constructors : rest) = do
  next <- registerDataDeclaration env name vars constructors
  return ((next, TUnit, rest),[])
checkStepWithWarnings env (_ : rest) = Right ((env, TUnit, rest),[])
checkStepWithWarnings env [] = Right ((env, TUnit, []),[])

typeCheckProgram :: Program -> Either TypeError Type
typeCheckProgram (Program levels) = go Map.empty levels
  where
    go _ [] = Right TUnit
    go env remaining = do
      (next, ty, rest) <- checkStep env remaining
      if null rest then Right ty else go next rest

typeCheckProgramWithDirIO :: (FilePath -> String -> IO (Either TypeError TypeEnv)) -> FilePath -> Program -> IO (Either TypeError Type)
typeCheckProgramWithDirIO loader dir program = fmap snd <$> inferProgramWithEnvIO loader dir Map.empty program

inferProgramWithEnvIO :: (FilePath -> String -> IO (Either TypeError TypeEnv)) -> FilePath -> TypeEnv -> Program -> IO (Either TypeError (TypeEnv, Type))
inferProgramWithEnvIO loader dir initial program = fmap fst <$>
  inferProgramWithWarningsIO (\path name -> fmap (,[]) <$> loader path name) dir initial program

inferProgramWithWarningsIO :: (FilePath -> String -> IO (Either TypeError (TypeEnv,[TypeWarning])))
                          -> FilePath -> TypeEnv -> Program -> IO (Either TypeError ((TypeEnv, Type),[TypeWarning]))
inferProgramWithWarningsIO loader dir initial (Program levels) = go initial [] levels
  where
    go env warnings [] = return $ Right ((env, TUnit),warnings)
    go env warnings (TLAt location (TLImport name) : rest) = loadImport env warnings (Just location) name rest
    go env warnings (TLImport name : rest) = loadImport env warnings Nothing name rest
    go env warnings remaining = case checkStepWithWarnings env remaining of
      Left err -> return $ Left err
      Right ((next,ty,rest),more) -> if null rest then return $ Right ((next,ty),warnings ++ more)
        else go next (warnings ++ more) rest
    loadImport env warnings location name rest = do
      result <- loader dir name
      let atImport failure = case location of
            Nothing -> failure
            Just source -> case failure of
              TypeAt {} -> TypeImport source name failure
              TypeImport {} -> TypeImport source name failure
              _ -> locateTypeError source failure
          imported = first atImport result
      case imported of
        Left err -> return $ Left err
        Right (incoming,more) -> case mergeTypeEnvironments incoming env of
          Left err -> return $ Left (atImport err)
          Right next -> go next (warnings ++ more) rest
