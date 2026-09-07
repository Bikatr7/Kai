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
    typeCheckWithEnv,
    typeCheckProgram,
    typeCheckProgramWithDirIO,
    inferProgramWithEnvIO,
    inferDefinitionType,
    inferRecursiveDefinitions
) where

import Syntax (Expr(..), SyntaxType, Program(..), TopLevel(..))
import DataDeclarations (registerDataDeclaration, mergeTypeEnvironments)
import TopLevelRecursion (collectConsecutiveLetrecs, dependencyOrderedLetrecGroups)
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Inference
import TypeChecker.Patterns
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (evalStateT)
import Control.Monad (foldM, unless, when)
import Control.Monad.Trans (lift)
import Text.Read (readMaybe)

-- Start beyond existing free inference variables when entering a saved environment.
runInference :: TypeEnv -> TypeInfer a -> Either TypeError a
runInference env action = evalStateT action seed
  where
    seed = maximum (0 : [n + 1 | 't':digits <- Set.toList (freeTypeVarsEnv env),
                                 Just n <- [readMaybe digits]])

typeCheck :: Expr -> Either TypeError Type
typeCheck = typeCheckWithEnv Map.empty

typeCheckWithEnv :: TypeEnv -> Expr -> Either TypeError Type
typeCheckWithEnv env expr = runInference env $ do
  (subst, ty) <- infer env expr
  return $ applySubst subst ty

inferDefinitionType :: TypeEnv -> String -> Maybe SyntaxType -> Expr -> Either TypeError (TypeEnv, Type)
inferDefinitionType env name annotation expr = runInference env $ do
  (subst, ty) <- infer env expr
  checked <- case annotation of
    Nothing -> return subst
    Just syntax -> do
      expected <- inferAnnotation env syntax
      constraint <- lift $ unify (applySubst subst ty) expected
      return $ composeSubst constraint subst
  let finalType = applySubst checked ty
      baseEnv = applySubstEnv checked env
      newEnv = if name == "_" then baseEnv else Map.insert name (generalize baseEnv finalType) baseEnv
  return (newEnv, finalType)

-- One fresh-variable supply and accumulated substitution per recursive block.
-- Independent components are generalized before checking their dependants.
inferRecursiveDefinitions :: TypeEnv -> [TopLevel] -> Either TypeError (TypeEnv, [(String, Type)])
inferRecursiveDefinitions env definitions = runInference env $ do
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
      subst <- foldM (checkBody outer mutual) Map.empty assumed
      let base = applySubstEnv subst outer
          bindings = [(name, generalize base (applySubst subst ty)) |
                        (name, _, scheme, _) <- assumed, let ty = schemeType scheme]
      return $ Map.union (Map.fromList bindings) base
    assume outer (TLDef name annotation (LetRec _ _ value _)) = do
      when (name == "_") $ lift $ Left $ InvalidWildcard "Wildcard variables (_) cannot be used in recursive definitions"
      scheme <- case annotation of
        Nothing -> monoScheme <$> freshTVar
        Just syntax -> generalize outer <$> inferAnnotation outer syntax
      return (name, annotation, scheme, value)
    assume _ _ = lift $ Left $ GeneralTypeError "Expected recursive definition"
    checkBody outer mutual accumulated (_, annotation, assumed, value) = do
      (subst, ty) <- infer (applySubstEnv accumulated mutual) value
      let combined = composeSubst subst accumulated
          actual = applySubst combined ty
          expected = applySubst combined (schemeType assumed)
      case annotation of
        Nothing -> do
          constraint <- lift $ unify expected actual
          return $ composeSubst constraint combined
        Just _ -> do
          let base = applySubstEnv combined outer
          matches <- schemeIsInstanceOf (generalize base expected) (generalize base actual)
          unless matches $ lift $ Left $ GeneralTypeError "Recursive definition does not satisfy its annotated polymorphic type"
          return combined

-- Shared non-import step used by file, module and REPL checking.
checkStep :: TypeEnv -> [TopLevel] -> Either TypeError (TypeEnv, Type, [TopLevel])
checkStep env (TLExpr expr : rest) = runInference env $ do
  (subst, ty) <- infer env expr
  return (applySubstEnv subst env, applySubst subst ty, rest)
checkStep env definitions@(TLDef name annotation expr : rest) = do
  case expr of
    LetRec {} -> do
      let (recursive, remaining) = collectConsecutiveLetrecs definitions
      (next, _) <- inferRecursiveDefinitions env recursive
      return (next, TUnit, remaining)
    _ -> do
      (next, _) <- inferDefinitionType env name annotation expr
      return (next, TUnit, rest)
checkStep env (TLData name vars constructors : rest) = do
  next <- registerDataDeclaration env name vars constructors
  return (next, TUnit, rest)
checkStep env (_ : rest) = Right (env, TUnit, rest)
checkStep env [] = Right (env, TUnit, [])

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
inferProgramWithEnvIO loader dir initial (Program levels) = go initial levels
  where
    go env [] = return $ Right (env, TUnit)
    go env (TLImport name : rest) = do
      imported <- loader dir name
      case imported >>= (`mergeTypeEnvironments` env) of
        Left err -> return $ Left err
        Right next -> go next rest
    go env remaining = case checkStep env remaining of
      Left err -> return $ Left err
      Right (next, ty, rest) -> if null rest then return $ Right (next, ty) else go next rest
