{-# LANGUAGE LambdaCase #-}

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
    typeCheckProgramWithDirIO
) where

import Syntax (Expr(..), Program(..), TopLevel(..))
import DataDeclarations (dataConstructorsTypeEnv)
import TopLevelRecursion (collectConsecutiveLetrecs, dependencyOrderedLetrecGroups)
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Inference
import TypeChecker.Patterns
import qualified Data.Map as Map
import Control.Monad.State (evalStateT)
import Control.Monad (foldM)
import Control.Monad.Trans (lift)
import Data.Either (lefts, rights)

mergeMutualSubstitutions :: [Substitution] -> Either TypeError Substitution
mergeMutualSubstitutions = foldM mergeSubstitution Map.empty
  where
    mergeSubstitution acc sub = foldM mergeBinding acc (Map.toList sub)

    mergeBinding acc (name, ty) =
      let ty' = applySubst acc ty
      in case Map.lookup name acc of
        Nothing -> Right $ Map.insert name ty' acc
        Just existing -> do
          unifySubst <- unify (applySubst acc existing) ty'
          let acc' = composeSubst unifySubst acc
          return $ Map.insert name (applySubst acc' ty') acc'

typeCheck :: Expr -> Either TypeError Type
typeCheck expr = case evalStateT (infer Map.empty expr) 0 of
  Left err -> Left err
  Right (subst, ty) -> Right $ applySubst subst ty

typeCheckWithEnv :: TypeEnv -> Expr -> Either TypeError Type
typeCheckWithEnv env expr = case evalStateT (infer env expr) 0 of
  Left err -> Left err
  Right (subst, ty) -> Right $ applySubst subst ty

-- Type check a program (without module support)
typeCheckProgram :: Program -> Either TypeError Type
typeCheckProgram program = evalStateT (typeCheckProgram' Map.empty program) 0
  where
    typeCheckProgram' :: TypeEnv -> Program -> TypeInfer Type
    typeCheckProgram' env (Program topLevels) = go env topLevels

    go :: TypeEnv -> [TopLevel] -> TypeInfer Type
    go env [] = return TUnit  -- Empty program has unit type
    go env [TLExpr expr] = do
      -- Final expression in the program
      (_, ty) <- infer env expr
      return ty
    go env (TLExpr expr : rest) = do
      -- Expression that's not final - evaluate but don't return its type
      _ <- infer env expr
      go env rest
    go env (TLDef var maybeType expr : rest) =
      case expr of
        LetRec _ _ _ _ -> do
          let (letrecs, remaining) = collectConsecutiveLetrecs (TLDef var maybeType expr : rest)
          newEnv <- processLetrecGroups env (dependencyOrderedLetrecGroups letrecs)
          go newEnv remaining
        _ -> do
          -- Regular let definition
          (subst, defTy) <- infer env expr
          let appliedTy = applySubst subst defTy
          -- Check type annotation if provided
          (definitionSubst, finalTy) <- case maybeType of
            Just expectedTy -> do
              let syntaxTy = syntaxTypeToType expectedTy
              unifySubst <- lift $ unify appliedTy syntaxTy
              return (composeSubst unifySubst subst, applySubst unifySubst appliedTy)
            Nothing -> return (subst, appliedTy)
          let baseEnv = applySubstEnv definitionSubst env
          let newEnv = Map.insert var (generalize baseEnv finalTy) baseEnv
          go newEnv rest
    go env (TLData typeName typeVars constructors : rest) =
      let newEnv = Map.union (dataConstructorsTypeEnv typeName typeVars constructors) env
      in go newEnv rest
    go env (TLImport _ : rest) = go env rest  -- Ignore imports in simple type checking
    go env (TLExport _ : rest) = go env rest  -- Ignore exports in simple type checking

    processLetrecGroups :: TypeEnv -> [[TopLevel]] -> TypeInfer TypeEnv
    processLetrecGroups env [] = return env
    processLetrecGroups env (group : groups) = do
      (newEnv, _) <- processMutualRecursion env group
      processLetrecGroups newEnv groups

    -- Process multiple letrec definitions together for mutual recursion
    processMutualRecursion :: TypeEnv -> [TopLevel] -> TypeInfer (TypeEnv, Substitution)
    processMutualRecursion env letrecs = do
      -- Create type variables for each letrec function
      funcTypes <- mapM (\case
            TLDef var Nothing _ -> return (var, Nothing, TVar var, monoScheme (TVar var))
            TLDef var (Just sType) _ -> do
              let annotatedType = syntaxTypeToType sType
              return (var, Just annotatedType, annotatedType, generalize env annotatedType)
            _ -> error "processMutualRecursion: expected TLDef") letrecs

      -- Add all functions to environment with their types
      let mutualEnv = Map.union (Map.fromList [(var, scheme) | (var, _, _, scheme) <- funcTypes]) env

      -- Type check each letrec body
      results <- mapM (typeCheckLetrec letrecs env mutualEnv) funcTypes

      -- Combine all substitutions
      combinedSubst <- lift $ mergeMutualSubstitutions (map fst results)
      let baseEnv = applySubstEnv combinedSubst env
      let finalTypes =
            zipWith
              (\(_, maybeAnnotatedType, _, _) inferredType ->
                 case maybeAnnotatedType of
                   Just annotatedType -> applySubst combinedSubst annotatedType
                   Nothing -> applySubst combinedSubst inferredType)
              funcTypes
              (map snd results)
      let generalized =
            zipWith
              (\(var, maybeAnnotatedType, _, _) ty ->
                 ( var
                 , case maybeAnnotatedType of
                     Just annotatedType -> generalize baseEnv (applySubst combinedSubst annotatedType)
                     Nothing -> generalize baseEnv ty
                 ))
              funcTypes
              finalTypes
      let finalEnv = Map.union (Map.fromList generalized) baseEnv
      return (finalEnv, combinedSubst)

    -- Type check a single letrec definition in the mutual environment
    typeCheckLetrec :: [TopLevel] -> TypeEnv -> TypeEnv -> (String, Maybe Type, Type, Scheme) -> TypeInfer (Substitution, Type)
    typeCheckLetrec letrecs outerEnv mutualEnv (var, maybeAnnotatedType, assumedType, _) =
      case Map.lookup var letrecMap of
        Just (TLDef _ _ (LetRec _ _ val _)) -> do
          (subst, valType) <- infer mutualEnv val
          case maybeAnnotatedType of
            Just annotatedType -> do
              let baseEnv = applySubstEnv subst outerEnv
              let annotatedScheme = generalize baseEnv (applySubst subst annotatedType)
              let inferredScheme = generalize baseEnv (applySubst subst valType)
              matches <- schemeIsInstanceOf annotatedScheme inferredScheme
              if matches
                then return (subst, applySubst subst annotatedType)
                else lift $ Left $ GeneralTypeError "Recursive definition does not satisfy its annotated polymorphic type"
            Nothing -> do
              let appliedType = applySubst subst assumedType
              unifySubst <- lift $ unify appliedType valType
              let finalSubst = composeSubst unifySubst subst
              let finalType = applySubst finalSubst appliedType
              return (finalSubst, finalType)
        _ -> error "typeCheckLetrec: expected TLDef with LetRec"
      where
        letrecMap = Map.fromList
          [ (name, topLevel)
          | topLevel@(TLDef name _ (LetRec _ _ _ _)) <- letrecs
          ]

-- Type check a program with module support (IO version)
typeCheckProgramWithDirIO :: (FilePath -> String -> IO (Either TypeError TypeEnv)) -> FilePath -> Program -> IO (Either TypeError Type)
typeCheckProgramWithDirIO loadModule currentDir = typeCheckProgramWithDir' Map.empty
  where
    typeCheckProgramWithDir' :: TypeEnv -> Program -> IO (Either TypeError Type)
    typeCheckProgramWithDir' env (Program topLevels) = go env topLevels

    go :: TypeEnv -> [TopLevel] -> IO (Either TypeError Type)
    go env [] = return $ Right TUnit
    go env [TLExpr expr] = do
      -- Final expression in the program
      let typeResult = evalStateT (infer env expr) 0
      case typeResult of
        Left err -> return $ Left err
        Right (subst, ty) -> return $ Right $ applySubst subst ty
    go env (TLExpr expr : rest) = do
      -- Expression that's not final
      let typeResult = evalStateT (infer env expr) 0
      case typeResult of
        Left err -> return $ Left err
        Right _ -> go env rest
    go env (TLDef var maybeType expr : rest) =
      case expr of
        LetRec _ _ _ _ -> do
          let (letrecs, remaining) = collectConsecutiveLetrecs (TLDef var maybeType expr : rest)
          mutualResult <- processLetrecGroupsIO env (dependencyOrderedLetrecGroups letrecs)
          case mutualResult of
            Left err -> return $ Left err
            Right newEnv -> go newEnv remaining
        _ -> do
          -- Regular let definition
          let typeResult = evalStateT (infer env expr) 0
          case typeResult of
            Left err -> return $ Left err
            Right (subst, defTy) -> do
              let appliedTy = applySubst subst defTy
              -- Check type annotation if provided
              case maybeType of
                Just expectedTy -> do
                  let syntaxTy = syntaxTypeToType expectedTy
                  case unify appliedTy syntaxTy of
                    Left unifyErr -> return $ Left unifyErr
                    Right unifySubst -> do
                      let definitionSubst = composeSubst unifySubst subst
                      let finalTy = applySubst unifySubst appliedTy
                      let baseEnv = applySubstEnv definitionSubst env
                      let newEnv = Map.insert var (generalize baseEnv finalTy) baseEnv
                      go newEnv rest
                Nothing -> do
                  let finalTy = appliedTy
                  let baseEnv = applySubstEnv subst env
                  let newEnv = Map.insert var (generalize baseEnv finalTy) baseEnv
                  go newEnv rest
    go env (TLData typeName typeVars constructors : rest) =
      let newEnv = Map.union (dataConstructorsTypeEnv typeName typeVars constructors) env
      in go newEnv rest
    go env (TLImport moduleName : rest) = do
      -- Load module type environment
      moduleResult <- loadModule currentDir moduleName
      case moduleResult of
        Left err -> return $ Left err
        Right moduleEnv -> do
          let newEnv = Map.union moduleEnv env  -- Module types take precedence
          go newEnv rest
    go env (TLExport _ : rest) = go env rest  -- Exports don't affect type environment

    processLetrecGroupsIO :: TypeEnv -> [[TopLevel]] -> IO (Either TypeError TypeEnv)
    processLetrecGroupsIO env [] = return $ Right env
    processLetrecGroupsIO env (group : groups) = do
      mutualResult <- processMutualRecursionIO env group
      case mutualResult of
        Left err -> return $ Left err
        Right newEnv -> processLetrecGroupsIO newEnv groups

    -- Process multiple letrec definitions together for mutual recursion (IO version)
    processMutualRecursionIO :: TypeEnv -> [TopLevel] -> IO (Either TypeError TypeEnv)
    processMutualRecursionIO env letrecs = do
      -- Create type variables for each letrec function
      let funcTypes = map (\case
            TLDef var Nothing _ -> (var, Nothing, TVar var, monoScheme (TVar var))
            TLDef var (Just sType) _ ->
              let annotatedType = syntaxTypeToType sType
              in (var, Just annotatedType, annotatedType, generalize env annotatedType)
            _ -> error "processMutualRecursionIO: expected TLDef") letrecs

      -- Add all functions to environment with their types
      let mutualEnv = Map.union (Map.fromList [(var, scheme) | (var, _, _, scheme) <- funcTypes]) env

      -- Type check each letrec body
      results <- mapM (typeCheckLetrecIO letrecs env mutualEnv) funcTypes

      -- Check for errors
      let errors = lefts results
      if not (null errors)
        then return $ Left (head errors)
        else do
          let successes = rights results
          case mergeMutualSubstitutions (map fst successes) of
            Left err -> return $ Left err
            Right combinedSubst -> do
              let baseEnv = applySubstEnv combinedSubst env
              let finalTypes =
                    zipWith
                      (\(_, maybeAnnotatedType, _, _) inferredType ->
                         case maybeAnnotatedType of
                           Just annotatedType -> applySubst combinedSubst annotatedType
                           Nothing -> applySubst combinedSubst inferredType)
                      funcTypes
                      (map snd successes)
              let generalized =
                    zipWith
                      (\(var, maybeAnnotatedType, _, _) ty ->
                         ( var
                         , case maybeAnnotatedType of
                             Just annotatedType -> generalize baseEnv (applySubst combinedSubst annotatedType)
                             Nothing -> generalize baseEnv ty
                         ))
                      funcTypes
                      finalTypes
              let finalEnv = Map.union (Map.fromList generalized) baseEnv
              return $ Right finalEnv

    -- Type check a single letrec definition in the mutual environment (IO version)
    typeCheckLetrecIO :: [TopLevel] -> TypeEnv -> TypeEnv -> (String, Maybe Type, Type, Scheme) -> IO (Either TypeError (Substitution, Type))
    typeCheckLetrecIO letrecs outerEnv env (var, maybeAnnotatedType, assumedType, _) = do
      let typeResult = evalStateT (infer env val) 0
      case typeResult of
        Left err -> return $ Left err
        Right (subst, valType) -> do
          case maybeAnnotatedType of
            Just annotatedType -> do
              let baseEnv = applySubstEnv subst outerEnv
              let annotatedScheme = generalize baseEnv (applySubst subst annotatedType)
              let inferredScheme = generalize baseEnv (applySubst subst valType)
              let schemeResult = evalStateT (schemeIsInstanceOf annotatedScheme inferredScheme) 0
              case schemeResult of
                Left err -> return $ Left err
                Right True -> return $ Right (subst, applySubst subst annotatedType)
                Right False -> return $ Left $ GeneralTypeError "Recursive definition does not satisfy its annotated polymorphic type"
            Nothing -> do
              let appliedType = applySubst subst assumedType
              case unify appliedType valType of
                Left unifyErr -> return $ Left unifyErr
                Right unifySubst -> do
                  let finalSubst = composeSubst unifySubst subst
                  let finalType = applySubst finalSubst appliedType
                  return $ Right (finalSubst, finalType)
      where
        val = case Map.lookup var letrecMap of
          Just (TLDef _ _ (LetRec _ _ body _)) -> body
          _ -> error "typeCheckLetrecIO: expected TLDef with LetRec"
        letrecMap = Map.fromList
          [ (name, topLevel)
          | topLevel@(TLDef name _ (LetRec _ _ _ _)) <- letrecs
          ]
