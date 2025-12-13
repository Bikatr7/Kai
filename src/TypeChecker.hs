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
import TypeChecker.Types
import TypeChecker.Substitution
import TypeChecker.Unification
import TypeChecker.Inference
import TypeChecker.Patterns
import qualified Data.Map as Map
import Control.Monad.State (evalStateT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)

-- Helper functions for handling mutual recursion in top-level definitions

-- Collect consecutive letrec definitions
collectConsecutiveLetrecs :: [TopLevel] -> ([TopLevel], [TopLevel])
collectConsecutiveLetrecs [] = ([], [])
collectConsecutiveLetrecs (TLDef var maybeType expr : rest) =
  case expr of
    LetRec _ _ _ _ ->
      let (moreLetrecs, remaining) = collectConsecutiveLetrecs rest
      in (TLDef var maybeType expr : moreLetrecs, remaining)
    _ -> ([], TLDef var maybeType expr : rest)
collectConsecutiveLetrecs (other : rest) = ([], other : rest)

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
    go env (TLExpr expr : []) = do
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
          -- Handle consecutive letrec definitions for mutual recursion
          let (letrecs, remaining) = collectConsecutiveLetrecs (TLDef var maybeType expr : rest)
          mutualResult <- processMutualRecursion env letrecs
          case mutualResult of
            Left err -> error $ show err  -- Should be handled better
            Right (newEnv, _) -> go newEnv remaining
        _ -> do
          -- Regular let definition
          (subst, defTy) <- infer env expr
          let appliedTy = applySubst subst defTy
          -- Check type annotation if provided
          finalTy <- case maybeType of
            Just expectedTy -> do
              let syntaxTy = syntaxTypeToType expectedTy
              unifySubst <- lift $ unify appliedTy syntaxTy
              return $ applySubst unifySubst appliedTy
            Nothing -> return appliedTy
          let newEnv = Map.insert var finalTy (applySubstEnv subst env)
          go newEnv rest
    go env (TLImport _ : rest) = go env rest  -- Ignore imports in simple type checking
    go env (TLExport _ : rest) = go env rest  -- Ignore exports in simple type checking

    -- Process multiple letrec definitions together for mutual recursion
    processMutualRecursion :: TypeEnv -> [TopLevel] -> TypeInfer (Either TypeError (TypeEnv, Substitution))
    processMutualRecursion env letrecs = do
      -- Create type variables for each letrec function
      let funcTypes = map (\topLevel ->
            case topLevel of
              TLDef var maybeType _ -> (var, case maybeType of
                Just sType -> syntaxTypeToType sType
                Nothing -> TVar var)  -- Use variable name as type variable name
              _ -> error "processMutualRecursion: expected TLDef") letrecs

      -- Add all functions to environment with their types
      let mutualEnv = Map.union (Map.fromList funcTypes) env

      -- Type check each letrec body
      results <- mapM (typeCheckLetrec mutualEnv) letrecs

      -- Combine all substitutions
      let combinedSubst = composeSubstList (map fst results)
      let finalTypes = map (applySubst combinedSubst . snd) funcTypes
      let finalEnv = Map.union (Map.fromList (zip (map fst funcTypes) finalTypes)) env

      return $ Right (applySubstEnv combinedSubst finalEnv, combinedSubst)

    -- Type check a single letrec definition in the mutual environment
    typeCheckLetrec :: TypeEnv -> TopLevel -> TypeInfer (Substitution, Type)
    typeCheckLetrec env (TLDef var maybeType (LetRec _ _ val _)) = do
      (subst, valType) <- infer env val
      let appliedType = applySubst subst (env Map.! var)
      unifySubst <- lift $ unify appliedType valType
      let finalSubst = composeSubst unifySubst subst
      let finalType = applySubst finalSubst appliedType
      return (finalSubst, finalType)
    typeCheckLetrec _ _ = error "typeCheckLetrec: expected TLDef with LetRec"

-- Type check a program with module support (IO version)
typeCheckProgramWithDirIO :: (FilePath -> String -> IO (Either TypeError TypeEnv)) -> FilePath -> Program -> IO (Either TypeError Type)
typeCheckProgramWithDirIO loadModule currentDir program = typeCheckProgramWithDir' Map.empty program
  where
    typeCheckProgramWithDir' :: TypeEnv -> Program -> IO (Either TypeError Type)
    typeCheckProgramWithDir' env (Program topLevels) = go env topLevels

    go :: TypeEnv -> [TopLevel] -> IO (Either TypeError Type)
    go env [] = return $ Right TUnit
    go env (TLExpr expr : []) = do
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
          -- Handle consecutive letrec definitions for mutual recursion
          let (letrecs, remaining) = collectConsecutiveLetrecs (TLDef var maybeType expr : rest)
          mutualResult <- processMutualRecursionIO env letrecs
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
                      let finalTy = applySubst unifySubst appliedTy
                      let newEnv = Map.insert var finalTy (applySubstEnv subst env)
                      go newEnv rest
                Nothing -> do
                  let finalTy = appliedTy
                  let newEnv = Map.insert var finalTy (applySubstEnv subst env)
                  go newEnv rest
    go env (TLImport moduleName : rest) = do
      -- Load module type environment
      moduleResult <- loadModule currentDir moduleName
      case moduleResult of
        Left err -> return $ Left err
        Right moduleEnv -> do
          let newEnv = Map.union moduleEnv env  -- Module types take precedence
          go newEnv rest
    go env (TLExport _ : rest) = go env rest  -- Exports don't affect type environment

    -- Process multiple letrec definitions together for mutual recursion (IO version)
    processMutualRecursionIO :: TypeEnv -> [TopLevel] -> IO (Either TypeError TypeEnv)
    processMutualRecursionIO env letrecs = do
      -- Create type variables for each letrec function
      let funcTypes = map (\topLevel ->
            case topLevel of
              TLDef var maybeType _ -> (var, case maybeType of
                Just sType -> syntaxTypeToType sType
                Nothing -> TVar var)  -- Use variable name as type variable name
              _ -> error "processMutualRecursionIO: expected TLDef") letrecs

      -- Add all functions to environment with their types
      let mutualEnv = Map.union (Map.fromList funcTypes) env

      -- Type check each letrec body
      results <- mapM (typeCheckLetrecIO mutualEnv) letrecs

      -- Check for errors
      let errors = [err | Left err <- results]
      if not (null errors)
        then return $ Left (head errors)
        else do
          let successes = [res | Right res <- results]
          let combinedSubst = composeSubstList (map fst successes)
          let finalTypes = map (applySubst combinedSubst . snd) funcTypes
          let finalEnv = Map.union (Map.fromList (zip (map fst funcTypes) finalTypes)) env
          return $ Right (applySubstEnv combinedSubst finalEnv)

    -- Type check a single letrec definition in the mutual environment (IO version)
    typeCheckLetrecIO :: TypeEnv -> TopLevel -> IO (Either TypeError (Substitution, Type))
    typeCheckLetrecIO env (TLDef var maybeType (LetRec _ _ val _)) = do
      let typeResult = evalStateT (infer env val) 0
      case typeResult of
        Left err -> return $ Left err
        Right (subst, valType) -> do
          let appliedType = applySubst subst (env Map.! var)
          case unify appliedType valType of
            Left unifyErr -> return $ Left unifyErr
            Right unifySubst -> do
              let finalSubst = composeSubst unifySubst subst
              let finalType = applySubst finalSubst appliedType
              return $ Right (finalSubst, finalType)
    typeCheckLetrecIO _ _ = return $ Left (GeneralTypeError "typeCheckLetrecIO: expected TLDef with LetRec")