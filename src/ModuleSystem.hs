module ModuleSystem (
    loadModule,
    filterByExports,
    resolveModulePath,
    ModuleInfo(..),
    loadModuleTypeEnvIO,
    loadModuleTypeEnvWithWarningsIO,
    extractTypeEnvIO
) where

import Syntax
import Parser
import Diagnostics (renderTypeError, renderRuntimeError)
import Text.Megaparsec (errorBundlePretty)
import DataDeclarations (filterTypeExports)
import TypeChecker (inferProgramWithWarningsIO, TypeEnv, TypeError(..), TypeWarning(..))
import Evaluator.Types
import Evaluator.Program (evaluateTopLevels)
import qualified Data.Map as Map
import System.FilePath (takeDirectory, (</>))
import System.Directory (doesFileExist)
import Control.Monad (foldM)
import Data.List (intercalate)
import SourceIO (readSourceFile)

data ModuleInfo = ModuleInfo
    { moduleName :: String
    , moduleProgram :: Program
    , moduleEnv :: Env
    , moduleExports :: [String]
    } deriving (Show)

resolveModulePath :: FilePath -> String -> IO (Either String FilePath)
resolveModulePath currentDir name = do
    let baseName = name ++ ".kai"
    let paths =
            [ currentDir </> baseName
            , currentDir </> name </> baseName
            , currentDir </> "examples" </> baseName
            , currentDir </> "examples" </> name </> baseName
            ]
    foldM tryPath (Left $ "Module not found: " ++ name) paths
  where
    tryPath (Right found) _ = return $ Right found
    tryPath _ path = do
        exists <- doesFileExist path
        if exists
            then return $ Right path
            else return $ Left $ "Module not found: " ++ name

loadModule :: (Env -> Expr -> IO (Either RuntimeError Value)) -> FilePath -> String -> [String] -> IO (Either String ModuleInfo)
loadModule evaluate currentDir name stack
  | name `elem` stack = return $ Left $ "Circular import detected: " ++ intercalate " -> " (stack ++ [name])
  | otherwise = do
      resolved <- resolveModulePath currentDir name
      case resolved of
        Left err -> return $ Left err
        Right path -> do
          source <- readSourceFile path
          case source of
            Left err -> return $ Left $ "IO error reading module " ++ name ++ ": " ++ show err
            Right content -> case parseLocatedProgram path content of
              Left err -> return $ Left $ "Parse error in module " ++ name ++ ": " ++ errorBundlePretty err
              Right program -> do
                let dir = takeDirectory path
                    nextStack = stack ++ [name]
                checked <- extractTypeEnvIOWithStack dir program nextStack
                case checked of
                  Left err -> return $ Left $ "In module " ++ name ++ ": " ++ renderTypeError err
                  Right _ -> do
                    result <- evaluateTopLevels evaluate (loadImport dir nextStack) Map.empty program
                    return $ case result of
                      Left err -> Left $ "In module " ++ name ++ ": " ++ renderRuntimeError err
                      Right (env, _) -> Right $ ModuleInfo name program env (extractExports program)
  where
    loadImport dir loading name' = do
      result <- loadModule evaluate dir name' loading
      return $ case result of
        Left err -> Left $ TypeError err
        Right info -> Right $ filterByExports (moduleEnv info) (moduleExports info)

extractExports :: Program -> [String]
extractExports (Program levels) = concat [names | TLExport names <- map unlocatedTopLevel levels]

filterByExports :: Map.Map String a -> [String] -> Map.Map String a
filterByExports env [] = env
filterByExports env names = Map.filterWithKey (\name _ -> name `elem` names) env

loadModuleTypeEnvIO :: FilePath -> String -> IO (Either TypeError TypeEnv)
loadModuleTypeEnvIO dir name = fmap fst <$> loadModuleTypeEnvWithWarningsIO dir name

loadModuleTypeEnvWithWarningsIO :: FilePath -> String -> IO (Either TypeError (TypeEnv,[TypeWarning]))
loadModuleTypeEnvWithWarningsIO dir name = loadModuleTypeEnvIOWithStack dir name []

loadModuleTypeEnvIOWithStack :: FilePath -> String -> [String] -> IO (Either TypeError (TypeEnv,[TypeWarning]))
loadModuleTypeEnvIOWithStack dir name stack
  | name `elem` stack = return $ Left $ GeneralTypeError $ "Circular import detected during type checking: " ++ intercalate " -> " (stack ++ [name])
  | otherwise = do
      resolved <- resolveModulePath dir name
      case resolved of
        Left err -> return $ Left $ GeneralTypeError err
        Right path -> do
          source <- readSourceFile path
          case source of
            Left err -> return $ Left $ GeneralTypeError $ "IO error reading module " ++ name ++ ": " ++ show err
            Right content -> case parseLocatedProgram path content of
              Left err -> return $ Left $ GeneralTypeError $ "Parse error in module " ++ name ++ ": " ++ errorBundlePretty err
              Right program -> do
                result <- extractTypeEnvWithWarnings (takeDirectory path) program (stack ++ [name])
                pure $ fmap (\(env,warnings) -> (env,map (InModule path) warnings)) result

extractTypeEnvIO :: FilePath -> Program -> IO (Either TypeError TypeEnv)
extractTypeEnvIO dir program = extractTypeEnvIOWithStack dir program []

extractTypeEnvIOWithStack :: FilePath -> Program -> [String] -> IO (Either TypeError TypeEnv)
extractTypeEnvIOWithStack dir program stack = fmap fst <$> extractTypeEnvWithWarnings dir program stack

extractTypeEnvWithWarnings :: FilePath -> Program -> [String] -> IO (Either TypeError (TypeEnv,[TypeWarning]))
extractTypeEnvWithWarnings dir program stack = do
  result <- inferProgramWithWarningsIO (\path name -> loadModuleTypeEnvIOWithStack path name stack) dir Map.empty program
  return $ fmap (\((env, _),warnings) -> (filterTypeExports env (extractExports program),warnings)) result
