module ModuleSystem (
    loadModule,
    resolveModulePath,
    ModuleInfo(..),
    loadModuleTypeEnvIO,
    extractTypeEnvIO
) where

import Syntax
import Parser
import TypeChecker (typeCheckProgramWithDirIO, TypeEnv, TypeError(..), typeCheckWithEnv, syntaxTypeToType)
import TypeChecker.Types (Type(..))
import TypeChecker.Substitution (freshTVar, applySubst, composeSubst)
import TypeChecker.Unification (unify)
import TypeChecker.Inference (infer)
import Evaluator.Types
import qualified Data.Map as Map
import System.FilePath (takeDirectory, (</>), takeFileName, dropExtension)
import System.Directory (doesFileExist)
import Control.Monad (foldM, liftM, when)
import Control.Monad.State (runStateT)
import Data.IORef (newIORef, writeIORef)
import Data.List (foldr, intercalate)
import Data.Foldable (foldl')
import System.IO.Error (catchIOError)
import Control.Monad.State (evalStateT, runStateT)

data ModuleInfo = ModuleInfo
    { moduleName :: String
    , moduleProgram :: Program
    , moduleEnv :: Env
    , moduleExports :: [String]
    } deriving (Show)

resolveModulePath :: FilePath -> String -> IO (Either String FilePath)
resolveModulePath currentDir moduleName = do
    let baseName = moduleName ++ ".kai"
    let paths =
            [ currentDir </> baseName
            , currentDir </> moduleName </> baseName
            , currentDir </> "examples" </> baseName
            , currentDir </> "examples" </> moduleName </> baseName
            ]
    foldM tryPath (Left $ "Module not found: " ++ moduleName) paths
  where
    tryPath (Right found) _ = return $ Right found
    tryPath _ path = do
        exists <- doesFileExist path
        if exists
            then return $ Right path
            else return $ Left $ "Module not found: " ++ moduleName

loadModule :: (Env -> Expr -> IO (Either RuntimeError Value)) -> FilePath -> String -> [String] -> IO (Either String ModuleInfo)
loadModule evalFunc currentDir moduleName loadingStack = do
    if moduleName `elem` loadingStack
        then return $ Left $ "Circular import detected: " ++ moduleName ++ " is already being loaded. Loading stack: " ++ intercalate " -> " (loadingStack ++ [moduleName])
        else do
            pathResult <- resolveModulePath currentDir moduleName
            case pathResult of
                Left err -> return $ Left err
                Right path -> do
                    content <- readFile path
                    case parseProgram content of
                        Left parseErr -> return $ Left $ "Parse error in module " ++ moduleName ++ ": " ++ show parseErr
                        Right program -> do
                            let exports = extractExports program
                            let moduleDir = takeDirectory path
                            typeResult <- typeCheckProgramWithDirIO loadModuleTypeEnvIO moduleDir program
                            case typeResult of
                              Left typeErr -> return $ Left $ "Type error in module " ++ moduleName ++ ": " ++ show typeErr
                              Right _ -> do
                                  let newLoadingStack = loadingStack ++ [moduleName]
                                  envResult <- evalModuleWithEnv evalFunc Map.empty moduleDir program newLoadingStack
                                  case envResult of
                                    Left runtimeErr -> return $ Left $ "Runtime error in module " ++ moduleName ++ ": " ++ show runtimeErr
                                    Right env -> return $ Right $ ModuleInfo
                                        { moduleName = moduleName
                                        , moduleProgram = program
                                        , moduleEnv = env
                                        , moduleExports = exports
                                        }

extractExports :: Program -> [String]
extractExports (Program topLevels) = concatMap extractExport topLevels
  where
    extractExport (TLExport names) = names
    extractExport _ = []

filterByExports :: Map.Map String a -> [String] -> Map.Map String a
filterByExports env [] = env
filterByExports env exports = Map.filterWithKey (\k _ -> k `elem` exports) env

evalModuleWithEnv :: (Env -> Expr -> IO (Either RuntimeError Value)) -> Env -> FilePath -> Program -> [String] -> IO (Either RuntimeError Env)
evalModuleWithEnv evalFunc env currentDir (Program topLevels) loadingStack = do
    importResult <- processImports evalFunc env currentDir topLevels [] loadingStack
    case importResult of
        Left err -> return $ Left err
        Right (importedEnv, remaining) -> do
            evalDefinitions evalFunc importedEnv remaining
  where
    processImports :: (Env -> Expr -> IO (Either RuntimeError Value)) -> Env -> FilePath -> [TopLevel] -> [TopLevel] -> [String] -> IO (Either RuntimeError (Env, [TopLevel]))
    processImports _ env _ [] acc _ = return $ Right (env, reverse acc)
    processImports evalFunc env currentDir (TLImport moduleName : rest) acc loadingStack = do
        moduleResult <- loadModule evalFunc currentDir moduleName loadingStack
        case moduleResult of
            Left err -> return $ Left $ TypeError $ "Failed to load module " ++ moduleName ++ ": " ++ err
            Right moduleInfo -> do
                let exportedEnv = filterByExports (moduleEnv moduleInfo) (moduleExports moduleInfo)
                let mergedEnv = Map.union exportedEnv env
                processImports evalFunc mergedEnv currentDir rest acc loadingStack
    processImports evalFunc env currentDir (TLExport _ : rest) acc loadingStack = do
        processImports evalFunc env currentDir rest acc loadingStack
    processImports evalFunc env currentDir (other : rest) acc loadingStack = do
        processImports evalFunc env currentDir rest (other : acc) loadingStack
    
    evalDefinitions :: (Env -> Expr -> IO (Either RuntimeError Value)) -> Env -> [TopLevel] -> IO (Either RuntimeError Env)
    evalDefinitions _ env [] = return $ Right env
    evalDefinitions evalFunc env (TLDef var maybeType expr : rest) = do
        case expr of
            LetRec _ _ _ _ -> do
                let (letrecs, remaining) = collectConsecutiveLetrecs (TLDef var maybeType expr : rest)
                result <- processMutualRecursion evalFunc env letrecs remaining
                case result of
                    Left err -> return $ Left err
                    Right finalEnv -> evalDefinitions evalFunc finalEnv remaining
            _ -> do
                result <- evalFunc env expr
                case result of
                    Left err -> return $ Left err
                    Right val -> do
                        let env' = Map.insert var val env
                        evalDefinitions evalFunc env' rest
    evalDefinitions evalFunc env (_ : rest) = evalDefinitions evalFunc env rest
    
    collectConsecutiveLetrecs :: [TopLevel] -> ([TopLevel], [TopLevel])
    collectConsecutiveLetrecs [] = ([], [])
    collectConsecutiveLetrecs (TLDef var maybeType expr : rest) =
        case expr of
            LetRec _ _ _ _ ->
                let (moreLetrecs, remaining) = collectConsecutiveLetrecs rest
                in (TLDef var maybeType expr : moreLetrecs, remaining)
            _ -> ([], TLDef var maybeType expr : rest)
    collectConsecutiveLetrecs (other : rest) = ([], other : rest)
    
    processMutualRecursion :: (Env -> Expr -> IO (Either RuntimeError Value)) -> Env -> [TopLevel] -> [TopLevel] -> IO (Either RuntimeError Env)
    processMutualRecursion evalFunc env letrecs remaining = do
        refs <- mapM (\_ -> newIORef (VFun "_placeholder" (IntLit 0) Map.empty)) letrecs
        let refMap = Map.fromList $ zipWith (\topLevel ref ->
                case topLevel of
                    TLDef var _ _ -> (var, VRef ref)
                    _ -> error "processMutualRecursion: expected TLDef") letrecs refs
        let mutualEnv = Map.union refMap env
        results <- mapM (\topLevel ->
            case topLevel of
                TLDef var _ expr ->
                    case expr of
                        LetRec _ _ recVal _ -> evalFunc mutualEnv recVal
                        _ -> return $ Left $ TypeError "Expected LetRec expression"
                _ -> return $ Left $ TypeError "Expected TLDef with LetRec") letrecs
        let findError = foldr (\result acc -> case result of Left err -> Left err; Right _ -> acc) (Right ()) results
        case findError of
            Left err -> return $ Left err
            Right _ -> do
                let recValues = [v | Right v <- results]
                let checkAndUpdate (recValue, ref) =
                        case recValue of
                            VFun _ _ _ -> do
                                writeIORef ref recValue
                                return $ Right ()
                            _ -> return $ Left $ TypeError ("LetRec value must be a function, got: " ++ show recValue)
                updateResults <- mapM checkAndUpdate (zip recValues refs)
                let checkUpdates = foldr (\result acc -> case result of Left err -> Left err; Right _ -> acc) (Right ()) updateResults
                case checkUpdates of
                    Left err -> return $ Left err
                    Right _ -> return $ Right mutualEnv

loadModuleTypeEnvIO :: FilePath -> String -> IO (Either TypeError TypeEnv)
loadModuleTypeEnvIO currentDir moduleName = loadModuleTypeEnvIOWithStack currentDir moduleName []

loadModuleTypeEnvIOWithStack :: FilePath -> String -> [String] -> IO (Either TypeError TypeEnv)
loadModuleTypeEnvIOWithStack currentDir moduleName loadingStack = do
  if moduleName `elem` loadingStack
      then return $ Left $ GeneralTypeError $ "Circular import detected during type checking: " ++ moduleName ++ " is already being loaded. Loading stack: " ++ intercalate " -> " (loadingStack ++ [moduleName])
      else do
          pathResult <- resolveModulePath currentDir moduleName
          case pathResult of
            Left err -> return $ Left $ GeneralTypeError $ "Failed to import module " ++ moduleName ++ ": " ++ err
            Right path -> do
              contentResult <- catchIOError (Right <$> readFile path) (\e -> return $ Left $ show e)
              case contentResult of
                Left err -> return $ Left $ GeneralTypeError $ "IO error reading module " ++ moduleName ++ ": " ++ err
                Right content -> do
                  case parseProgram content of
                    Left parseErr -> return $ Left $ GeneralTypeError $ "Parse error in module " ++ moduleName ++ ": " ++ show parseErr
                    Right program -> do
                      let isImport' (TLImport _) = True
                          isImport' _ = False
                          hasImports = case program of
                            Program topLevels -> any isImport' topLevels
                      let isLetrecDef' (TLDef _ _ (LetRec _ _ _ _)) = True
                          isLetrecDef' _ = False
                          hasLetrec = case program of
                            Program topLevels -> any isLetrecDef' topLevels

                      let moduleDir = takeDirectory path
                      let newLoadingStack = loadingStack ++ [moduleName]
                      typeResult <- typeCheckProgramWithDirIO (loadModuleTypeEnvIOWithStackWrapper newLoadingStack) moduleDir program
                      case typeResult of
                        Left typeErr -> return $ Left $ GeneralTypeError $ "Type error in module " ++ moduleName ++ ": " ++ show typeErr
                        Right _ -> do
                          extractTypeEnvIOWithStack moduleDir program newLoadingStack

loadModuleTypeEnvIOWithStackWrapper :: [String] -> FilePath -> String -> IO (Either TypeError TypeEnv)
loadModuleTypeEnvIOWithStackWrapper loadingStack currentDir moduleName =
    loadModuleTypeEnvIOWithStack currentDir moduleName loadingStack

extractTypeEnvIO :: FilePath -> Program -> IO (Either TypeError TypeEnv)
extractTypeEnvIO currentDir program = extractTypeEnvIOWithStack currentDir program []

extractTypeEnvIOWithStack :: FilePath -> Program -> [String] -> IO (Either TypeError TypeEnv)
extractTypeEnvIOWithStack currentDir program loadingStack = do
    let (Program topLevels) = program
    let exports = extractExports program
    result <- go Map.empty topLevels
    case result of
        Left err -> return $ Left err
        Right env -> return $ Right $ filterByExports env exports
  where
    go env [] = return $ Right env
    go env (TLImport moduleName : rest) = do
      moduleTypeEnvResult <- loadModuleTypeEnvIOWithStack currentDir moduleName loadingStack
      case moduleTypeEnvResult of
        Left err -> return $ Left err
        Right moduleTypeEnv -> do
          let mergedEnv = Map.union moduleTypeEnv env
          go mergedEnv rest
    go env (TLExport _ : rest) = go env rest
    go env (TLDef var maybeType expr : rest) = do
      case expr of
        LetRec _ _ _ _ -> do
          let (letrecs, remaining) = collectConsecutiveLetrecsExtract (TLDef var maybeType expr : rest)
          mutualEnvResult <- processMutualRecursionTypeExtract env letrecs
          case mutualEnvResult of
            Left err -> return $ Left err
            Right mutualEnv -> go mutualEnv remaining
        _ -> do
          case typeCheckWithEnv env expr of
            Left err -> return $ Left err
            Right defTy -> do
              let finalTy = case maybeType of
                    Just annotatedTy -> let syntaxTy = syntaxTypeToType annotatedTy
                                        in if defTy == syntaxTy then defTy
                                           else error "Type mismatch in definition"
                    Nothing -> defTy
              let env' = Map.insert var finalTy env
              go env' rest
    go env (_ : rest) = go env rest
    
    collectConsecutiveLetrecsExtract :: [TopLevel] -> ([TopLevel], [TopLevel])
    collectConsecutiveLetrecsExtract [] = ([], [])
    collectConsecutiveLetrecsExtract (TLDef var maybeType expr : rest) =
      case expr of
        LetRec _ _ _ _ ->
          let (moreLetrecs, remaining) = collectConsecutiveLetrecsExtract rest
          in (TLDef var maybeType expr : moreLetrecs, remaining)
        _ -> ([], TLDef var maybeType expr : rest)
    collectConsecutiveLetrecsExtract (other : rest) = ([], other : rest)
    
    processMutualRecursionTypeExtract :: TypeEnv -> [TopLevel] -> IO (Either TypeError TypeEnv)
    processMutualRecursionTypeExtract env letrecs = do
      let funcTypes = map (\topLevel ->
            case topLevel of
              TLDef var _ _ -> (var, TVar var)
              _ -> error "processMutualRecursionTypeExtract: expected TLDef") letrecs
      return $ Right (Map.union (Map.fromList funcTypes) env)

