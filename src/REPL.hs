{-# LANGUAGE LambdaCase #-}

module REPL
  ( runREPL
  , renderType
  ) where

import Control.Monad (foldM, when)
import Control.Monad.State (evalStateT)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char (isSpace)
import Data.IORef (IORef, newIORef, writeIORef)
import Data.Version (showVersion)
import Data.Void (Void)
import Paths_kai_lang (version)
import System.Directory (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), isAbsolute, takeDirectory)
import System.IO (hFlush, isEOF, stdout)
import Text.Megaparsec (ParseError(..), ParseErrorBundle(..))
import Text.Megaparsec.Error (ErrorItem(..))

import DataDeclarations (constructorScheme, dataConstructorsTypeEnv, dataConstructorsValueEnv)
import Evaluator (Env, RuntimeError(..), Value(..), evalWithEnv)
import Evaluator.Helpers (showValue)
import ModuleSystem (ModuleInfo(..), loadModule, loadModuleTypeEnvIO)
import Parser (parseExpr, parseProgram)
import Syntax
import TypeChecker (Type(..), TypeEnv, TypeError(..), infer, syntaxTypeToType, typeCheckWithEnv)
import TypeChecker.Substitution (applySubst, applySubstEnv, composeSubst, generalize, schemeIsInstanceOf)
import TypeChecker.Types (Scheme, Substitution, monoScheme, schemeType)
import TypeChecker.Unification (unify)

data ReplState = ReplState
  { replEnv :: Env
  , replTypeEnv :: TypeEnv
  , replCurrentDir :: FilePath
  , replLoadedFile :: Maybe FilePath
  , replArgs :: [String]
  }

data ReplInput
  = ReplCommand String
  | ReplProgram String

versionString :: String
versionString = "Kai v" ++ showVersion version

runREPL :: Bool -> [String] -> IO ExitCode
runREPL debug scriptArgs = do
  currentDir <- getCurrentDirectory
  putStrLn $ versionString ++ " REPL"
  putStrLn "Commands: :type EXPR, :load FILE, :reload, :quit, :help"
  putStrLn "Session args come from: kai repl arg1 arg2  (or: kai --repl arg1 arg2)"
  loop (baseState currentDir scriptArgs)
  where
    loop state = do
      input <- readReplInput
      case input of
        Nothing -> return ExitSuccess
        Just (ReplCommand commandLine) -> do
          commandResult <- handleCommand debug state commandLine
          case commandResult of
            Left message -> putStrLn message >> loop state
            Right (Just exitCode, _) -> return exitCode
            Right (Nothing, newState) -> loop newState
        Just (ReplProgram source)
          | all isSpace source -> loop state
          | otherwise -> do
              sourceResult <- handleProgramInput debug True state source
              case sourceResult of
                Left message -> putStrLn message >> loop state
                Right (Just exitCode, _) -> return exitCode
                Right (Nothing, newState) -> loop newState

baseState :: FilePath -> [String] -> ReplState
baseState currentDir scriptArgs =
  ReplState
    { replEnv = cliArgsEnv scriptArgs
    , replTypeEnv = Map.empty
    , replCurrentDir = currentDir
    , replLoadedFile = Nothing
    , replArgs = scriptArgs
    }

cliArgsEnv :: [String] -> Env
cliArgsEnv scriptArgs =
  let argValues = VList (map VStr scriptArgs)
  in Map.fromList
       [ ("__args__", argValues)
       , ("args", argValues)
       ]

readReplInput :: IO (Maybe ReplInput)
readReplInput = do
  putStr "kai> "
  hFlush stdout
  eof <- isEOF
  if eof
    then return Nothing
    else do
      firstLine <- getLine
      let trimmed = dropWhile isSpace firstLine
      if null trimmed
        then readReplInput
        else if ":" `List.isPrefixOf` trimmed
          then return $ Just $ ReplCommand trimmed
          else Just . ReplProgram <$> collectUntilComplete ".... " parseProgram firstLine

collectUntilComplete :: String -> (String -> Either (ParseErrorBundle String Void) a) -> String -> IO String
collectUntilComplete continuationPrompt parser = go
  where
    go current =
      case parser current of
        Right _ -> return current
        Left err
          | isIncompleteParse err -> do
              putStr continuationPrompt
              hFlush stdout
              eof <- isEOF
              if eof
                then return current
                else do
                  nextLine <- getLine
                  go (current ++ "\n" ++ nextLine)
          | otherwise -> return current

handleCommand :: Bool -> ReplState -> String -> IO (Either String (Maybe ExitCode, ReplState))
handleCommand debug state commandLine =
  case break isSpace commandLine of
    (":quit", _) -> return $ Right (Just ExitSuccess, state)
    (":q", _) -> return $ Right (Just ExitSuccess, state)
    (":help", _) -> do
      putStrLn "Commands:"
      putStrLn "  :type EXPR   show the inferred type of an expression"
      putStrLn "  :load FILE   reset the session and load a file"
      putStrLn "  :reload      reload the most recently loaded file"
      putStrLn "  :quit        exit the REPL"
      putStrLn "  End a data declaration or case branch with `|` to continue it on the next line."
      putStrLn "  Start with `kai repl foo bar` or `kai --repl foo bar` to populate args."
      return $ Right (Nothing, state)
    (":load", rest) -> do
      let rawPath = dropWhile isSpace rest
      if null rawPath
        then return $ Left "Usage: :load FILE"
        else loadFileIntoState debug state rawPath
    (":reload", _) ->
      case replLoadedFile state of
        Nothing -> return $ Left "No file has been loaded yet."
        Just loadedPath -> loadFileIntoState debug state loadedPath
    (":type", rest) -> do
      let initialExpr = dropWhile isSpace rest
      exprSource <-
        if null initialExpr
          then collectUntilComplete "type> " parseExpr ""
          else collectUntilComplete "type> " parseExpr initialExpr
      case parseExpr exprSource of
        Left parseErr -> return $ Left $ "Parse error: " ++ show parseErr
        Right expr ->
          case typeCheckWithEnv (replTypeEnv state) expr of
            Left err -> return $ Left $ "Type error: " ++ show err
            Right ty -> do
              putStrLn $ renderType ty
              return $ Right (Nothing, state)
    _ -> return $ Left $ "Unknown command: " ++ commandLine

loadFileIntoState :: Bool -> ReplState -> FilePath -> IO (Either String (Maybe ExitCode, ReplState))
loadFileIntoState debug state rawPath = do
  resolvedPath <- resolvePath (replCurrentDir state) rawPath
  exists <- doesFileExist resolvedPath
  if not exists
    then return $ Left $ "File not found: " ++ rawPath
    else do
      content <- readFile resolvedPath
      let resetState =
            (baseState (takeDirectory resolvedPath) (replArgs state))
              { replLoadedFile = Just resolvedPath
              }
      result <- handleProgramInput debug False resetState content
      case result of
        Left message -> return $ Left message
        Right (exitCode, newState) -> do
          putStrLn $ "Loaded " ++ rawPath
          return $ Right (exitCode, newState { replLoadedFile = Just resolvedPath })

resolvePath :: FilePath -> FilePath -> IO FilePath
resolvePath currentDir rawPath =
  makeAbsolute $
    if isAbsolute rawPath
      then rawPath
      else currentDir </> rawPath

handleProgramInput :: Bool -> Bool -> ReplState -> String -> IO (Either String (Maybe ExitCode, ReplState))
handleProgramInput debug announceDefinitions state source =
  case parseProgram source of
    Left parseErr -> return $ Left $ "Parse error: " ++ show parseErr
    Right (Program topLevels) -> processTopLevels debug announceDefinitions state topLevels

processTopLevels :: Bool -> Bool -> ReplState -> [TopLevel] -> IO (Either String (Maybe ExitCode, ReplState))
processTopLevels _ _ state [] = return $ Right (Nothing, state)
processTopLevels debug _ state [TLExpr expr] = do
  when debug $ putStrLn $ "AST: " ++ show expr
  case typeCheckWithEnv (replTypeEnv state) expr of
    Left err -> return $ Left $ "Type error: " ++ show err
    Right ty -> do
      when debug $ putStrLn $ "Type: " ++ renderType ty
      evalResult <- evalWithEnv (replEnv state) expr
      case evalResult of
        Left (ExitRequested code) -> return $ Right (Just (toExitCode code), state)
        Left err -> return $ Left $ "Runtime error: " ++ show err
        Right value -> do
          putStrLn $ showValue value
          return $ Right (Nothing, state)
processTopLevels _ _ _ (TLExpr _ : _) =
  return $ Left "Expressions must be at the end of the input."
processTopLevels debug announce state (TLImport moduleName : rest) = do
  typeEnvResult <- loadModuleTypeEnvIO (replCurrentDir state) moduleName
  case typeEnvResult of
    Left err -> return $ Left $ "Type error: " ++ show err
    Right importedTypeEnv -> do
      moduleResult <- loadModule evalWithEnv (replCurrentDir state) moduleName []
      case moduleResult of
        Left err -> return $ Left $ "Runtime error: " ++ err
        Right moduleInfo -> do
          let importedEnv = filterByExports (moduleEnv moduleInfo) (moduleExports moduleInfo)
          let newState =
                state
                  { replEnv = Map.union importedEnv (replEnv state)
                  , replTypeEnv = Map.union importedTypeEnv (replTypeEnv state)
                  }
          when announce $ putStrLn $ "imported " ++ moduleName
          processTopLevels debug announce newState rest
processTopLevels debug announce state (TLData typeName typeVars constructors : rest) = do
  let constructorTypeEnv = dataConstructorsTypeEnv typeName typeVars constructors
  let constructorValueEnv = dataConstructorsValueEnv constructors
  let newState =
        state
          { replEnv = Map.union constructorValueEnv (replEnv state)
          , replTypeEnv = Map.union constructorTypeEnv (replTypeEnv state)
          }
  when announce $
    mapM_ (putStrLn . renderConstructorBinding typeName typeVars) constructors
  processTopLevels debug announce newState rest
processTopLevels debug announce state (TLExport _ : rest) =
  processTopLevels debug announce state rest
processTopLevels debug announce state defs@(TLDef _ _ expr : _)
  | isLetrecExpr expr = do
      let (letrecs, remaining) = collectConsecutiveLetrecs defs
      case processMutualRecursionTypes (replTypeEnv state) letrecs of
        Left err -> return $ Left $ "Type error: " ++ show err
        Right (newTypeEnv, bindingTypes) -> do
          valueResult <- processMutualRecursionValues (replEnv state) letrecs
          case valueResult of
            Left (ExitRequested code) -> return $ Right (Just (toExitCode code), state)
            Left err -> return $ Left $ "Runtime error: " ++ show err
            Right newEnv -> do
              when announce $
                mapM_ (\(name, ty) -> putStrLn $ name ++ " : " ++ renderType ty) bindingTypes
              processTopLevels
                debug
                announce
                state { replEnv = newEnv, replTypeEnv = newTypeEnv }
                remaining
processTopLevels debug announce state (TLDef var maybeType expr : rest) =
  case inferDefinitionType (replTypeEnv state) var maybeType expr of
    Left err -> return $ Left $ "Type error: " ++ show err
    Right (newTypeEnv, defType) -> do
      when debug $ putStrLn $ "AST: " ++ show expr
      evalResult <- evalWithEnv (replEnv state) expr
      case evalResult of
        Left (ExitRequested code) -> return $ Right (Just (toExitCode code), state)
        Left err -> return $ Left $ "Runtime error: " ++ show err
        Right value -> do
          when announce $ putStrLn $ var ++ " : " ++ renderType defType
          let newEnv =
                if var == "_"
                  then replEnv state
                  else Map.insert var value (replEnv state)
          processTopLevels debug announce state { replEnv = newEnv, replTypeEnv = newTypeEnv } rest

inferDefinitionType :: TypeEnv -> String -> Maybe SyntaxType -> Expr -> Either TypeError (TypeEnv, Type)
inferDefinitionType env var maybeType expr = do
  (subst, defType) <- evalStateT (infer env expr) 0
  let inferredType = applySubst subst defType
  let baseEnv = applySubstEnv subst env
  case maybeType of
    Just annotatedType -> do
      let expectedType = syntaxTypeToType annotatedType
      unifySubst <- unify inferredType expectedType
      let finalSubst = composeSubst unifySubst subst
      let finalType = applySubst finalSubst inferredType
      let finalBaseEnv = applySubstEnv finalSubst env
      let newEnv =
            if var == "_"
              then finalBaseEnv
              else Map.insert var (generalize finalBaseEnv finalType) finalBaseEnv
      return (newEnv, finalType)
    Nothing -> do
      let newEnv =
            if var == "_"
              then baseEnv
              else Map.insert var (generalize baseEnv inferredType) baseEnv
      return (newEnv, inferredType)

processMutualRecursionTypes :: TypeEnv -> [TopLevel] -> Either TypeError (TypeEnv, [(String, Type)])
processMutualRecursionTypes env letrecs = do
  let funcTypes = map toFuncType letrecs
  let mutualEnv = Map.union (Map.fromList [(var, scheme) | (var, _, _, scheme) <- funcTypes]) env
  results <- mapM (typeCheckLetrec env mutualEnv) funcTypes
  combinedSubst <- mergeMutualSubstitutions (map fst results)
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
  return (finalEnv, zip [var | (var, _, _, _) <- funcTypes] finalTypes)
  where
    toFuncType :: TopLevel -> (String, Maybe Type, Type, Scheme)
    toFuncType (TLDef var Nothing _) = (var, Nothing, TVar var, monoScheme (TVar var))
    toFuncType (TLDef var (Just sType) _) =
      let annotatedType = syntaxTypeToType sType
      in (var, Just annotatedType, annotatedType, generalize env annotatedType)
    toFuncType _ = error "processMutualRecursionTypes: expected letrec definition"

    typeCheckLetrec :: TypeEnv -> TypeEnv -> (String, Maybe Type, Type, Scheme) -> Either TypeError (Substitution, Type)
    typeCheckLetrec outerEnv mutualEnv (var, maybeAnnotatedType, assumedType, _) =
      case Map.lookup var letrecMap of
        Just (TLDef _ _ (LetRec _ _ value _)) -> do
          (subst, valueType) <- evalStateT (infer mutualEnv value) 0
          case maybeAnnotatedType of
            Just annotatedType -> do
              let baseEnv = applySubstEnv subst outerEnv
              let annotatedScheme = generalize baseEnv (applySubst subst annotatedType)
              let inferredScheme = generalize baseEnv (applySubst subst valueType)
              matches <- evalStateT (schemeIsInstanceOf annotatedScheme inferredScheme) 0
              if matches
                then return (subst, applySubst subst annotatedType)
                else Left $ GeneralTypeError "Recursive definition does not satisfy its annotated polymorphic type"
            Nothing -> do
              let assumedType' = applySubst subst assumedType
              unifySubst <- unify assumedType' (applySubst subst valueType)
              let finalSubst = composeSubst unifySubst subst
              let finalType = applySubst finalSubst assumedType'
              return (finalSubst, finalType)
        _ -> Left $ GeneralTypeError "Expected letrec definition"

    letrecMap = Map.fromList
      [ (name, topLevel)
      | topLevel@(TLDef name _ (LetRec _ _ _ _)) <- letrecs
      ]

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

processMutualRecursionValues :: Env -> [TopLevel] -> IO (Either RuntimeError Env)
processMutualRecursionValues env letrecs = do
  refs <- mapM (\_ -> newIORef (VFun "_placeholder" (IntLit 0) Map.empty)) letrecs
  let refMap =
        Map.fromList
          [ (var, VRef ref)
          | (TLDef var _ _, ref) <- zip letrecs refs
          ]
  let mutualEnv = Map.union refMap env
  results <- mapM (evalLetrecBody mutualEnv) letrecs
  case firstLeft results of
    Just err -> return $ Left err
    Nothing -> do
      let values = rightsOnly results
      updateResults <- mapM updateRef (zip values refs)
      case firstLeft updateResults of
        Just err -> return $ Left err
        Nothing -> return $ Right mutualEnv
  where
    evalLetrecBody :: Env -> TopLevel -> IO (Either RuntimeError Value)
    evalLetrecBody mutualEnv (TLDef _ _ (LetRec _ _ value _)) = evalWithEnv mutualEnv value
    evalLetrecBody _ _ = return $ Left $ TypeError "Expected letrec definition"

    updateRef :: (Value, IORef Value) -> IO (Either RuntimeError ())
    updateRef (value, ref) =
      case value of
        VFun {} -> writeIORef ref value >> return (Right ())
        _ -> return $ Left $ TypeError ("LetRec value must be a function, got: " ++ show value)

rightsOnly :: [Either e a] -> [a]
rightsOnly = foldr (\result acc -> case result of Right value -> value : acc; Left _ -> acc) []

firstLeft :: [Either e a] -> Maybe e
firstLeft [] = Nothing
firstLeft (Left err : _) = Just err
firstLeft (_ : rest) = firstLeft rest

filterByExports :: Map.Map String a -> [String] -> Map.Map String a
filterByExports env [] = env
filterByExports env exports = Map.filterWithKey (\name _ -> name `elem` exports) env

renderConstructorBinding :: String -> [String] -> DataConstructor -> String
renderConstructorBinding typeName typeVars constructorDecl@(DataConstructor constructorName _) =
  constructorName ++ " : " ++ renderType (schemeType (constructorScheme typeName typeVars constructorDecl))

renderType :: Type -> String
renderType ty =
  case ty of
    TFun left right -> renderTypeAtom left ++ " -> " ++ renderType right
    _ -> renderTypeAtom ty

renderTypeAtom :: Type -> String
renderTypeAtom = \case
  TInt -> "Int"
  TBool -> "Bool"
  TString -> "String"
  TUnit -> "Unit"
  TVar name -> name
  TCustom name [] -> name
  TCustom name args -> unwords (name : map renderTypeAtom args)
  TMaybe ty -> "Maybe " ++ renderTypeAtom ty
  TEither left right -> "Either " ++ renderTypeAtom left ++ " " ++ renderTypeAtom right
  TList ty -> "[" ++ renderType ty ++ "]"
  TRecord fields ->
    "{" ++ List.intercalate ", " [name ++ ": " ++ renderType fieldType | (name, fieldType) <- Map.toList fields] ++ "}"
  TTuple tys ->
    "(" ++ List.intercalate ", " (map renderType tys) ++ ")"
  TFun left right ->
    "(" ++ renderType left ++ " -> " ++ renderType right ++ ")"

isLetrecExpr :: Expr -> Bool
isLetrecExpr (LetRec _ _ _ _) = True
isLetrecExpr _ = False

collectConsecutiveLetrecs :: [TopLevel] -> ([TopLevel], [TopLevel])
collectConsecutiveLetrecs [] = ([], [])
collectConsecutiveLetrecs (topLevel@(TLDef _ _ expr) : rest)
  | isLetrecExpr expr =
      let (moreLetrecs, remaining) = collectConsecutiveLetrecs rest
      in (topLevel : moreLetrecs, remaining)
collectConsecutiveLetrecs topLevels = ([], topLevels)

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode code = ExitFailure code

isIncompleteParse :: ParseErrorBundle String Void -> Bool
isIncompleteParse bundle = any isIncompleteError (bundleErrors bundle)
  where
    isIncompleteError :: ParseError String Void -> Bool
    isIncompleteError (TrivialError _ unexpected _) = unexpected == Just EndOfInput
    isIncompleteError _ = False
