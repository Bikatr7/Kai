module REPL
  ( runREPL
  , renderType
  ) where

import Control.Monad (when)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Data.Version (showVersion)
import Data.Void (Void)
import Paths_kai_lang (version)
import System.Directory (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), isAbsolute, takeDirectory)
import System.IO (hFlush, isEOF, stdout)
import Text.Megaparsec (ParseError(..), ParseErrorBundle(..), errorBundlePretty)
import Text.Megaparsec.Error (ErrorItem(..))

import DataDeclarations (constructorScheme, registerDataDeclaration, mergeTypeEnvironments, dataConstructorsValueEnv)
import Evaluator (Env, RuntimeError(..), evalWithEnv)
import Evaluator.Helpers (showValue)
import ModuleSystem (filterByExports, ModuleInfo(..), loadModule, loadModuleTypeEnvIO, loadModuleTypeEnvWithWarningsIO)
import Parser (parseExpr, parseProgram, parseLocatedExpr, parseLocatedProgram)
import Diagnostics (renderTypeError, renderRuntimeError)
import Evaluator.IOOps (cliArgsEnv)
import Syntax
import TypeChecker (Type(..), TypeEnv, typeCheckWithEnv, typeCheckWithWarnings, inferProgramWithWarningsIO, inferDefinitionType, inferRecursiveDefinitions)
import TypeChecker.Warnings (reportWarnings)
import TypeChecker.Pretty (renderType)
import TypeChecker.Types (schemeType, Predicate(..))
import Evaluator.Recursion (initializeRecursiveBindings)
import SourceIO (readSourceFile)

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
      case parseLocatedExpr "<repl>" exprSource of
        Left parseErr -> return $ Left $ "Parse error: " ++ errorBundlePretty parseErr
        Right expr ->
          case typeCheckWithWarnings (replTypeEnv state) expr of
            Left err -> return $ Left $ if debug then "Type error: " ++ show err else renderTypeError err
            Right (ty,warnings) -> do
              reportWarnings warnings
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
      source <- readSourceFile resolvedPath
      case source of
        Left err -> return $ Left $ "IO error: " ++ show err
        Right content -> do
          let resetState = (baseState (takeDirectory resolvedPath) (replArgs state))
                             { replLoadedFile = Just resolvedPath }
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
  case parseLocatedProgram (if announceDefinitions then "<repl>" else fromMaybe "<repl>" (replLoadedFile state)) source of
    Left parseErr -> return $ Left $ "Parse error: " ++ errorBundlePretty parseErr
    Right program@(Program topLevels) -> do
      checked <- inferProgramWithWarningsIO loadModuleTypeEnvWithWarningsIO (replCurrentDir state) (replTypeEnv state) program
      case checked of
        Left err -> return $ Left $ if debug then "Type error: " ++ show err else renderTypeError err
        Right (_,warnings) -> do
          reportWarnings warnings
          processTopLevels debug announceDefinitions state topLevels

processTopLevels :: Bool -> Bool -> ReplState -> [TopLevel] -> IO (Either String (Maybe ExitCode, ReplState))
processTopLevels _ _ state [] = return $ Right (Nothing, state)
processTopLevels debug announce state (TLAt _ level:rest) = processTopLevels debug announce state (level:rest)
processTopLevels debug announce state (TLExpr expr : rest) = do
  when debug $ putStrLn $ "AST: " ++ show expr
  case typeCheckWithEnv (replTypeEnv state) expr of
    Left err -> return $ Left $ if debug then "Type error: " ++ show err else renderTypeError err
    Right ty -> do
      when debug $ putStrLn $ "Type: " ++ renderType ty
      evalResult <- evalWithEnv (replEnv state) expr
      case evalResult of
        Left (ExitRequested code) -> return $ Right (Just (toExitCode code), state)
        Left err -> return $ Left $ if debug then "Runtime error: " ++ show err else renderRuntimeError err
        Right value -> do
          when (null rest) $ putStrLn $ showValue value
          processTopLevels debug announce state rest
processTopLevels debug announce state (TLImport importedName : rest) = do
  typeEnvResult <- loadModuleTypeEnvIO (replCurrentDir state) importedName
  case typeEnvResult >>= (`mergeTypeEnvironments` replTypeEnv state) of
    Left err -> return $ Left $ if debug then "Type error: " ++ show err else renderTypeError err
    Right importedTypeEnv -> do
      moduleResult <- loadModule evalWithEnv (replCurrentDir state) importedName []
      case moduleResult of
        Left err -> return $ Left $ "Runtime error: " ++ err
        Right moduleInfo -> do
          let importedEnv = filterByExports (moduleEnv moduleInfo) (moduleExports moduleInfo)
          let newState =
                state
                  { replEnv = Map.union importedEnv (replEnv state)
                  , replTypeEnv = importedTypeEnv
                  }
          when announce $ putStrLn $ "imported " ++ importedName
          processTopLevels debug announce newState rest
processTopLevels debug announce state (TLData typeName typeVars constructors : rest) =
  case registerDataDeclaration (replTypeEnv state) typeName typeVars constructors of
    Left err -> return $ Left $ if debug then "Type error: " ++ show err else renderTypeError err
    Right next -> do
      let newState = state
            { replEnv = Map.union (dataConstructorsValueEnv constructors) (replEnv state)
            , replTypeEnv = next }
      when announce $ mapM_ (putStrLn . renderConstructorBinding typeName typeVars) constructors
      processTopLevels debug announce newState rest
processTopLevels debug announce state (TLExport _ : rest) =
  processTopLevels debug announce state rest
processTopLevels debug announce state defs@(TLDef _ _ expr : _)
  | isLetrecExpr expr = do
      let (letrecs, remaining) = collectConsecutiveLetrecs defs
      case inferRecursiveDefinitions (replTypeEnv state) letrecs of
        Left err -> return $ Left $ if debug then "Type error: " ++ show err else renderTypeError err
        Right (newTypeEnv, bindingTypes) -> do
          valueResult <- initializeRecursiveBindings evalWithEnv (replEnv state) [(name, value) | TLDef name _ (LetRec _ _ value _) <- letrecs]
          case valueResult of
            Left (ExitRequested code) -> return $ Right (Just (toExitCode code), state)
            Left err -> return $ Left $ if debug then "Runtime error: " ++ show err else renderRuntimeError err
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
    Left err -> return $ Left $ if debug then "Type error: " ++ show err else renderTypeError err
    Right (newTypeEnv, defType) -> do
      when debug $ putStrLn $ "AST: " ++ show expr
      evalResult <- evalWithEnv (replEnv state) expr
      case evalResult of
        Left (ExitRequested code) -> return $ Right (Just (toExitCode code), state)
        Left err -> return $ Left $ if debug then "Runtime error: " ++ show err else renderRuntimeError err
        Right value -> do
          when announce $ putStrLn $ var ++ " : " ++ renderType defType
          let newEnv =
                if var == "_"
                  then replEnv state
                  else Map.insert var value (replEnv state)
          processTopLevels debug announce state { replEnv = newEnv, replTypeEnv = newTypeEnv } rest

renderConstructorBinding :: String -> [String] -> DataConstructor -> String
renderConstructorBinding typeName typeVars constructorDecl@(DataConstructor constructorName _) =
  constructorName ++ " : " ++ renderType (schemeType (constructorScheme typeName typeVars constructorDecl))

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
