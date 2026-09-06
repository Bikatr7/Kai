module CLI (
    runCLI,
    versionString
) where

import Syntax
import TypeChecker (typeCheck, typeCheckProgramWithDirIO)
import Evaluator
import REPL (runREPL)
import Parser
import Evaluator.Types (Value(..))
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import Data.List (intercalate)
import qualified Data.Map as Map
import Paths_kai_lang (version)
import Data.Version (showVersion)
import Control.Monad (when, void)
import Control.Exception (IOException, try)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import SourceIO (readSourceFile)
import ScriptCheck (checkScriptFile)
import qualified ModuleSystem

versionString :: String
versionString = "Kai v" ++ showVersion version

usageText :: String
usageText = unlines
  [ versionString
  , "Usage:"
  , "  kai                          # start the REPL"
  , "  kai repl [args...]           # start the REPL with args available via args"
  , "  kai --repl [args...]         # same as above"
  , "  kai FILE.kai [args...]       # run a script file with optional arguments"
  , "  kai --debug FILE.kai [args...] # run a script file with debug output"
  , "  kai -e 'EXPR'                # evaluate a one-liner expression"
  , "  kai --debug -e 'EXPR'        # evaluate with debug output"
  , "  kai --check FILE.kai         # verify script expectation directives"
  , "  kai --version, kai -V        # show the Kai version"
  , "  kai --help                   # this message"
  ]

cliArgsEnv :: [String] -> Map.Map String Value
cliArgsEnv scriptArgs =
  let argValues = VList (map VStr scriptArgs)
  in Map.fromList
       [ ("__args__", argValues)
       , ("args", argValues)
       ]

reportFailure :: String -> IO ExitCode
reportFailure message = do
  written <- try (putStrLn message >> hFlush stdout) :: IO (Either IOException ())
  case written of
    Left _ -> reportToStderr message
    Right () -> pure ()
  return (ExitFailure 1)

reportToStderr :: String -> IO ()
reportToStderr message =
  void (try (hPutStrLn stderr message >> hFlush stderr) :: IO (Either IOException ()))

runtimeToExitCode :: RuntimeError -> IO ExitCode
runtimeToExitCode (ExitRequested 0) = return ExitSuccess
runtimeToExitCode (ExitRequested code) = return $ ExitFailure code
runtimeToExitCode err = reportFailure $ "Runtime error: " ++ show err

runExpression :: Bool -> String -> IO ExitCode
runExpression debug input = do
  when debug $ putStrLn $ "\nExpression: " ++ input
  case parseProgram input of
    Right program -> runProgram debug "." [] program
    Left _ -> case parseExpr input of
      Left parseErr -> reportFailure $ "Parse error: " ++ show parseErr
      Right expr -> do
        when debug $ putStrLn $ "AST: " ++ show expr
        when debug $ putStr "Type: "
        case typeCheck expr of
          Left err -> reportFailure $ "Type error: " ++ show err
          Right ty -> do
            when debug $ print ty
            when debug $ putStr "Evaluation: "
            result <- eval expr
            case result of
              Left err -> runtimeToExitCode err
              Right val -> do
                when debug $ print val
                return ExitSuccess

runProgram :: Bool -> FilePath -> [String] -> Program -> IO ExitCode
runProgram debug currentDir scriptArgs program = do
  when debug $ putStrLn $ "Program AST: " ++ show program
  when debug $ putStr "Type: "
  typeResult <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO currentDir program
  case typeResult of
    Left err -> reportFailure $ "Type error: " ++ show err
    Right ty -> do
      when debug $ print ty
      when debug $ putStr "Evaluation: "
      result <- evalProgramWithEnv (cliArgsEnv scriptArgs) currentDir program
      case result of
        Left err -> runtimeToExitCode err
        Right val -> do
          when debug $ print val
          return ExitSuccess

runFile :: Bool -> FilePath -> [String] -> IO ExitCode
runFile debug filename scriptArgs = do
  when debug $ putStrLn $ "Running file: " ++ filename
  readResult <- readSourceFile filename
  case readResult of
    Left ioErr -> reportFailure $ "IO error: " ++ show ioErr
    Right content -> do
      let currentDir = takeDirectory filename
      case parseProgram content of
        Right program -> do
          when debug $ putStrLn "Parsed as program"
          runProgram debug currentDir scriptArgs program
        Left _ -> do
          when debug $ putStrLn "Falling back to legacy parsing"
          case parseFileExpr content of
            Left _ -> case parseStatements content of
              Left parseErr -> reportFailure $ "Parse error: " ++ show parseErr
              Right stmts -> runStatements debug scriptArgs stmts
            Right expr -> runSingleExpression debug scriptArgs expr

runSingleExpression :: Bool -> [String] -> Expr -> IO ExitCode
runSingleExpression debug scriptArgs expr = do
  when debug $ putStrLn $ "AST: " ++ show expr
  when debug $ putStr "Type: "
  case typeCheck expr of
    Left err -> reportFailure $ "Type error: " ++ show err
    Right ty -> do
      when debug $ print ty
      when debug $ putStr "Evaluation: "
      result <- evalWithEnv (cliArgsEnv scriptArgs) expr
      case result of
        Left err -> runtimeToExitCode err
        Right val -> do
          when debug $ print val
          return ExitSuccess

runStatements :: Bool -> [String] -> [Expr] -> IO ExitCode
runStatements debug scriptArgs stmts =
  runProgram debug "." scriptArgs (Program (map TLExpr stmts))

runCLI :: [String] -> IO ExitCode
runCLI args = do
  result <- try run :: IO (Either IOException ExitCode)
  case result of
    Right code -> return code
    Left err -> reportToStderr ("IO error: " ++ show err) >> return (ExitFailure 1)
  where
    run = do
      code <- case args of
        "--debug" : rest -> runCLIWithDebug True rest
        _ -> runCLIWithDebug False args
      when (code == ExitSuccess) $ hFlush stdout
      return code

runCLIWithDebug :: Bool -> [String] -> IO ExitCode
runCLIWithDebug debug args =
  case args of
    ["--help"] -> putStrLn usageText >> return ExitSuccess
    ["-h"] -> putStrLn usageText >> return ExitSuccess
    ["--version"] -> putStrLn versionString >> return ExitSuccess
    ["-V"] -> putStrLn versionString >> return ExitSuccess
    ["--check", filename] -> do
      result <- checkScriptFile filename
      case result of
        Left err -> reportFailure $ "Script check failed: " ++ err
        Right () -> putStrLn "Script checks passed" >> return ExitSuccess
    ["-e", exprStr] -> runExpression debug exprStr
    [] -> runREPL debug []
    ("repl" : scriptArgs) -> runREPL debug scriptArgs
    ("--repl" : scriptArgs) -> runREPL debug scriptArgs
    (filename : scriptArgs) -> runFile debug filename scriptArgs
