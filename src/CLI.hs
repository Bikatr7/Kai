module CLI (
    runCLI,
    versionString
) where

import Syntax
import TypeChecker (typeCheck, typeCheckProgramWithDirIO)
import Evaluator
import Parser
import Evaluator.Types (Value(..))
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import Data.List (intercalate)
import qualified Data.Map as Map
import Paths_kai_lang (version)
import Data.Version (showVersion)
import Control.Monad (when)
import Control.Exception (try, IOException)
import qualified ModuleSystem

versionString :: String
versionString = "Kai v" ++ showVersion version

usageText :: String
usageText = unlines
  [ versionString
  , "Usage:"
  , "  kai                          # show help and examples"
  , "  kai FILE.kai [args...]       # run a script file with optional arguments"
  , "  kai --debug FILE.kai [args...] # run a script file with debug output"
  , "  kai -e 'EXPR'                # evaluate a one-liner expression"
  , "  kai --debug -e 'EXPR'        # evaluate with debug output"
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
  putStrLn message
  return (ExitFailure 1)

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
              Left err -> reportFailure $ "Runtime error: " ++ show err
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
        Left err -> reportFailure $ "Runtime error: " ++ show err
        Right val -> do
          when debug $ print val
          return ExitSuccess

runFile :: Bool -> FilePath -> [String] -> IO ExitCode
runFile debug filename scriptArgs = do
  when debug $ putStrLn $ "Running file: " ++ filename
  readResult <- try (readFile filename) :: IO (Either IOException String)
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
        Left err -> reportFailure $ "Runtime error: " ++ show err
        Right val -> do
          when debug $ print val
          return ExitSuccess

runStatements :: Bool -> [String] -> [Expr] -> IO ExitCode
runStatements debug scriptArgs stmts =
  if null stmts
    then do
      putStrLn "No statements found"
      return ExitSuccess
    else do
      let argsEnv = cliArgsEnv scriptArgs
      result <- evalStatements argsEnv stmts
      case result of
        Left err -> reportFailure $ "Runtime error: " ++ show err
        Right val -> do
          let expr = last stmts
          when debug $ putStrLn $ "AST: " ++ show expr
          when debug $ putStr "Type: "
          case typeCheck expr of
            Left err -> reportFailure $ "Type error: " ++ show err
            Right ty -> do
              when debug $ print ty
              when debug $ putStr "Evaluation: "
              when debug $ print val
              return ExitSuccess
  where
    evalStatements :: Map.Map String Value -> [Expr] -> IO (Either RuntimeError Value)
    evalStatements _ [] = return $ Right VUnit
    evalStatements env [stmt] = evalWithEnv env stmt
    evalStatements env (stmt : rest) = do
      result <- evalWithEnv env stmt
      case result of
        Left err -> return $ Left err
        Right _ -> evalStatements env rest

runCLI :: [String] -> IO ExitCode
runCLI args =
  case args of
    ["--help"] -> putStrLn usageText >> return ExitSuccess
    ["-h"] -> putStrLn usageText >> return ExitSuccess
    ["-e", exprStr] -> runExpression False exprStr
    ["--debug", "-e", exprStr] -> runExpression True exprStr
    [] -> do
      putStrLn $ versionString ++ " — pass a file, -e 'expr', or --help for usage."
      return ExitSuccess
    ("--debug" : filename : scriptArgs) -> runFile True filename scriptArgs
    (filename : scriptArgs) -> runFile False filename scriptArgs
