module CLI (
    runCLI,
    versionString
) where

import Evaluator.IOOps (cliArgsEnv)
import Syntax
import TypeChecker (inferProgramWithWarningsIO)
import TypeChecker.Warnings (reportWarnings)
import qualified Data.Map as Map
import Evaluator
import REPL (runREPL)
import Parser
import Diagnostics (renderTypeError, renderRuntimeError)
import Text.Megaparsec (errorBundlePretty)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
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

runtimeToExitCode :: Bool -> RuntimeError -> IO ExitCode
runtimeToExitCode _ (ExitRequested 0) = return ExitSuccess
runtimeToExitCode _ (ExitRequested code) = return $ ExitFailure code
runtimeToExitCode debug err = reportFailure $
  if debug then "Runtime error: " ++ show err else renderRuntimeError err

runExpression :: Bool -> String -> IO ExitCode
runExpression debug input = do
  when debug $ putStrLn $ "\nExpression: " ++ input
  case parseLocatedProgram "<expression>" input of
    Right program -> runProgram debug "." [] program
    Left parseErr -> reportFailure $ "Parse error: " ++ errorBundlePretty parseErr

runProgram :: Bool -> FilePath -> [String] -> Program -> IO ExitCode
runProgram debug currentDir scriptArgs program = do
  when debug $ putStrLn $ "Program AST: " ++ show program
  when debug $ putStr "Type: "
  typeResult <- inferProgramWithWarningsIO ModuleSystem.loadModuleTypeEnvWithWarningsIO currentDir Map.empty program
  case typeResult of
    Left err -> reportFailure $ if debug then "Type error: " ++ show err else renderTypeError err
    Right ((_,ty),warnings) -> do
      reportWarnings warnings
      when debug $ print ty
      when debug $ putStr "Evaluation: "
      result <- evalProgramWithEnv (cliArgsEnv scriptArgs) currentDir program
      case result of
        Left err -> runtimeToExitCode debug err
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
      case parseLocatedProgram filename content of
        Right program -> do
          when debug $ putStrLn "Parsed as program"
          runProgram debug currentDir scriptArgs program
        Left parseErr -> reportFailure $ "Parse error: " ++ errorBundlePretty parseErr

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
