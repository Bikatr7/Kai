module Main where

import Syntax
import TypeChecker
import Evaluator
import Parser
import System.Environment
import System.IO
import Data.List (intercalate)
import qualified Data.Map as Map
import Paths_kai_lang (version)
import Data.Version (showVersion)
import Control.Monad (when)

versionString :: String
versionString = "Kai v" ++ showVersion version

examples :: [String]
examples =
  [ "1 + 2"
  , "3 * (4 - 2)"
  , "3 < 5"
  , "10 / 0"
  , "1 + true"
  , "if 3 < 5 then 1 else 0"
  , "if 3 < 5 then true else false"
  , "if 3 < 5 then 1 else false"
  , "\\x -> x + 1"
  , "(\\x -> x + 1) 5"
  , "(\\x -> x * 2) 10"
  , "(\\x -> if x > 0 then x else 0) (-5)"
  , "(\\f -> f 42) (\\x -> x + 1)"
  ]

runExpression :: Bool -> String -> IO ()
runExpression debug input = do
  when debug $ putStrLn $ "\nExpression: " ++ input
  
  case parseExpr input of
    Left parseErr -> putStrLn $ "Parse error: " ++ show parseErr
    Right expr -> do
      when debug $ putStrLn $ "AST: " ++ show expr
      
      when debug $ putStr "Type: "
      case typeCheck expr of
        Left err -> putStrLn $ "Type error: " ++ show err
        Right ty -> do
          when debug $ print ty
          
          when debug $ putStr "Evaluation: "
          result <- eval expr
          case result of
            Left err -> putStrLn $ "Runtime error: " ++ show err
            Right val -> when debug $ print val

runFile :: Bool -> FilePath -> [String] -> IO ()
runFile debug filename scriptArgs = do
  when debug $ putStrLn $ "Running file: " ++ filename
  content <- readFile filename
  -- Try single expression first, then fall back to multi-statement
  case parseFileExpr content of
    Left _ -> case parseStatements content of
      Left parseErr -> putStrLn $ "Parse error: " ++ show parseErr
      Right stmts -> runStatements debug scriptArgs stmts
    Right expr -> runSingleExpression debug scriptArgs expr

runSingleExpression :: Bool -> [String] -> Expr -> IO ()
runSingleExpression debug scriptArgs expr = do
  when debug $ putStrLn $ "AST: " ++ show expr

  when debug $ putStr "Type: "
  case typeCheck expr of
    Left err -> putStrLn $ "Type error: " ++ show err
    Right ty -> do
      when debug $ print ty

      when debug $ putStr "Evaluation: "
      let argsEnv = Map.singleton "__args__" (VList (map VStr scriptArgs))
      result <- evalWithEnv argsEnv expr
      case result of
        Left err -> putStrLn $ "Runtime error: " ++ show err
        Right val -> when debug $ print val

runStatements :: Bool -> [String] -> [Expr] -> IO ()
runStatements debug scriptArgs stmts = do
  if null stmts then putStrLn "No statements found" else do
    -- Execute all statements and show their output
    let argsEnv = Map.singleton "__args__" (VList (map VStr scriptArgs))
    mapM_ (\stmt -> do
      result <- evalWithEnv argsEnv stmt
      case result of
        Left err -> putStrLn $ "Runtime error: " ++ show err
        Right val -> return ()  -- Print statements handle their own output
      ) stmts

    let expr = last stmts  -- Last statement is the main expression
    when debug $ putStrLn $ "AST: " ++ show expr

    when debug $ putStr "Type: "
    case typeCheck expr of
      Left err -> putStrLn $ "Type error: " ++ show err
      Right ty -> do
        when debug $ print ty

        when debug $ putStr "Evaluation: "
        result <- eval expr
        case result of
          Left err -> putStrLn $ "Runtime error: " ++ show err
          Right val -> when debug $ print val

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> do
      putStrLn (unlines
        [ versionString
        , "Usage:"
        , "  kai                          # show help and examples"
        , "  kai FILE.kai [args...]       # run a script file with optional arguments"
        , "  kai --debug FILE.kai [args...] # run a script file with debug output"
        , "  kai -e 'EXPR'                # evaluate a one-liner expression"
        , "  kai --debug -e 'EXPR'        # evaluate with debug output"
        , "  kai --help                   # this message"
        ])
    ["-h"] -> do
      putStrLn (unlines
        [ versionString
        , "Usage:"
        , "  kai                          # show help and examples"
        , "  kai FILE.kai [args...]       # run a script file with optional arguments"
        , "  kai --debug FILE.kai [args...] # run a script file with debug output"
        , "  kai -e 'EXPR'                # evaluate a one-liner expression"
        , "  kai --debug -e 'EXPR'        # evaluate with debug output"
        , "  kai --help                   # this message"
        ])
    ["-e", exprStr] -> runExpression False exprStr
    ["--debug", "-e", exprStr] -> runExpression True exprStr
    [] -> do
      putStrLn $ versionString ++ " — pass a file, -e 'expr', or --help for usage."
    ("--debug":filename:scriptArgs) -> runFile True filename scriptArgs
    (filename:scriptArgs) -> runFile False filename scriptArgs 