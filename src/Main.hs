module Main where

import Syntax
import TypeChecker
import Evaluator
import Parser
import System.Environment
import System.IO

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
  ]

runExpression :: String -> IO ()
runExpression input = do
  putStrLn $ "\nExpression: " ++ input
  
  case parseExpr input of
    Left parseErr -> putStrLn $ "Parse error: " ++ show parseErr
    Right expr -> do
      putStrLn $ "AST: " ++ show expr
      
      putStr "Type: "
      case typeCheck expr of
        Left err -> putStrLn $ "Type error: " ++ show err
        Right ty -> do
          putStrLn $ show ty
          
          putStr "Evaluation: "
          case eval expr of
            Left err -> putStrLn $ "Runtime error: " ++ show err
            Right val -> putStrLn $ show val

runFile :: FilePath -> IO ()
runFile filename = do
  putStrLn $ "Running file: " ++ filename
  content <- readFile filename
  case parseFile filename content of
    Left parseErr -> putStrLn $ "Parse error: " ++ show parseErr
    Right expr -> do
      putStrLn $ "AST: " ++ show expr
      
      putStr "Type: "
      case typeCheck expr of
        Left err -> putStrLn $ "Type error: " ++ show err
        Right ty -> do
          putStrLn $ show ty
          
          putStr "Evaluation: "
          case eval expr of
            Left err -> putStrLn $ "Runtime error: " ++ show err
            Right val -> putStrLn $ show val

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Kai Language Interpreter"
      putStrLn "======================="
      putStrLn "Running example expressions:"
      mapM_ runExpression examples
    [filename] -> runFile filename
    _ -> putStrLn "Usage: kai [filename]" 