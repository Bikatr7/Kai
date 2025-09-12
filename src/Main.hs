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
  case parseStatements content of
    Left parseErr -> putStrLn $ "Parse error: " ++ show parseErr
    Right stmts -> do
      if null stmts then putStrLn "No statements found" else do
        -- Execute all statements and show their output
        mapM_ (\stmt -> do
          result <- evalWithEnvIO Map.empty stmt
          case result of
            Left err -> putStrLn $ "Runtime error: " ++ show err
            Right val -> return ()  -- Print statements handle their own output
          ) stmts
        
        let expr = last stmts  -- Last statement is the main expression
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
    ["--help"] -> do
      putStrLn (unlines
        [ versionString
        , "Usage:"
        , "  kai                      # show help and examples"
        , "  kai FILE.kai             # run a script file"
        , "  kai -e 'EXPR'            # evaluate a one-liner expression"
        , "  kai --help               # this message"
        ])
    ["-h"] -> do
      putStrLn (unlines
        [ versionString
        , "Usage:"
        , "  kai                      # show help and examples"
        , "  kai FILE.kai             # run a script file"
        , "  kai -e 'EXPR'            # evaluate a one-liner expression"
        , "  kai --help               # this message"
        ])
    ["-e", exprStr] -> runExpression exprStr
    [] -> do
      putStrLn $ versionString ++ " — pass a file, -e 'expr', or --help for usage."
    [filename] -> runFile filename
    _ -> putStrLn "Usage: kai [filename]" 