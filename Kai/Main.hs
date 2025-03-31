module Main where

import Syntax
import TypeChecker
import Evaluator

examples :: [(String, Expr)]
examples =
  [ ("1 + 2", Add (IntLit 1) (IntLit 2))
  , ("3 * (4 - 2)", Mul (IntLit 3) (Sub (IntLit 4) (IntLit 2)))
  , ("3 < 5", Lt (IntLit 3) (IntLit 5))
  , ("10 / 0", Div (IntLit 10) (IntLit 0))
  , ("1 + true", Add (IntLit 1) (BoolLit True))
  , ("if 3 < 5 then 1 else 0", If (Lt (IntLit 3) (IntLit 5)) (IntLit 1) (IntLit 0))
  , ("if 3 < 5 then true else false", If (Lt (IntLit 3) (IntLit 5)) (BoolLit True) (BoolLit False))
  , ("if 3 < 5 then 1 else false", If (Lt (IntLit 3) (IntLit 5)) (IntLit 1) (BoolLit False))
  ]

runExample :: (String, Expr) -> IO ()
runExample (desc, expr) = do
  putStrLn $ "\nExpression: " ++ desc
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
  putStrLn "Kai Language Interpreter"
  putStrLn "======================="
  mapM_ runExample examples 