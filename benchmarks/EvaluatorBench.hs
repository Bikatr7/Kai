module EvaluatorBench where

import Criterion
import Parser (parseExpr)
import Evaluator (evalPure, evalPureWithEnv, Value)
import qualified Data.Map as Map

evalBenchmarks :: Benchmark
evalBenchmarks = bgroup "Evaluator Benchmarks"
  [ bgroup "Arithmetic"
      [ bench "Simple addition" $ nf (\() -> evalPureExpr "1 + 2") ()
      , bench "Complex arithmetic" $ nf (\() -> evalPureExpr "((1 + 2) * 3 - 4) / 2") ()
      , bench "Large arithmetic" $ nf (\() -> evalPureExpr (concat (replicate 100 "1 + ") ++ "0")) ()
      ]

  , bgroup "Boolean Logic"
      [ bench "Simple boolean" $ nf (\() -> evalPureExpr "true and false") ()
      , bench "Complex boolean" $ nf (\() -> evalPureExpr "true and (false or (true and false))") ()
      , bench "Comparison chain" $ nf (\() -> evalPureExpr "1 < 2 and 2 < 3 and 3 < 4") ()
      ]

  , bgroup "Conditionals"
      [ bench "Simple if" $ nf (\() -> evalPureExpr "if true then 1 else 2") ()
      , bench "Nested if" $ nf (\() -> evalPureExpr "if 1 < 2 then if 3 < 4 then 5 else 6 else 7") ()
      ]

  , bgroup "Functions"
      [ bench "Identity function" $ nf (\() -> evalPureExpr "(\\x -> x) 42") ()
      , bench "Simple lambda" $ nf (\() -> evalPureExpr "let f = \\x -> x * 2 in f 21") ()
      , bench "Higher order" $ nf (\() -> evalPureExpr "let apply = \\f x -> f x in apply (\\y -> y + 1) 5") ()
      ]

  , bgroup "Recursion"
      [ bench "Simple recursion" $ nf (\() -> evalPureExpr "letrec sum = \\n -> if n <= 0 then 0 else n + sum (n - 1) in sum 5") ()
      , bench "Fibonacci (n=8)" $ nf (\() -> evalPureExpr "letrec fib = \\n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 8") ()
      , bench "Factorial (n=8)" $ nf (\() -> evalPureExpr "letrec fact = \\n -> if n <= 1 then 1 else n * fact (n - 1) in fact 8") ()
      ]

  , bgroup "Data Structures"
      [ bench "List creation" $ nf (\() -> evalPureExpr "[1,2,3,4,5]") ()
      , bench "List operations" $ nf (\() -> evalPureExpr "length (map (\\x -> x * 2) [1,2,3,4,5,6,7,8,9,10])") ()
      , bench "Record access" $ nf (\() -> evalPureExpr "let r = {x: 1, y: 2} in r.x + r.y") ()
      , bench "Tuple operations" $ nf (\() -> evalPureExpr "fst (1, 2) + snd (3, 4)") ()
      ]

  , bgroup "String Operations"
      [ bench "String concat" $ nf (\() -> evalPureExpr "\"hello\" ++ \" \" ++ \"world\"") ()
      , bench "String functions" $ nf (\() -> evalPureExpr "strLength (trim \"  hello world  \")") ()
      , bench "String processing" $ nf (\() -> evalPureExpr "join \",\" (split \" \" \"hello world test string\")") ()
      ]

  , bgroup "Deep Nesting"
      [ bench "Arithmetic nesting (10)" $ nf (\() -> evalPureExpr (deepNest 10 "1 + 2")) ()
      , bench "Arithmetic nesting (20)" $ nf (\() -> evalPureExpr (deepNest 20 "1 + 2")) ()
      , bench "Function nesting (5)" $ nf (\() -> evalPureExpr (deepFunc 5)) ()
      ]

  , bgroup "Large Data"
      [ bench "Large list (50 elems)" $ nf (\() -> evalPureExpr ("[1" ++ concat (replicate 49 ",1") ++ "]")) ()
      , bench "Large string" $ nf (\() -> evalPureExpr ("\"" ++ replicate 500 'a' ++ "\"")) ()
      ]
  ]

evalPureExpr :: String -> Either String Value
evalPureExpr input = case parseExpr input of
  Left err -> Left (show err)
  Right expr -> case evalPure expr of
    Left err -> Left (show err)
    Right val -> Right val

deepNest :: Int -> String -> String
deepNest 0 expr = expr
deepNest n expr = "(" ++ deepNest (n-1) expr ++ ")"

deepFunc :: Int -> String
deepFunc 0 = "42"
deepFunc n = "(\\x -> " ++ deepFunc (n-1) ++ ") 0"
