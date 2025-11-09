module TypeCheckerBench where

import Criterion
import Parser (parseExpr)
import TypeChecker (typeCheck, Type)

-- Type checking benchmarks
typeCheckBenchmarks :: Benchmark
typeCheckBenchmarks = bgroup "Type Checker Benchmarks"
  [ bgroup "Basic Types"
      [ bench "Integer literal" $ nf (\() -> typeCheckExpr "42") ()
      , bench "Boolean literal" $ nf (\() -> typeCheckExpr "true") ()
      , bench "String literal" $ nf (\() -> typeCheckExpr "\"hello\"") ()
      , bench "Unit literal" $ nf (\() -> typeCheckExpr "()") ()
      ]

  , bgroup "Arithmetic"
      [ bench "Simple addition" $ nf (\() -> typeCheckExpr "1 + 2") ()
      , bench "Complex arithmetic" $ nf (\() -> typeCheckExpr "(1 + 2) * (3 - 4) / 2") ()
      , bench "Mixed operations" $ nf (\() -> typeCheckExpr "1 + 2 * 3 - 4 / 2") ()
      ]

  , bgroup "Functions"
      [ bench "Identity function" $ nf (\() -> typeCheckExpr "\\x -> x") ()
      , bench "Simple lambda" $ nf (\() -> typeCheckExpr "\\x -> x + 1") ()
      , bench "Higher order" $ nf (\() -> typeCheckExpr "\\f x -> f (f x)") ()
      , bench "Curried function" $ nf (\() -> typeCheckExpr "\\x y -> x + y") ()
      ]

  , bgroup "Polymorphism"
      [ bench "Generic identity" $ nf (\() -> typeCheckExpr "let id = \\x -> x in id 5") ()
      , bench "Generic map" $ nf (\() -> typeCheckExpr "let map = \\f l -> case l of [] -> [] ; (h:t) -> f h : map f t in map (\\x -> x * 2) [1,2,3]") ()
      ]

  , bgroup "Recursion"
      [ bench "Simple recursion" $ nf (\() -> typeCheckExpr "letrec sum = \\n -> if n <= 0 then 0 else n + sum (n - 1) in sum") ()
      , bench "Fibonacci" $ nf (\() -> typeCheckExpr "letrec fib = \\n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib") ()
      , bench "Factorial" $ nf (\() -> typeCheckExpr "letrec fact = \\n -> if n <= 1 then 1 else n * fact (n - 1) in fact") ()
      ]

  , bgroup "Data Structures"
      [ bench "List literal" $ nf (\() -> typeCheckExpr "[1,2,3,4,5]") ()
      , bench "Empty list" $ nf (\() -> typeCheckExpr "[]") ()
      , bench "Record literal" $ nf (\() -> typeCheckExpr "{x: 1, y: \"hello\"}") ()
      , bench "Tuple literal" $ nf (\() -> typeCheckExpr "(1, \"hello\", true)") ()
      ]

  , bgroup "Complex Expressions"
      [ bench "Let binding" $ nf (\() -> typeCheckExpr "let x = 5 in let y = x + 1 in y * 2") ()
      , bench "Conditional" $ nf (\() -> typeCheckExpr "if true then 1 else 2") ()
      , bench "Nested conditionals" $ nf (\() -> typeCheckExpr "if 1 < 2 then if 3 < 4 then 5 else 6 else 7") ()
      , bench "Pattern matching" $ nf (\() -> typeCheckExpr "case (1, 2) of (x, y) -> x + y") ()
      ]

  , bgroup "Large Programs"
      [ bench "Many bindings" $ nf (\() -> typeCheckExpr (manyLets 10)) ()
      , bench "Deep nesting" $ nf (\() -> typeCheckExpr (deepNesting 15)) ()
      , bench "Complex recursive" $ nf (\() -> typeCheckExpr complexRecursive) ()
      ]

  , bgroup "Type Annotations"
      [ bench "Explicit annotation" $ nf (\() -> typeCheckExpr "(\\x -> x) : Int -> Int") ()
      , bench "Annotated let" $ nf (\() -> typeCheckExpr "let f : Int -> Int = \\x -> x in f 5") ()
      ]
  ]

-- Helper functions
typeCheckExpr :: String -> Either String Type
typeCheckExpr input = case parseExpr input of
  Left err -> Left (show err)
  Right expr -> case typeCheck expr of
    Left err -> Left (show err)
    Right typ -> Right typ

manyLets :: Int -> String
manyLets 0 = "42"
manyLets n = "let x" ++ show n ++ " = " ++ show n ++ " in " ++ manyLets (n-1)

deepNesting :: Int -> String
deepNesting 0 = "42"
deepNesting n = "if true then " ++ deepNesting (n-1) ++ " else 0"

complexRecursive :: String
complexRecursive = unlines
  [ "letrec fib = \\n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in"
  , "letrec fact = \\n -> if n <= 1 then 1 else n * fact (n - 1) in"
  , "letrec gcd = \\a b -> if b == 0 then a else gcd b (a % b) in"
  , "fib 8 + fact 5 + gcd 48 18"
  ]
