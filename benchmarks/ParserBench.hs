module ParserBench where

import Criterion
import Parser (parseExpr, parseStatements)
import Syntax (Expr)
import Data.List (intercalate)

smallExpr :: String
smallExpr = "1 + 2"

mediumExpr :: String
mediumExpr = "let x = 5 in let y = 10 in if x < y then x + y else x - y"

largeExpr :: String
largeExpr = "letrec fib = \\n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 10"

veryLargeExpr :: String
veryLargeExpr = concat (replicate 100 "1 + ") ++ "0"

nestedExpr :: Int -> String
nestedExpr 0 = "1"
nestedExpr n = "(" ++ nestedExpr (n - 1) ++ " + 1)"

lambdaChain :: Int -> String
lambdaChain 0 = "\\x -> x"
lambdaChain n = "\\x -> " ++ lambdaChain (n - 1) ++ " x"

listOps :: Int -> String
listOps n = "map (\\x -> x * 2) [" ++ intercalate "," (replicate n "1") ++ "]"

-- Benchmarks
parserBenchmarks :: Benchmark
parserBenchmarks = bgroup "Parser Benchmarks"
  [ bgroup "Expression Size"
      [ bench "Small Expression" $ nf parseExprChecked smallExpr
      , bench "Medium Expression" $ nf parseExprChecked mediumExpr
      , bench "Large Expression" $ nf parseExprChecked largeExpr
      , bench "Very Large Expression" $ nf parseExprChecked veryLargeExpr
      ]

  , bgroup "Nesting Depth"
      [ bench "5 levels" $ nf parseExprChecked (nestedExpr 5)
      , bench "10 levels" $ nf parseExprChecked (nestedExpr 10)
      , bench "15 levels" $ nf parseExprChecked (nestedExpr 15)
      ]

  , bgroup "Lambda Chain"
      [ bench "3 lambdas" $ nf parseExprChecked (lambdaChain 3)
      , bench "5 lambdas" $ nf parseExprChecked (lambdaChain 5)
      , bench "8 lambdas" $ nf parseExprChecked (lambdaChain 8)
      ]

  , bgroup "List Operations"
      [ bench "Small list (5 elems)" $ nf parseExprChecked (listOps 5)
      , bench "Medium list (10 elems)" $ nf parseExprChecked (listOps 10)
      , bench "Large list (20 elems)" $ nf parseExprChecked (listOps 20)
      ]

  , bgroup "Statements"
      [ bench "Single statement" $ nf parseStatementsChecked "1 + 2"
      , bench "Multiple statements" $ nf parseStatementsChecked "let x = 1 in x\nlet y = 2 in y\n1 + 2"
      ]
  ]

parseExprChecked :: String -> Expr
parseExprChecked input =
  case parseExpr input of
    Left err -> error $ "Invalid parser benchmark expression: " ++ show err
    Right parsed -> parsed

parseStatementsChecked :: String -> [Expr]
parseStatementsChecked input =
  case parseStatements input of
    Left err -> error $ "Invalid parser benchmark statements: " ++ show err
    Right parsed -> parsed
