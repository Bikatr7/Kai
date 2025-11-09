module ParserBench where

import Criterion
import Parser (parseExpr, parseStatements)
import qualified Data.Text as T

smallExpr :: String
smallExpr = "1 + 2" 

mediumExpr :: String
mediumExpr = "let x = 5 in let y = 10 in if x < y then x + y else x - y"

largeExpr :: String
largeExpr = "letrec fib = \\n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 10"

veryLargeExpr :: String
veryLargeExpr = "letrec fib = \\n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 10"

nestedExpr :: Int -> String
nestedExpr 0 = "1"
nestedExpr n = "(" ++ nestedExpr (n-1) ++ " + 1)"

lambdaChain :: Int -> String
lambdaChain 0 = "\\x -> x"
lambdaChain n = "\\x -> " ++ lambdaChain (n-1) ++ " x"

listOps :: Int -> String
listOps n = "map (\\x -> x * 2) [1" ++ concat (replicate n ",1") ++ "]"

-- Benchmarks
parserBenchmarks :: Benchmark
parserBenchmarks = bgroup "Parser Benchmarks"
  [ bgroup "Expression Size"
      [ bench "Small Expression" $ nf (\() -> parseExpr smallExpr) ()
      , bench "Medium Expression" $ nf (\() -> parseExpr mediumExpr) ()
      , bench "Large Expression" $ nf (\() -> parseExpr largeExpr) ()
      , bench "Very Large Expression" $ nf (\() -> parseExpr veryLargeExpr) ()
      ]

  , bgroup "Nesting Depth"
      [ bench "5 levels" $ nf (\() -> parseExpr (nestedExpr 5)) ()
      , bench "10 levels" $ nf (\() -> parseExpr (nestedExpr 10)) ()
      , bench "15 levels" $ nf (\() -> parseExpr (nestedExpr 15)) ()
      ]

  , bgroup "Lambda Chain"
      [ bench "3 lambdas" $ nf (\() -> parseExpr (lambdaChain 3)) ()
      , bench "5 lambdas" $ nf (\() -> parseExpr (lambdaChain 5)) ()
      , bench "8 lambdas" $ nf (\() -> parseExpr (lambdaChain 8)) ()
      ]

  , bgroup "List Operations"
      [ bench "Small list (5 elems)" $ nf (\() -> parseExpr (listOps 5)) ()
      , bench "Medium list (10 elems)" $ nf (\() -> parseExpr (listOps 10)) ()
      , bench "Large list (20 elems)" $ nf (\() -> parseExpr (listOps 20)) ()
      ]

  , bgroup "Statements"
      [ bench "Single statement" $ nf (\() -> parseStatements "1 + 2") ()
      , bench "Multiple statements" $ nf (\() -> parseStatements "x = 1\ny = 2\nx + y") ()
      ]
  ]
