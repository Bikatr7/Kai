{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import ParserBench
import EvaluatorBench
import TypeCheckerBench
import Syntax (Expr(..))
import Parser (parseExpr)
import Evaluator (evalPure, Value(..))
import TypeChecker (typeCheck, Type(..))
import System.IO (hFlush, stdout)
import qualified Weigh as W

fibExpr :: String
fibExpr = "letrec fib = \\n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 10"

factorialExpr :: String
factorialExpr = "letrec fact = \\n -> if n <= 1 then 1 else n * fact (n - 1) in fact 10"

listOpsExpr :: String
listOpsExpr = "let list = [1,2,3,4,5,6,7,8,9,10] in length (map (\\x -> x * 2) (filter (\\x -> x % 2 == 0) list))"

stringOpsExpr :: String
stringOpsExpr = "let str = \"hello world this is a test string for benchmarking\" in strLength (join \",\" (split \" \" str))"

deepNestingExpr :: String
deepNestingExpr = "((((((((((((((((1))))))))))))))))"

complexExpr :: String
complexExpr = "let x = 5 in let y = 10 in if x < y then let z = x + y in z * 2 else 0"

parseEval :: String -> Either String Value
parseEval input = case parseExpr input of
  Left err -> Left (show err)
  Right expr -> case evalPure expr of
    Left err -> Left (show err)
    Right val -> Right val

parseTypeCheck :: String -> Either String Type
parseTypeCheck input = case parseExpr input of
  Left err -> Left (show err)
  Right expr -> case typeCheck expr of
    Left err -> Left (show err)
    Right typ -> Right typ

speedBenchmarks :: Benchmark
speedBenchmarks = bgroup "Speed Benchmarks"
  [ bgroup "End-to-End"
      [ bench "Fibonacci (n=10)" $ nf (\() -> parseEval fibExpr) ()
      , bench "Factorial (n=10)" $ nf (\() -> parseEval factorialExpr) ()
      , bench "List Operations" $ nf (\() -> parseEval listOpsExpr) ()
      , bench "String Operations" $ nf (\() -> parseEval stringOpsExpr) ()
      , bench "Deep Nesting" $ nf (\() -> parseEval deepNestingExpr) ()
      , bench "Complex Expression" $ nf (\() -> parseEval complexExpr) ()
      ]
  , bgroup "Type Checking"
      [ bench "Fibonacci" $ nf (\() -> parseTypeCheck fibExpr) ()
      , bench "Complex Expression" $ nf (\() -> parseTypeCheck complexExpr) ()
      ]

  , bgroup "Memory Usage (Allocations)"
      [ bench "Parse Small Expr" $ whnf (\() -> parseExpr "1 + 2") ()
      , bench "Parse List" $ whnf (\() -> parseExpr "[1,2,3,4,5]") ()
      , bench "Eval Simple" $ whnf (\() -> parseEval "42") ()
      , bench "Eval List Length" $ whnf (\() -> parseEval "length [1,2,3,4,5]") ()
      , bench "Type Check Simple" $ whnf (\() -> parseTypeCheck "1 + 2") ()
      ]

  , parserBenchmarks
  , evalBenchmarks
  , typeCheckBenchmarks
  ]

memoryBenchmarks :: W.Weigh ()
memoryBenchmarks = do
  W.action "Memory: Parse Small Expr" (return $ parseExpr "1 + 2")
  W.action "Memory: Parse List" (return $ parseExpr "[1,2,3,4,5]")
  W.action "Memory: Parse String" (return $ parseExpr "\"hello\"")
  W.action "Memory: Eval Simple" (return $ parseEval "42")
  W.action "Memory: Eval List" (return $ parseEval "length [1,2,3]")
  W.action "Memory: Eval String" (return $ parseEval "strLength \"hi\"")
  W.action "Memory: TypeCheck Simple" (return $ parseTypeCheck "1 + 2")
  W.action "Memory: TypeCheck Function" (return $ parseTypeCheck "\\x -> x")

main :: IO ()
main = do
  putStrLn "Kai Language Benchmark Suite"
  putStrLn "============================"
  putStrLn ""
  putStrLn "Running comprehensive benchmarks for speed and memory usage..."
  putStrLn ""
  hFlush stdout
  putStrLn "Memory Benchmarks (Weigh):"
  hFlush stdout
  W.mainWith memoryBenchmarks
  hFlush stdout
  putStrLn "Speed Benchmarks (Criterion):"
  hFlush stdout
  defaultMain [speedBenchmarks]
