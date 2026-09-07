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
import System.Environment (lookupEnv)
import Control.Monad (unless)
import Data.Maybe (isJust)
import qualified Weigh as W

fibExpr :: String
fibExpr = "letrec fib = \\n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 10"

factorialExpr :: String
factorialExpr = "letrec fact = \\n -> if n < 2 then 1 else n * fact (n - 1) in fact 10"

listOpsExpr :: String
listOpsExpr = "let list = [1,2,3,4,5,6,7,8,9,10] in length (map (\\x -> x * 2) (filter (\\x -> x > 5) list))"

stringOpsExpr :: String
stringOpsExpr = "let str = \"hello world this is a test string for benchmarking\" in strLength (join \",\" (split \" \" str))"

deepNestingExpr :: String
deepNestingExpr = "((((((((((((((((1))))))))))))))))"

complexExpr :: String
complexExpr = "let x = 5 in let y = 10 in if x < y then let z = x + y in z * 2 else 0"

parseEval :: String -> Value
parseEval input = case parseExpr input of
  Left err -> error $ "Invalid evaluator benchmark expression: " ++ show err
  Right expr -> case evalPure expr of
    Left err -> error $ "Evaluator benchmark failed: " ++ show err
    Right val -> val

parseTypeCheck :: String -> Type
parseTypeCheck input = case parseExpr input of
  Left err -> error $ "Invalid type-check benchmark expression: " ++ show err
  Right expr -> case typeCheck expr of
    Left err -> error $ "Type-check benchmark failed: " ++ show err
    Right typ -> typ

speedBenchmarks :: Benchmark
speedBenchmarks = bgroup "Speed Benchmarks"
  [ bgroup "End-to-End"
      [ bench "Fibonacci (n=10)" $ nf parseEval fibExpr
      , bench "Factorial (n=10)" $ nf parseEval factorialExpr
      , bench "List Operations" $ nf parseEval listOpsExpr
      , bench "String Operations" $ nf parseEval stringOpsExpr
      , bench "Deep Nesting" $ nf parseEval deepNestingExpr
      , bench "Complex Expression" $ nf parseEval complexExpr
      ]
  , bgroup "Type Checking"
      [ bench "Fibonacci" $ nf parseTypeCheck fibExpr
      , bench "Complex Expression" $ nf parseTypeCheck complexExpr
      ]

  , bgroup "Small-input latency"
      [ bench "Parse Small Expr" $ whnf parseExpr "1 + 2"
      , bench "Parse List" $ whnf parseExpr "[1,2,3,4,5]"
      , bench "Eval Simple" $ whnf parseEval "42"
      , bench "Eval List Length" $ whnf parseEval "length [1,2,3,4,5]"
      , bench "Type Check Simple" $ whnf parseTypeCheck "1 + 2"
      ]

  , parserBenchmarks
  , evalBenchmarks
  , typeCheckBenchmarks
  ]

memoryBenchmarks :: W.Weigh ()
memoryBenchmarks = do
  W.func "Memory: Parse Small Expr" parseExpr "1 + 2"
  W.func "Memory: Parse List" parseExpr "[1,2,3,4,5]"
  W.func "Memory: Parse String" parseExpr "\"hello\""
  W.func "Memory: Eval Simple" parseEval "42"
  W.func "Memory: Eval List" parseEval "length [1,2,3]"
  W.func "Memory: Eval String" parseEval "strLength \"hi\""
  W.func "Memory: TypeCheck Simple" parseTypeCheck "1 + 2"
  W.func "Memory: TypeCheck Function" parseTypeCheck "\\x -> x"

main :: IO ()
main = do
  -- Weigh re-executes this binary for each allocation case, then returns.
  -- Read its child marker before mainWith, which also sets it in the parent.
  memoryChild <- isJust <$> lookupEnv "WEIGH_CASE"
  unless memoryChild $ do
    putStrLn "Kai Language Benchmark Suite"
    putStrLn "============================"
    putStrLn ""
    putStrLn "Running comprehensive benchmarks for speed and memory usage..."
    putStrLn ""
    hFlush stdout
    putStrLn "Memory Benchmarks (Weigh):"
    hFlush stdout
  W.mainWith memoryBenchmarks
  unless memoryChild $ do
    hFlush stdout
    putStrLn "Speed Benchmarks (Criterion):"
    hFlush stdout
    defaultMain [speedBenchmarks]
