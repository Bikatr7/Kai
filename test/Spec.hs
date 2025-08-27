module Main where

import Test.Hspec

import qualified ArithmeticSpec
import qualified BooleanSpec
import qualified ComparisonSpec
import qualified ConditionalSpec
import qualified EdgeCaseSpec
import qualified LambdaSpec
import qualified ParserSpec
import qualified RuntimeErrorSpec
import qualified ScriptSpec
import qualified TypeErrorSpec

main :: IO ()
main = hspec $ do
  ArithmeticSpec.spec
  BooleanSpec.spec
  ComparisonSpec.spec
  ConditionalSpec.spec
  EdgeCaseSpec.spec
  LambdaSpec.spec
  ParserSpec.spec
  RuntimeErrorSpec.spec
  -- Kai script files discovered under tests/ and test/
  ScriptSpec.spec
  TypeErrorSpec.spec
