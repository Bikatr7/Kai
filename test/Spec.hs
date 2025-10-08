module Main where

import Test.Hspec

import qualified ArithmeticSpec
import qualified BooleanSpec
import qualified ComparisonSpec
import qualified ConditionalSpec
import qualified EdgeCaseSpec
import qualified IntegerOverflowSpec
import qualified InputSpec
import qualified LambdaSpec
import qualified ParserSpec
import qualified PropertyBasedSpec
import qualified RuntimeErrorSpec
import qualified ScriptSpec
import qualified StressTestSpec
import qualified TypeErrorSpec
import qualified TypeInferenceSpec
import qualified UnificationSpec
import qualified DataStructureSpec
import qualified WildcardSpec
import qualified SequencingSpec
import qualified TupleSpec
import qualified ListFunctionsSpec
import qualified StringFunctionsSpec

main :: IO ()
main = hspec $ do
  ArithmeticSpec.spec
  BooleanSpec.spec
  ComparisonSpec.spec
  ConditionalSpec.spec
  EdgeCaseSpec.spec
  IntegerOverflowSpec.spec
  InputSpec.spec
  LambdaSpec.spec
  ParserSpec.spec
  PropertyBasedSpec.spec
  RuntimeErrorSpec.spec
  -- Kai script files discovered under tests/ and test/
  ScriptSpec.spec
  StressTestSpec.spec
  TypeErrorSpec.spec
  TypeInferenceSpec.spec
  UnificationSpec.spec
  DataStructureSpec.spec
  WildcardSpec.spec
  SequencingSpec.spec
  TupleSpec.spec
  ListFunctionsSpec.spec
  StringFunctionsSpec.spec