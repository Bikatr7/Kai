module Main where

import Test.Hspec

import qualified ArithmeticSpec
import qualified BooleanSpec
import qualified ComparisonSpec
import qualified ConditionalSpec
import qualified CLISpec
import qualified EdgeCaseSpec
import qualified ExampleSpec
import qualified IntegerOverflowSpec
import qualified InputSpec
import qualified LambdaSpec
import qualified LetBindingSpec
import qualified ParserSpec
import qualified ProgramSpec
import qualified PropertyBasedSpec
import qualified ReleaseWorkflowSpec
import qualified RuntimeErrorSpec
import qualified ScriptSpec
import qualified StressTestSpec
import qualified StringSpec
import qualified TypeErrorSpec
import qualified TypeInferenceSpec
import qualified UnificationSpec
import qualified DataStructureSpec
import qualified WildcardSpec
import qualified SequencingSpec
import qualified TupleSpec
import qualified ListFunctionsSpec
import qualified StringFunctionsSpec
import qualified ModuleSpec

main :: IO ()
main = hspec $ do
  ArithmeticSpec.spec
  BooleanSpec.spec
  ComparisonSpec.spec
  ConditionalSpec.spec
  CLISpec.spec
  EdgeCaseSpec.spec
  ExampleSpec.spec
  IntegerOverflowSpec.spec
  InputSpec.spec
  LambdaSpec.spec
  LetBindingSpec.spec
  ParserSpec.spec
  ProgramSpec.spec
  PropertyBasedSpec.spec
  ReleaseWorkflowSpec.spec
  RuntimeErrorSpec.spec
  -- Kai script files discovered under tests/ and test/
  ScriptSpec.spec
  StressTestSpec.spec
  StringSpec.spec
  TypeErrorSpec.spec
  TypeInferenceSpec.spec
  UnificationSpec.spec
  DataStructureSpec.spec
  WildcardSpec.spec
  SequencingSpec.spec
  TupleSpec.spec
  ListFunctionsSpec.spec
  StringFunctionsSpec.spec
  ModuleSpec.spec
