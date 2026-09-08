module Main where

import Test.Hspec
import TestReport (runReportedSpec)
import qualified TestReportSpec
import qualified TestSupportSpec
import qualified OrdinaryBuiltinsSpec
import qualified ShortCircuitSpec
import qualified ErrorRecoverySpec
import qualified SourceLocationSpec
import qualified CoverageSpec
import qualified ConstraintSpec
import qualified OpenRecordSpec

import qualified SelfReviewSpec
import qualified RunnerSpec
import qualified DocumentationSpec
import qualified SourceDistributionSpec
import qualified OutputFailureSpec
import qualified UTF8Spec
import qualified AuditRegressionSpec
import qualified ArithmeticSpec
import qualified BooleanSpec
import qualified ComparisonSpec
import qualified ConditionalSpec
import qualified CLISpec
import qualified EdgeCaseSpec
import qualified ExampleSpec
import qualified ExampleAssertionsSpec
import qualified EvaluatorConsistencySpec
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
import qualified DataTypeSpec
import qualified IOSpec
import qualified WildcardSpec
import qualified SequencingSpec
import qualified SiteExportSpec
import qualified TupleSpec
import qualified ListFunctionsSpec
import qualified StringFunctionsSpec
import qualified ModuleSpec
import qualified ReplSpec

main :: IO ()
main = runReportedSpec $ do
  TestReportSpec.spec
  TestSupportSpec.spec
  OrdinaryBuiltinsSpec.spec
  ShortCircuitSpec.spec
  ErrorRecoverySpec.spec
  SourceLocationSpec.spec
  CoverageSpec.spec
  ConstraintSpec.spec
  OpenRecordSpec.spec
  RunnerSpec.spec
  DocumentationSpec.spec
  SourceDistributionSpec.spec
  OutputFailureSpec.spec
  UTF8Spec.spec
  SelfReviewSpec.spec
  AuditRegressionSpec.spec
  ArithmeticSpec.spec
  BooleanSpec.spec
  ComparisonSpec.spec
  ConditionalSpec.spec
  CLISpec.spec
  EdgeCaseSpec.spec
  ExampleSpec.spec
  ExampleAssertionsSpec.spec
  EvaluatorConsistencySpec.spec
  IntegerOverflowSpec.spec
  InputSpec.spec
  LambdaSpec.spec
  LetBindingSpec.spec
  ParserSpec.spec
  ProgramSpec.spec
  PropertyBasedSpec.spec
  ReleaseWorkflowSpec.spec
  RuntimeErrorSpec.spec
  -- Kai scripts plus repository-wide expectation coverage
  ScriptSpec.spec
  StressTestSpec.spec
  StringSpec.spec
  TypeErrorSpec.spec
  TypeInferenceSpec.spec
  UnificationSpec.spec
  DataStructureSpec.spec
  DataTypeSpec.spec
  IOSpec.spec
  WildcardSpec.spec
  SequencingSpec.spec
  SiteExportSpec.spec
  TupleSpec.spec
  ListFunctionsSpec.spec
  StringFunctionsSpec.spec
  ModuleSpec.spec
  ReplSpec.spec
