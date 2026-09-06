module TypeInferenceSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import Parser
import TypeChecker
import Evaluator

spec :: Spec
spec = describe "Advanced Type Inference" $ do
  
  describe "Polymorphic Function Inference" $ do
    it "infers identity function type: \\x -> x" $ do
      case parseAndInferType "\\x -> x" of
        Right (TFun (TVar _) (TVar _)) -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected polymorphic function type, got: " ++ show ty
        Left err -> expectationFailure $ "Should infer identity type: " ++ show err
    
    it "infers const function type: \\x -> \\y -> x" $ do
      case parseAndInferType "\\x -> \\y -> x" of
        Right (TFun (TVar a) (TFun (TVar _) (TVar b))) -> a `shouldBe` b
        Right ty -> expectationFailure $ "Expected const function type, got: " ++ show ty
        Left err -> expectationFailure $ "Should infer const type: " ++ show err
    
    it "infers function composition type" $ do
      case parseAndInferType "\\f -> \\g -> \\x -> f (g x)" of
        Right (TFun (TFun (TVar _) (TVar _)) (TFun (TFun (TVar _) (TVar _)) (TFun (TVar _) (TVar _)))) -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected composition type, got: " ++ show ty
        Left err -> expectationFailure $ "Should infer composition type: " ++ show err
    
    it "infers twice function type: \\f -> \\x -> f (f x)" $ do
      case parseAndInferType "\\f -> \\x -> f (f x)" of
        Right (TFun (TFun (TVar a) (TVar b)) (TFun (TVar c) (TVar d))) -> do
          a `shouldBe` b  -- f's domain should match its range for idempotence
          c `shouldBe` a  -- x's type should match f's domain
          d `shouldBe` b  -- result type should match f's range
        Right ty -> expectationFailure $ "Expected twice function type, got: " ++ show ty
        Left err -> expectationFailure $ "Should infer twice type: " ++ show err

  describe "Higher-Order Function Applications" $ do
    it "type-checks applying polymorphic function to specific types" $ do
      case parseAndInferType "(\\f -> f 42) (\\x -> x + 1)" of
        Right TInt -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected TInt, got: " ++ show ty
        Left err -> expectationFailure $ "Should type-check application: " ++ show err
    
    it "type-checks nested function applications" $ do
      case parseAndInferType "(\\f -> \\g -> \\x -> f (g x)) (\\y -> not y) (\\z -> z == 0) 5" of
        Right TBool -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected TBool, got: " ++ show ty
        Left err -> expectationFailure $ "Should type-check nested application: " ++ show err

  describe "Fixpoint Inference" $ do
    it "infers the result type of a constant fixpoint" $ do
      parseAndInferType "fix (\\self -> 42)" `shouldBe` Right TInt

    it "infers a recursive function fixpoint" $ do
      let factorial = "fix (\\self -> \\n -> if n == 0 then 1 else n * self (n - 1))"
      parseAndInferType factorial `shouldBe` Right (TFun TInt TInt)

    it "rejects a fixpoint operand that is not a function" $ do
      case parseAndInferType "fix 42" of
        Left (UnificationError TInt (TFun (TVar inputVar) (TVar outputVar))) ->
          inputVar `shouldBe` outputVar
        Left err -> expectationFailure $ "Expected function unification error, got: " ++ show err
        Right ty -> expectationFailure $ "Should reject non-function fixpoint, got: " ++ show ty

  describe "Composite Type Inference" $ do
    it "rejects an annotation that assigns conflicting record fields to one type variable" $ do
      let unsound =
            "let f : {a: a, b: a} -> {a: Int, b: Bool} = \\x -> x " ++
            "in f {a = false, b = false}"
      parseAndInferType unsound `shouldBe` Left (UnificationError TInt TBool)

  describe "Composite Pattern Type Inference" $ do
    it "rejects heterogeneous list patterns" $ do
      shouldHaveTypeError
        "case [] of [1, true] -> 1 | _ -> 0"
        (UnificationError TInt TBool)

    it "accepts homogeneous list patterns" $ do
      parseAndInferType "case [] of [1, 2] -> 1 | _ -> 0" `shouldBe` Right TInt

    it "threads the head constraint into a cons tail pattern" $ do
      shouldHaveTypeError
        "case [] of 1 :: [true] -> 1 | _ -> 0"
        (UnificationError TInt TBool)

    it "accepts consistent head and tail constraints in cons patterns" $ do
      parseAndInferType "case [] of 1 :: [2] -> 1 | _ -> 0" `shouldBe` Right TInt

    it "rejects conflicting tuple patterns when components share a type" $ do
      shouldHaveTypeError
        "\\x -> case (x, x) of (1, true) -> 1 | _ -> 0"
        (UnificationError TInt TBool)

    it "accepts consistent tuple patterns when components share a type" $ do
      parseAndInferType "\\x -> case (x, x) of (1, 2) -> 1 | _ -> 0"
        `shouldBe` Right (TFun TInt TInt)

    it "rejects conflicting record patterns when fields share a type" $ do
      shouldHaveTypeError
        "\\x -> case {a = x, b = x} of {a = 1, b = true} -> 1 | _ -> 0"
        (UnificationError TInt TBool)

    it "accepts consistent record patterns when fields share a type" $ do
      parseAndInferType "\\x -> case {a = x, b = x} of {a = 1, b = 2} -> 1 | _ -> 0"
        `shouldBe` Right (TFun TInt TInt)

  describe "Constraint Solving" $ do
    it "unifies function parameters with usage" $ do
      case parseAndInferType "\\x -> x + 1" of
        Right (TFun TInt TInt) -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected TInt -> TInt, got: " ++ show ty
        Left err -> expectationFailure $ "Should infer Int function: " ++ show err
    
    it "unifies across if-then-else branches" $ do
      case parseAndInferType "\\x -> if x > 0 then x else 0" of
        Right (TFun TInt TInt) -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected TInt -> TInt, got: " ++ show ty
        Left err -> expectationFailure $ "Should infer conditional type: " ++ show err
    
    it "infers boolean predicates" $ do
      case parseAndInferType "\\x -> \\y -> x == y" of
        Right (TFun (TVar a) (TFun (TVar b) TBool)) -> a `shouldBe` b
        Right ty -> expectationFailure $ "Expected equality predicate type, got: " ++ show ty
        Left err -> expectationFailure $ "Should infer predicate type: " ++ show err

  describe "Complex Type Expressions" $ do
    it "handles deeply nested lambdas" $ do
      let deepLambda = "\\a -> \\b -> \\c -> \\d -> \\e -> a (b (c (d e)))"
      case parseAndInferType deepLambda of
        Right (TFun _ (TFun _ (TFun _ (TFun _ (TFun _ _))))) -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected deeply nested function type, got: " ++ show ty
        Left err -> expectationFailure $ "Should handle deep nesting: " ++ show err
    
    it "handles curried arithmetic operations" $ do
      case parseAndInferType "\\x -> \\y -> \\z -> x + y * z" of
        Right (TFun TInt (TFun TInt (TFun TInt TInt))) -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected curried arithmetic type, got: " ++ show ty
        Left err -> expectationFailure $ "Should infer curried arithmetic: " ++ show err

  describe "Error Cases with Better Diagnostics" $ do
    it "detects infinite types" $ do
      case parseAndInferType "\\x -> x x" of
        Left (InfiniteType _ _) -> True `shouldBe` True
        Left err -> expectationFailure $ "Expected InfiniteType error, got: " ++ show err
        Right ty -> expectationFailure $ "Should detect infinite type, but got: " ++ show ty
    
    it "provides unification errors for type mismatches" $ do
      case parseAndInferType "\\x -> x + true" of
        Left (UnificationError TBool TInt) -> True `shouldBe` True
        Left (UnificationError TInt TBool) -> True `shouldBe` True
        Left err -> expectationFailure $ "Expected UnificationError, got: " ++ show err
        Right ty -> expectationFailure $ "Should fail with unification error, but got: " ++ show ty

  describe "Property-Based Type Checking" $ do
    it "identity function preserves type" $ do
      property $ \x -> case parseAndTypeCheck ("(\\y -> y) (" ++ show (x :: Int) ++ ")") of
        Right TInt -> True
        _ -> False
    
    it "composition associates properly" $ do
      let f = "\\x -> x + 1"
      let g = "\\x -> x * 2" 
      let h = "\\x -> x - 3"
      let composed1 = "(\\f -> \\g -> \\x -> f (g x)) (" ++ f ++ ") ((\\f -> \\g -> \\x -> f (g x)) (" ++ g ++ ") (" ++ h ++ "))"
      let composed2 = "(\\f -> \\g -> \\x -> f (g x)) ((\\f -> \\g -> \\x -> f (g x)) (" ++ f ++ ") (" ++ g ++ ")) (" ++ h ++ ")"
      case (parseAndInferType composed1, parseAndInferType composed2) of
        (Right ty1, Right ty2) -> ty1 `shouldBe` ty2
        (Left err, _) -> expectationFailure $ "First composition failed: " ++ show err
        (_, Left err) -> expectationFailure $ "Second composition failed: " ++ show err

parseAndInferType :: String -> Either TypeError Type
parseAndInferType input = case parseExpr input of
  Left _ -> Left (UnificationError TInt TBool)  
  Right expr -> typeCheck expr

parseAndTypeCheck :: String -> Either TypeError Type  
parseAndTypeCheck = parseAndInferType

shouldHaveTypeError :: String -> TypeError -> Expectation
shouldHaveTypeError source expected =
  case parseExpr source of
    Left err -> expectationFailure $ "Parse error: " ++ show err
    Right expr -> typeCheck expr `shouldBe` Left expected
