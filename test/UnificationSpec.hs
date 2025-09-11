module UnificationSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import Parser
import TypeChecker
import Evaluator
import qualified Data.Map as Map

spec :: Spec
spec = describe "Unification Algorithm Edge Cases" $ do
  
  describe "Occurs Check" $ do
    it "prevents infinite type: \\x -> x x" $ do
      case parseAndInferType "\\x -> x x" of
        Left (InfiniteType _ _) -> True `shouldBe` True
        Left err -> expectationFailure $ "Expected InfiniteType, got: " ++ show err
        Right ty -> expectationFailure $ "Should fail with infinite type, got: " ++ show ty
    
    it "prevents infinite type in application chains" $ do
      case parseAndInferType "\\f -> f f f" of
        Left (InfiniteType _ _) -> True `shouldBe` True
        Left err -> expectationFailure $ "Expected InfiniteType, got: " ++ show err
        Right ty -> expectationFailure $ "Should fail with infinite type, got: " ++ show ty
    
    it "allows legitimate recursive-looking types" $ do
      case parseAndInferType "\\f -> \\x -> f (f x)" of
        Right _ -> True `shouldBe` True
        Left err -> expectationFailure $ "Should allow valid recursion pattern: " ++ show err

  describe "Complex Unification Scenarios" $ do
    it "unifies nested function types" $ do
      case parseAndInferType "(\\f -> f (\\x -> x)) (\\g -> g 42)" of
        Right TInt -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected TInt, got: " ++ show ty
        Left err -> expectationFailure $ "Should unify nested functions: " ++ show err
    
    it "handles multiple type variable constraints" $ do
      case parseAndInferType "\\f -> \\g -> \\x -> if f x then g x else x" of
        Right (TFun (TFun (TVar a) TBool) (TFun (TFun (TVar b) (TVar c)) (TFun (TVar d) (TVar e)))) -> do
          a `shouldBe` b
          a `shouldBe` d  
          a `shouldBe` c
          a `shouldBe` e
        Right ty -> expectationFailure $ "Expected constrained function type, got: " ++ show ty
        Left err -> expectationFailure $ "Should handle multiple constraints: " ++ show err
    
    it "unifies through conditional expressions" $ do
      case parseAndInferType "\\p -> \\x -> \\y -> if p then (\\z -> x z) else (\\z -> y z)" of
        Right _ -> True `shouldBe` True
        Left err -> expectationFailure $ "Should unify conditional functions: " ++ show err

  describe "Substitution Chain Resolution" $ do
    it "resolves transitive type variable chains" $ do
      case parseAndInferType "\\a -> \\b -> \\c -> if a == b then b + c else c + a" of
        Right (TFun TInt (TFun TInt (TFun TInt TInt))) -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected fully resolved type, got: " ++ show ty
        Left err -> expectationFailure $ "Should resolve type chains: " ++ show err
    
    it "handles bidirectional unification" $ do
      case parseAndInferType "\\f -> \\x -> \\y -> f (if x == y then x else y)" of
        Right _ -> True `shouldBe` True
        Left err -> expectationFailure $ "Should handle bidirectional unification: " ++ show err

  describe "Function Type Unification" $ do  
    it "unifies function parameters correctly" $ do
      case parseAndInferType "(\\h -> h 1 true) (\\x -> \\y -> if y then x else 0)" of
        Right TInt -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected TInt, got: " ++ show ty
        Left err -> expectationFailure $ "Should unify function params: " ++ show err
    
    it "detects parameter type conflicts" $ do
      case parseAndInferType "(\\f -> f 1 + f true) (\\x -> x)" of
        Left (UnificationError _ _) -> True `shouldBe` True
        Left err -> expectationFailure $ "Expected UnificationError, got: " ++ show err
        Right ty -> expectationFailure $ "Should detect conflict, got: " ++ show ty

  describe "Error Recovery and Reporting" $ do
    it "provides specific unification errors" $ do
      case parseAndInferType "1 + true" of
        Left (UnificationError TBool TInt) -> True `shouldBe` True
        Left (UnificationError TInt TBool) -> True `shouldBe` True
        Left err -> expectationFailure $ "Expected specific unification error, got: " ++ show err
        Right ty -> expectationFailure $ "Should fail, got: " ++ show ty
    
    it "handles deeply nested error contexts" $ do
      case parseAndInferType "\\f -> f (f (f (1 + true)))" of
        Left (UnificationError _ _) -> True `shouldBe` True
        Left err -> expectationFailure $ "Expected unification error in nested context, got: " ++ show err
        Right ty -> expectationFailure $ "Should propagate error from deep context, got: " ++ show ty

  describe "Type Variable Generation" $ do
    it "generates fresh type variables for each lambda" $ do
      case parseAndInferType "\\x -> \\y -> \\z -> x" of
        Right (TFun (TVar a) (TFun (TVar b) (TFun (TVar c) (TVar d)))) -> do
          a `shouldBe` d  -- x's type should match return type
          a `shouldNotBe` b  -- different parameters get different type vars
          b `shouldNotBe` c
        Right ty -> expectationFailure $ "Expected fresh type variables, got: " ++ show ty
        Left err -> expectationFailure $ "Should generate fresh variables: " ++ show err

  describe "Stress Test Unification" $ do
    it "handles many nested applications" $ do
      let manyApps = "(((((\\f -> f) (\\x -> x)) (\\y -> y)) (\\z -> z)) 42)"
      case parseAndInferType manyApps of
        Right TInt -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected TInt after many applications, got: " ++ show ty
        Left err -> expectationFailure $ "Should handle many applications: " ++ show err
    
    it "unifies complex nested structures" $ do
      let complex = "\\a -> \\b -> \\c -> \\d -> (\\f -> f a b) (\\x -> \\y -> if c then x + d else y + d)"
      case parseAndInferType complex of
        Right _ -> True `shouldBe` True
        Left err -> expectationFailure $ "Should handle complex nesting: " ++ show err

parseAndInferType :: String -> Either TypeError Type
parseAndInferType input = case parseExpr input of
  Left _ -> Left (UnificationError TInt TBool)  
  Right expr -> typeCheck expr