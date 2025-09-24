module LambdaSpec where

import Test.Hspec
import Syntax
import TypeChecker
import Evaluator
import Parser

spec :: Spec
spec = describe "Lambda Functions" $ do
  
  describe "Basic Lambda Creation" $ do
    it "creates a simple lambda function" $ do
      case parseEvaluate "\\x -> x + 1" of
        Right (VFun "x" _ _) -> True `shouldBe` True
        _ -> expectationFailure "Should create a lambda function"
  
  describe "Function Application" $ do
    it "applies simple increment function" $ do
      parseEvaluate "(\\x -> x + 1) 5" `shouldBe` Right (VInt 6)
    
    it "applies multiplication function" $ do
      parseEvaluate "(\\x -> x * 2) 10" `shouldBe` Right (VInt 20)
    
    it "applies boolean function" $ do
      parseEvaluate "(\\x -> x > 0) 5" `shouldBe` Right (VBool True)
  
  describe "Complex Lambda Bodies" $ do
    it "lambda with arithmetic expression" $ do
      parseEvaluate "(\\x -> x * x + 1) 4" `shouldBe` Right (VInt 17)
    
    it "lambda with conditional" $ do
      parseEvaluate "(\\x -> if x > 0 then x else 0) (-5)" `shouldBe` Right (VInt 0)
  
  describe "Higher-Order Functions" $ do
    it "function that takes a function" $ do
      parseEvaluate "(\\f -> f 42) (\\x -> x + 1)" `shouldBe` Right (VInt 43)
  
  describe "Edge Cases" $ do
    it "identity function" $ do
      parseEvaluate "(\\x -> x) 42" `shouldBe` Right (VInt 42)
    
    it "constant function" $ do
      parseEvaluate "(\\x -> 42) 100" `shouldBe` Right (VInt 42)

-- Helper function
parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> evalPure expr