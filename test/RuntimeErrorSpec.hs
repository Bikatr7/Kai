module RuntimeErrorSpec where

import Test.Hspec
import Syntax
import Evaluator
import Parser

spec :: Spec
spec = describe "Runtime Errors" $ do
  
  describe "Division by Zero" $ do
    it "5 / 0 throws division by zero error" $ do
      case parseAndEvaluate "5 / 0" of
        Left DivByZero -> True `shouldBe` True
        _ -> expectationFailure "Should be division by zero error"
    
    it "handles division by zero in expressions" $ do
      case parseAndEvaluate "10 + (5 / 0)" of
        Left DivByZero -> True `shouldBe` True
        _ -> expectationFailure "Should be division by zero error"
    
    it "conditional prevents division by zero" $ do
      parseAndEvaluate "if false then (1 / 0) else 42" `shouldBe` Right (VInt 42)
  
  describe "Variable Errors" $ do
    it "unbound variable x" $ do
      case parseAndEvaluate "x + 1" of
        Left (UnboundVariable "x") -> True `shouldBe` True
        _ -> expectationFailure "Should be unbound variable error"
    
    it "unbound variable in function body" $ do
      case parseAndEvaluate "(\\x -> y + x) 5" of
        Left (UnboundVariable "y") -> True `shouldBe` True
        _ -> expectationFailure "Should be unbound variable error"

parseAndEvaluate :: String -> Either RuntimeError Value
parseAndEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> eval expr