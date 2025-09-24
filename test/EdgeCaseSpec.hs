module EdgeCaseSpec where

import Test.Hspec
import Syntax
import TypeChecker
import Evaluator
import Parser

spec :: Spec  
spec = describe "Edge Cases and Integration Tests" $ do
  
  describe "Large Numbers" $ do
    it "handles large integers" $ do
      parseEvaluate "999999 + 1" `shouldBe` Right (VInt 1000000)
  
  describe "Deep Nesting" $ do
    it "handles deeply nested arithmetic" $ do
      parseEvaluate "(((1 + 2) + 3) + 4)" `shouldBe` Right (VInt 10)
    
    it "handles deeply nested function applications" $ do
      parseEvaluate "((\\x -> x + 1) ((\\y -> y * 2) 5))" `shouldBe` Right (VInt 11)
  
  describe "Complex Lambda Expressions" $ do
    it "curried functions" $ do
      parseEvaluate "(\\x -> (\\y -> x + y)) 5 10" `shouldBe` Right (VInt 15)
  
  describe "Whitespace Handling" $ do
    it "handles extra whitespace" $ do
      parseEvaluate "  1   +   2  " `shouldBe` Right (VInt 3)

parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> evalPure expr