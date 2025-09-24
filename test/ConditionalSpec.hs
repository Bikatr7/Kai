module ConditionalSpec where

import Test.Hspec
import Syntax
import TypeChecker
import Evaluator
import Parser

spec :: Spec
spec = describe "Conditional Expressions (if-then-else)" $ do
  
  describe "Basic Conditionals" $ do
    it "if true then 1 else 2 evaluates to 1" $ do
      parseEvaluate "if true then 1 else 2" `shouldBe` Right (VInt 1)
    
    it "if false then 1 else 2 evaluates to 2" $ do
      parseEvaluate "if false then 1 else 2" `shouldBe` Right (VInt 2)
  
  describe "Conditionals with Expressions" $ do
    it "if 5 > 3 then 10 else 0 evaluates to 10" $ do
      parseEvaluate "if 5 > 3 then 10 else 0" `shouldBe` Right (VInt 10)
    
    it "if 2 < 1 then 10 else 20 evaluates to 20" $ do
      parseEvaluate "if 2 < 1 then 10 else 20" `shouldBe` Right (VInt 20)
  
  describe "Nested Conditionals" $ do
    it "evaluates nested if-then-else" $ do
      let expr = "if true then (if false then 1 else 2) else 3"
      parseEvaluate expr `shouldBe` Right (VInt 2)

parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> evalPure expr