module ComparisonSpec where

import Test.Hspec
import Syntax
import TypeChecker
import Evaluator
import Parser

spec :: Spec
spec = describe "Comparison Operations" $ do
  
  describe "Equality (==)" $ do
    it "5 == 5 is true" $ do
      parseEvaluate "5 == 5" `shouldBe` Right (VBool True)
    
    it "5 == 3 is false" $ do
      parseEvaluate "5 == 3" `shouldBe` Right (VBool False)
    
    it "true == true is true" $ do
      parseEvaluate "true == true" `shouldBe` Right (VBool True)
  
  describe "Less Than (<)" $ do
    it "3 < 5 is true" $ do
      parseEvaluate "3 < 5" `shouldBe` Right (VBool True)
    
    it "5 < 3 is false" $ do
      parseEvaluate "5 < 3" `shouldBe` Right (VBool False)
  
  describe "Greater Than (>)" $ do
    it "5 > 3 is true" $ do
      parseEvaluate "5 > 3" `shouldBe` Right (VBool True)
    
    it "3 > 5 is false" $ do
      parseEvaluate "3 > 5" `shouldBe` Right (VBool False)

parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> evalPure expr