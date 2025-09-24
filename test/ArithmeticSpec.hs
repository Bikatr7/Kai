module ArithmeticSpec where

import Test.Hspec
import Syntax
import TypeChecker
import Evaluator
import Parser

spec :: Spec
spec = describe "Arithmetic Operations" $ do
  
  describe "Basic Addition" $ do
    it "evaluates 1 + 2 to 3" $ do
      parseEvaluate "1 + 2" `shouldBe` Right (VInt 3)
    
    it "evaluates 0 + 0 to 0" $ do
      parseEvaluate "0 + 0" `shouldBe` Right (VInt 0)
    
    it "handles negative numbers" $ do
      parseEvaluate "(-5) + 3" `shouldBe` Right (VInt (-2))
  
  describe "Basic Subtraction" $ do
    it "evaluates 5 - 3 to 2" $ do
      parseEvaluate "5 - 3" `shouldBe` Right (VInt 2)
    
    it "evaluates 0 - 5 to -5" $ do
      parseEvaluate "0 - 5" `shouldBe` Right (VInt (-5))
    
    it "handles double negatives" $ do
      parseEvaluate "10 - (-3)" `shouldBe` Right (VInt 13)

    it "supports unary minus on variable and expression" $ do
      parseEvaluate "-(1 + 2)" `shouldBe` Right (VInt (-3))
  
  describe "Basic Multiplication" $ do
    it "evaluates 3 * 4 to 12" $ do
      parseEvaluate "3 * 4" `shouldBe` Right (VInt 12)
    
    it "evaluates 0 * 100 to 0" $ do
      parseEvaluate "0 * 100" `shouldBe` Right (VInt 0)
    
    it "handles negative multiplication" $ do
      parseEvaluate "(-3) * 4" `shouldBe` Right (VInt (-12))
  
  describe "Basic Division" $ do
    it "evaluates 12 / 3 to 4" $ do
      parseEvaluate "12 / 3" `shouldBe` Right (VInt 4)
    
    it "evaluates 7 / 2 to 3 (integer division)" $ do
      parseEvaluate "7 / 2" `shouldBe` Right (VInt 3)
    
    it "handles division by zero" $ do
      case parseEvaluate "5 / 0" of
        Left DivByZero -> True `shouldBe` True
        _ -> expectationFailure "Should return division by zero error"
  
  describe "Operator Precedence" $ do
    it "multiplication before addition: 2 + 3 * 4 = 14" $ do
      parseEvaluate "2 + 3 * 4" `shouldBe` Right (VInt 14)
    
    it "division before subtraction: 10 - 6 / 2 = 7" $ do
      parseEvaluate "10 - 6 / 2" `shouldBe` Right (VInt 7)
    
    it "parentheses override precedence: (2 + 3) * 4 = 20" $ do
      parseEvaluate "(2 + 3) * 4" `shouldBe` Right (VInt 20)
  
  describe "Complex Expressions" $ do
    it "evaluates nested arithmetic" $ do
      parseEvaluate "((5 + 3) * 2) - (4 / 2)" `shouldBe` Right (VInt 14)
    
    it "handles large expressions" $ do
      parseEvaluate "1 + 2 * 3 - 4 / 2 + 5" `shouldBe` Right (VInt 10)

  describe "String Concatenation" $ do
    it "concatenates literals" $ do
      parseEvaluate "\"foo\" ++ \"bar\"" `shouldBe` Right (VStr "foobar")

    it "concatenates nested" $ do
      parseEvaluate "(\"a\" ++ \"b\") ++ \"c\"" `shouldBe` Right (VStr "abc")
-- Helper function
parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> evalPure expr