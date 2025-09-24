{-# LANGUAGE ScopedTypeVariables #-}
module IntegerOverflowSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import Parser
import TypeChecker
import Evaluator

spec :: Spec
spec = describe "Integer Overflow Protection" $ do
  
  describe "Parser Bounds Checking" $ do
    it "accepts maximum Int value" $ do
      let maxIntStr = show (maxBound :: Int)
      case parseExpr maxIntStr of
        Right (IntLit n) -> n `shouldBe` (maxBound :: Int)
        _ -> expectationFailure $ "Should parse max Int: " ++ maxIntStr
    
    it "accepts minimum Int value" $ do
      let minIntStr = show (minBound :: Int)
      case parseExpr minIntStr of
        Right (IntLit n) -> n `shouldBe` (minBound :: Int)
        _ -> expectationFailure $ "Should parse min Int: " ++ minIntStr
    
    it "rejects integer larger than maxBound" $ do
      let tooBig = show ((fromIntegral (maxBound :: Int) :: Integer) + 1)
      case parseExpr tooBig of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure $ "Should reject integer larger than maxBound: " ++ tooBig
    
    it "rejects extremely large integers" $ do
      case parseExpr "99999999999999999999999999999" of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Should reject extremely large integer"
    
    it "handles edge case: exactly maxBound + 1" $ do
      let exactlyTooBig = show ((fromIntegral (maxBound :: Int) :: Integer) + 1)
      case parseExpr exactlyTooBig of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure $ "Should reject exactly maxBound + 1: " ++ exactlyTooBig

  describe "Arithmetic Overflow Protection" $ do
    it "handles large valid integers in arithmetic" $ do
      let largeInt = maxBound `div` 2 :: Int
      let expr = show largeInt ++ " + " ++ show largeInt
      case testParseTypeCheckEval expr of
        Right (VInt result) -> result `shouldBe` (largeInt * 2)
        Left err -> expectationFailure $ "Should handle large valid arithmetic: " ++ err
        Right _ -> expectationFailure "Should return VInt"
    
    it "prevents parsing of overflow-prone literals" $ do
      property $ \(Positive (n :: Integer)) -> 
        let intVal = n + fromIntegral (maxBound :: Int)
            testStr = show intVal
        in case parseExpr testStr of
             Left _ -> True
             Right _ -> False

  describe "Boundary Value Testing" $ do
    it "correctly parses zero" $ do
      parseEvaluate "0" `shouldBe` Right (VInt 0)
    
    it "correctly parses one" $ do
      parseEvaluate "1" `shouldBe` Right (VInt 1)
    
    it "correctly parses large valid positive number" $ do
      let large = maxBound `div` 4 :: Int
      parseEvaluate (show large) `shouldBe` Right (VInt large)

parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> case typeCheck expr of
    Left _ -> Left (TypeError "Type error")
    Right _ -> evalPure expr

-- Helper function for testing arithmetic with proper error handling
testParseTypeCheckEval :: String -> Either String Value
testParseTypeCheckEval input = case parseExpr input of
  Left parseErr -> Left $ "Parse error: " ++ show parseErr
  Right expr -> case typeCheck expr of
    Left typeErr -> Left $ "Type error: " ++ show typeErr
    Right _ -> case evalPure expr of
      Left runtimeErr -> Left $ "Runtime error: " ++ show runtimeErr
      Right value -> Right value