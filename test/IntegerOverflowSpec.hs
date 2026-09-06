{-# LANGUAGE ScopedTypeVariables #-}
module IntegerOverflowSpec where

import Test.Hspec
import qualified TestSupport
import Test.QuickCheck
import Syntax
import Parser
import TypeChecker
import Evaluator

spec :: Spec
spec = describe "Integer Overflow Protection" $ do
  
  describe "Parser Bounds Checking" $ do
    it "accepts the maximum 32-bit Int value" $ do
      let maxIntStr = show kaiIntMax
      case parseExpr maxIntStr of
        Right (IntLit n) -> n `shouldBe` fromInteger kaiIntMax
        _ -> expectationFailure $ "Should parse max Int: " ++ maxIntStr

    it "accepts the minimum 32-bit Int value" $ do
      let minIntStr = show kaiIntMin
      case parseExpr minIntStr of
        Right (IntLit n) -> n `shouldBe` fromInteger kaiIntMin
        _ -> expectationFailure $ "Should parse min Int: " ++ minIntStr

    it "rejects an integer larger than the 32-bit maximum" $ do
      let tooBig = show (kaiIntMax + 1)
      case parseExpr tooBig of
        Left err -> TestSupport.isIntegerOverflowParseError (read tooBig) err `shouldBe` True
        Right _ -> expectationFailure $ "Should reject integer larger than maxBound: " ++ tooBig

    it "rejects an integer smaller than the 32-bit minimum" $ do
      let tooSmall = show (kaiIntMin - 1)
      case parseExpr tooSmall of
        Left err -> TestSupport.isIntegerOverflowParseError (read tooSmall) err `shouldBe` True
        Right _ -> expectationFailure $ "Should reject integer smaller than minBound: " ++ tooSmall
    
    it "rejects extremely large integers" $ do
      case parseExpr "99999999999999999999999999999" of
        Left err -> TestSupport.isIntegerOverflowParseError 99999999999999999999999999999 err `shouldBe` True
        Right _ -> expectationFailure "Should reject extremely large integer"
    
    it "handles edge case: exactly 32-bit maxBound + 1" $ do
      let exactlyTooBig = show (kaiIntMax + 1)
      case parseExpr exactlyTooBig of
        Left err -> TestSupport.isIntegerOverflowParseError (read exactlyTooBig) err `shouldBe` True
        Right _ -> expectationFailure $ "Should reject exactly maxBound + 1: " ++ exactlyTooBig

  describe "Arithmetic Overflow Protection" $ do
    it "handles large valid integers in arithmetic" $ do
      let largeInt = fromInteger (kaiIntMax `div` 2) :: Int
      let expr = show largeInt ++ " + " ++ show largeInt
      case testParseTypeCheckEval expr of
        Right (VInt result) -> result `shouldBe` (largeInt * 2)
        Left err -> expectationFailure $ "Should handle large valid arithmetic: " ++ err
        Right _ -> expectationFailure "Should return VInt"

    it "rejects addition above the 32-bit maximum" $ do
      parseEvaluate (show kaiIntMax ++ " + 1") `shouldBe` Left IntegerOverflow

    it "rejects subtraction below the 32-bit minimum" $ do
      parseEvaluate (show kaiIntMin ++ " - 1") `shouldBe` Left IntegerOverflow

    it "rejects multiplication outside the 32-bit range" $ do
      parseEvaluate (show kaiIntMax ++ " * 2") `shouldBe` Left IntegerOverflow

    it "rejects the signed division overflow edge case" $ do
      parseEvaluate (show kaiIntMin ++ " / -1") `shouldBe` Left IntegerOverflow
    
    it "prevents parsing of overflow-prone literals" $ do
      property $ \(Positive (n :: Integer)) -> 
        let intVal = n + kaiIntMax
            testStr = show intVal
        in case parseExpr testStr of
             Left err -> TestSupport.isIntegerOverflowParseError intVal err
             Right _ -> False

  describe "Boundary Value Testing" $ do
    it "correctly parses zero" $ do
      parseEvaluate "0" `shouldBe` Right (VInt 0)
    
    it "correctly parses one" $ do
      parseEvaluate "1" `shouldBe` Right (VInt 1)
    
    it "correctly parses large valid positive number" $ do
      let large = fromInteger (kaiIntMax `div` 4) :: Int
      parseEvaluate (show large) `shouldBe` Right (VInt large)

    it "parseInt accepts both boundaries and rejects values outside them" $ do
      parseEvaluate ("parseInt \"" ++ show kaiIntMax ++ "\"")
        `shouldBe` Right (VJust (VInt (fromInteger kaiIntMax)))
      parseEvaluate ("parseInt \"" ++ show kaiIntMin ++ "\"")
        `shouldBe` Right (VJust (VInt (fromInteger kaiIntMin)))
      parseEvaluate ("parseInt \"" ++ show (kaiIntMax + 1) ++ "\"") `shouldBe` Right VNothing
      parseEvaluate ("parseInt \"" ++ show (kaiIntMin - 1) ++ "\"") `shouldBe` Right VNothing

parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate = TestSupport.evaluateCheckedSource

-- Helper function for testing arithmetic with proper error handling
testParseTypeCheckEval :: String -> Either String Value
testParseTypeCheckEval input = case parseExpr input of
  Left parseErr -> Left $ "Parse error: " ++ show parseErr
  Right expr -> case typeCheck expr of
    Left typeErr -> Left $ "Type error: " ++ show typeErr
    Right _ -> case evalPure expr of
      Left runtimeErr -> Left $ "Runtime error: " ++ show runtimeErr
      Right value -> Right value
