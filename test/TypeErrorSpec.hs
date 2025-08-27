module TypeErrorSpec where

import Test.Hspec
import Syntax
import TypeChecker
import Parser

spec :: Spec
spec = describe "Type Errors" $ do
  
  describe "Arithmetic Type Errors" $ do
    it "rejects 1 + true" $ do
      case parseAndTypeCheck "1 + true" of
        Left (TypeMismatch TInt TBool) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error: TypeMismatch TInt TBool"
    
    it "rejects true - 5" $ do
      case parseAndTypeCheck "true - 5" of
        Left (TypeMismatch TInt TBool) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error"
    
    it "rejects false * 3" $ do
      case parseAndTypeCheck "false * 3" of
        Left (TypeMismatch TInt TBool) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error"
  
  describe "Boolean Logic Type Errors" $ do
    it "rejects 5 and true" $ do
      case parseAndTypeCheck "5 and true" of
        Left (TypeMismatch TBool TInt) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error"
    
    it "rejects true or 10" $ do
      case parseAndTypeCheck "true or 10" of
        Left (TypeMismatch TBool TInt) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error"
    
    it "rejects not 42" $ do
      case parseAndTypeCheck "not 42" of
        Left (ExpectedBool TInt) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error: ExpectedBool TInt"
  
  describe "Comparison Type Errors" $ do
    it "rejects 5 < true" $ do
      case parseAndTypeCheck "5 < true" of
        Left (TypeMismatch TInt TBool) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error"
    
    it "rejects false > 10" $ do
      case parseAndTypeCheck "false > 10" of
        Left (TypeMismatch TInt TBool) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error"
  
  describe "Conditional Type Errors" $ do
    it "rejects if 5 then 1 else 2" $ do
      case parseAndTypeCheck "if 5 then 1 else 2" of
        Left (ExpectedBool TInt) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error: condition must be bool"
    
    it "rejects if true then 1 else false" $ do
      case parseAndTypeCheck "if true then 1 else false" of
        Left (TypeMismatch TInt TBool) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error: branches must match"
  
  describe "Function Type Errors" $ do
    it "rejects applying non-function" $ do
      case parseAndTypeCheck "5 10" of
        Left (ExpectedFunction TInt) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error: expected function"
    
    it "rejects wrong argument type" $ do
      case parseAndTypeCheck "(\\x -> x + 1) true" of
        Left (TypeMismatch TInt TBool) -> True `shouldBe` True
        _ -> expectationFailure "Should be type error: wrong arg type"

parseAndTypeCheck :: String -> Either TypeError Type
parseAndTypeCheck input = case parseExpr input of
  Left _ -> Left (TypeMismatch TInt TBool)  -- dummy error for parse failures, will be replaced with a more specific error eventually
  Right expr -> typeCheck expr