module ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import Parser

spec :: Spec
spec = describe "Parser Tests" $ do
  
  describe "Number Parsing" $ do
    it "parses positive integers" $ do
      parseExpr "42" `shouldBe` Right (IntLit 42)
    
    it "parses zero" $ do
      parseExpr "0" `shouldBe` Right (IntLit 0)
    
    it "parses multi-digit numbers" $ do
      parseExpr "12345" `shouldBe` Right (IntLit 12345)
  
  describe "Boolean Parsing" $ do
    it "parses true" $ do
      parseExpr "true" `shouldBe` Right (BoolLit True)
    
    it "parses false" $ do
      parseExpr "false" `shouldBe` Right (BoolLit False)
  
  describe "Variable Parsing" $ do
    it "parses single letter variable" $ do
      parseExpr "x" `shouldBe` Right (Var "x")
    
    it "parses multi-letter variable" $ do
      parseExpr "hello" `shouldBe` Right (Var "hello")
    
    it "parses variable with underscores" $ do
      parseExpr "my_var" `shouldBe` Right (Var "my_var")
  
  describe "Operator Parsing" $ do
    it "parses addition" $ do
      parseExpr "1 + 2" `shouldBe` Right (Add (IntLit 1) (IntLit 2))
    
    it "parses subtraction" $ do
      parseExpr "5 - 3" `shouldBe` Right (Sub (IntLit 5) (IntLit 3))
    
    it "parses multiplication" $ do
      parseExpr "3 * 4" `shouldBe` Right (Mul (IntLit 3) (IntLit 4))
    
    it "parses division" $ do
      parseExpr "8 / 2" `shouldBe` Right (Div (IntLit 8) (IntLit 2))

  describe "String Parsing" $ do
    it "parses empty string" $ do
      parseExpr "\"\"" `shouldBe` Right (StrLit "")

    it "parses simple string" $ do
      parseExpr "\"hello\"" `shouldBe` Right (StrLit "hello")

    it "parses escaped quote (\")" $ do
      parseExpr "\"\\\"\"" `shouldBe` Right (StrLit "\"")

    it "parses escaped backslash (\\)" $ do
      parseExpr "\"\\\\\"" `shouldBe` Right (StrLit "\\")

    it "parses unary minus on literals" $ do
      parseExpr "-5" `shouldBe` Right (IntLit (-5))

    it "parses unary minus on variables" $ do
      parseExpr "-x" `shouldBe` Right (Sub (IntLit 0) (Var "x"))

    it "parses unary minus on parenthesized expr" $ do
      parseExpr "-(1 + 2)" `shouldBe` Right (Sub (IntLit 0) (Add (IntLit 1) (IntLit 2)))
  
  describe "Precedence Parsing" $ do
    it "multiplication before addition" $ do
      parseExpr "1 + 2 * 3" `shouldBe` Right (Add (IntLit 1) (Mul (IntLit 2) (IntLit 3)))
    
    it "parentheses override precedence" $ do
      parseExpr "(1 + 2) * 3" `shouldBe` Right (Mul (Add (IntLit 1) (IntLit 2)) (IntLit 3))
  
  describe "Lambda Parsing" $ do
    it "parses simple lambda" $ do
      parseExpr "\\x -> x" `shouldBe` Right (Lambda "x" Nothing (Var "x"))
    
    it "parses lambda with expression body" $ do
      parseExpr "\\x -> x + 1" `shouldBe` Right (Lambda "x" Nothing (Add (Var "x") (IntLit 1)))
  
  describe "Function Application Parsing" $ do
    it "parses simple application" $ do
      parseExpr "f x" `shouldBe` Right (App (Var "f") (Var "x"))
    
    it "parses lambda application" $ do
      parseExpr "(\\x -> x) 5" `shouldBe` Right (App (Lambda "x" Nothing (Var "x")) (IntLit 5))
  
  describe "Conditional Parsing" $ do
    it "parses if-then-else" $ do
      parseExpr "if true then 1 else 2" 
        `shouldBe` Right (If (BoolLit True) (IntLit 1) (IntLit 2))
  
  describe "Complex Expression Parsing" $ do
    it "parses nested expressions" $ do
      parseExpr "if (5 > 3) then (2 + 3) else (4 * 1)"
        `shouldBe` Right (If (Gt (IntLit 5) (IntLit 3)) 
                            (Add (IntLit 2) (IntLit 3)) 
                            (Mul (IntLit 4) (IntLit 1)))
  
  describe "Parse Errors" $ do
    it "rejects empty input" $ do
      case parseExpr "" of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Should fail to parse empty input"
    
    it "rejects invalid syntax" $ do
      case parseExpr "1 + + 2" of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Should fail to parse invalid syntax"
    
    it "rejects incomplete expressions" $ do
      case parseExpr "if true then" of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Should fail to parse incomplete if"