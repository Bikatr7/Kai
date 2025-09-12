module StringSpec where

import Test.Hspec
import Syntax
import Parser
import Evaluator
import TypeChecker

spec :: Spec
spec = describe "String Support" $ do
  describe "Parsing" $ do
    it "parses simple literal" $ do
      parseExpr "\"hi\"" `shouldBe` Right (StrLit "hi")

    it "parses concatenation" $ do
      parseExpr "\"a\" ++ \"b\"" `shouldBe` Right (Concat (StrLit "a") (StrLit "b"))

  describe "Typing" $ do
    it "types string literal as TString" $ do
      case parseExpr "\"x\"" of
        Right e -> typeCheck e `shouldBe` Right TString
        Left _ -> expectationFailure "parse"

    it "types concatenation as TString" $ do
      case parseExpr "\"a\" ++ \"b\"" of
        Right e -> typeCheck e `shouldBe` Right TString
        Left _ -> expectationFailure "parse"

  describe "Evaluation" $ do
    it "evaluates concatenation" $ do
      case parseExpr "\"foo\" ++ \"bar\"" of
        Right e -> eval e `shouldBe` Right (VStr "foobar")
        Left _ -> expectationFailure "parse"

  describe "Invalid" $ do
    it "rejects ++ with non-strings" $ do
      case parseExpr "1 ++ \"a\"" of
        Right e -> case typeCheck e of
          Left _ -> True `shouldBe` True
          Right _ -> expectationFailure "should be type error"
        Left _ -> expectationFailure "parse"


