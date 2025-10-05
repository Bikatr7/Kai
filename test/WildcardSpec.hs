module WildcardSpec where

import Test.Hspec
import Test.QuickCheck

import Parser
import Evaluator (evalPure, evalWithEnv, Value(..))
import qualified Evaluator as E
import TypeChecker
import Syntax
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Wildcard Variables (_)" $ do
    describe "Basic Wildcard Usage" $ do
      it "accepts wildcard in let binding" $ do
        let expr = "let _ = 42 in 1"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 1)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "accepts multiple wildcards" $ do
        let expr = "let _ = 10 in let _ = 20 in 5"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 5)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "wildcard with print statement returns unit" $ do
        let expr = "let _ = print \"hello\" in 42"
        case parseExpr expr of
          Right ast -> do
            result <- evalWithEnv Map.empty ast
            case result of
              Right val -> val `shouldBe` VInt 42
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Wildcard Type Checking" $ do
      it "wildcard does not interfere with type inference" $ do
        let expr = "let _ = 42 in let x = true in x"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TBool
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "wildcard can bind different types in sequence" $ do
        let expr = "let _ = 42 in let _ = true in let _ = \"hello\" in 1"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TInt
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Wildcard Error Cases" $ do
      it "rejects wildcard in letrec" $ do
        let expr = "letrec _ = 42 in 1"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Left (InvalidWildcard _) -> return ()
              Left err -> expectationFailure $ "Wrong error type: " ++ show err
              Right _ -> expectationFailure "Should have failed type checking"
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Wildcard with Complex Expressions" $ do
      it "wildcard with function application" $ do
        let expr = "let f = \\x -> x + 1 in let _ = f 5 in 42"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "wildcard with conditional" $ do
        let expr = "let _ = if true then 1 else 2 in 99"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 99)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "wildcard does not shadow regular variables" $ do
        let expr = "let x = 10 in let _ = 20 in let y = 30 in x + y"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 40)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Wildcard Practical Usage" $ do
      it "simulates print sequencing pattern" $ do
        let expr = "let _ = print \"first\" in let _ = print \"second\" in 42"
        case parseExpr expr of
          Right ast -> do
            result <- evalWithEnv Map.empty ast
            case result of
              Right val -> val `shouldBe` VInt 42
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err