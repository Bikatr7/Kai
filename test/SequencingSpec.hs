{-# LANGUAGE ScopedTypeVariables #-}
module SequencingSpec where

import Test.Hspec
import TestIO (captureOutput)
import Test.QuickCheck

import Parser
import Evaluator (evalPure, evalWithEnv, Value(..))
import qualified Evaluator as E
import TypeChecker
import Syntax
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Semicolon Sequencing (;)" $ do
    describe "Basic Sequencing" $ do
      it "sequences two integers, returns second" $ do
        let expr = "1; 42"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "sequences arithmetic expressions" $ do
        let expr = "1 + 2; 3 * 4"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 12)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "sequences boolean and integer" $ do
        let expr = "true; 99"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 99)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing Type Checking" $ do
      it "type of sequence is type of second expression" $ do
        let expr = "42; true"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TBool
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "first expression can be any type" $ do
        let expr = "\"hello\"; 42"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TInt
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles multiple sequences" $ do
        let expr = "1; true; \"result\""
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TString
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing Precedence" $ do
      it "semicolon has lower precedence than arithmetic" $ do
        let expr = "1 + 2; 3 * 4"
        case parseExpr expr of
          Right (Seq (Add (IntLit 1) (IntLit 2)) (Mul (IntLit 3) (IntLit 4))) -> return ()
          Right ast -> expectationFailure $ "Wrong AST structure: " ++ show ast
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "semicolon has lower precedence than comparison" $ do
        let expr = "1 == 1; 42"
        case parseExpr expr of
          Right (Seq (Eq (IntLit 1) (IntLit 1)) (IntLit 42)) -> return ()
          Right ast -> expectationFailure $ "Wrong AST structure: " ++ show ast
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "semicolon is right-associative" $ do
        let expr = "1; 2; 3"
        case parseExpr expr of
          Right (Seq (IntLit 1) (Seq (IntLit 2) (IntLit 3))) -> return ()
          Right ast -> expectationFailure $ "Wrong AST structure: " ++ show ast
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing with I/O" $ do
      it "sequences print statements with parentheses" $ do
        let expr = "(print \"first\"); (print \"second\"); 42"
        case parseExpr expr of
          Right ast -> do
            (result, output) <- captureOutput $ evalWithEnv Map.empty ast
            output `shouldBe` "first\nsecond\n"
            case result of
              Right val -> val `shouldBe` VInt 42
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "sequences print without requiring parentheses" $ do
        let expr = "print \"hello\"; 42"
        case parseExpr expr of
          Right (Seq (App (Var "print") (StrLit "hello")) (IntLit 42)) -> return ()
          Right ast -> expectationFailure $ "Wrong AST structure: " ++ show ast
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "do blocks sequence print statements without parentheses" $ do
        let expr = "do { print \"first\"; print \"second\"; 42 }"
        case parseExpr expr of
          Right ast -> do
            (result, output) <- captureOutput $ evalWithEnv Map.empty ast
            output `shouldBe` "first\nsecond\n"
            case result of
              Right val -> val `shouldBe` VInt 42
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "empty do blocks evaluate to unit" $ do
        let expr = "do {}"
        case parseExpr expr of
          Right ast -> do
            typeCheck ast `shouldBe` Right TUnit
            evalPure ast `shouldBe` Right VUnit
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "do blocks can nest" $ do
        let expr = "do { print \"outer\"; do { print \"inner\"; 9 } }"
        case parseExpr expr of
          Right ast -> do
            typeCheck ast `shouldBe` Right TInt
            (result, output) <- captureOutput $ evalWithEnv Map.empty ast
            output `shouldBe` "outer\ninner\n"
            case result of
              Right val -> val `shouldBe` VInt 9
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "do blocks work in conditional branches" $ do
        let expr = "if true then do { print \"branch\"; 1 } else 2"
        case parseExpr expr of
          Right ast -> do
            typeCheck ast `shouldBe` Right TInt
            (result, output) <- captureOutput $ evalWithEnv Map.empty ast
            output `shouldBe` "branch\n"
            case result of
              Right val -> val `shouldBe` VInt 1
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing with Complex Expressions" $ do
      it "sequences let bindings" $ do
        let expr = "let x = 10 in x; let y = 20 in y"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 20)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "sequences function applications" $ do
        let expr = "(\\x -> x + 1) 5; (\\x -> x * 2) 4"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 8)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "distinguishes sequencing conditionals from a sequence inside an else branch" $ do
        let expr = "if true then 1 else 2; if false then 3 else 4"
        case parseExpr expr of
          -- This parses as: if true then 1 else (2; if false then 3 else 4)
          -- Since true, it evaluates to 1
          Right ast -> evalPure ast `shouldBe` Right (VInt 1)
          Left err -> expectationFailure $ "Parse error: " ++ show err
        case parseExpr "(if true then 1 else 2); (if false then 3 else 4)" of
          Right ast -> do
            ast `shouldBe` Seq (If (BoolLit True) (IntLit 1) (IntLit 2))
                              (If (BoolLit False) (IntLit 3) (IntLit 4))
            evalPure ast `shouldBe` Right (VInt 4)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing Evaluation Order" $ do
      it "evaluates first expression for side effects" $ do
        let expr = "let x = 1 in print (toString x); 42"
        case parseExpr expr of
          Right ast -> do
            (result, output) <- captureOutput $ evalWithEnv Map.empty ast
            output `shouldBe` "1\n"
            case result of
              Right val -> val `shouldBe` VInt 42
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing Property Tests" $ do
      it "sequencing is right-associative" $ property $
        forAll kaiInt $ \x -> forAll kaiInt $ \y -> forAll kaiInt $ \z ->
        let expr1 = show x ++ "; " ++ show y ++ "; " ++ show z
            expr2 = show x ++ "; (" ++ show y ++ "; " ++ show z ++ ")"
            expected = Seq (IntLit x) (Seq (IntLit y) (IntLit z))
        in case (parseExpr expr1, parseExpr expr2) of
             (Right ast1, Right ast2) -> ast1 == expected && ast2 == expected
             _ -> False

      it "sequence always returns type of second expression" $ property $
        forAll (elements [(IntLit 42,TInt), (BoolLit True,TBool), (StrLit "Kai",TString), (UnitLit,TUnit)]) $ \(first, _) ->
        forAll (elements [(IntLit 7,TInt), (BoolLit False,TBool), (StrLit "result",TString), (UnitLit,TUnit)]) $ \(second, expected) ->
          typeCheck (Seq first second) === Right expected

kaiInt :: Gen Int
kaiInt = choose (fromInteger kaiIntMin, fromInteger kaiIntMax)
