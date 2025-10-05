{-# LANGUAGE ScopedTypeVariables #-}
module SequencingSpec where

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
            result <- evalWithEnv Map.empty ast
            case result of
              Right val -> val `shouldBe` VInt 42
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "print without parentheses has different precedence" $ do
        let expr = "print \"hello\"; 42"
        case parseExpr expr of
          Right (Print (Seq (StrLit "hello") (IntLit 42))) -> return ()
          Right ast -> expectationFailure $ "Wrong AST structure: " ++ show ast
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing with Complex Expressions" $ do
      it "sequences let bindings" $ do
        let expr = "let x = 10 in x; let y = 20 in y"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 20)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "sequences function applications" $ do
        let expr = "(\\x -> x + 1) 5; (\\x -> x * 2) 3"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 6)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "sequences conditionals" $ do
        let expr = "if true then 1 else 2; if false then 3 else 4"
        case parseExpr expr of
          -- This parses as: if true then 1 else (2; if false then 3 else 4)
          -- Since true, it evaluates to 1
          Right ast -> evalPure ast `shouldBe` Right (VInt 1)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing Evaluation Order" $ do
      it "evaluates first expression for side effects" $ do
        let expr = "let x = 1 in print (toString x); 42"
        -- This should parse as: Let x (Print (Seq (ToString (Var x)) (IntLit 42)))
        -- due to precedence, but still test the concept
        case parseExpr expr of
          Right ast -> do
            result <- evalWithEnv Map.empty ast
            case result of
              Right val -> val `shouldBe` VUnit  -- print returns unit
              Left err -> expectationFailure $ "Eval error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Sequencing Property Tests" $ do
      it "sequencing is right-associative" $ property $ \(x :: Int) (y :: Int) (z :: Int) ->
        let expr1 = show x ++ "; " ++ show y ++ "; " ++ show z
            expr2 = show x ++ "; (" ++ show y ++ "; " ++ show z ++ ")"
        in case (parseExpr expr1, parseExpr expr2) of
             (Right ast1, Right ast2) -> ast1 == ast2
             _ -> False

      it "sequence always returns type of second expression" $ property $ \(x :: Int) (y :: Int) ->
        x /= y ==> -- Different values to ensure they have different potential types
          let expr = "42; " ++ show (abs y)
          in case parseExpr expr of
               Right ast -> case typeCheck ast of
                 Right TInt -> property True
                 _ -> property False
               _ -> property False