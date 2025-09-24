module LetBindingSpec where

import Test.Hspec
import Test.QuickCheck

import Parser
import Evaluator (evalPure, Value(VInt))
import qualified Evaluator as E
import TypeChecker
import Syntax

spec :: Spec
spec = do
  describe "Let Bindings" $ do
    describe "Basic Let Bindings" $ do
      it "evaluates simple let binding" $ do
        let expr = "let x = 42 in x"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "evaluates let binding with arithmetic" $ do
        let expr = "let x = 10 in let y = 5 in x + y"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 15)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "evaluates let binding with function" $ do
        let expr = "let f = \\x -> x * 2 in f 5"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 10)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Scoping" $ do
      it "handles variable shadowing correctly" $ do
        let expr = "let x = 10 in let x = 20 in x"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 20)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "inner bindings don't affect outer scope" $ do
        let expr = "let x = 10 in let y = (let x = 20 in x) in x + y"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 30)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Type Checking" $ do
      it "infers correct types for let bindings" $ do
        let expr = "let x = 42 in x"
        case parseExpr expr of
          Right ast -> typeCheck ast `shouldBe` Right TInt
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "infers function types in let bindings" $ do
        let expr = "let f = \\x -> x + 1 in f"
        case parseExpr expr of
          Right ast -> case typeCheck ast of
            Right (TFun TInt TInt) -> return ()
            Right other -> expectationFailure $ "Expected TFun TInt TInt, got " ++ show other
            Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "catches type errors in let bindings" $ do
        let expr = "let x = 42 in let y = true in x + y"
        case parseExpr expr of
          Right ast -> case typeCheck ast of
            Left _ -> return ()
            Right ty -> expectationFailure $ "Expected type error, but got type: " ++ show ty
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Complex Expressions" $ do
      it "handles nested let bindings with closures" $ do
        let expr = "let x = 5 in let f = \\y -> x + y in f 10"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 15)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles let bindings with conditionals" $ do
        let expr = "let x = 10 in if x > 5 then let y = x * 2 in y else 0"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 20)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Complex Scenarios" $ do
      it "handles deeply nested let bindings (10+ levels)" $ do
        let expr = "let a = 1 in let b = 2 in let c = 3 in let d = 4 in let e = 5 in let f = 6 in let g = 7 in let h = 8 in let i = 9 in let j = 10 in a + b + c + d + e + f + g + h + i + j"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 55)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles complex variable shadowing with functions" $ do
        let expr = "let f = \\x -> x + 1 in let f = \\x -> x * 2 in let f = \\x -> x - 3 in f 10"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 7)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles let bindings with complex arithmetic chains" $ do
        let expr = "let a = 2 in let b = a * 3 in let c = b + 4 in let d = c * 5 in let e = d - 10 in let f = e / 2 in f"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 25)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles higher-order functions with multiple parameters" $ do
        let expr = "let curry = \\f -> \\x -> \\y -> f x y in let add = \\a -> \\b -> a + b in let curried_add = curry add in let add5 = curried_add 5 in add5 10"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 15)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles let bindings inside function bodies" $ do
        let expr = "let f = \\x -> let double = \\y -> y * 2 in let result = double x in result + 1 in f 5"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 11)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles mixed boolean and arithmetic in let bindings" $ do
        let expr = "let x = 5 in let isPositive = x > 0 in let doubled = x * 2 in if isPositive then doubled else 0"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 10)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Edge Cases and Stress Tests" $ do
      it "handles extremely nested expressions without stack overflow" $ do
        let expr = foldr (\i acc -> "let x" ++ show i ++ " = " ++ show i ++ " in " ++ acc) "x1 + x2 + x3 + x4 + x5" [1..50]
        case parseExpr expr of
          Right ast -> case evalPure ast of
            Right (VInt n) -> n `shouldBe` 15
            Left err -> expectationFailure $ "Evaluation error: " ++ show err
            Right other -> expectationFailure $ "Expected VInt, got: " ++ show other
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles complex function composition chains" $ do
        let expr = "let f1 = \\x -> x + 1 in let f2 = \\x -> x * 2 in let f3 = \\x -> x - 3 in let compose = \\g -> \\h -> \\x -> g (h x) in let comp1 = compose f1 f2 in let comp2 = compose comp1 f3 in comp2 10"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 15)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles variable capture in complex closure scenarios" $ do
        let expr = "let makeAdder = \\x -> let inner = x in \\y -> inner + y in let add10 = makeAdder 10 in let add20 = makeAdder 20 in (add10 5) + (add20 3)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 38)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Error Cases" $ do
      it "reports unbound variables correctly" $ do
        let expr = "let x = y in x"
        case parseExpr expr of
          Right ast -> case evalPure ast of
            Left (E.UnboundVariable "y") -> return ()
            Left other -> expectationFailure $ "Expected UnboundVariable y, got " ++ show other
            Right val -> expectationFailure $ "Expected error, but got value: " ++ show val
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles unbound variables in nested contexts" $ do
        let expr = "let x = 10 in let f = \\y -> x + z in f 5"
        case parseExpr expr of
          Right ast -> case evalPure ast of
            Left (E.UnboundVariable "z") -> return ()
            Left other -> expectationFailure $ "Expected UnboundVariable z, got " ++ show other
            Right val -> expectationFailure $ "Expected error, but got value: " ++ show val
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles type errors in let bindings gracefully" $ do
        let expr = "let x = true in let y = 5 in x + y"
        case parseExpr expr of
          Right ast -> case typeCheck ast of
            Left _ -> return ()
            Right ty -> expectationFailure $ "Expected type error, but got type: " ++ show ty
          Left err -> expectationFailure $ "Parse error: " ++ show err