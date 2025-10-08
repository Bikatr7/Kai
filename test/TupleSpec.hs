module TupleSpec where

import Test.Hspec
import Test.QuickCheck

import Parser
import Evaluator (evalPure, Value(..))
import TypeChecker
import Syntax

spec :: Spec
spec = do
  describe "Tuple Support" $ do
    describe "Tuple Literals" $ do
      it "creates a 2-tuple" $ do
        let expr = "(1, 2)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 1, VInt 2])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "creates a 3-tuple" $ do
        let expr = "(42, true, \"hello\")"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 42, VBool True, VStr "hello"])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "creates a nested tuple" $ do
        let expr = "((1, 2), (3, 4))"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VTuple [VInt 1, VInt 2], VTuple [VInt 3, VInt 4]])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "creates a single-element tuple is just parentheses" $ do
        let expr = "(42)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "creates a tuple with expressions" $ do
        let expr = "(1 + 2, 3 * 4)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 3, VInt 12])
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Tuple Type Checking" $ do
      it "infers type of 2-tuple" $ do
        let expr = "(1, true)"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TTuple [TInt, TBool]
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "infers type of 3-tuple" $ do
        let expr = "(42, \"hello\", false)"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TTuple [TInt, TString, TBool]
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "infers type of nested tuples" $ do
        let expr = "((1, 2), (true, false))"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TTuple [TTuple [TInt, TInt], TTuple [TBool, TBool]]
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "fst Function" $ do
      it "extracts first element of tuple" $ do
        let expr = "fst (42, true)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "works with nested tuples" $ do
        let expr = "fst ((1, 2), (3, 4))"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 1, VInt 2])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "fst (42, true)"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TInt
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "snd Function" $ do
      it "extracts second element of tuple" $ do
        let expr = "snd (42, true)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VBool True)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "works with nested tuples" $ do
        let expr = "snd ((1, 2), (3, 4))"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 3, VInt 4])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "snd (42, true)"
        case parseExpr expr of
          Right ast -> do
            case typeCheck ast of
              Right typ -> typ `shouldBe` TBool
              Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Tuple Pattern Matching" $ do
      it "matches 2-tuple pattern" $ do
        let expr = "case (1, 2) of (x, y) -> x + y"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 3)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "matches 3-tuple pattern" $ do
        let expr = "case (10, 20, 30) of (a, b, c) -> a + b + c"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 60)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "matches nested tuple pattern" $ do
        let expr = "case ((1, 2), 3) of ((a, b), c) -> a + b + c"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 6)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "matches with mixed types" $ do
        let expr = "case (42, \"hello\") of (n, s) -> n"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "uses wildcard in tuple pattern" $ do
        let expr = "case (1, 2, 3) of (x, _, z) -> x + z"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 4)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Tuple Equality" $ do
      it "compares equal tuples" $ do
        let expr = "(1, 2) == (1, 2)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VBool True)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "compares unequal tuples" $ do
        let expr = "(1, 2) == (1, 3)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VBool False)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "compares nested tuples" $ do
        let expr = "((1, 2), 3) == ((1, 2), 3)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VBool True)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Tuples with Functions" $ do
      it "stores functions in tuples" $ do
        let expr = "let pair = (\\x -> x + 1, \\x -> x * 2) in (fst pair) 10"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 11)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "returns tuple from function" $ do
        let expr = "let makePair = \\x -> \\y -> (x, y) in makePair 1 2"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 1, VInt 2])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "uses tuple as function argument" $ do
        let expr = "let add = \\p -> case p of (x, y) -> x + y in add (3, 4)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VInt 7)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Complex Tuple Expressions" $ do
      it "combines tuples with let bindings" $ do
        let expr = "let x = 10 in let y = 20 in (x + y, x * y)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 30, VInt 200])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "uses tuples in conditionals" $ do
        let expr = "if true then (1, 2) else (3, 4)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 1, VInt 2])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles tuple swap idiom" $ do
        let expr = "let swap = \\p -> case p of (x, y) -> (y, x) in swap (1, 2)"
        case parseExpr expr of
          Right ast -> evalPure ast `shouldBe` Right (VTuple [VInt 2, VInt 1])
          Left err -> expectationFailure $ "Parse error: " ++ show err
