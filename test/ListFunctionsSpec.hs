module ListFunctionsSpec (spec) where

import Test.Hspec
import Parser (parseExpr)
import Evaluator (evalPure, Value(..))
import TypeChecker (typeCheck, Type(..))
import Data.Either (isLeft, isRight)

spec :: Spec
spec = do
  describe "List Functions" $ do
    describe "map" $ do
      it "maps increment over list" $ do
        let expr = "map (\\x -> x + 1) [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 2, VInt 3, VInt 4])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "maps double over list" $ do
        let expr = "map (\\x -> x * 2) [1, 2, 3, 4]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 2, VInt 4, VInt 6, VInt 8])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "maps over empty list" $ do
        let expr = "map (\\x -> x + 1) []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "map (\\x -> x + 1) [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right (TList TInt)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "filter" $ do
      it "filters even numbers" $ do
        let expr = "filter (\\x -> x - (x / 2 * 2) == 0) [1, 2, 3, 4, 5, 6]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 2, VInt 4, VInt 6])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "filters positive numbers" $ do
        let expr = "filter (\\x -> x > 0) [-2, -1, 0, 1, 2]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 1, VInt 2])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "filters empty list" $ do
        let expr = "filter (\\x -> x > 0) []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "filters none match" $ do
        let expr = "filter (\\x -> x > 10) [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "filter (\\x -> x > 0) [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right (TList TInt)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "foldl" $ do
      it "sums a list" $ do
        let expr = "foldl (\\acc -> \\x -> acc + x) 0 [1, 2, 3, 4]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 10)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "multiplies a list" $ do
        let expr = "foldl (\\acc -> \\x -> acc * x) 1 [2, 3, 4]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 24)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "folds empty list returns accumulator" $ do
        let expr = "foldl (\\acc -> \\x -> acc + x) 42 []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "builds list in reverse (left fold)" $ do
        let expr = "foldl (\\acc -> \\x -> x :: acc) [] [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 3, VInt 2, VInt 1])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "foldl (\\acc -> \\x -> acc + x) 0 [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right TInt
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "length" $ do
      it "gets length of non-empty list" $ do
        let expr = "length [1, 2, 3, 4, 5]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 5)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "gets length of empty list" $ do
        let expr = "length []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 0)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "gets length of single element list" $ do
        let expr = "length [42]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 1)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "length [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right TInt
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "reverse" $ do
      it "reverses non-empty list" $ do
        let expr = "reverse [1, 2, 3, 4, 5]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 5, VInt 4, VInt 3, VInt 2, VInt 1])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "reverses empty list" $ do
        let expr = "reverse []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "reverses single element list" $ do
        let expr = "reverse [42]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 42])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "reverse [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right (TList TInt)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "take" $ do
      it "takes first n elements" $ do
        let expr = "take 3 [1, 2, 3, 4, 5]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 1, VInt 2, VInt 3])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "takes 0 elements" $ do
        let expr = "take 0 [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "takes more than list length" $ do
        let expr = "take 10 [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 1, VInt 2, VInt 3])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "takes from empty list" $ do
        let expr = "take 5 []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "take 3 [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right (TList TInt)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "drop" $ do
      it "drops first n elements" $ do
        let expr = "drop 2 [1, 2, 3, 4, 5]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 3, VInt 4, VInt 5])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "drops 0 elements" $ do
        let expr = "drop 0 [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 1, VInt 2, VInt 3])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "drops more than list length" $ do
        let expr = "drop 10 [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "drops from empty list" $ do
        let expr = "drop 5 []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "drop 2 [1, 2, 3]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right (TList TInt)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "zip" $ do
      it "zips two lists of same length" $ do
        let expr = "zip [1, 2, 3] [4, 5, 6]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe`
            Right (VList [VTuple [VInt 1, VInt 4], VTuple [VInt 2, VInt 5], VTuple [VInt 3, VInt 6]])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "zips lists of different lengths (shorter wins)" $ do
        let expr = "zip [1, 2] [3, 4, 5, 6]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe`
            Right (VList [VTuple [VInt 1, VInt 3], VTuple [VInt 2, VInt 4]])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "zips empty lists" $ do
        let expr = "zip [] []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "zips with one empty list" $ do
        let expr = "zip [1, 2, 3] []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "zip [1, 2, 3] [4, 5, 6]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right (TList (TTuple [TInt, TInt]))
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Composition of list functions" $ do
      it "map then filter" $ do
        let expr = "filter (\\x -> x > 3) (map (\\x -> x * 2) [1, 2, 3, 4])"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 4, VInt 6, VInt 8])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "map then reverse" $ do
        let expr = "reverse (map (\\x -> x + 1) [1, 2, 3])"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 4, VInt 3, VInt 2])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "take then length" $ do
        let expr = "length (take 3 [1, 2, 3, 4, 5])"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 3)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "zip then map" $ do
        let expr = "map (\\p -> (fst p) + (snd p)) (zip [1, 2, 3] [4, 5, 6])"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VInt 5, VInt 7, VInt 9])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "complex: filter, map, foldl" $ do
        let expr = "foldl (\\acc -> \\x -> acc + x) 0 (map (\\x -> x * 2) (filter (\\x -> x > 2) [1, 2, 3, 4, 5]))"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 24)  -- [3,4,5] -> [6,8,10] -> 24
          Left err -> expectationFailure $ "Parse error: " ++ show err
