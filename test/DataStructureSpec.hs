module DataStructureSpec where

import Test.Hspec
import Syntax
import Parser
import Evaluator
import qualified Data.Map as Map

spec :: Spec
spec = describe "Data Structures" $ do
  describe "Lists" $ do
    it "parses and evaluates an empty list" $ do
      let Right (ListLit []) = parseExpr "[]"
      evalPure (ListLit []) `shouldBe` Right (VList [])

    it "parses and evaluates a list of integers" $ do
      let Right (ListLit [IntLit 1, IntLit 2, IntLit 3]) = parseExpr "[1, 2, 3]"
      evalPure (ListLit [IntLit 1, IntLit 2, IntLit 3]) `shouldBe` Right (VList [VInt 1, VInt 2, VInt 3])

    it "parses and evaluates a list with cons operator" $ do
      let Right (Cons (IntLit 1) (ListLit [IntLit 2, IntLit 3])) = parseExpr "1 :: [2, 3]"
      evalPure (Cons (IntLit 1) (ListLit [IntLit 2, IntLit 3])) `shouldBe` Right (VList [VInt 1, VInt 2, VInt 3])

    it "evaluates head of a list" $ do
      let Right list = parseExpr "[1, 2, 3]"
      evalPure (Head list) `shouldBe` Right (VInt 1)

    it "evaluates tail of a list" $ do
      let Right list = parseExpr "[1, 2, 3]"
      evalPure (Tail list) `shouldBe` Right (VList [VInt 2, VInt 3])

    it "evaluates null on a non-empty list" $ do
      let Right list = parseExpr "[1, 2, 3]"
      evalPure (Null list) `shouldBe` Right (VBool False)

    it "evaluates null on an empty list" $ do
      let Right list = parseExpr "[]"
      evalPure (Null list) `shouldBe` Right (VBool True)

  describe "Records" $ do
    it "parses and evaluates a record" $ do
      let Right (RecordLit [("a", IntLit 1), ("b", BoolLit True)]) = parseExpr "{a = 1, b = true}"
      let expected = VRecord (Map.fromList [("a", VInt 1), ("b", VBool True)])
      evalPure (RecordLit [("a", IntLit 1), ("b", BoolLit True)]) `shouldBe` Right expected

    it "accesses a record field" $ do
      let Right record = parseExpr "{a = 1, b = true}"
      evalPure (RecordAccess record "a") `shouldBe` Right (VInt 1)
