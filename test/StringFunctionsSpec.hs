module StringFunctionsSpec (spec) where

import Test.Hspec
import Parser (parseExpr)
import Evaluator (evalPure, Value(..))
import TypeChecker (typeCheck, Type(..))
import Data.Either (isLeft, isRight)

spec :: Spec
spec = do
  describe "String Functions" $ do
    describe "split" $ do
      it "splits string by delimiter" $ do
        let expr = "split \",\" \"a,b,c\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VStr "a", VStr "b", VStr "c"])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "splits string by space" $ do
        let expr = "split \" \" \"hello world test\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VStr "hello", VStr "world", VStr "test"])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "splits string with no delimiter found" $ do
        let expr = "split \",\" \"hello\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VStr "hello"])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "splits empty string" $ do
        let expr = "split \",\" \"\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VStr ""])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "split \",\" \"a,b,c\""
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right (TList TString)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "join" $ do
      it "joins list of strings with delimiter" $ do
        let expr = "join \",\" [\"a\", \"b\", \"c\"]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "a,b,c")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "joins list of strings with space" $ do
        let expr = "join \" \" [\"hello\", \"world\"]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello world")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "joins empty list" $ do
        let expr = "join \",\" []"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "joins single element list" $ do
        let expr = "join \",\" [\"hello\"]"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "join \",\" [\"a\", \"b\"]"
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right TString
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "trim" $ do
      it "trims whitespace from both ends" $ do
        let expr = "trim \"  hello  \""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "trims only leading whitespace" $ do
        let expr = "trim \"  hello\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "trims only trailing whitespace" $ do
        let expr = "trim \"hello  \""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "trims newlines" $ do
        let expr = "trim \"\\nhello\\n\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "does not trim internal whitespace" $ do
        let expr = "trim \"  hello world  \""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello world")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "returns empty string for whitespace-only" $ do
        let expr = "trim \"   \""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "trim \"  hello  \""
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right TString
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "replace" $ do
      it "replaces all occurrences" $ do
        let expr = "replace \"a\" \"x\" \"banana\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "bxnxnx")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "replaces single occurrence" $ do
        let expr = "replace \"world\" \"universe\" \"hello world\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello universe")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "replaces with empty string (removal)" $ do
        let expr = "replace \"x\" \"\" \"xhellox\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "replaces nothing when substring not found" $ do
        let expr = "replace \"z\" \"x\" \"hello\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "replaces empty string" $ do
        let expr = "replace \"a\" \"x\" \"\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "replace \"a\" \"x\" \"banana\""
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right TString
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "strLength" $ do
      it "gets length of non-empty string" $ do
        let expr = "strLength \"hello\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 5)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "gets length of empty string" $ do
        let expr = "strLength \"\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 0)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "gets length with special characters" $ do
        let expr = "strLength \"hello\\nworld\""
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 11)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "has correct type" $ do
        let expr = "strLength \"hello\""
        case parseExpr expr of
          Right parsed -> typeCheck parsed `shouldBe` Right TInt
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Composition of string functions" $ do
      it "split then join" $ do
        let expr = "join \"-\" (split \",\" \"a,b,c\")"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "a-b-c")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "trim then split" $ do
        let expr = "split \" \" (trim \"  hello world  \")"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VList [VStr "hello", VStr "world"])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "replace then strLength" $ do
        let expr = "strLength (replace \"a\" \"xx\" \"banana\")"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VInt 9)  -- "bxxnxxnxx" = 9 chars
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "split, map, join" $ do
        let expr = "join \" \" (map (\\s -> s ++ \"!\") (split \",\" \"hello,world\"))"
        case parseExpr expr of
          Right parsed -> evalPure parsed `shouldBe` Right (VStr "hello! world!")
          Left err -> expectationFailure $ "Parse error: " ++ show err
