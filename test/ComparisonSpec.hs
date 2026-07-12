module ComparisonSpec where

import Test.Hspec
import Control.Monad (forM_)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Syntax
import TypeChecker
import Evaluator
import Parser

spec :: Spec
spec = describe "Comparison Operations" $ do
  
  describe "Equality (==)" $ do
    it "5 == 5 is true" $ do
      parseEvaluate "5 == 5" `shouldBe` Right (VBool True)
    
    it "5 == 3 is false" $ do
      parseEvaluate "5 == 3" `shouldBe` Right (VBool False)
    
    it "true == true is true" $ do
      parseEvaluate "true == true" `shouldBe` Right (VBool True)

    it "returns false for different Maybe and Either variants" $ do
      assertEqualityBoth "Nothing == Just 1" (Right (VBool False))
      assertEqualityBoth "Left 1 == Right 1" (Right (VBool False))

    it "compares nested Maybe, Either, list, record, and tuple values structurally" $ do
      let cases =
            [ ("Just [1, 2] == Just [1, 2]", True)
            , ("Left {value = (1, true)} == Left {value = (1, true)}", True)
            , ("[(1, true)] == [(1, false)]", False)
            , ("{value = Just 1} == {value = Nothing}", False)
            ]
      forM_ cases $ \(source, expected) ->
        assertEqualityBoth source (Right (VBool expected))

    it "rejects callable values at any nesting depth in pure and IO evaluation" $ do
      let expected = Left callableEqualityError
          sources =
            [ "(\\x -> x) == (\\x -> x)"
            , "[(\\x -> x)] == []"
            , "Just (\\x -> x) == Nothing"
            , "Left (\\x -> x) == Right (\\x -> x)"
            , "{callable = (\\x -> x)} == {callable = (\\x -> x)}"
            , "((\\x -> x), 1) == ((\\x -> x), 2)"
            ]
      forM_ sources $ \source -> assertEqualityBoth source expected

    it "rejects recursive references nested inside composite values" $ do
      ref <- newIORef (VInt 1)
      let env = Map.singleton "value" (VList [VRef ref])
          expression = Eq (Var "value") (Var "value")
          expected = Left callableEqualityError
      evalPureWithEnv env expression `shouldBe` expected
      evalWithEnv env expression `shouldReturn` expected
  
  describe "Less Than (<)" $ do
    it "3 < 5 is true" $ do
      parseEvaluate "3 < 5" `shouldBe` Right (VBool True)
    
    it "5 < 3 is false" $ do
      parseEvaluate "5 < 3" `shouldBe` Right (VBool False)
  
  describe "Greater Than (>)" $ do
    it "5 > 3 is true" $ do
      parseEvaluate "5 > 3" `shouldBe` Right (VBool True)
    
    it "3 > 5 is false" $ do
      parseEvaluate "3 > 5" `shouldBe` Right (VBool False)

parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> evalPure expr

assertEqualityBoth :: String -> Either RuntimeError Value -> Expectation
assertEqualityBoth source expected = case parseExpr source of
  Left err -> expectationFailure $ "Parse error: " ++ show err
  Right expression -> do
    typeCheck expression `shouldBe` Right TBool
    evalPure expression `shouldBe` expected
    eval expression `shouldReturn` expected

callableEqualityError :: RuntimeError
callableEqualityError =
  TypeError "Equality is not defined for callable or recursive reference values"
