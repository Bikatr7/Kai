module BooleanSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import TypeChecker
import Evaluator
import Parser

spec :: Spec
spec = describe "Boolean Logic Operations" $ do
  
  describe "Boolean Literals" $ do
    it "evaluates true to VBool True" $ do
      parseEvaluate "true" `shouldBe` Right (VBool True)
    
    it "evaluates false to VBool False" $ do
      parseEvaluate "false" `shouldBe` Right (VBool False)
  
  describe "Logical AND" $ do
    it "true and true = true" $ do
      parseEvaluate "true and true" `shouldBe` Right (VBool True)
    
    it "true and false = false" $ do
      parseEvaluate "true and false" `shouldBe` Right (VBool False)
    
    it "false and true = false" $ do
      parseEvaluate "false and true" `shouldBe` Right (VBool False)
    
    it "false and false = false" $ do
      parseEvaluate "false and false" `shouldBe` Right (VBool False)
    
    it "is associative: (a and b) and c = a and (b and c)" $ property $
      \a b c -> let boolStr x = if x then "true" else "false"
                in parseEvaluate (boolStr a ++ " and (" ++ boolStr b ++ " and " ++ boolStr c ++ ")")
                == parseEvaluate ("(" ++ boolStr a ++ " and " ++ boolStr b ++ ") and " ++ boolStr c)
  
  describe "Logical OR" $ do
    it "true or true = true" $ do
      parseEvaluate "true or true" `shouldBe` Right (VBool True)
    
    it "true or false = true" $ do
      parseEvaluate "true or false" `shouldBe` Right (VBool True)
    
    it "false or true = true" $ do
      parseEvaluate "false or true" `shouldBe` Right (VBool True)
    
    it "false or false = false" $ do
      parseEvaluate "false or false" `shouldBe` Right (VBool False)
    
    it "is commutative: a or b = b or a" $ property $
      \a b -> let boolStr x = if x then "true" else "false"
              in parseEvaluate (boolStr a ++ " or " ++ boolStr b)
              == parseEvaluate (boolStr b ++ " or " ++ boolStr a)
  
  describe "Logical NOT" $ do
    it "not true = false" $ do
      parseEvaluate "not true" `shouldBe` Right (VBool False)
    
    it "not false = true" $ do
      parseEvaluate "not false" `shouldBe` Right (VBool True)
    
    it "double negation: not (not x) = x" $ property $
      \a -> let boolStr x = if x then "true" else "false"
            in parseEvaluate ("not (not " ++ boolStr a ++ ")")
            == parseEvaluate (boolStr a)

    it "negation toggles equality" $ property $
      forAll (arbitrary :: Gen Int) $ \x ->
      forAll (arbitrary :: Gen Int) $ \y ->
        let e1 = Eq (IntLit x) (IntLit y)
            e2 = Not e1
        in case (eval e1, eval e2) of
             (Right (VBool b1), Right (VBool b2)) -> b1 /= b2
             _ -> False
  
  describe "Complex Boolean Expressions" $ do
    it "De Morgan's Law: not (a and b) = (not a) or (not b)" $ property $
      \a b -> let boolStr x = if x then "true" else "false"
              in parseEvaluate ("not (" ++ boolStr a ++ " and " ++ boolStr b ++ ")")
              == parseEvaluate ("(not " ++ boolStr a ++ ") or (not " ++ boolStr b ++ ")")
    
    it "De Morgan's Law: not (a or b) = (not a) and (not b)" $ property $
      \a b -> let boolStr x = if x then "true" else "false"
              in parseEvaluate ("not (" ++ boolStr a ++ " or " ++ boolStr b ++ ")")
              == parseEvaluate ("(not " ++ boolStr a ++ ") and (not " ++ boolStr b ++ ")")
    
    it "evaluates complex nested expressions" $ do
      parseEvaluate "(true or false) and (not false)" `shouldBe` Right (VBool True)
    
    it "precedence: not before and before or" $ do
      parseEvaluate "not true or false and true" `shouldBe` Right (VBool False)
    
    it "parentheses override precedence" $ do
      parseEvaluate "not (true or false) and true" `shouldBe` Right (VBool False)

-- Helper function
parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> eval expr