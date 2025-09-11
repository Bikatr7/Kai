module StressTestSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import Parser
import TypeChecker
import Evaluator

-- Helper function to chain parse -> typecheck -> eval
parseTypeCheckEval :: String -> Either String Value
parseTypeCheckEval input = case parseExpr input of
  Left parseErr -> Left $ "Parse error: " ++ show parseErr
  Right expr -> case typeCheck expr of
    Left typeErr -> Left $ "Type error: " ++ show typeErr
    Right _ -> case eval expr of
      Left runtimeErr -> Left $ "Runtime error: " ++ show runtimeErr
      Right value -> Right value

-- Helper function to chain parse -> typecheck
parseTypeCheck :: String -> Either String Type
parseTypeCheck input = case parseExpr input of
  Left parseErr -> Left $ "Parse error: " ++ show parseErr
  Right expr -> case typeCheck expr of
    Left typeErr -> Left $ "Type error: " ++ show typeErr
    Right ty -> Right ty

spec :: Spec
spec = describe "Stress Tests" $ do
  
  describe "Deep Nesting Stress Tests" $ do
    it "handles deeply nested arithmetic (100 levels)" $ do
      let deepArith = buildDeepArithmetic 100
      case parseTypeCheckEval deepArith of
        Right (VInt result) -> result `shouldBe` 100
        Left err -> expectationFailure $ "Deep arithmetic should work: " ++ err
        Right _ -> expectationFailure "Should return VInt"
    
    it "handles deeply nested boolean logic (50 levels)" $ do  
      let deepBool = buildDeepBoolean 50
      case parseTypeCheckEval deepBool of
        Right (VBool _) -> True `shouldBe` True
        Left err -> expectationFailure $ "Deep boolean should work: " ++ err  
        Right _ -> expectationFailure "Should return VBool"
    
    it "handles deeply nested function applications (30 levels)" $ do
      let deepFunc = buildDeepApplication 30
      case parseTypeCheckEval deepFunc of
        Right (VInt result) -> result `shouldBe` 42
        Left err -> expectationFailure $ "Deep application should work: " ++ err
        Right _ -> expectationFailure "Should return VInt"
    
    it "handles deeply nested lambda abstractions (20 levels)" $ do
      let deepLambda = buildDeepLambda 20
      case parseTypeCheck deepLambda of
        Right _ -> True `shouldBe` True
        Left err -> expectationFailure $ "Deep lambda should type-check: " ++ err
    
    it "handles deeply nested conditional expressions (25 levels)" $ do
      let deepIf = buildDeepConditional 25
      case parseTypeCheckEval deepIf of
        Right (VInt result) -> result `shouldBe` 42
        Left err -> expectationFailure $ "Deep conditional should work: " ++ err
        Right _ -> expectationFailure "Should return VInt"

  describe "Complex Expression Stress Tests" $ do
    it "handles expression with many variables" $ do
      let manyVars = buildManyVariables 15
      case parseTypeCheck manyVars of
        Right _ -> True `shouldBe` True  
        Left err -> expectationFailure $ "Many variables should type-check: " ++ err
    
    it "handles large arithmetic expressions" $ do
      let largeExpr = buildLargeArithmetic 50
      case parseTypeCheckEval largeExpr of
        Right (VInt _) -> True `shouldBe` True
        Left err -> expectationFailure $ "Large arithmetic should work: " ++ err
        Right _ -> expectationFailure "Should return VInt"
    
    it "handles complex function composition chains" $ do
      let compChain = buildCompositionChain 10  
      case parseTypeCheckEval compChain of
        Right (VInt result) -> result `shouldBe` 52  -- 42 + 10
        Left err -> expectationFailure $ "Composition chain should work: " ++ err
        Right _ -> expectationFailure "Should return VInt"

  describe "Memory and Performance Stress Tests" $ do
    it "doesn't stack overflow on deep right-associative operations" $ do
      let rightAssoc = buildRightAssociative 1000
      case parseExpr rightAssoc of
        Right _ -> True `shouldBe` True
        Left _ -> expectationFailure "Should parse deeply right-associative expression"
    
    it "doesn't stack overflow on deep left-associative operations" $ do  
      let leftAssoc = buildLeftAssociative 1000
      case parseExpr leftAssoc of
        Right _ -> True `shouldBe` True
        Left _ -> expectationFailure "Should parse deeply left-associative expression"
    
    it "handles very wide expressions (many siblings)" $ do
      let wideExpr = buildWideExpression 100
      case parseTypeCheckEval wideExpr of
        Right (VInt result) -> result `shouldBe` 5050  -- sum from 1 to 100
        Left err -> expectationFailure $ "Wide expression should work: " ++ err
        Right _ -> expectationFailure "Should return VInt"

  describe "Type System Stress Tests" $ do
    it "handles many type variables in complex inference" $ do
      let manyTypeVars = buildManyTypeVariables 20
      case parseTypeCheck manyTypeVars of
        Right _ -> True `shouldBe` True
        Left err -> expectationFailure $ "Many type variables should infer: " ++ err
    
    it "handles complex unification scenarios" $ do
      let complexUnify = buildComplexUnification 15
      case parseTypeCheck complexUnify of
        Right _ -> True `shouldBe` True
        Left err -> expectationFailure $ "Complex unification should work: " ++ err

  describe "Real-World-Like Expressions" $ do
    it "handles mathematical expression with precedence" $ do
      let mathExpr = "((2 + 3) * 4 - 1) * ((7 + 8) / 3) + (5 * (6 - 2))"
      case parseTypeCheckEval mathExpr of
        Right (VInt result) -> result `shouldBe` (((2 + 3) * 4 - 1) * ((7 + 8) `div` 3) + (5 * (6 - 2)))
        Left err -> expectationFailure $ "Math expression should work: " ++ err
        Right _ -> expectationFailure "Should return VInt"
    
    it "handles complex boolean logic expression" $ do
      let boolExpr = "(true and (false or true)) and (not false or (true and not true))"
      case parseTypeCheckEval boolExpr of
        Right (VBool result) -> result `shouldBe` ((True && (False || True)) && (not False || (True && not True)))
        Left err -> expectationFailure $ "Boolean expression should work: " ++ err
        Right _ -> expectationFailure "Should return VBool"
    
    it "handles realistic function with conditionals and arithmetic" $ do
      let realFunc = "\\x -> \\y -> if x > y then (x + y) * 2 else (x - y) + (y * 3)"
      case parseTypeCheck realFunc of
        Right (TFun TInt (TFun TInt TInt)) -> True `shouldBe` True
        Right ty -> expectationFailure $ "Expected TInt -> TInt -> TInt, got: " ++ show ty
        Left err -> expectationFailure $ "Realistic function should type-check: " ++ err

-- Helper functions to build stress test expressions

buildDeepArithmetic :: Int -> String
buildDeepArithmetic 0 = "0"
buildDeepArithmetic n = "1 + " ++ buildDeepArithmetic (n - 1)

buildDeepBoolean :: Int -> String  
buildDeepBoolean 0 = "true"
buildDeepBoolean n = "true and " ++ buildDeepBoolean (n - 1)

buildDeepApplication :: Int -> String
buildDeepApplication n = replicate n '(' ++ "\\x -> x" ++ replicate n ')' ++ " 42"

buildDeepLambda :: Int -> String
buildDeepLambda n = buildLambda n ++ " " ++ unwords (replicate n "0")
  where
    buildLambda 0 = "\\x -> x"
    buildLambda i = "\\x" ++ show i ++ " -> " ++ buildLambda (i - 1)

buildDeepConditional :: Int -> String
buildDeepConditional 0 = "42"
buildDeepConditional n = "if true then " ++ buildDeepConditional (n - 1) ++ " else 0"

buildManyVariables :: Int -> String  
buildManyVariables n = buildLambdas n ++ " " ++ buildVarSum n
  where
    buildLambdas 0 = ""
    buildLambdas i = "\\x" ++ show i ++ " -> " ++ buildLambdas (i - 1)
    buildVarSum 1 = "x1"
    buildVarSum i = "x" ++ show i ++ " + " ++ buildVarSum (i - 1)

buildLargeArithmetic :: Int -> String
buildLargeArithmetic n = intercalate " + " (map show [1..n])
  where
    intercalate :: String -> [String] -> String
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

buildCompositionChain :: Int -> String
buildCompositionChain n = "(" ++ compose ++ ") 42"
  where
    compose = foldl (\acc _ -> "(\\f -> \\g -> \\x -> f (g x)) (\\y -> y + 1) (" ++ acc ++ ")") "\\z -> z" [1..n]

buildRightAssociative :: Int -> String  
buildRightAssociative 0 = "1"
buildRightAssociative n = "1 + (" ++ buildRightAssociative (n - 1) ++ ")"

buildLeftAssociative :: Int -> String
buildLeftAssociative n = foldl (\acc i -> "(" ++ acc ++ " + " ++ show i ++ ")") "1" [2..n]

buildWideExpression :: Int -> String
buildWideExpression n = intercalate " + " (map show [1..n])
  where
    intercalate :: String -> [String] -> String
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

buildManyTypeVariables :: Int -> String
buildManyTypeVariables n = buildNested n ++ " " ++ show n
  where
    buildNested 0 = "\\f -> f"  
    buildNested i = "\\f" ++ show i ++ " -> " ++ buildNested (i - 1)

buildComplexUnification :: Int -> String
buildComplexUnification n = "\\f -> " ++ buildApps n
  where
    buildApps 0 = "f 42"
    buildApps i = "f (" ++ buildApps (i - 1) ++ ")"