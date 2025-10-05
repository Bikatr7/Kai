{-# LANGUAGE ScopedTypeVariables #-}
module PropertyBasedSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import Parser
import TypeChecker
import Evaluator (Value(VInt, VBool, VFun, VRef), evalPure)
import Control.Monad (liftM, liftM2, liftM3)

-- Generator for valid Kai expressions
newtype ValidExpr = ValidExpr Expr deriving (Show, Eq)

instance Arbitrary ValidExpr where
  arbitrary = ValidExpr <$> sized arbitraryExpr

arbitraryExpr :: Int -> Gen Expr  
arbitraryExpr 0 = oneof
  [ IntLit <$> arbitrary
  , BoolLit <$> arbitrary
  ]
arbitraryExpr n = oneof
  [ IntLit <$> (arbitrary `suchThat` (\x -> x >= (minBound :: Int) && x <= (maxBound :: Int)))
  , BoolLit <$> arbitrary
  , liftM2 Add (arbitraryExpr n') (arbitraryExpr n')
  , liftM2 Sub (arbitraryExpr n') (arbitraryExpr n')  
  , liftM2 Mul (arbitraryExpr n') (arbitraryExpr n')
  , liftM2 And (arbitraryExpr n') (arbitraryExpr n')
  , liftM2 Or (arbitraryExpr n') (arbitraryExpr n')
  , fmap Not (arbitraryExpr n')
  , liftM2 Eq (arbitraryExpr n') (arbitraryExpr n')
  , liftM2 Lt (arbitraryExpr n') (arbitraryExpr n')
  , liftM2 Gt (arbitraryExpr n') (arbitraryExpr n')
  , liftM3 If (arbitraryExpr n') (arbitraryExpr n') (arbitraryExpr n')
  , Lambda <$> arbitraryVar <*> pure Nothing <*> arbitraryExpr n'
  , liftM2 App (arbitraryExpr n') (arbitraryExpr n')
  ]
  where 
    n' = n `div` 2
    arbitraryVar = elements ["x", "y", "z", "f", "g", "h", "a", "b", "c"]

-- Generator for integers that should cause overflow
newtype OverflowInt = OverflowInt Integer deriving (Show, Eq)

instance Arbitrary OverflowInt where
  arbitrary = OverflowInt <$> oneof
    [ choose (fromIntegral (maxBound :: Int) + 1, fromIntegral (maxBound :: Int) + 1000000)
    , choose (fromIntegral (minBound :: Int) - 1000000, fromIntegral (minBound :: Int) - 1)
    ]

spec :: Spec
spec = describe "Property-Based Testing" $ do
  
  describe "Parser Properties" $ do
    it "parsing never crashes on valid integers" $ do
      property $ \(x :: Int) -> 
        case parseExpr (show x) of
          Right (IntLit n) -> n == x
          Right _ -> False
          Left _ -> False
    
    it "rejects all overflow integers" $ do
      property $ \(OverflowInt n) -> 
        case parseExpr (show n) of
          Left _ -> True
          Right _ -> False
    
    it "pretty-print is stable after parse" $ do
      property $ \(ValidExpr expr) ->
        let s = prettyExpr expr in
        case parseExpr s of
          Right e1 -> prettyExpr e1 == s
          Left _ -> False
    
    it "parsing is deterministic" $ do
      property $ \(ValidExpr expr) ->
        let exprStr = prettyExpr expr
            result1 = parseExpr exprStr
            result2 = parseExpr exprStr
        in result1 == result2

  describe "Type System Properties" $ do
    it "type checking is deterministic" $ do
      property $ \(ValidExpr expr) ->
        let result1 = typeCheck expr
            result2 = typeCheck expr
        in result1 == result2
    
    it "well-typed expressions don't crash evaluator" $ do
      property $ \(ValidExpr expr) ->
        case typeCheck expr of
          Right _ -> case evalPure expr of
            Right _ -> True
            Left _ -> True  -- Runtime errors are OK (like div by zero)
          Left _ -> True  -- Type errors are expected for some random expressions
    
    it "type preservation: evaluation preserves types" $ do
      property $ forAll (resize 3 arbitrary) $ \(ValidExpr expr) ->
        case (typeCheck expr, evalPure expr) of
          (Right TInt, Right (VInt _)) -> True
          (Right TBool, Right (VBool _)) -> True
          (Right (TFun _ _), Right VFun {}) -> True
          (Left _, _) -> True  -- Type errors are fine
          (_, Left _) -> True  -- Runtime errors are fine
          _ -> False

  describe "Arithmetic Properties" $ do
    it "addition is commutative" $ do
      property $ \x y ->
        let expr1 = Add (IntLit x) (IntLit y)
            expr2 = Add (IntLit y) (IntLit x)
        in case (evalPure expr1, evalPure expr2) of
             (Right v1, Right v2) -> v1 == v2
             _ -> True  -- Errors are fine (overflow etc)
    
    it "addition is associative" $ do
      property $ \x y z ->
        let expr1 = Add (Add (IntLit x) (IntLit y)) (IntLit z)
            expr2 = Add (IntLit x) (Add (IntLit y) (IntLit z))
        in case (evalPure expr1, evalPure expr2) of
             (Right v1, Right v2) -> v1 == v2
             _ -> True
    
    it "multiplication by zero gives zero" $ do
      property $ \x ->
        let expr = Mul (IntLit x) (IntLit 0)
        in case evalPure expr of
             Right (VInt 0) -> True
             _ -> False

    it "negation distributes over addition" $ do
      property $ \x y ->
        let lhs = Sub (IntLit 0) (Add (IntLit x) (IntLit y))
            rhs = Add (Sub (IntLit 0) (IntLit x)) (Sub (IntLit 0) (IntLit y))
        in evalPure lhs == evalPure rhs

  describe "String Properties" $ do
    it "concatenation is associative" $ do
      property $ \(a :: String) (b :: String) (c :: String) ->
        let e1 = Concat (Concat (StrLit a) (StrLit b)) (StrLit c)
            e2 = Concat (StrLit a) (Concat (StrLit b) (StrLit c))
        in evalPure e1 == evalPure e2
  describe "Boolean Logic Properties" $ do
    it "boolean logic follows De Morgan's laws" $ do
      property $ \p q ->
        let notPAndNotQ = And (Not (BoolLit p)) (Not (BoolLit q))
            notPOrQ = Not (Or (BoolLit p) (BoolLit q))
        in case (evalPure notPAndNotQ, evalPure notPOrQ) of
             (Right v1, Right v2) -> v1 == v2
             _ -> False
    
    it "AND is commutative" $ do
      property $ \p q ->
        let expr1 = And (BoolLit p) (BoolLit q)
            expr2 = And (BoolLit q) (BoolLit p)
        in case (evalPure expr1, evalPure expr2) of
             (Right v1, Right v2) -> v1 == v2
             _ -> False
    
    it "double negation elimination" $ do
      property $ \p ->
        let expr = Not (Not (BoolLit p))
        in case evalPure expr of
             Right (VBool result) -> result == p
             _ -> False

  describe "Function Properties" $ do
    it "identity function returns input" $ do
      property $ \(x :: Int) ->
        let identity = Lambda "x" Nothing (Var "x")
            application = App identity (IntLit x)
        in case evalPure application of
             Right (VInt result) -> result == x
             _ -> False
    
    it "constant function ignores second argument" $ do
      property $ \x y ->
        let constFunc = Lambda "x" Nothing (Lambda "y" Nothing (Var "x"))
            application = App (App constFunc (IntLit x)) (IntLit y)
        in case evalPure application of
             Right (VInt result) -> result == x
             _ -> False
    
    it "function composition works correctly" $ do
      property $ \(x :: Int) ->
        let f = Lambda "x" Nothing (Add (Var "x") (IntLit 1))  -- x + 1
            g = Lambda "x" Nothing (Mul (Var "x") (IntLit 2))  -- x * 2
            compose = Lambda "f" Nothing (Lambda "g" Nothing (Lambda "x" Nothing (App (Var "f") (App (Var "g") (Var "x")))))
            composed = App (App compose f) g
            result = App composed (IntLit x)
        in case evalPure result of
             Right (VInt n) -> n == (x * 2) + 1
             _ -> False

  describe "Conditional Properties" $ do
    it "if-then-else selects correct branch" $ do
      property $ \condition x y ->
        let expr = If (BoolLit condition) (IntLit x) (IntLit y)
        in case evalPure expr of
             Right (VInt result) -> result == if condition then x else y
             _ -> False
    
    it "conditional with same branches returns that value" $ do
      property $ \condition (x :: Int) ->
        let expr = If (BoolLit condition) (IntLit x) (IntLit x)
        in case evalPure expr of
             Right (VInt result) -> result == x
             _ -> False

  describe "Error Handling Properties" $ do
    it "type errors are consistent" $ do
      property $ forAll (resize 2 arbitrary) $ \(ValidExpr expr) ->
        case typeCheck expr of
          Left err1 -> case typeCheck expr of
            Left err2 -> err1 == err2
          Right _ -> True
    
    it "evaluation errors are deterministic" $ do
      property $ forAll (resize 2 arbitrary) $ \(ValidExpr expr) ->
        let result1 = evalPure expr
            result2 = evalPure expr
        in case (result1, result2) of
             (Left err1, Left err2) -> err1 == err2  -- Errors should be identical
             (Right val1, Right val2) -> comparableValues val1 val2 -- Values should be identical if comparable
             _ -> False  -- Different result types shouldn't happen
      where
        -- Check if two values can be meaningfully compared for equality
        comparableValues (VFun {}) (VFun {}) = True  -- Functions are deterministic but not comparable
        comparableValues (VRef _) (VRef _) = True        -- References are deterministic but not comparable
        comparableValues v1 v2 = v1 == v2                -- Everything else should be equal

-- Helper function to normalize expressions for comparison
normalizeExpr :: Expr -> Expr
normalizeExpr = id  -- For now, no normalization needed

prettyExpr :: Expr -> String
prettyExpr (IntLit n) = show n
prettyExpr (BoolLit True) = "true"
prettyExpr (BoolLit False) = "false"
prettyExpr (Var x) = x
prettyExpr (Add a b) = "(" ++ prettyExpr a ++ " + " ++ prettyExpr b ++ ")"
prettyExpr (Sub a b) = "(" ++ prettyExpr a ++ " - " ++ prettyExpr b ++ ")"
prettyExpr (Mul a b) = "(" ++ prettyExpr a ++ " * " ++ prettyExpr b ++ ")"
prettyExpr (Div a b) = "(" ++ prettyExpr a ++ " / " ++ prettyExpr b ++ ")"
prettyExpr (And a b) = "(" ++ prettyExpr a ++ " and " ++ prettyExpr b ++ ")"
prettyExpr (Or a b)  = "(" ++ prettyExpr a ++ " or "  ++ prettyExpr b ++ ")"
prettyExpr (Not a)   = "(not " ++ prettyExpr a ++ ")"
prettyExpr (Eq a b)  = "(" ++ prettyExpr a ++ " == " ++ prettyExpr b ++ ")"
prettyExpr (Lt a b)  = "(" ++ prettyExpr a ++ " < " ++ prettyExpr b ++ ")"
prettyExpr (Gt a b)  = "(" ++ prettyExpr a ++ " > " ++ prettyExpr b ++ ")"
prettyExpr (If c t e) = "(if " ++ prettyExpr c ++ " then " ++ prettyExpr t ++ " else " ++ prettyExpr e ++ ")"
prettyExpr (Lambda p _ b) = "(\\" ++ p ++ " -> " ++ prettyExpr b ++ ")"
prettyExpr (App f x) = "(" ++ prettyExpr f ++ " " ++ prettyExpr x ++ ")"