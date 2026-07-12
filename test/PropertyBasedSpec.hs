{-# LANGUAGE ScopedTypeVariables #-}
module PropertyBasedSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import Parser
import TypeChecker
import Evaluator (Value(..), evalPure)
import Control.Monad (liftM, liftM2, liftM3)
import qualified Data.Map as Map

-- Generator for valid Kai expressions
newtype ValidExpr = ValidExpr Expr deriving (Show, Eq)

instance Arbitrary ValidExpr where
  arbitrary = ValidExpr <$> sized arbitraryExpr

arbitraryExpr :: Int -> Gen Expr  
arbitraryExpr 0 = oneof
  [ IntLit <$> genKaiInt
  , BoolLit <$> arbitrary
  ]
arbitraryExpr n = oneof
  [ IntLit <$> genKaiInt
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

genKaiInt :: Gen Int
genKaiInt = choose (fromInteger kaiIntMin, fromInteger kaiIntMax)

genSmallKaiInt :: Gen Int
genSmallKaiInt = choose (-100000, 100000)

typedPureExpr :: Int -> Gen Expr
typedPureExpr size = oneof
  [ typedIntExpr size
  , typedBoolExpr size
  , typedStringExpr size
  , typedListExpr size
  , typedCompositeExpr size
  ]

typedIntExpr :: Int -> Gen Expr
typedIntExpr 0 = IntLit <$> genSmallKaiInt
typedIntExpr size = oneof
  [ IntLit <$> genSmallKaiInt
  , liftM2 Add smaller smaller
  , liftM2 Sub smaller smaller
  , If <$> typedBoolExpr next <*> smaller <*> smaller
  , pure $ Let "x" Nothing (IntLit 10) (Add (Var "x") (IntLit 5))
  , pure $ Case (MJust (IntLit 7)) [(PJust (PVar "x"), Var "x"), (PNothing, IntLit 0)]
  ]
  where
    next = size `div` 2
    smaller = typedIntExpr next

typedBoolExpr :: Int -> Gen Expr
typedBoolExpr 0 = BoolLit <$> arbitrary
typedBoolExpr size = oneof
  [ BoolLit <$> arbitrary
  , liftM2 And smaller smaller
  , liftM2 Or smaller smaller
  , Not <$> smaller
  , liftM2 Eq (typedIntExpr next) (typedIntExpr next)
  , If <$> smaller <*> smaller <*> smaller
  ]
  where
    next = size `div` 2
    smaller = typedBoolExpr next

typedStringExpr :: Int -> Gen Expr
typedStringExpr 0 = StrLit <$> elements ["", "kai", "typed"]
typedStringExpr size = oneof
  [ StrLit <$> elements ["", "kai", "typed"]
  , liftM2 Concat smaller smaller
  , If <$> typedBoolExpr next <*> smaller <*> smaller
  , Show <$> typedIntExpr next
  ]
  where
    next = size `div` 2
    smaller = typedStringExpr next

typedListExpr :: Int -> Gen Expr
typedListExpr size = oneof
  [ ListLit . map IntLit <$> listOf genSmallKaiInt
  , Cons <$> typedIntExpr next <*> (ListLit . map IntLit <$> listOf genSmallKaiInt)
  , Map (Lambda "x" Nothing (Add (Var "x") (IntLit 1))) <$> baseList
  , Reverse <$> baseList
  ]
  where
    next = size `div` 2
    baseList = ListLit . map IntLit <$> listOf genSmallKaiInt

typedCompositeExpr :: Int -> Gen Expr
typedCompositeExpr size = elements
  [ TupleLit [IntLit 1, BoolLit True]
  , RecordLit [("count", IntLit 2), ("ready", BoolLit False)]
  , Let "id" Nothing (Lambda "x" Nothing (Var "x"))
      (TupleLit [App (Var "id") (IntLit 1), App (Var "id") (BoolLit True)])
  , Case (ListLit [IntLit 1, IntLit 2])
      [(PList [], IntLit 0), (PCons (PVar "x") (PVar "xs"), Var "x")]
  , If (BoolLit True) (TupleLit [IntLit size, BoolLit True]) (TupleLit [IntLit 0, BoolLit False])
  ]

-- Generator for integers that should cause overflow
newtype OverflowInt = OverflowInt Integer deriving (Show, Eq)

instance Arbitrary OverflowInt where
  arbitrary = OverflowInt <$> oneof
    [ choose (kaiIntMax + 1, kaiIntMax + 1000000)
    , choose (kaiIntMin - 1000000, kaiIntMin - 1)
    ]

spec :: Spec
spec = describe "Property-Based Testing" $ do
  
  describe "Parser Properties" $ do
    it "parsing never crashes on valid integers" $ do
      property $ forAll genKaiInt $ \x ->
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
    
    it "generated well-typed expressions type-check and evaluate" $ do
      property $ forAll (sized typedPureExpr) $ \expr ->
        case typeCheck expr of
          Left err -> counterexample ("Unexpected type error for " ++ show expr ++ ": " ++ show err) False
          Right ty -> case evalPure expr of
            Left err -> counterexample ("Unexpected runtime error for " ++ show expr ++ ": " ++ show err) False
            Right value -> counterexample
              ("Value " ++ show value ++ " does not have inferred type " ++ show ty)
              (valueHasType ty value)
    
    it "type preservation covers composite values" $ do
      property $ forAll (resize 8 (sized typedPureExpr)) $ \expr ->
        case (typeCheck expr, evalPure expr) of
          (Right ty, Right value) -> valueHasType ty value
          _ -> False

  describe "Arithmetic Properties" $ do
    it "addition is commutative" $ do
      property $ forAll genSmallKaiInt $ \x -> forAll genSmallKaiInt $ \y ->
        let expr1 = Add (IntLit x) (IntLit y)
            expr2 = Add (IntLit y) (IntLit x)
        in case (evalPure expr1, evalPure expr2) of
             (Right v1, Right v2) -> v1 == v2
             _ -> True  -- Errors are fine (overflow etc)
    
    it "addition is associative" $ do
      property $ forAll genSmallKaiInt $ \x -> forAll genSmallKaiInt $ \y -> forAll genSmallKaiInt $ \z ->
        let expr1 = Add (Add (IntLit x) (IntLit y)) (IntLit z)
            expr2 = Add (IntLit x) (Add (IntLit y) (IntLit z))
        in case (evalPure expr1, evalPure expr2) of
             (Right v1, Right v2) -> v1 == v2
             _ -> True
    
    it "multiplication by zero gives zero" $ do
      property $ forAll genKaiInt $ \x ->
        let expr = Mul (IntLit x) (IntLit 0)
        in case evalPure expr of
             Right (VInt 0) -> True
             _ -> False

    it "negation distributes over addition" $ do
      property $ forAll genSmallKaiInt $ \x -> forAll genSmallKaiInt $ \y ->
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
      property $ forAll genKaiInt $ \x ->
        let identity = Lambda "x" Nothing (Var "x")
            application = App identity (IntLit x)
        in case evalPure application of
             Right (VInt result) -> result == x
             _ -> False
    
    it "constant function ignores second argument" $ do
      property $ forAll genKaiInt $ \x -> forAll genKaiInt $ \y ->
        let constFunc = Lambda "x" Nothing (Lambda "y" Nothing (Var "x"))
            application = App (App constFunc (IntLit x)) (IntLit y)
        in case evalPure application of
             Right (VInt result) -> result == x
             _ -> False
    
    it "function composition works correctly" $ do
      property $ forAll genSmallKaiInt $ \x ->
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
      property $ \condition -> forAll genKaiInt $ \x -> forAll genKaiInt $ \y ->
        let expr = If (BoolLit condition) (IntLit x) (IntLit y)
        in case evalPure expr of
             Right (VInt result) -> result == if condition then x else y
             _ -> False
    
    it "conditional with same branches returns that value" $ do
      property $ \condition -> forAll genKaiInt $ \x ->
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

valueHasType :: Type -> Value -> Bool
valueHasType TInt (VInt _) = True
valueHasType TBool (VBool _) = True
valueHasType TString (VStr _) = True
valueHasType TUnit VUnit = True
valueHasType (TFun _ _) VFun {} = True
valueHasType (TFun _ _) VConstructor {} = True
valueHasType (TMaybe _) VNothing = True
valueHasType (TMaybe ty) (VJust value) = valueHasType ty value
valueHasType (TEither leftTy _) (VLeft value) = valueHasType leftTy value
valueHasType (TEither _ rightTy) (VRight value) = valueHasType rightTy value
valueHasType (TList ty) (VList values) = all (valueHasType ty) values
valueHasType (TTuple tys) (VTuple values) =
  length tys == length values && and (zipWith valueHasType tys values)
valueHasType (TRecord tys) (VRecord values) =
  Map.keysSet tys == Map.keysSet values &&
    and [maybe False (valueHasType ty) (Map.lookup name values) | (name, ty) <- Map.toList tys]
valueHasType (TCustom _ _) VData {} = True
valueHasType (TVar _) _ = True
valueHasType _ _ = False

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
