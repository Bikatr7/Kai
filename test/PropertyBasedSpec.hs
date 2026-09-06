{-# LANGUAGE ScopedTypeVariables #-}
module PropertyBasedSpec where

import Test.Hspec
import Test.QuickCheck
import Syntax
import Parser
import TypeChecker
import Evaluator (Value(..), evalPure, evalProgram)
import qualified Evaluator as E
import TestSupport (isIntegerOverflowParseError)
import Control.Monad (liftM2, liftM3)
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
          Left err -> isIntegerOverflowParseError n err
          Right _ -> False
    
    it "pretty-print is stable after parse" $ do
      property $ \(ValidExpr expr) ->
        let s = prettyExpr expr in
        case parseExpr s of
          Right e1 -> e1 == expr && prettyExpr e1 == s
          Left _ -> False
    
    it "preserves the generated AST across whitespace and comments" $ do
      property $ \(ValidExpr expr) ->
        let source = prettyExpr expr
        in conjoin [parseExpr source === Right expr,
                    parseExpr (" /* leading /* nested */ comment */ \n" ++ source ++ " // trailing\n") === Right expr]

  describe "Type System Properties" $ do
    it "preserves inferred types and errors through source parsing" $ do
      property $ \(ValidExpr expr) ->
        case parseExpr (prettyExpr expr) of
          Left err -> counterexample (show err) False
          Right parsed -> conjoin [parsed === expr, typeCheck parsed === typeCheck expr]
    
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
             (Right v1, Right v2) -> v1 == VInt (x+y) && v2 == VInt (x+y)
             _ -> False  -- Generated operands cannot overflow signed 32-bit addition.
    
    it "addition is associative" $ do
      property $ forAll genSmallKaiInt $ \x -> forAll genSmallKaiInt $ \y -> forAll genSmallKaiInt $ \z ->
        let expr1 = Add (Add (IntLit x) (IntLit y)) (IntLit z)
            expr2 = Add (IntLit x) (Add (IntLit y) (IntLit z))
        in case (evalPure expr1, evalPure expr2) of
             (Right v1, Right v2) -> v1 == VInt (x+y+z) && v2 == VInt (x+y+z)
             _ -> False  -- The three bounded operands also remain within range.
    
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
        in conjoin [evalPure lhs === Right (VInt (negate (x+y))),
                    evalPure rhs === Right (VInt (negate (x+y)))]

  describe "String Properties" $ do
    it "concatenation is associative" $ do
      property $ \(a :: String) (b :: String) (c :: String) ->
        let e1 = Concat (Concat (StrLit a) (StrLit b)) (StrLit c)
            e2 = Concat (StrLit a) (Concat (StrLit b) (StrLit c))
        in conjoin [evalPure e1 === Right (VStr (a++b++c)),
                    evalPure e2 === Right (VStr (a++b++c))]
  describe "Boolean Logic Properties" $ do
    it "boolean logic follows De Morgan's laws" $ do
      property $ \p q ->
        let notPAndNotQ = And (Not (BoolLit p)) (Not (BoolLit q))
            notPOrQ = Not (Or (BoolLit p) (BoolLit q))
        in case (evalPure notPAndNotQ, evalPure notPOrQ) of
             (Right v1, Right v2) -> v1 == VBool (not (p || q)) && v2 == VBool (not (p || q))
             _ -> False
    
    it "AND is commutative" $ do
      property $ \p q ->
        let expr1 = And (BoolLit p) (BoolLit q)
            expr2 = And (BoolLit q) (BoolLit p)
        in case (evalPure expr1, evalPure expr2) of
             (Right v1, Right v2) -> v1 == VBool (p && q) && v2 == VBool (p && q)
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

  describe "Declaration and pattern preservation properties" $ do
    it "rejects repeated record binders for either field order" $ property $
      forAll genKaiInt $ \n flag reverseFields ->
        let fields = [("number", PVar "x"), ("flag", PVar "x")]
            patternFields = if reverseFields then reverse fields else fields
            expression = Case (RecordLit [("number",IntLit n),("flag",BoolLit flag)])
                           [(PRecord patternFields, Var "x")]
        in typeCheck expression == Left (DuplicatePatternBinding "x")
    it "preserves generated custom constructor payloads through evaluation" $ property $
      forAll genKaiInt $ \n flag -> ioProperty $ do
        let declarations = [TLData "Envelope" ["a"] [DataConstructor "Wrap" [STVar "a", STBool]]]
            expression = App (App (Var "Wrap") (IntLit n)) (BoolLit flag)
            program = Program (declarations ++ [TLExpr expression])
            constructors = Map.singleton "Wrap" (TCustom "Envelope" [TVar "a"], [TVar "a", TBool])
        actual <- evalProgram program
        return $ case (typeCheckProgram program, actual) of
          (Right ty, Right value) -> valueHasTypeWith constructors ty value && value == VData "Wrap" [VInt n,VBool flag]
          _ -> False
    it "rejects generated incompatible redeclarations before evaluation" $ property $
      forAll genKaiInt $ \n ->
        let program = Program [TLData "T" [] [DataConstructor "Mk" [STInt]],
                               TLDef "old" Nothing (App (Var "Mk") (IntLit n)),
                               TLData "T" [] [DataConstructor "Mk" [STBool]], TLExpr (Var "old")]
        in case typeCheckProgram program of
          Left (InvalidDataDeclaration _) -> True
          _ -> False
    it "validates custom constructor identity, arity, and every payload" $ do
      let schemas = Map.singleton "Wrap" (TCustom "Envelope" [TVar "a"], [TVar "a", TBool])
          expected = TCustom "Envelope" [TInt]
          check = valueHasTypeWith schemas expected
      check (VData "Wrap" [VInt 1,VBool True]) `shouldBe` True
      map check [VData "Missing" [VInt 1,VBool True], VData "Wrap" [],
                 VData "Wrap" [VBool True,VBool True], VData "Wrap" [VInt 1,VInt 2],
                 VData "Wrap" [VInt 1,VBool True,VInt 2]] `shouldBe` replicate 5 False
      valueHasTypeWith schemas (TCustom "Other" [TInt]) (VData "Wrap" [VInt 1,VBool True]) `shouldBe` False
      valueHasType expected (VData "Wrap" [VInt 1,VBool True]) `shouldBe` False

  describe "Error Handling Properties" $ do
    it "reports specific errors for generated ill-typed expressions" $ property $
      forAll genKaiInt $ \n flag -> forAll (elements
        [(Add (IntLit n) (BoolLit flag), UnificationError TBool TInt),
         (If (IntLit n) (BoolLit flag) (BoolLit (not flag)), UnificationError TInt TBool),
         (If (BoolLit flag) (IntLit n) (BoolLit flag), UnificationError TInt TBool),
         (Var ("missing" ++ show (abs (toInteger n))), UnboundVariable ("missing" ++ show (abs (toInteger n))))]) $
          \(expr, expected) -> typeCheck expr === Left expected

    it "preserves specific runtime errors in pure and IO evaluation" $ property $
      forAll genKaiInt $ \n -> forAll (elements
        [(Div (IntLit n) (IntLit 0), E.DivByZero),
         (Add (IntLit (fromInteger kaiIntMax)) (IntLit 1), E.IntegerOverflow),
         (Sub (IntLit (fromInteger kaiIntMin)) (IntLit 1), E.IntegerOverflow),
         (Var ("missing" ++ show (toInteger n)), E.UnboundVariable ("missing" ++ show (toInteger n))),
         (RecordAccess (RecordLit [("present",IntLit n)]) "missing", E.RecordFieldNotFound "missing")]) $
          \(expr, expected) -> ioProperty $ do
            actual <- E.eval expr
            pure $ conjoin [evalPure expr === Left expected, actual === Left expected]

  describe "Type preservation oracle" $ do
    it "checks scalar types and signed 32-bit integer bounds" $ do
      valueHasType TInt (VInt 42) `shouldBe` True
      valueHasType TInt (VBool True) `shouldBe` False
      valueHasType TInt (VInt (fromInteger (kaiIntMax+1))) `shouldBe` False
      valueHasType TInt (VInt (fromInteger (kaiIntMin-1))) `shouldBe` False
    it "checks every tuple element and its arity" $ do
      let check = valueHasType (TTuple [TInt,TBool])
      check (VTuple [VInt 1,VBool True]) `shouldBe` True
      map (check . VTuple) [[VInt 1], [VInt 1,VInt 2], [VInt 1,VBool True,VUnit]]
        `shouldBe` replicate 3 False
    it "checks record keys and field types" $ do
      let check = valueHasType (TRecord (Map.singleton "x" TInt)) . VRecord . Map.fromList
      check [("x",VInt 1)] `shouldBe` True
      map check [[], [("y",VInt 1)], [("x",VBool True)], [("x",VInt 1),("y",VInt 2)]]
        `shouldBe` replicate 4 False
    it "checks contained values but accepts absent polymorphic payloads" $ do
      valueHasType (TList TInt) (VList [VInt 1,VBool True]) `shouldBe` False
      valueHasType (TMaybe TInt) (VJust (VBool True)) `shouldBe` False
      valueHasType (TEither TInt TBool) (VLeft (VBool True)) `shouldBe` False
      valueHasType (TEither TInt TBool) (VRight (VInt 1)) `shouldBe` False
      valueHasType (TList (TVar "a")) (VList []) `shouldBe` True
      valueHasType (TMaybe (TVar "a")) VNothing `shouldBe` True
    it "does not claim to prove unresolved or callable value types" $ do
      valueHasType (TVar "a") (VInt 42) `shouldBe` False
      valueHasType (TFun TInt TInt) (VFun "x" (BoolLit True) Map.empty) `shouldBe` False
      valueHasType (TFun TInt TInt) (VConstructor "Unknown" 1 []) `shouldBe` False

-- Constructor schemas include the declared result and payload types. An
-- unregistered constructor cannot establish preservation for a custom type.
valueHasType :: Type -> Value -> Bool
valueHasType = valueHasTypeWith Map.empty

valueHasTypeWith :: Map.Map String (Type, [Type]) -> Type -> Value -> Bool
valueHasTypeWith constructors = matches
  where
    matches TInt (VInt n) = toInteger n >= kaiIntMin && toInteger n <= kaiIntMax
    matches TBool (VBool _) = True
    matches TString (VStr _) = True
    matches TUnit VUnit = True
    matches (TMaybe _) VNothing = True
    matches (TMaybe ty) (VJust value) = matches ty value
    matches (TEither leftTy _) (VLeft value) = matches leftTy value
    matches (TEither _ rightTy) (VRight value) = matches rightTy value
    matches (TList ty) (VList values) = all (matches ty) values
    matches (TTuple tys) (VTuple values) =
      length tys == length values && and (zipWith matches tys values)
    matches (TRecord tys) (VRecord values) =
      Map.keysSet tys == Map.keysSet values &&
        and [maybe False (matches ty) (Map.lookup name values) | (name, ty) <- Map.toList tys]
    matches ty@(TCustom _ _) (VData name values) = case Map.lookup name constructors of
      Nothing -> False
      Just (declared, payload) -> case unify declared ty of
        Left _ -> False
        Right subst -> length payload == length values &&
          and (zipWith matches (map (applySubst subst) payload) values)
    matches _ _ = False

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
prettyExpr (App f x) = "(" ++ prettyExpr f ++ " (" ++ prettyExpr x ++ "))"
