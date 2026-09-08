module EvaluatorConsistencySpec where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Evaluator (Value(..), RuntimeError(..), eval, evalPure)
import Syntax
import Test.Hspec
import TestIO (captureOutput)
import TestSupport (parseExpression)

data OperationCase = OperationCase String ([Expr] -> Expr) [Expr] (Either RuntimeError Value)

spec :: Spec
spec = describe "Evaluator consistency" $ do
  forM_ operations $ \(OperationCase name build arguments expected) -> describe name $ do
    it "returns the specified value or exact error in both evaluators" $ do
      evalPure (build arguments) `shouldBe` expected
      eval (build arguments) `shouldReturn` expected
    checkEffects (OperationCase name build arguments expected)
  forM_ invalidIOOperations $ \fixture@(OperationCase name _ _ _) ->
    describe name $ checkEffects fixture
  forM_
    [ ("if true then (print \"yes\"; 42) else (print \"no\"; 1/0)", Right (VInt 42), "yes\n")
    , ("if false then (print \"no\"; 1/0) else (print \"yes\"; 42)", Right (VInt 42), "yes\n")
    , ("case Just 42 of Nothing -> (print \"no\"; 1/0) | Just n -> (print \"yes\"; n)", Right (VInt 42), "yes\n")
    , ("map (\\n -> print n; if n == 2 then 1/0 else n) [1,2,3]", Left DivByZero, "1\n2\n")
    , ("filter (\\n -> print n; if n == 2 then 1/0 == 0 else true) [1,2,3]", Left DivByZero, "1\n2\n")
    , ("foldl (\\a -> \\n -> print n; if n == 2 then 1/0 else a+n) 0 [1,2,3]", Left DivByZero, "1\n2\n")
    ] $ \(source, expected, output) -> it source $
      captureOutput (eval $ parseExpression source) `shouldReturn` (expected, output)
  where
    checkEffects (OperationCase _ build arguments expected) = do
      let required = case build arguments of
            And (BoolLit False) _ -> 1
            Or (BoolLit True) _ -> 1
            _ -> length arguments
      it "evaluates required arguments once in source order" $ do
        (result, output) <- captureOutput $ eval $ build $ zipWith trace [1..] arguments
        result `shouldBe` expected
        output `shouldBe` traceOutput required
      forM_ [0 .. length arguments - 1] $ \failed ->
        it ("stops at failed argument " ++ show (failed + 1)) $ do
          let modified = [trace index (if index == failed + 1 then Div (IntLit 1) (IntLit 0) else argument)
                         | (index, argument) <- zip [1..] arguments]
          (result, output) <- captureOutput $ eval $ build modified
          result `shouldBe` (if failed < required then Left DivByZero else expected)
          output `shouldBe` traceOutput (min required (failed + 1))
    trace index = Seq (Print (IntLit index))
    traceOutput count = unlines (map show [1..count])

operations :: [OperationCase]
operations =
  [ binary "addition" Add "20" "22" (ok $ VInt 42)
  , binary "subtraction" Sub "20" "22" (ok $ VInt (-2))
  , binary "multiplication" Mul "-6" "7" (ok $ VInt (-42))
  , binary "division floors negatives" Div "-3" "2" (ok $ VInt (-2))
  , binary "division by zero" Div "1" "0" (Left DivByZero)
  , binary "addition overflow" Add "2147483647" "1" (Left IntegerOverflow)
  , binary "division overflow" Div "-2147483648" "-1" (Left IntegerOverflow)
  , binary "invalid addition" Add "true" "1" (bad "Addition requires integer operands")
  , binary "string concatenation" Concat "\"a\"" "\"雪\"" (ok $ VStr "a雪")
  , binary "list concatenation" Concat "[1]" "[2]" (ok $ VList [VInt 1,VInt 2])
  , binary "invalid concatenation" Concat "1" "[]" (bad "Concatenation requires string or list operands")
  , binary "short-circuit and" And "false" "true" (ok $ VBool False)
  , binary "short-circuit or" Or "true" "false" (ok $ VBool True)
  , unary "not" Not "false" (ok $ VBool True)
  , unary "invalid not" Not "1" (bad "NOT requires a boolean operand")
  , binary "equality" Eq "(Just 1,[true])" "(Just 1,[true])" (ok $ VBool True)
  , binary "callable equality" Eq "[\\x -> x]" "[]" (bad "Equality is not defined for callable or recursive reference values")
  , binary "less than" Lt "-1" "0" (ok $ VBool True)
  , binary "greater than" Gt "1" "0" (ok $ VBool True)
  , unary "parseInt valid" ParseInt "\"-2147483648\"" (ok $ VJust $ VInt (-2147483648))
  , unary "parseInt invalid" ParseInt "\"2147483648\"" (ok VNothing)
  , unary "parseInt wrong type" ParseInt "1" (bad "parseInt requires string argument")
  , unary "toString" ToString "-42" (ok $ VStr "-42")
  , unary "toString wrong type" ToString "true" (bad "toString requires integer argument")
  , unary "show" Show "[1,2]" (ok $ VStr "[1, 2]")
  , unary "discard" Discard "42" (ok VUnit)
  , unary "Just" MJust "42" (ok $ VJust $ VInt 42)
  , unary "Left" ELeft "42" (ok $ VLeft $ VInt 42)
  , unary "Right" ERight "true" (ok $ VRight $ VBool True)
  , binary "split" Split "\",\"" "\"a,,b\"" (ok $ VList [VStr "a",VStr "",VStr "b"])
  , binary "split wrong delimiter" Split "1" "\"a\"" (bad "split: first argument must be a string")
  , binary "join" Join "\",\"" "[\"a\",\"b\"]" (ok $ VStr "a,b")
  , binary "join invalid element" Join "\",\"" "[1]" (bad "join: list must contain only strings")
  , unary "trim" Trim "\"  雪 \\n\"" (ok $ VStr "雪")
  , ternary "replace" Replace "\"a\"" "\"雪\"" "\"aba\"" (ok $ VStr "雪b雪")
  , ternary "replace invalid argument" Replace "\"a\"" "1" "\"aba\"" (bad "replace: second argument must be a string")
  , unary "strLength Unicode" StrLength "\"a雪\"" (ok $ VInt 2)
  , OperationCase "list literals" ListLit [IntLit 1,IntLit 2] (ok $ VList [VInt 1,VInt 2])
  , OperationCase "tuple literals" TupleLit [IntLit 1,BoolLit True] (ok $ VTuple [VInt 1,VBool True])
  , OperationCase "duplicate record fields" (RecordLit . zip ["a","a"]) [IntLit 1,BoolLit True] (ok $ VRecord $ Map.singleton "a" (VBool True))
  , binary "cons" Cons "1" "[2]" (ok $ VList [VInt 1,VInt 2])
  , unary "head" Head "[1,2]" (ok $ VInt 1)
  , unary "empty head" Head "[]" (Left $ EmptyListError "head")
  , unary "tail" Tail "[1,2]" (ok $ VList [VInt 2])
  , unary "empty tail" Tail "[]" (Left $ EmptyListError "tail")
  , unary "null" Null "[]" (ok $ VBool True)
  , unary "record access" (`RecordAccess` "a") "{a=1}" (ok $ VInt 1)
  , unary "missing field" (`RecordAccess` "b") "{a=1}" (Left $ RecordFieldNotFound "b")
  , unary "fst" Fst "(1,true)" (ok $ VInt 1)
  , unary "snd" Snd "(1,true)" (ok $ VBool True)
  , binary "map" Map "(\\n -> n+1)" "[1,2]" (ok $ VList [VInt 2,VInt 3])
  , binary "empty map still checks callback" Map "1" "[]" (bad "map: first argument must be callable")
  , binary "filter" Filter "(\\n -> n>1)" "[1,2]" (ok $ VList [VInt 2])
  , binary "filter invalid predicate result" Filter "(\\n -> n)" "[1]" (bad "filter: predicate must return a boolean")
  , ternary "foldl" Foldl "(\\a -> \\b -> a+b)" "10" "[1,2]" (ok $ VInt 13)
  , ternary "empty foldl" Foldl "(\\a -> \\b -> a+b)" "10" "[]" (ok $ VInt 10)
  , unary "length" Length "[]" (ok $ VInt 0)
  , unary "reverse" Reverse "[1,2]" (ok $ VList [VInt 2,VInt 1])
  , binary "negative take" Take "-1" "[1,2]" (ok $ VList [])
  , binary "negative drop" Drop "-1" "[1,2]" (ok $ VList [VInt 1,VInt 2])
  , binary "unequal zip" Zip "[1,2]" "[true]" (ok $ VList [VTuple [VInt 1,VBool True]])
  ]
  where
    ok = Right
    bad = Left . TypeError
    unary name constructor argument = OperationCase name (\[a] -> constructor a) [parseExpression argument]
    binary name constructor first second = OperationCase name (\[a,b] -> constructor a b) (map parseExpression [first,second])
    ternary name constructor first second third = OperationCase name (\[a,b,c] -> constructor a b c) (map parseExpression [first,second,third])

-- Invalid arguments must fail before any filesystem, environment, or process action.
invalidIOOperations :: [OperationCase]
invalidIOOperations =
  [ OperationCase (name ++ " rejects a non-string argument") (\[a] -> constructor a) [IntLit 1] (Left $ TypeError message)
  | (name, constructor, message) <-
      [ ("readFile", ReadFile, "readFile: path must be a string")
      , ("fileExists", FileExists, "fileExists: path must be a string")
      , ("listDirectory", ListDirectory, "listDirectory: path must be a string")
      , ("createDirectory", CreateDirectory, "createDirectory: path must be a string")
      , ("removeDirectory", RemoveDirectory, "removeDirectory: path must be a string")
      , ("setCurrentDirectory", SetCurrentDirectory, "setCurrentDirectory: path must be a string")
      , ("system", System, "system: command must be a string")
      , ("getEnv", GetEnv, "getEnv: name must be a string")
      ]
  ] ++
  [ OperationCase (name ++ " argument types " ++ show (first, second)) (\[a,b] -> constructor a b)
      [first, second] (Left $ TypeError $ name ++ ": " ++ message)
  | (name, constructor, firstLabel, secondLabel) <-
      [("writeFile", WriteFile, "path", "content"), ("appendFile", AppendFile, "path", "content"), ("setEnv", SetEnv, "name", "value")]
  , (first, second, message) <-
      [(IntLit 1, StrLit "ignored", firstLabel ++ " must be a string")
      ,(IntLit 1, IntLit 2, firstLabel ++ " must be a string")
      ,(StrLit "ignored", IntLit 2, secondLabel ++ " must be a string")]
  ]
