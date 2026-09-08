module OrdinaryBuiltinsSpec where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Evaluator (Value(..), RuntimeError(..), eval)
import Parser (parseExpr)
import Syntax
import Test.Hspec
import TestSupport
import TestIO (captureOutput, withStdin)
import qualified TypeChecker as T

spec :: Spec
spec = describe "Ordinary builtin application" $ do
  it "parses builtins and user functions with the same application tree" $ do
    parseExpr "map length xs" `shouldBe` Right (App (App (Var "map") (Var "length")) (Var "xs"))
    parseExpr "map size xs" `shouldBe` Right (App (App (Var "map") (Var "size")) (Var "xs"))

  forM_
    [ ("map length [[1], [2,3]]", VList [VInt 1,VInt 2])
    , ("map (length) [[1], [2,3]]", VList [VInt 1,VInt 2])
    , ("let size = length in map size [[1], [2,3]]", VList [VInt 1,VInt 2])
    , ("let length = \\x -> x + 10 in map length [1,2]", VList [VInt 11,VInt 12])
    , ("(\\length -> length 4) (\\n -> n + 2)", VInt 6)
    , ("head([1,2])", VInt 1)
    , ("let f = take 2 in (f [1,2,3], f [true,false,true])", VTuple [VList [VInt 1,VInt 2],VList [VBool True,VBool False]])
    , ("let functions = [length, length] in map (\\f -> f [1,2]) functions", VList [VInt 2,VInt 2])
    , ("map Just [1,2]", VList [VJust (VInt 1),VJust (VInt 2)])
    , ("let Just = \\n -> n + 1 in Just 4", VInt 5)
    , ("let r = {fn = length} in r.fn [1,2]", VInt 2)
    , ("let f = \\x -> x + 1 in let r = {n = 4} in f r.n", VInt 5)
    , ("let f = \\n -> {value = n + 1} in (f 4).value", VInt 5)
    , ("let f = \\n -> n * 2 in f (-3)", VInt (-6))
    ] $ \(source,expected) -> it ("evaluates " ++ source) $ do
      evaluateCheckedSource source `shouldBe` Right expected
      eval (parseExpression source) `shouldReturn` Right expected

  it "evaluates and captures a supplied effect exactly once" $ do
    let source = "let f = take (do { print \"capture\"; 1 }) in do { print (f [1,2]); print (f [3,4]) }"
    inferSource source `shouldBe` Right T.TUnit
    captureOutput (eval (parseExpression source)) `shouldReturn` (Right VUnit,"capture\n[1]\n[3]\n")

  it "preserves left-to-right supplied argument effects" $ do
    let source = "map (do { print \"function\"; \\n -> n + 1 }) (do { print \"argument\"; [1,2] })"
    inferSource source `shouldBe` Right (T.TList T.TInt)
    captureOutput (eval (parseExpression source)) `shouldReturn`
      (Right (VList [VInt 2,VInt 3]),"function\nargument\n")

  it "stops before later argument effects after a supplied argument fails" $ do
    let source = "take (1 / 0) (do { print \"must not run\"; [1] })"
    inferSource source `shouldBe` Right (T.TList T.TInt)
    captureOutput (eval (parseExpression source)) `shouldReturn` (Left DivByZero,"")

  it "allows a builtin result to be applied when it is a function" $
    evaluateCheckedSource "head [\\n -> n + 1] 4" `shouldBe` Right (VInt 5)

  it "rejects extra arguments to a nonfunction result statically" $
    case inferSource "length [1] 2" of
      Left (T.UnificationError T.TInt (T.TFun T.TInt _)) -> pure ()
      result -> expectationFailure (show result)

  it "keeps input's legacy zero-argument evaluation behavior" $ do
    withStdin "a\nb\n" (eval (parseExpression "(input, input)")) `shouldReturn`
      Right (VTuple [VStr "a", VStr "b"])

  it "does not let local names capture standard function internals" $
    evaluateCheckedSource "let x = 999 in let y = 999 in map (\\n -> n + 1) [1]" `shouldBe` Right (VList [VInt 2])

  it "distinguishes accessing a result from accessing an argument" $ do
    parseExpr "f x.a" `shouldBe` Right (App (Var "f") (RecordAccess (Var "x") "a"))
    parseExpr "(f x).a" `shouldBe` Right (RecordAccess (App (Var "f") (Var "x")) "a")
    parseExpr "r.fn x" `shouldBe` Right (App (RecordAccess (Var "r") "fn") (Var "x"))

  it "retains an exact record value after calling its stored builtin" $
    evaluateCheckedSource "let r = {fn = length} in {n = r.fn [1,2]}" `shouldBe`
      Right (VRecord (Map.singleton "n" (VInt 2)))
