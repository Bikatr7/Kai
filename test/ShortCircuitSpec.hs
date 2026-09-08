module ShortCircuitSpec where

import Control.Monad (forM_)
import Evaluator (Value(..), RuntimeError(..), eval)
import Test.Hspec
import TestSupport
import TestIO (captureOutput, withStdin)
import ExampleSpec (withTempDir)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified TypeChecker as T

spec :: Spec
spec = describe "Short-circuit boolean guards" $ do
  forM_
    [ ("false and (1 / 0 == 0)", Right (VBool False))
    , ("true or (1 / 0 == 0)", Right (VBool True))
    , ("true and (1 / 0 == 0)", Left DivByZero)
    , ("false or (1 / 0 == 0)", Left DivByZero)
    , ("(1 / 0 == 0) and false", Left DivByZero)
    , ("(1 / 0 == 0) or true", Left DivByZero)
    , ("false and (head ([] : [Int]) == 1)", Right (VBool False))
    , ("true or false and (1 / 0 == 0)", Right (VBool True))
    , ("(false and (1 / 0 == 0)) or true", Right (VBool True))
    ] $ \(source,expected) -> it source $ do
      evaluateCheckedSource source `shouldBe` expected
      eval (parseExpression source) `shouldReturn` expected

  forM_ [("false and",False),("true or",True)] $ \(guard,result) -> do
    it (guard ++ " skips output and preserves unread stdin") $ do
      let source = "let result = " ++ guard ++ " (do { print \"unexpected\"; input == \"a\" }) in (result, input)"
      inferSource source `shouldBe` Right (T.TTuple [T.TBool,T.TString])
      withStdin "a\nb\n" (captureOutput (eval (parseExpression source))) `shouldReturn`
        (Right (VTuple [VBool result,VStr "a"]),"")
    it (guard ++ " skips a file write") $ withTempDir $ \dir -> do
      let file = dir </> "skipped.txt"
          source = guard ++ " (do { writeFile " ++ show file ++ " \"unexpected\"; true })"
      inferSource source `shouldBe` Right T.TBool
      eval (parseExpression source) `shouldReturn` Right (VBool result)
      doesFileExist file `shouldReturn` False
    it (guard ++ " skips process exit") $ do
      let source = guard ++ " (exit 7)"
      evaluateCheckedSource source `shouldBe` Right (VBool result)
      eval (parseExpression source) `shouldReturn` Right (VBool result)
    it (guard ++ " skips a shell process that would create a file") $ withTempDir $ \dir -> do
      let file = dir </> "process marker.txt"
          command = "echo unexpected > \"" ++ file ++ "\""
          process = "system " ++ show command
          source = guard ++ " (" ++ process ++ " == 0)"
      inferSource source `shouldBe` Right T.TBool
      eval (parseExpression source) `shouldReturn` Right (VBool result)
      doesFileExist file `shouldReturn` False
      -- Prove the same command really creates the marker when evaluated.
      eval (parseExpression process) `shouldReturn` Right (VInt 0)
      doesFileExist file `shouldReturn` True

  forM_ ["true and", "false or"] $ \guard ->
    it (guard ++ " evaluates required input and output exactly once") $ do
      let source = "let result = " ++ guard ++ " (do { print \"required\"; input == \"a\" }) in (result, input)"
      inferSource source `shouldBe` Right (T.TTuple [T.TBool,T.TString])
      withStdin "a\nb\n" (captureOutput (eval (parseExpression source))) `shouldReturn`
        (Right (VTuple [VBool True,VStr "b"]),"required\n")

  it "keeps explicitly sequenced effects when migrating an eager guard" $ do
    let source = "let right = do { print \"intentional\"; true } in false and right"
    inferSource source `shouldBe` Right T.TBool
    captureOutput (eval (parseExpression source)) `shouldReturn` (Right (VBool False),"intentional\n")

  forM_ ["false and 1", "true or 1"] $ \source ->
    it ("still type-checks the skipped operand in " ++ source) $
      inferSource source `shouldBe` Left (T.UnificationError T.TInt T.TBool)

  it "does not execute the right operand after a left failure" $ do
    let source = "(1 / 0 == 0) and (do { print \"unexpected\"; true })"
    inferSource source `shouldBe` Right T.TBool
    captureOutput (eval (parseExpression source)) `shouldReturn` (Left DivByZero,"")
