module InputSpec where

import Test.Hspec
import System.IO
import Control.Exception (bracket, evaluate)
import Control.Monad (forM_)
import qualified Data.Map as Map
import Syntax
import Parser
import Evaluator
import TypeChecker

import TestIO (captureOutput, withStdin)

spec :: Spec
spec = describe "Input Support" $ do
  it "reads from stdin and uses the value" $ do
    let kaiScript = "print (\"Hello, \" ++ input)"
    let expectedOutput = "Hello, World\n"
    let providedInput = "World\n"

    case parseExpr kaiScript of
      Left err -> expectationFailure $ "Parser error: " ++ show err
      Right expr -> do
        case typeCheck expr of
          Left err -> expectationFailure $ "Type error: " ++ show err
          Right ty -> do
            ty `shouldBe` TUnit
            (result, output) <- captureOutput $ withStdin providedInput $ eval expr
            result `shouldBe` Right VUnit
            output `shouldBe` expectedOutput

  forM_ [("empty line", "\n", ""),
         ("spaces", "  Ada  \n", "  Ada  "),
         ("quotes", "a\"b\n", "a\"b"),
         ("no final newline", "Ada", "Ada"),
         ("one line at a time", "Ada\nWorld\n", "Ada"),
         ("long line", replicate 100000 'x' ++ "\n", replicate 100000 'x')] $ \(label, supplied, name) ->
    it ("prints the exact greeting for " ++ label) $ do
      case parseExpr "print (\"Hello, \" ++ input ++ \"!\")" of
        Left err -> expectationFailure (show err)
        Right expr -> do
          (result, output) <- captureOutput $ withStdin supplied $ eval expr
          result `shouldBe` Right VUnit
          output `shouldBe` "Hello, " ++ name ++ "!\n"
