module InputSpec where

import Test.Hspec
import System.IO
import Control.Exception (bracket, evaluate)
import qualified Data.Map as Map
import Syntax
import Parser
import Evaluator
import TypeChecker

import TestIO (captureOutput, withStdin)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim str = go str []
  where
    go [] acc = [reverse acc]
    go xs acc
      | delim `isPrefixOf` xs = reverse acc : go (drop (length delim) xs) []
      | otherwise = go (tail xs) (head xs : acc)
    
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys



spec :: Spec
spec = describe "Input Support" $ do
  it "reads from stdin and uses the value" $ do
    let kaiScript = "print (\"Hello, \" ++ input)"
    let expectedOutput = "Hello, World\n"
    let providedInput = "World\n"

    case parseExpr kaiScript of
      Left _ -> expectationFailure "Parser error"
      Right expr -> do
        case typeCheck expr of
          Left err -> expectationFailure $ "Type error: " ++ show err
          Right _ -> do
            (_, output) <- captureOutput $ withStdin providedInput $ eval expr
            output `shouldBe` expectedOutput
