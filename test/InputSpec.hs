module InputSpec where

import Test.Hspec
import System.IO
import Control.Exception (bracket, evaluate)
import qualified Data.Map as Map
import Syntax
import Parser
import Evaluator
import TypeChecker

-- Note: This test is POSIX-specific because it uses functions from System.Posix.IO.
import System.Posix.IO

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

captureOutput :: IO a -> IO (a, String)
captureOutput action = do
    (readFd, writeFd) <- createPipe
    oldStdout <- dup stdOutput
    dupTo writeFd stdOutput
    closeFd writeFd
    result <- action
    dupTo oldStdout stdOutput
    closeFd oldStdout
    readHandle <- fdToHandle readFd
    hGetContents readHandle >>= \out -> evaluate (length out) >> return (result, out)

withStdin :: String -> IO a -> IO a
withStdin input action = do
    (readFd, writeFd) <- createPipe
    writeHandle <- fdToHandle writeFd
    hPutStrLn writeHandle input
    hClose writeHandle
    oldStdin <- dup stdInput
    dupTo readFd stdInput
    closeFd readFd
    result <- action
    dupTo oldStdin stdInput
    closeFd oldStdin
    return result

spec :: Spec
spec = describe "Input Support" $ do
  it "reads from stdin and uses the value" $ do
    let kaiScript = "print (\"Hello, \" ++ input)"
    let expectedOutput = "Hello, World\n"
    let providedInput = "World"

    case parseExpr kaiScript of
      Left _ -> expectationFailure "Parser error"
      Right expr -> do
        case typeCheck expr of
          Left err -> expectationFailure $ "Type error: " ++ show err
          Right _ -> do
            (_, output) <- captureOutput $ withStdin providedInput $ eval expr
            -- Extract the actual program output by filtering out test framework progress dots
            let cleanOutput = filter (\c -> c /= '.' && c /= '\r') output
            cleanOutput `shouldBe` expectedOutput
