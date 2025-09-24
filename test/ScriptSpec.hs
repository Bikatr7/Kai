module ScriptSpec where

import Test.Hspec
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (sort)
import Control.Monad (forM, forM_)

import Parser
import Evaluator
import TypeChecker
import Syntax
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Script files in tests/" $ do
    files <- runIO $ kaiFilesIn "tests"
    forM_ files $ \fp -> do
      it fp $ do
        content <- readFile fp
        -- For multi-statement files, use parseStatements directly
        -- Only use parseFileExpr for files that can't be parsed as multiple statements
        case parseStatements content of
          Right stmts | length stmts > 1 -> do
            let expr = last stmts  -- Last statement is the main expression to test
            testExpr expr content
          _ -> case parseFileExpr content of
            Left perr -> expectationFailure ("Parse error: " ++ show perr)
            Right expr -> testExpr expr content

  describe "Script files in test/" $ do
    files <- runIO $ kaiFilesIn "test"
    forM_ files $ \fp -> do
      it fp $ do
        content <- readFile fp
        -- For multi-statement files, use parseStatements directly
        -- Only use parseFileExpr for files that can't be parsed as multiple statements
        case parseStatements content of
          Right stmts | length stmts > 1 -> do
            let expr = last stmts  -- Last statement is the main expression to test
            testExpr expr content
          _ -> case parseFileExpr content of
            Left perr -> expectationFailure ("Parse error: " ++ show perr)
            Right expr -> testExpr expr content

testExpr :: Expr -> String -> IO ()
testExpr expr content = do
  case parseExpect content of
    Just (ExpectValue expStr) -> do
      case parseExpr expStr of
        Left perr -> expectationFailure ("Bad expect expr: " ++ show perr)
        Right eexp -> do
          if requiresIO expr
            then do
              result <- eval expr
              case (result, evalPure eexp) of
                (Right v, Right vexp) -> do
                  if v == vexp
                    then putStrLn $ "✅ PASS: Expected " ++ show vexp ++ ", got " ++ show v
                    else expectationFailure $ "❌ FAIL: Expected " ++ show vexp ++ ", got " ++ show v
                (Left rerr, _) -> expectationFailure ("❌ FAIL: Runtime error: " ++ show rerr)
                _ -> expectationFailure "❌ FAIL: Unexpected eval failure in expected expression"
            else do
              case (evalPure expr, evalPure eexp) of
                (Right v, Right vexp) -> do
                  if v == vexp
                    then putStrLn $ "✅ PASS: Expected " ++ show vexp ++ ", got " ++ show v
                    else expectationFailure $ "❌ FAIL: Expected " ++ show vexp ++ ", got " ++ show v
                (Left rerr, _) -> expectationFailure ("❌ FAIL: Runtime error: " ++ show rerr)
                _ -> expectationFailure "❌ FAIL: Unexpected eval failure in expected expression"
    Just (ExpectType tyStr) -> do
      let expectedTy = case tyStr of
            "TInt" -> Right TInt
            "TBool" -> Right TBool
            "TString" -> Right TString
            "TUnit" -> Right TUnit
            _ -> Left ("Unknown type in expect-type: " ++ tyStr)
      case expectedTy of
        Left msg -> expectationFailure msg
        Right ety -> case typeCheck expr of
          Right ty -> do
            if ty == ety
              then putStrLn $ "✅ PASS: Expected type " ++ show ety ++ ", got " ++ show ty
              else expectationFailure $ "❌ FAIL: Expected type " ++ show ety ++ ", got " ++ show ty
          Left err -> expectationFailure ("❌ FAIL: Type error: " ++ show err)
    Just ExpectError -> do
      if requiresIO expr
        then do
          result <- eval expr
          case result of
            Left err -> putStrLn $ "✅ PASS: Expected error, got: " ++ show err
            Right v -> expectationFailure ("❌ FAIL: Expected error, got: " ++ show v)
        else do
          case evalPure expr of
            Left err -> putStrLn $ "✅ PASS: Expected error, got: " ++ show err
            Right v -> expectationFailure ("❌ FAIL: Expected error, got: " ++ show v)
    Nothing -> do
      if requiresIO expr
        then do
          -- For IO expressions, use eval with real input
          result <- eval expr
          case result of
            Left rerr -> expectationFailure ("❌ FAIL: Runtime error: " ++ show rerr)
            Right v -> putStrLn $ "✅ PASS: Expression with input evaluated to: " ++ show v
        else do
          case evalPure expr of
            Left rerr -> expectationFailure ("❌ FAIL: Runtime error: " ++ show rerr)
            Right v -> putStrLn $ "✅ PASS: Expression evaluated to: " ++ show v

requiresIO :: Expr -> Bool
requiresIO Input = True
requiresIO (Add e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Sub e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Mul e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Div e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Concat e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (And e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Or e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Not e) = requiresIO e
requiresIO (Eq e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Lt e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Gt e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (If cond thenE elseE) = requiresIO cond || requiresIO thenE || requiresIO elseE
requiresIO (Lambda _ _ body) = requiresIO body
requiresIO (App f arg) = requiresIO f || requiresIO arg
requiresIO (Let _ _ val body) = requiresIO val || requiresIO body
requiresIO (LetRec _ _ val body) = requiresIO val || requiresIO body
requiresIO (Print e) = requiresIO e
requiresIO _ = False

-- Utilities
kaiFilesIn :: FilePath -> IO [FilePath]
kaiFilesIn dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    pure $ sort [ dir </> e | e <- entries, takeExtension e == ".kai" ]

data ExpectDirective
  = ExpectValue String
  | ExpectType String
  | ExpectError

parseExpect :: String -> Maybe ExpectDirective
parseExpect content =
  let ls = lines content
      isExpect s = "// expect:" `prefixOf` s
      isType s = "// expect-type:" `prefixOf` s
      isErr s = "// expect-error" `prefixOf` s
      prefixOf p s = take (length p) s == p
  in listToMaybe $ map toDir $ filter (\s -> isExpect s || isType s || isErr s) ls
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    toDir s
      | "// expect-type:" `isPref` s = ExpectType (trim (drop (length "// expect-type:") s))
      | "// expect:" `isPref` s = ExpectValue (trim (drop (length "// expect:") s))
      | otherwise = ExpectError
    isPref p s = take (length p) s == p