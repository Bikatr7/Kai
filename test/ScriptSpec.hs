module ScriptSpec where

import Test.Hspec
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (sort)
import Control.Monad (filterM, forM, forM_)

import Parser
import Evaluator
import TypeChecker
import Syntax
import Data.Maybe (isNothing, listToMaybe)
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Script expectation coverage" $ do
    files <- runIO $ allKaiFilesIn "."
    it "requires an expectation directive in every repository .kai file" $ do
      missing <- filterM (fmap (isNothing . parseExpect) . readFile) files
      missing `shouldBe` []

  describe "Script files in tests/" $ do
    files <- runIO $ kaiFilesIn "tests"
    forM_ files $ \fp -> do
      it fp $ do
        content <- readFile fp
        requireExpectation fp content
        -- Try parsing as program first (new top-level definitions)
        case parseProgram content of
          Right program -> testProgram program content
          Left _ -> case parseStatements content of
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
        requireExpectation fp content
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
      requireExprTypeChecks expr
      case parseExpr expStr of
        Left perr -> expectationFailure ("Bad expect expr: " ++ show perr)
        Right eexp -> do
          requireExprTypeChecks eexp
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
      requireExprTypeChecks expr
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
    Nothing -> expectationFailure "Missing required // expect directive"

requiresIO :: Expr -> Bool
requiresIO Input = True
requiresIO Args = True
requiresIO (ReadFile _) = True
requiresIO (WriteFile _ _) = True
requiresIO (AppendFile _ _) = True
requiresIO (FileExists _) = True
requiresIO (ListDirectory _) = True
requiresIO (CreateDirectory _) = True
requiresIO (RemoveDirectory _) = True
requiresIO GetCurrentDirectory = True
requiresIO (SetCurrentDirectory _) = True
requiresIO (System _) = True
requiresIO (GetEnv _) = True
requiresIO (SetEnv _ _) = True
requiresIO (Exit _) = True
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
requiresIO (Seq e1 e2) = requiresIO e1 || requiresIO e2
requiresIO (Case scrutinee branches) = requiresIO scrutinee || any (requiresIO . snd) branches
requiresIO (RecordLit fields) = any (requiresIO . snd) fields
requiresIO (RecordAccess e _) = requiresIO e
requiresIO (ListLit es) = any requiresIO es
requiresIO (TupleLit es) = any requiresIO es
requiresIO _ = False

testProgram :: Program -> String -> IO ()
testProgram program content = do
  case parseExpect content of
    Just (ExpectValue expStr) -> do
      requireProgramTypeChecks program
      case parseExpr expStr of
        Left perr -> expectationFailure ("Bad expect expr: " ++ show perr)
        Right eexp -> do
          requireExprTypeChecks eexp
          result <- evalProgram program
          case (result, evalPure eexp) of
            (Right v, Right vexp) -> v `shouldBe` vexp
            (Left rerr, _) -> expectationFailure ("Runtime error: " ++ show rerr)
            _ -> expectationFailure "Unexpected eval failure in expected expression"
    Just (ExpectType tyStr) -> do
      let expectedTy = case tyStr of
            "TInt" -> Right TInt
            "TBool" -> Right TBool
            "TString" -> Right TString
            "TUnit" -> Right TUnit
            _ -> Left ("Unknown type in expect-type: " ++ tyStr)
      case expectedTy of
        Left msg -> expectationFailure msg
        Right ety -> case typeCheckProgram program of
          Right ty -> ty `shouldBe` ety
          Left err -> expectationFailure ("Type error: " ++ show err)
    Just ExpectError -> do
      requireProgramTypeChecks program
      result <- evalProgram program
      case result of
        Left _ -> return ()  -- Expected error, test passes
        Right v -> expectationFailure ("Expected error, got: " ++ show v)
    Nothing -> expectationFailure "Missing required // expect directive"

requireExpectation :: FilePath -> String -> IO ()
requireExpectation fp content =
  case parseExpect content of
    Just _ -> return ()
    Nothing -> expectationFailure $ fp ++ " must contain // expect:, // expect-type:, or // expect-error"

requireExprTypeChecks :: Expr -> IO ()
requireExprTypeChecks expr =
  case typeCheck expr of
    Left err -> expectationFailure $ "Type error before script evaluation: " ++ show err
    Right _ -> return ()

requireProgramTypeChecks :: Program -> IO ()
requireProgramTypeChecks program =
  case typeCheckProgram program of
    Left err -> expectationFailure $ "Type error before program evaluation: " ++ show err
    Right _ -> return ()

-- Utilities
kaiFilesIn :: FilePath -> IO [FilePath]
kaiFilesIn dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    pure $ sort [ dir </> e | e <- entries, takeExtension e == ".kai" ]

allKaiFilesIn :: FilePath -> IO [FilePath]
allKaiFilesIn dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    nested <- forM entries $ \entry -> do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then if entry `elem` ignoredDirectories then pure [] else allKaiFilesIn path
        else pure [path | takeExtension path == ".kai"]
    pure $ sort (concat nested)
  where
    ignoredDirectories = [".git", ".stack-work", "dist-site", "dist-newstyle"]

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
