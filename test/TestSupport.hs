module TestSupport
  ( parseExpression
  , evaluateSource
  , evaluateCheckedSource
  , inferSource
  , canonicalType
  , shouldInfer
  , readFixture
  , isIntegerOverflowParseError
  ) where

import Control.Monad.State.Strict (State, evalState, get, put)
import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle(..), ParseError(..), ErrorFancy(..))
import qualified Evaluator as E
import Parser (parseExpr)
import Syntax (Expr)
import qualified TypeChecker as T
import Test.Hspec (Expectation, shouldBe)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, utf8, withFile)

-- A malformed fixture is a test failure, never a language type/runtime error.
parseExpression :: String -> Expr
parseExpression source = case parseExpr source of
  Left err -> error $ "Parse failure in Kai test fixture: " ++ show err
  Right expression -> expression

evaluateSource :: String -> Either E.RuntimeError E.Value
evaluateSource = E.evalPure . parseExpression

evaluateCheckedSource :: String -> Either E.RuntimeError E.Value
evaluateCheckedSource source =
  let expression = parseExpression source
  in case T.typeCheck expression of
    Left err -> error $ "Type failure in Kai test fixture: " ++ show err
    Right _ -> E.evalPure expression

inferSource :: String -> Either T.TypeError T.Type
inferSource = T.typeCheck . parseExpression

-- Compare polymorphic structure without depending on fresh-variable numbering.
-- Renaming is bijective: a -> a must still differ from a -> b.
canonicalType :: T.Type -> T.Type
canonicalType ty = evalState (walk ty) Map.empty
  where
    walk :: T.Type -> State (Map.Map String Int) T.Type
    walk (T.TVar name) = do
      names <- get
      index <- case Map.lookup name names of
        Just existing -> pure existing
        Nothing -> do
          let fresh = Map.size names
          put $ Map.insert name fresh names
          pure fresh
      pure $ T.TVar (show index)
    walk (T.TQualified ps value) = do
      value' <- walk value
      ps' <- mapM walkPredicate ps
      pure $ T.TQualified ps' value'
    walk (T.TFun argument result) = T.TFun <$> walk argument <*> walk result
    walk (T.TCustom name arguments) = T.TCustom name <$> mapM walk arguments
    walk (T.TMaybe element) = T.TMaybe <$> walk element
    walk (T.TEither left right) = T.TEither <$> walk left <*> walk right
    walk (T.TList element) = T.TList <$> walk element
    walk (T.TRecord fields) = T.TRecord <$> mapM walk fields
    walk (T.TOpenRecord fields row) = T.TOpenRecord <$> mapM walk fields <*> walk row
    walk (T.TRowVar name) = do
      renamed <- walk (T.TVar name)
      case renamed of
        T.TVar variable -> pure (T.TRowVar variable)
        _ -> error "Row variable renaming must remain a variable"
    walk (T.TTuple elements) = T.TTuple <$> mapM walk elements
    walk primitive = pure primitive
    walkPredicate (T.PredicateAt _ predicate) = walkPredicate predicate
    walkPredicate (T.Equality value) = T.Equality <$> walk value
    walkPredicate (T.Appendable value) = T.Appendable <$> walk value

shouldInfer :: String -> T.Type -> Expectation
shouldInfer source expected =
  fmap canonicalType (inferSource source) `shouldBe` Right (canonicalType expected)

readFixture :: FilePath -> IO String
readFixture path = withFile path ReadMode $ \handle -> do
  hSetEncoding handle utf8
  contents <- hGetContents handle
  _ <- evaluate (length contents)
  pure contents

isIntegerOverflowParseError :: Integer -> ParseErrorBundle String Void -> Bool
isIntegerOverflowParseError value = any matches . bundleErrors
  where
    expected = ErrorFail $ "Integer literal " ++ show value ++ " is outside 32-bit signed Int bounds"
    matches (FancyError _ errors) = Set.member expected errors
    matches _ = False
