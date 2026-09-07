module TestSupportSpec (spec) where

import Control.Exception (ErrorCall, bracket, displayException, evaluate)
import Control.Monad (forM_)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding)
import System.IO (latin1)
import System.FilePath ((</>))
import ExampleSpec (withTempDir)
import qualified Evaluator as E
import Parser (parseExpr)
import Test.Hspec
import TestSupport
import qualified TypeChecker as T

spec :: Spec
spec = describe "Test fixture assertions" $ do
  it "does not turn malformed source into a runtime error" $
    evaluate (evaluateSource "let x =") `shouldThrow` fixtureFailure "Parse"
  it "does not turn malformed source into a type error" $
    evaluate (inferSource "1 +") `shouldThrow` fixtureFailure "Parse"
  it "does not turn a type failure into a runtime error" $
    evaluate (evaluateCheckedSource "1 + true") `shouldThrow` fixtureFailure "Type"
  it "preserves specific type errors" $
    inferSource "1 + true" `shouldBe` Left (T.UnificationError T.TBool T.TInt)
  it "preserves specific runtime errors" $
    evaluateCheckedSource "1 / 0" `shouldBe` Left E.DivByZero
  it "allows direct evaluator tests of statically invalid expressions" $
    evaluateSource "missing" `shouldBe` Left (E.UnboundVariable "missing")
  it "evaluates well-typed expressions" $
    evaluateCheckedSource "(\\x -> x + 1) 41" `shouldBe` Right (E.VInt 42)

  it "recognizes the specific overflowing literal diagnostic" $
    case parseExpr "2147483648" of
      Left err -> do
        isIntegerOverflowParseError 2147483648 err `shouldBe` True
        isIntegerOverflowParseError 2147483649 err `shouldBe` False
      Right expression -> expectationFailure $ "Accepted an overflowing literal: " ++ show expression
  it "does not mistake a syntax error for integer overflow" $
    case parseExpr "let x =" of
      Left err -> isIntegerOverflowParseError 2147483648 err `shouldBe` False
      Right expression -> expectationFailure $ "Accepted malformed source: " ++ show expression

  it "reads UTF-8 fixture text under a legacy locale" $ withTempDir $ \directory -> do
    let path = directory </> "fixture.kai"
    BS.writeFile path (BS.pack [0x22,0xc3,0xa9,0xe9,0x9b,0xaa,0x22])
    bracket getLocaleEncoding setLocaleEncoding $ \_ -> do
      setLocaleEncoding latin1
      readFixture path `shouldReturn` "\"é雪\""

  let a = T.TVar "a"
      b = T.TVar "b"
      renamed = T.TVar "fresh99"
  forM_ [T.TFun a a, T.TCustom "Box" [a], T.TMaybe a,
         T.TEither a (T.TList a), T.TTuple [a, T.TInt],
         T.TRecord (Map.fromList [("first", a), ("second", T.TBool)])] $ \ty ->
    it ("recognizes consistent renaming in " ++ show ty) $
      canonicalType ty `shouldBe` canonicalType (T.applySubst (Map.singleton "a" renamed) ty)
  forM_ [(T.TFun a a, T.TFun a b),
         (T.TFun T.TInt a, T.TFun a T.TInt),
         (T.TList a, T.TMaybe a),
         (T.TTuple [a], T.TTuple [a,b]),
         (T.TCustom "Box" [a], T.TCustom "Other" [a]),
         (T.TCustom "Box" [a], T.TCustom "Box" [a,b]),
         (T.TRecord (Map.singleton "x" a), T.TRecord (Map.singleton "y" a)),
         (T.TEither a a, T.TEither a b)] $ \(left, right) ->
    it ("distinguishes " ++ show left ++ " from " ++ show right) $
      canonicalType left `shouldNotBe` canonicalType right

fixtureFailure :: String -> ErrorCall -> Bool
fixtureFailure stage = isPrefixOf (stage ++ " failure in Kai test fixture:") . displayException
