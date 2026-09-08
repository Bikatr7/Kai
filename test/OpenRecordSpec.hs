module OpenRecordSpec where

import Control.Monad (forM_)
import qualified Data.Map as Map
import CLI (runCLI)
import Evaluator (Value(..), eval)
import ExampleSpec (withTempDir)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck
import TestIO (captureOutput, withStdin)
import TestSupport
import qualified TypeChecker as T

spec :: Spec
spec = describe "Open record inference" $ do
  forM_
    [ ("let get = \\r -> r.a in get {a=1,extra=true}", VInt 1)
    , ("let total = \\r -> r.a + r.b in total {a=1,b=2,extra=true}", VInt 3)
    , ("let total = \\r -> r.a + r.b in total {b=2,extra=true,a=1}", VInt 3)
    , ("let get = \\r -> r.a in (get {a=1,b=true},get {a=\"x\",c=2})", VTuple [VInt 1,VStr "x"])
    , ("let get = \\r -> r.outer.inner in get {outer={inner=7,more=true},extra=2}", VInt 7)
    , ("let total = \\r -> r.a + r.b in map total [{a=1,b=2,c=true},{a=3,b=4,c=false}]", VList [VInt 3,VInt 7])
    , ("let get = \\r : {a:Int | row} -> r.a in get {a=1,b=true}", VInt 1)
    , ("let get = \\r : {a:Int | row} -> r.a in get {a=1}", VInt 1)
    , ("let identity = \\r : {|row} -> r in identity {}", VRecord Map.empty)
    , ("let f : {a:Int | r} -> {b:Bool | r} -> Int = \\x -> \\y -> x.a in f {a=1,z=true} {b=true,z=false}", VInt 1)
    , ("letrec total = \\n -> \\r -> if n == 0 then r.a + r.b else total (n-1) r in total 5 {a=2,b=3,c=true}", VInt 5)
    , ("letrec get : Int -> {a:Int | r} -> Int = \\n -> \\v -> if n == 0 then v.a else get (n-1) v in get 3 {a=7,z=true}", VInt 7)
    ] $ \(source,expected) -> it source $ do
      evaluateCheckedSource source `shouldBe` Right expected
      eval (parseExpression source) `shouldReturn` Right expected

  it "retains all required fields and a fresh open tail" $
    shouldInfer "\\r -> r.a + r.b"
      (T.TFun (T.TOpenRecord (Map.fromList [("a",T.TInt),("b",T.TInt)]) (T.TRowVar "r")) T.TInt)

  it "rejects extra fields for a closed annotation" $
    inferSource "(\\r : {a:Int} -> r.a) {a=1,b=true}" `shouldBe`
      Left (T.UnificationError (T.TRecord (Map.singleton "a" T.TInt))
        (T.TRecord (Map.fromList [("a",T.TInt),("b",T.TBool)])))

  it "rejects conflicting field types" $
    case inferSource "(\\r -> r.a + r.b) {a=1,b=true}" of
      Left (T.UnificationError T.TInt T.TBool) -> pure ()
      other -> expectationFailure (show other)

  it "rejects a required field missing from a closed argument" $
    case inferSource "(\\r -> r.a + r.b) {a=1}" of
      Left (T.UnificationError T.TOpenRecord {} T.TRecord {}) -> pure ()
      other -> expectationFailure (show other)

  it "rejects a row used as a value type in the same annotation" $
    inferSource "\\x : ({a:Int | r}, r) -> x" `shouldBe` Left (T.ConflictingVariableKind "r")

  it "rejects duplicate labels introduced through a shared row" $
    inferSource "let f : {a:Int | r} -> {b:Bool | r} -> Int = \\x -> \\y -> x.a in f {a=1,b=true} {b=true}"
      `shouldBe` Left (T.DuplicateRecordField "b")

  it "rejects conflicting payload types in a shared tail" $
    case inferSource "let f : {a:Int | r} -> {b:Bool | r} -> Int = \\x -> \\y -> x.a in f {a=1,z=true} {b=true,z=1}" of
      Left (T.UnificationError T.TBool T.TInt) -> pure ()
      other -> expectationFailure (show other)

  it "rejects infinite records" $
    case inferSource "\\r -> if true then r.a else r" of
      Left (T.InfiniteType _ _) -> pure ()
      other -> expectationFailure (show other)

  forM_ ["{a=1,a=2}", "(\\x : {a:Int,a:Int | r} -> x) {a=1}"] $ \source ->
    it ("rejects duplicate labels in " ++ source) $
      inferSource source `shouldBe` Left (T.DuplicateRecordField "a")

  it "rejects row/value kind confusion in standalone unification" $
    T.unify (T.TVar "a") (T.TRowVar "r") `shouldBe`
      Left (T.KindMismatch (T.TVar "a") (T.TRowVar "r"))

  it "works for generated payloads independently of field order" $ property $
    forAll (choose (-10000,10000 :: Int)) $ \a ->
    forAll (choose (-10000,10000 :: Int)) $ \b ->
    forAll arbitrary $ \flag ->
      let source = "let f = \\r -> r.a + r.b in f {b=" ++ show b ++ ",extra=" ++
            (if flag then "true" else "false") ++ ",a=" ++ show a ++ "}"
      in evaluateCheckedSource source === Right (VInt (a+b))

  it "preserves open rows through module exports and local shadowing" $ withTempDir $ \dir -> do
    let modulePath = dir </> "Rows.kai"
        mainPath = dir </> "main.kai"
    writeFile modulePath "// expect: ()\nexport get, total\nlet get = \\r -> r.a\nlet total = \\r -> r.a + r.b\n"
    writeFile mainPath "// expect: ()\nimport Rows\nprint (get {a=7,x=true},total {a=2,b=3,z=\"extra\"})\n"
    captureOutput (runCLI [mainPath]) `shouldReturn` (ExitSuccess,"(7, 5)\n")

  it "keeps row polymorphism across separate REPL inputs" $ do
    (code,out) <- captureOutput $ withStdin
      "let get = \\r -> r.a\nget {a=1,b=true}\nget {a=\"text\",c=2}\n:quit\n" (runCLI [])
    code `shouldBe` ExitSuccess
    out `shouldContain` "kai> 1\nkai> text\nkai> "
    out `shouldNotContain` "Type error"

  it "checks a thousand nested field accesses" $ do
    let source = "let get = \\r -> r" ++ concat (replicate 1000 ".a") ++ " in get " ++
          concat (replicate 1000 "{a=") ++ "7" ++ replicate 1000 '}'
    evaluateCheckedSource source `shouldBe` Right (VInt 7)
