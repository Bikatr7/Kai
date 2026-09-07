module SelfReviewSpec where

import Control.Monad (forM_)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck
import CLI (runCLI)
import Evaluator (Value(..), RuntimeError(..), evalPure, eval, evalProgram)
import Parser (parseExpr, parseProgram)
import qualified TypeChecker as T
import TestIO (captureOutput, withStdin)
import ExampleSpec (withTempDir)
import AuditRegressionSpec (loadedOutput)

spec :: Spec
spec = describe "Self-review regression tests" $ do
  describe "Repeated prefix operators" $ do
    forM_ [("not not true", VBool True), ("not not not false", VBool True),
           ("- - - 1", VInt (-1)), ("--1", VInt 1),
           ("- -2147483647", VInt 2147483647),
           ("not not ((\\x -> x) true)", VBool True),
           ("- - (\\x -> x+1) 4", VInt 5),
           (unwords (replicate 1000 "not") ++ " true", VBool True),
           (unwords (replicate 1000 "-") ++ " 1", VInt 1)] $ \(source,value) ->
      it (take 80 source) $ expression source $ \expr -> do
        let expectedType = case value of
              VBool _ -> T.TBool
              _ -> T.TInt
        T.typeCheck expr `shouldBe` Right expectedType
        evalPure expr `shouldBe` Right value
        eval expr `shouldReturn` Right value
    forM_ ["not not 1", "- - true", "not - false"] $ \source ->
      it ("rejects invalid prefix operands: " ++ source) $
        expression source $ \expr -> T.typeCheck expr `shouldSatisfy` isUnificationError
    it "checks overflow at each negation" $
      expression "- -2147483648" $ \expr -> do
        T.typeCheck expr `shouldBe` Right T.TInt
        evalPure expr `shouldBe` Left IntegerOverflow
        eval expr `shouldReturn` Left IntegerOverflow

  describe "Annotation variable isolation" $ do
    forM_ ["a", "t0", "t1", "t2", "t10", "t9223372036854775807"] $ \name ->
      forM_ ["(\\x -> \\y : " ++ name ++ " -> (x,y)) 1 true",
             "(\\x : " ++ name ++ " -> \\y -> (x,y)) 1 true",
             "let x : " ++ name ++ " = 1 in let y : " ++ name ++ " = true in (x,y)",
             "letrec identity : " ++ name ++ " -> " ++ name ++ " = \\x -> x in (identity 1, identity true)"] $ \source ->
        it source $ expression source $ \expr -> do
          T.typeCheck expr `shouldBe` Right (T.TTuple [T.TInt,T.TBool])
          evalPure expr `shouldBe` Right (VTuple [VInt 1,VBool True])
          eval expr `shouldReturn` Right (VTuple [VInt 1,VBool True])
    it "preserves semantics when annotation variables are renamed" $ property $
      forAll (choose (0,10000 :: Int)) $ \index ->
        let source = "(\\x -> \\y : t" ++ show index ++ " -> (x,y)) 1 true"
        in case parseExpr source of
          Right expr -> T.typeCheck expr == Right (T.TTuple [T.TInt,T.TBool]) &&
                        evalPure expr == Right (VTuple [VInt 1,VBool True])
          Left _ -> False
    it "keeps repeated variables within the same annotation constrained" $
      expression "let r : {a:t0,b:t0} = {a=1,b=true} in r" $ \expr ->
        T.typeCheck expr `shouldSatisfy` isUnificationError
    it "checks annotated function inputs and outputs against the same variable" $
      expression "(\\f : (t0 -> t0) -> f 1) (\\n -> true)" $ \expr ->
        T.typeCheck expr `shouldSatisfy` isUnificationError

  describe "Constructor declaration identity and visibility" $ do
    forM_ ["direct", "repl"] $ \mode -> do
      it ("accepts parameter renaming across modules through " ++ mode) $
        modules mode "data Box a = Box a\nlet first = Box 1\nexport first\n"
                     "data Box b = Box b\nlet second = Box true\nexport second\n"
                     "import A\nimport B\nprint (first,second)" $ \path code out -> do
          code `shouldBe` ExitSuccess
          out `shouldBe` loadedOutput mode path "(Box(1), Box(True))\n" "()"
      it ("rejects reordered payload types through " ++ mode) $
        modules mode "data Pair a b = Pair a b\nlet first = Pair 1 true\nexport first\n"
                     "data Pair x y = Pair y x\nlet second = Pair 1 true\nexport second\n"
                     "import A\nimport B\nprint first" $ \_ code out -> do
          code `shouldBe` if mode == "repl" then ExitSuccess else ExitFailure 1
          out `shouldContain` "Conflicting imported type: Pair"
      it ("rejects reuse of a hidden constructor name through " ++ mode) $
        modules mode privateModule ""
                     "import A\ndata U = Mk Bool\nprint (Mk true)" $ \_ code out -> do
          code `shouldBe` if mode == "repl" then ExitSuccess else ExitFailure 1
          out `shouldContain` "Duplicate constructor: Mk"
      forM_ ["let Mk = \\x -> x", "let Mk = \\x -> old", "letrec Mk = \\x -> old"] $ \binding ->
        it ("keeps a hidden constructor private after " ++ binding ++ " through " ++ mode) $
          modules mode privateModule ""
                  ("import A\n" ++ binding ++ "\nprint (case old of Mk x -> x)") $ \_ code out -> do
            code `shouldBe` if mode == "repl" then ExitSuccess else ExitFailure 1
            out `shouldContain` "Type error: UnboundVariable \"Mk\""
      it ("keeps a constructor private across re-exports through " ++ mode) $
        modules mode "data T = Mk Int\nlet old = Mk 1\nexport Mk,old\n"
                     "import A\nexport old\n"
                     "import B\nlet Mk = \\x -> old\nprint (case old of Mk x -> x)" $ \_ code out -> do
          code `shouldBe` if mode == "repl" then ExitSuccess else ExitFailure 1
          out `shouldContain` "Type error: UnboundVariable \"Mk\""
      it ("allows public constructor patterns after a second private import through " ++ mode) $
        modules mode "data T = Mk Int\nlet old = Mk 1\nexport Mk,old\n"
                     "import A\nexport old\n"
                     "import A\nimport B\nprint (case old of Mk x -> x)" $ \path code out -> do
          code `shouldBe` ExitSuccess
          out `shouldBe` loadedOutput mode path "1\n" "()"
    it "distinguishes constructor owners even when their payloads match" $
      modules "direct" "data T = Mk Int\nlet old = Mk 1\nexport old\n"
                       "data U = Mk Int\nlet new = Mk 2\nexport new\n"
                       "import A\nimport B\nprint old" $ \_ code out -> do
        code `shouldBe` ExitFailure 1
        out `shouldContain` "Conflicting imported constructor: Mk"

  describe "Constructor patterns require saturation" $ do
    forM_ [("data Box a = Box a\ncase Box of Box -> 42", "Box",1,0),
           ("data Pair a b = Pair a b\ncase Pair 1 of Pair x -> x", "Pair",2,1),
           ("data T = T\ncase T of T x -> x", "T",0,1),
           ("data Box = Box Int\ncase Box 1 of Box x y -> x", "Box",1,2)] $ \(source,name,expected,actual) ->
      it source $ program source $ \ast ->
        T.typeCheckProgram ast `shouldBe` Left (T.ConstructorPatternArity name expected actual)
    it "counts a function-valued payload as one field" $
      program "data F = F (Int -> Int)\ncase F (\\x -> x+1) of F f -> f 41" $ \ast -> do
        T.typeCheckProgram ast `shouldBe` Right T.TInt
        evalProgram ast `shouldReturn` Right (VInt 42)

privateModule :: String
privateModule = "data T = Mk Int\nlet old = Mk 1\nexport old\n"

expression source action = case parseExpr source of
  Left err -> expectationFailure (show err)
  Right expr -> action expr
program source action = case parseProgram source of
  Left err -> expectationFailure (show err)
  Right ast -> action ast
isUnificationError (Left (T.UnificationError _ _)) = True
isUnificationError _ = False

modules mode first second mainSource assertion = withTempDir $ \dir -> do
  writeFile (dir </> "A.kai") first
  writeFile (dir </> "B.kai") second
  let mainFile = dir </> "main.kai"
  writeFile mainFile mainSource
  (code,out) <- captureOutput $ case mode of
    "repl" -> withStdin (":load " ++ mainFile ++ "\n:quit\n") $ runCLI ["repl"]
    _ -> runCLI [mainFile]
  assertion mainFile code out
