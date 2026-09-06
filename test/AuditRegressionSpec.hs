module AuditRegressionSpec where

import Test.Hspec
import Control.Exception (IOException, throwIO, try)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Timeout (timeout)
import CLI (runCLI)
import Evaluator (Value(..), RuntimeError(..), evalPure, eval, evalProgram)
import Parser (parseExpr, parseProgram)
import qualified TypeChecker as T
import TestIO (captureOutput, withStdin)
import CLISpec (withTempKaiFile)
import ExampleSpec (withTempDir)

spec :: Spec
spec = describe "Audit regressions" $ do
  describe "Parser and first-class builtins" $ do
    forM_ [("1+2", VInt 3), ("7 -2", VInt 5), ("7- 2", VInt 5),
           ("7 +2", VInt 9), ("7--2", VInt 9),
           ("(\\x : Int -> x+1) 41", VInt 42),
           ("(\\f : (Int -> Int) -> f 41) (\\x -> x+1)", VInt 42),
           ("let p : (Int, Bool) = (1,true) in p", VTuple [VInt 1,VBool True]),
           ("let p : () = () in p", VUnit),
           ("let f = length in (f [], f [1,2])", VTuple [VInt 0,VInt 2]),
           ("let f = map (\\x -> x+1) in f [1,2]", VList [VInt 2,VInt 3]),
           ("let f = foldl (\\a -> \\b -> a+b) in f 0 [1,2,3]", VInt 6),
           ("let f = foldl (\\a -> \\b -> a+b) 1 in f [2,3]", VInt 6),
           ("let f = replace in f \"a\" \"b\" \"aaa\"", VStr "bbb"),
           ("let f = Just in f 42", VJust (VInt 42)),
           ("map (length) [[1],[2,3]]", VList [VInt 1,VInt 2]),
           ("let f = map (map (\\x -> x+1)) in f [[1],[2]]", VList [VList [VInt 2],VList [VInt 3]]),
           ("let f = \\x -> x in f (-1)", VInt (-1))] $ \(source, expected) ->
      it source $ withExpression source $ \expr -> do
        T.typeCheck expr `shouldSatisfy` isRight
        evalPure expr `shouldBe` Right expected
        eval expr `shouldReturn` Right expected
    forM_ ["42 /*", "42 /* missing close", "// first\n/*", "/* outer /* inner */"] $ \source ->
      it ("rejects unterminated comment " ++ show source) $
        parseProgram source `shouldSatisfy` isLeft
    it "retains closed comments and comment-like string contents" $
      withExpression "/*outer /*inner*/ end*/ \"/*\"" $ \expr -> evalPure expr `shouldBe` Right (VStr "/*")
    it "evaluates supplied partial arguments immediately" $
      withExpression "let f = take (1/0) in 42" $ \expr -> do
        evalPure expr `shouldBe` Left DivByZero
        eval expr `shouldReturn` Left DivByZero

  describe "Pattern and declaration safety" $ do
    forM_ ["case {a=1,b=true} of {b=x,a=x} -> x",
           "case (1,true) of (x,x) -> x",
           "case [1,2] of [x,x] -> x",
           "case [1,2] of x :: x -> x",
           "case (Just 1,Right true) of (Just x,Right x) -> x"] $ \source ->
      it ("rejects repeated binding: " ++ source) $ withExpression source $ \expr ->
        T.typeCheck expr `shouldBe` Left (T.DuplicatePatternBinding "x")
    it "rejects duplicate record pattern fields" $ withExpression "case {x=true} of {x=a,x=b} -> a" $ \expr ->
      T.typeCheck expr `shouldBe` Left (T.DuplicateRecordField "x")
    it "rejects duplicate fields in type annotations" $
      withExpression "let x : {a:Int,a:Bool} = {a=true} in x" $ \expr ->
        T.typeCheck expr `shouldBe` Left (T.DuplicateRecordField "a")
    it "allows repeated wildcards and separate branch bindings" $ withExpression "case (1,true) of (_,_) -> 42" $ \expr -> do
      T.typeCheck expr `shouldBe` Right T.TInt
      eval expr `shouldReturn` Right (VInt 42)
    forM_ ["data T = Mk Int\nlet old = Mk 1\ndata T = Mk Bool\nold",
           "data Bad = Bad Missing", "data Bad = Bad a", "data Bad a a = Bad a",
           "data Bad = A | A", "data A = Mk\ndata B = Mk",
           "data Box a = Box a\nlet x : Box = Box 1\nx",
           "data Box a = Box a\nlet x : Box Int Bool = Box 1\nx",
           "let x : Missing = 1\nx"] $ \source ->
      it ("validates declarations: " ++ source) $ withProgram source $ \program ->
        T.typeCheckProgram program `shouldSatisfy` isDeclarationError
    it "rejects duplicate names within a constructor pattern" $
      withProgram "data Pair a b = Pair a b\ncase Pair 1 true of Pair x x -> x" $ \program ->
        T.typeCheckProgram program `shouldBe` Left (T.DuplicatePatternBinding "x")

  describe "Recursion and execution modes" $ do
    forM_ ["letrec x=x in x", "letrec x : Int=x in x", "letrec x=[x] in x"] $ \source ->
      it ("does not expose an initializer: " ++ source) $ withExpression source $ \expr -> do
        timeout 1000000 (eval expr) `shouldReturn` Just (Left (UninitializedRecursion "x"))
        evalPure expr `shouldBe` Left (UninitializedRecursion "x")
    it "preserves the original initializer error" $ withExpression "letrec x=1/0 in 42" $ \expr -> do
      evalPure expr `shouldBe` Left DivByZero
      eval expr `shouldReturn` Left DivByZero
    it "rejects wildcard recursive definitions at top level" $
      withProgram "letrec _ = 42\n42" $ \program ->
        T.typeCheckProgram program `shouldBe` Left (T.InvalidWildcard "Wildcard variables (_) cannot be used in recursive definitions")
    it "rejects duplicate names in a recursive block" $
      withProgram "letrec f = \\x -> x+1\nletrec f = \\x -> not x\nf true" $ \program ->
        T.typeCheckProgram program `shouldBe` Left (T.GeneralTypeError "Duplicate name in recursive binding block")
    it "rejects unavailable pure input and reads actual IO input" $ withExpression "input" $ \expr -> do
      evalPure expr `shouldBe` Left (TypeError "input not available in pure evaluation")
      withStdin "not World\n" (eval expr) `shouldReturn` Right (VStr "not World")
    forM_ ["direct", "import", "repl"] $ \mode -> do
      it ("infers independent recursive helpers through " ++ mode) $
        runMode mode "letrec number = \\x -> x+1\nletrec invert = \\x -> not x\n" "print (number 41, invert false)" $ \code out -> do
          code `shouldBe` ExitSuccess
          out `shouldContain` "(42, True)"
          out `shouldNotContain` "error:"
      it ("generalizes independent recursive helpers through " ++ mode) $
        runMode mode "letrec id = \\x -> x\nletrec use = \\x -> (id x, id true)\n" "print (use 42)" $ \code out -> do
          code `shouldBe` ExitSuccess
          out `shouldContain` "(42, True)"
          out `shouldNotContain` "error:"
      it ("stops after an initializer failure through " ++ mode) $
        runMode mode "letrec first = (print (1/0); \\x -> x)\nletrec second = (print \"LEAK\"; \\x -> x)\n" "42" $ \_ out -> do
          out `shouldContain` "DivByZero"
          out `shouldNotContain` "LEAK"
      it ("preserves initializer effect order through " ++ mode) $
        runMode mode "letrec zebra = (print \"FIRST\"; \\x -> x)\nletrec alpha = (print \"SECOND\"; \\x -> x)\n" "42" $ \code out -> do
          code `shouldBe` ExitSuccess
          out `shouldContain` "FIRST\nSECOND\n"
      it ("initializes constants in source order through " ++ mode) $
        runMode mode "letrec first = 41\nletrec second = first+1\n" "print second" $ \code out -> do
          code `shouldBe` ExitSuccess
          out `shouldContain` "42"
          out `shouldNotContain` "error:"
    it "runs module expressions in order" $
      runMode "import" "print \"FIRST\"\nprint \"SECOND\"\n" "42" $ \code out -> do
        code `shouldBe` ExitSuccess
        out `shouldBe` "FIRST\nSECOND\n"
    it "checks an entire REPL load before running effects" $
      runMode "repl" "let first = print \"LEAK\"\n" "1+true" $ \_ out -> do
        out `shouldContain` "Type error:"
        out `shouldNotContain` "LEAK"
    it "rejects conflicting imported declarations including hidden constructors" $ withTempDir $ \dir -> do
      writeFile (dir </> "A.kai") "data T = Mk Int\nlet old = Mk 1\nexport old\n"
      writeFile (dir </> "B.kai") "data T = Mk Bool\nexport Mk\n"
      let mainFile = dir </> "main.kai"
      writeFile mainFile "import A\nimport B\nprint (case old of Mk b -> if b then 1 else 0)\n"
      (code, out) <- captureOutput $ runCLI [mainFile]
      code `shouldBe` ExitFailure 1
      out `shouldContain` "Conflicting imported type: T"

  describe "Source IO and verification" $ do
    forM_ ["direct", "import", "repl"] $ \mode ->
      it ("contains source decoding errors through " ++ mode) $ withTempDir $ \dir -> do
        let bad = dir </> "Bad.kai"
        BS.writeFile bad (BS.pack [0xff,0xfe,0xff])
        (code, out) <- captureOutput $ case mode of
          "direct" -> runCLI [bad]
          "import" -> do
            let mainFile = dir </> "main.kai"
            writeFile mainFile "import Bad\n42"
            runCLI [mainFile]
          _ -> withStdin (":load " ++ bad ++ "\n42\n:quit\n") $ runCLI ["repl"]
        out `shouldContain` "IO error"
        if mode == "repl" then do
          code `shouldBe` ExitSuccess
          out `shouldContain` "kai> 42"
        else code `shouldBe` ExitFailure 1
    forM_ [("// expect: 42\n0",False), ("// expect: 42\n42",True),
           ("// expect-type: TInt\n42",False),
           ("// expect: 42\n// expect-type: TBool\n42",False),
           ("// expect: error DivByZero\n1/0",True),
           ("// expect: error DivByZero\nhead []",False),
           ("// expect: 42\n// expect: 0\n42",False),
           ("// expect: input\n42",False),
           ("// expect: if true then 42 else false\n42",False),
           ("// expect: 42   \n42",True)] $ \(source, success) ->
      it ("checks actual script expectations " ++ show source) $ withTempKaiFile source $ \path -> do
        (code, _) <- captureOutput $ withStdin "" $ runCLI ["--check",path]
        code `shouldBe` if success then ExitSuccess else ExitFailure 1
    it "captures large output and restores stdout after exceptions" $ do
      (_, text) <- captureOutput $ putStr (replicate 100000 'x')
      text `shouldBe` replicate 100000 'x'
      result <- try (captureOutput (throwIO (userError "test"))) :: IO (Either IOException ((),String))
      result `shouldSatisfy` isLeft
      captureOutput (putStr "restored") `shouldReturn` ((),"restored")
    it "restores stdin after EOF and exceptions" $ do
      result <- try (withStdin "" getLine) :: IO (Either IOException String)
      result `shouldSatisfy` isLeft
      withStdin "next\n" getLine `shouldReturn` "next"

withExpression source action = case parseExpr source of
  Left err -> expectationFailure (show err)
  Right expression -> action expression
withProgram source action = case parseProgram source of
  Left err -> expectationFailure (show err)
  Right program -> action program
isRight (Right _) = True
isRight _ = False
isLeft (Left _) = True
isLeft _ = False
isDeclarationError (Left (T.InvalidDataDeclaration _)) = True
isDeclarationError _ = False

runMode mode definitions expression assertion = withTempDir $ \dir -> do
  let moduleFile = dir </> "Helpers.kai"
      directFile = dir </> "direct.kai"
      importFile = dir </> "main.kai"
  writeFile moduleFile definitions
  writeFile directFile (definitions ++ expression ++ "\n")
  writeFile importFile ("import Helpers\n" ++ expression ++ "\n")
  (code,out) <- captureOutput $ case mode of
    "direct" -> runCLI [directFile]
    "import" -> runCLI [importFile]
    _ -> withStdin (":load " ++ directFile ++ "\n:quit\n") $ runCLI ["repl"]
  assertion code out
