module SourceLocationSpec where

import Control.Monad (forM_)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Diagnostics
import qualified Evaluator as E
import Parser
import Syntax
import Test.Hspec
import Test.QuickCheck
import TestSupport (canonicalType)
import qualified TypeChecker as T
import Text.Megaparsec (errorBundlePretty)
import DocumentationSpec (requireExecutable)
import ExampleSpec (withTempDir)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

spec :: Spec
spec = describe "Source locations and diagnostics" $ do
  forM_
    [ "1 + 2 * 3"
    , "let f = \\x -> x + 1 in f 7"
    , "let append = \\x -> \\y -> x ++ y in (append [1] [2],append \"a\" \"b\")"
    , "letrec f = \\n -> if n == 0 then 0 else f (n-1) in f 5"
    , "(\\r -> r.a + r.b) {a=1,b=2,c=true}"
    , "case Just true of Just value -> value | Nothing -> false"
    , "do { discard 1; discard 2; 3 }"
    , "false and (1 / 0 == 0)"
    , "attempt (\\unit -> 1 / 0)"
    ] $ \source -> it ("preserves located expression behavior: " ++ source) $ do
      let plain = parseOrFail (parseExpr source)
          located = locatedExpression source
      fmap canonicalType (T.typeCheck located) `shouldBe` fmap canonicalType (T.typeCheck plain)
      E.evalPure located `shouldBe` E.evalPure plain
      E.eval located `shouldReturn` E.evalPure plain

  it "keeps physical line numbers through shebangs, comments and blank lines" $ do
    let source = unlines ["#!/usr/bin/env kai", "// heading", "", "/* nested /* note */ comment */",
                          "let x = 1", "", "x / 0"]
    result <- E.evalProgram (locatedProgram source)
    case result of
      Left (E.RuntimeAt location E.DivByZero) ->
        position location `shouldBe` ("source.kai",7,1,"x / 0")
      other -> expectationFailure (show other)

  forM_
    [ ("let x = 1 in x", E.VInt 1)
    , ("let x : Int = 2 in x", E.VInt 2)
    , ("let _ = true in 3", E.VInt 3)
    , ("let x = 4\n// continued binding\n\nin x", E.VInt 4)
    , ("let x = 5 in // same-line comment\n x", E.VInt 5)
    , ("letrec f = \\n -> if n == 0 then 6 else f (n-1) in f 2", E.VInt 6)
    , ("letrec f : Int -> Int = \\x -> x\n\nin f 7", E.VInt 7)
    , ("let x = 8\nlet y = 1 in x + y", E.VInt 9)
    , ("let x = 10\nlet inner = let y = 1 in x + y\ninner", E.VInt 11)
    , ("let inputValue = 12\ninputValue", E.VInt 12)
    ] $ \(source,expected) -> it ("distinguishes complete definitions from let-in expressions: " ++ source) $ do
      let plain = parseOrFail (parseProgram source)
          located = locatedProgram source
      fmap canonicalType (T.typeCheckProgram located) `shouldBe` fmap canonicalType (T.typeCheckProgram plain)
      E.evalProgram plain `shouldReturn` Right expected
      E.evalProgram located `shouldReturn` Right expected

  it "preserves columns after comments on the same line" $ do
    let expression = locatedExpression "/* note */ 1 / 0"
    case E.evalPure expression of
      Left (E.RuntimeAt location E.DivByZero) -> position location `shouldBe` ("source.kai",1,12,"/* note */ 1 / 0")
      other -> expectationFailure (show other)

  it "tracks CRLF source lines and does not print carriage returns" $ do
    result <- E.evalProgram (locatedProgram "// heading\r\n\r\n1 / 0\r\n")
    case result of
      Left failure@(E.RuntimeAt location E.DivByZero) -> do
        position location `shouldBe` ("source.kai",3,1,"1 / 0")
        renderRuntimeError failure `shouldBe` "source.kai:3:1: Runtime error: Division by zero.\n3 | 1 / 0\n  | ^"
      other -> expectationFailure (show other)

  it "retains blank lines before top-level continuation pipes" $ do
    let source = unlines ["case Nothing of", "  Just x -> x", "", "// comment before branch", "  | Nothing -> 1 / 0"]
    result <- E.evalProgram (locatedProgram source)
    case result of
      Left (E.RuntimeAt location E.DivByZero) -> position location `shouldBe`
        ("source.kai",5,16,"  | Nothing -> 1 / 0")
      other -> expectationFailure (show other)

  it "retains source and call context for a function defined earlier" $ do
    let source = unlines ["let divide = \\x -> 10 / x", "", "divide 0"]
    result <- E.evalProgram (locatedProgram source)
    case result of
      Left failure@(E.RuntimeContext _ call (E.RuntimeAt definition E.DivByZero)) -> do
        position definition `shouldBe` ("source.kai",1,20,"let divide = \\x -> 10 / x")
        position call `shouldBe` ("source.kai",3,1,"divide 0")
        renderRuntimeError failure `shouldContain` "while evaluating call to divide at source.kai:3:1"
      other -> expectationFailure (show other)

  it "locates delayed equality constraints at their use site" $ do
    let source = "let same = \\x -> x == x\nsame (\\x -> x)\n"
    result <- T.inferProgramWithEnvIO noImports "." Map.empty (locatedProgram source)
    case result of
      Left failure@(T.TypeAt location (T.UnsatisfiedConstraint (T.Equality T.TFun {}))) -> do
        position location `shouldBe` ("source.kai",2,1,"same (\\x -> x)")
        renderTypeError failure `shouldContain` "Equality is not supported"
        renderTypeError failure `shouldNotContain` "TVar"
      other -> expectationFailure (show other)

  it "locates unsupported concatenation at the operator expression" $
    case T.typeCheck (locatedExpression "\n  1 ++ 2") of
      Left (T.TypeAt location (T.UnsatisfiedConstraint (T.Appendable T.TInt))) ->
        position location `shouldBe` ("source.kai",2,3,"  1 ++ 2")
      other -> expectationFailure (show other)

  it "locates type failures inside nested arguments" $
    case T.typeCheck (locatedExpression "print (1 + true)") of
      Left failure@(T.TypeAt location _) -> do
        position location `shouldBe` ("source.kai",1,8,"print (1 + true)")
        T.stripTypeLocation failure `shouldBe` T.UnificationError T.TBool T.TInt
        renderTypeError failure `shouldContain` "Cannot match Bool with Int."
      other -> expectationFailure (show other)

  it "reports unreachable patterns at their branch location" $
    case T.typeCheckWithWarnings Map.empty (locatedExpression "case true of\n  _ -> 7\n  | true -> 9") of
      Right (_, [T.WarningAt location (T.UnreachableAlternative 2)]) -> do
        position location `shouldBe` ("source.kai",3,5,"  | true -> 9")
        renderTypeWarning (T.WarningAt location (T.UnreachableAlternative 2)) `shouldContain` "source.kai:3:5"
      other -> expectationFailure (show other)

  it "locates declaration and annotation failures" $ do
    result <- T.inferProgramWithEnvIO noImports "." Map.empty
      (locatedProgram "// header\n\nlet value : Missing = 1\n")
    case result of
      Left (T.TypeAt location (T.InvalidDataDeclaration "Unknown type: Missing")) ->
        position location `shouldBe` ("source.kai",3,1,"let value : Missing = 1")
      other -> expectationFailure (show other)

  it "keeps located mutually recursive top-level definitions in one group" $ do
    let source = unlines ["letrec even = \\n -> if n == 0 then true else odd (n-1)",
                          "letrec odd = \\n -> if n == 0 then false else even (n-1)", "even 8"]
    T.typeCheckProgram (locatedProgram source) `shouldBe` Right T.TBool
    E.evalProgram (locatedProgram source) `shouldReturn` Right (E.VBool True)

  it "preserves explicit recovery through located call contexts" $ do
    let expression = locatedExpression "let fail = \\unit -> 1 / 0 in attempt fail"
    E.evalPure expression `shouldBe` Right (E.VLeft (E.VData "DivisionByZero" []))
    E.eval expression `shouldReturn` Right (E.VLeft (E.VData "DivisionByZero" []))

  forM_ [0,7] $ \code -> it ("does not wrap or catch exit " ++ show code) $
    E.eval (locatedExpression ("attempt (\\unit -> exit " ++ show code ++ ")")) `shouldReturn` Left (E.ExitRequested code)

  it "formats tabs using the same columns as the parser" $
    case T.typeCheck (locatedExpression "\tmissing") of
      Left failure@(T.TypeAt location (T.UnboundVariable "missing")) -> do
        spanColumn location `shouldBe` 9
        renderTypeError failure `shouldBe`
          "source.kai:1:9: Type error: Unknown name 'missing'.\n1 |         missing\n  |         ^"
      other -> expectationFailure (show other)

  it "preserves Unicode excerpts without counting UTF-8 bytes as columns" $
    case T.typeCheck (locatedExpression "let café = 1 in café + missing") of
      Left (T.TypeAt location (T.UnboundVariable "missing")) ->
        position location `shouldBe` ("source.kai",1,24,"let café = 1 in café + missing")
      other -> expectationFailure (show other)

  it "keeps original line numbers in parser failures" $
    case parseLocatedProgram "bad.kai" "#!/usr/bin/env kai\n// header\n\nlet x = @\n" of
      Left failure -> errorBundlePretty failure `shouldContain` "bad.kai:4:9"
      Right program -> expectationFailure (show program)

  it "preserves values across generated comments and blank lines" $ property $
    forAll (choose (0,30 :: Int)) $ \blankLines ->
    forAll (choose (-1000,1000 :: Int)) $ \value ->
      let source = "// heading\n" ++ replicate blankLines '\n' ++ "/* note */ " ++ show value
      in E.evalPure (locatedExpression source) == Right (E.VInt value)

  forM_
    [ (T.TMaybe (T.TMaybe T.TInt), "Maybe (Maybe Int)")
    , (T.TEither (T.TMaybe T.TInt) (T.TEither T.TBool T.TString), "Either (Maybe Int) (Either Bool String)")
    , (T.TFun (T.TFun (T.TFun T.TInt T.TBool) T.TString) T.TUnit, "((Int -> Bool) -> String) -> Unit")
    , (T.TCustom "Box" [T.TMaybe T.TInt], "Box (Maybe Int)")
    ] $ \(ty,expected) -> it ("preserves type grouping in " ++ expected) $
      renderType ty `shouldBe` expected

  it "preserves source metadata through a thousand levels of parsing and evaluation" $ do
    let source = "// header\n" ++ replicate 1000 '(' ++ "1 + 2" ++ replicate 1000 ')'
    completed <- timeout 10000000 $ do
      program <- evaluate (force (locatedProgram source))
      T.typeCheckProgram program `shouldBe` Right T.TInt
      E.evalProgram program `shouldReturn` Right (E.VInt 3)
    completed `shouldBe` Just ()

  it "reports file diagnostics after a shebang and blank lines through the executable" $ withTempDir $ \dir -> do
    kai <- requireExecutable "kai"
    let path = dir </> "source with spaces.kai"
    writeFile path "#!/usr/bin/env kai\n// expect: error DivByZero\n\n1 / 0\n"
    result <- readProcessWithExitCode kai [path] ""
    result `shouldBe` (ExitFailure 1,path ++ ":4:1: Runtime error: Division by zero.\n4 | 1 / 0\n  | ^\n","")

  it "retains structured failures in debug output" $ do
    kai <- requireExecutable "kai"
    (status,out,err) <- readProcessWithExitCode kai ["--debug","-e","1 / 0"] ""
    status `shouldBe` ExitFailure 1
    out `shouldContain` "RuntimeAt"
    out `shouldContain` "DivByZero"
    out `shouldContain` "spanLine = 1"
    err `shouldBe` ""

  it "reports the definition and caller for a function imported from another file" $ withTempDir $ \dir -> do
    kai <- requireExecutable "kai"
    let helper = dir </> "Helper.kai"
        mainFile = dir </> "Main.kai"
    writeFile helper "// expect: ()\nlet divide = \\x -> 10 / x\n"
    writeFile mainFile "// expect: ()\nimport Helper\ndivide 0\n"
    result <- readProcessWithExitCode kai [mainFile] ""
    result `shouldBe` (ExitFailure 1, helper ++ ":2:20: Runtime error: Division by zero.\n2 | let divide = \\x -> 10 / x\n  |                    ^\n  while evaluating call to divide at " ++ mainFile ++ ":3:1\n", "")

  it "retains every import site for a nested imported type failure" $ withTempDir $ \dir -> do
    kai <- requireExecutable "kai"
    let broken = dir </> "Broken.kai"
        helper = dir </> "Helper.kai"
        mainFile = dir </> "Main.kai"
    writeFile broken "// expect: ()\n1 + true\n"
    writeFile helper "// expect: ()\nimport Broken\n"
    writeFile mainFile "// expect: ()\nimport Helper\n"
    result <- readProcessWithExitCode kai [mainFile] ""
    result `shouldBe` (ExitFailure 1,
      broken ++ ":2:1: Type error: Cannot match Bool with Int.\n2 | 1 + true\n  | ^\n" ++
      helper ++ ":2:1: while importing module Broken\n2 | import Broken\n  | ^\n" ++
      mainFile ++ ":2:1: while importing module Helper\n2 | import Helper\n  | ^\n", "")

  it "locates missing modules at the import expression" $ withTempDir $ \dir -> do
    kai <- requireExecutable "kai"
    let path = dir </> "Main.kai"
    writeFile path "// expect: ()\n\nimport Missing\n"
    result <- readProcessWithExitCode kai [path] ""
    result `shouldBe` (ExitFailure 1,path ++ ":3:1: Type error: Module not found: Missing\n3 | import Missing\n  | ^\n", "")

  it "locates conflicting declarations at the responsible import" $ withTempDir $ \dir -> do
    kai <- requireExecutable "kai"
    let path = dir </> "Main.kai"
    writeFile (dir </> "A.kai") "// expect: ()\ndata Value = Make Int\n"
    writeFile (dir </> "B.kai") "// expect: ()\ndata Value = Make Bool\n"
    writeFile path "// expect: ()\nimport A\nimport B\n"
    result <- readProcessWithExitCode kai [path] ""
    result `shouldBe` (ExitFailure 1,path ++ ":3:1: Type error: Conflicting imported type: Value\n3 | import B\n  | ^\n", "")

  it "retains definition excerpts across later REPL inputs" $ do
    kai <- requireExecutable "kai"
    (status,out,err) <- readProcessWithExitCode kai ["repl"]
      "let divide = \\x -> 10 / x\ndivide 0\n42\n:quit\n"
    status `shouldBe` ExitSuccess
    out `shouldContain` "<repl>:1:20: Runtime error: Division by zero.\n1 | let divide = \\x -> 10 / x\n  |                    ^\n  while evaluating call to divide at <repl>:1:1\nkai> 42\nkai> "
    err `shouldBe` ""

locatedExpression :: String -> Expr
locatedExpression = parseOrFail . parseLocatedExpr "source.kai"

locatedProgram :: String -> Program
locatedProgram = parseOrFail . parseLocatedProgram "source.kai"

parseOrFail :: Show error => Either error a -> a
parseOrFail (Left failure) = error (show failure)
parseOrFail (Right value) = value

position :: SourceSpan -> (FilePath,Int,Int,String)
position location = (spanFile location,spanLine location,spanColumn location,Text.unpack (spanExcerpt location))

noImports :: FilePath -> String -> IO (Either T.TypeError T.TypeEnv)
noImports _ name = pure $ Left (T.GeneralTypeError ("Unexpected import: " ++ name))
