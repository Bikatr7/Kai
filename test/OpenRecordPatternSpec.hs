module OpenRecordPatternSpec where

import Control.Monad (forM_)
import qualified Data.Map as Map
import CLI (runCLI)
import Evaluator (Value(..), eval, evalPure)
import ExampleSpec (withTempDir)
import Parser (parseExpr)
import Syntax
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec
import Test.QuickCheck
import TestIO (captureOutput, withStdin)
import TestSupport
import qualified TypeChecker as T

spec :: Spec
spec = describe "Open record patterns" $ do
  forM_
    [ ("case {a=1,b=true} of {a=x | rest} -> (x,rest)",
        VTuple [VInt 1,VRecord (Map.singleton "b" (VBool True))])
    , ("case {a=1} of {a=x | rest} -> rest", VRecord Map.empty)
    , ("case {} of {| rest} -> rest", VRecord Map.empty)
    , ("case {a=1} of {| rest} -> rest.a", VInt 1)
    , ("case {a=1,b=2,c=3} of {b=y,a=x | rest} -> (x,y,rest.c)", VTuple [VInt 1,VInt 2,VInt 3])
    , ("case {a=1,b=true} of {a=x | _} -> x", VInt 1)
    , ("case {outer={a=3,b=4},flag=true} of {outer={a=n | inside} | outside} -> (n,inside.b,outside.flag)",
        VTuple [VInt 3,VInt 4,VBool True])
    , ("case {a=false,b=2} of {a=true | _} -> 1 | {a=false | _} -> 2", VInt 2)
    , ("let dropA = \\r -> case r of {a=_ | rest} -> rest in (dropA {a=1,b=true},dropA {a=false,c=2})",
        VTuple [VRecord (Map.singleton "b" (VBool True)),VRecord (Map.singleton "c" (VInt 2))])
    , ("let sameRest = \\x -> \\y -> case x of {a=_ | rest} -> rest == y in sameRest {a=1,b=true} {b=true}", VBool True)
    , ("letrec sum = \\n -> \\r -> case r of {a=x | rest} -> if n == 0 then x + rest.b else sum (n-1) r in sum 3 {a=2,b=4,c=true}", VInt 6)
    , ("let rest = 99 in case {a=1,b=2} of {a=_ | rest} -> rest.b", VInt 2)
    , ("let dropA : {a:Int | row} -> {| row} = \\r -> case r of {a=_ | rest} -> rest in dropA {a=1,b=true}",
        VRecord (Map.singleton "b" (VBool True)))
    , ("case Just {a=3,b=true} of Just {a=x | _} -> x | Nothing -> 0", VInt 3)
    , ("case {fn=\\x -> x,ok=true} of {fn=_ | rest} -> rest == {ok=true}", VBool True)
    ] $ \(source,expected) -> it source $ do
      evaluateCheckedSource source `shouldBe` Right expected
      eval (parseExpression source) `shouldReturn` Right expected

  it "parses the explicit rest binding without consuming case alternatives" $
    parseExpr "case r of {a=true | rest} -> 1 | {a=false | _} -> 2" `shouldBe`
      Right (Case (Var "r") [(POpenRecord [("a",PBool True)] "rest",IntLit 1),
                            (POpenRecord [("a",PBool False)] "_",IntLit 2)])

  it "preserves the row relationship between the argument and returned remainder" $
    shouldInfer "\\r -> case r of {a=_ | rest} -> rest"
      (T.TFun (T.TOpenRecord (Map.singleton "a" (T.TVar "a")) (T.TRowVar "row"))
        (T.TOpenRecord Map.empty (T.TRowVar "row")))

  forM_ [("case {a=1} of {a=x | x} -> x",T.DuplicatePatternBinding "x"),
         ("case {a=1} of {a=_,a=_ | _} -> 1",T.DuplicateRecordField "a"),
         ("case {a={b=1}} of {a={b=x | rest} | rest} -> x",T.DuplicatePatternBinding "rest")] $ \(source,failure) ->
    it ("rejects duplicate names in " ++ source) $
      inferSource source `shouldBe` Left failure

  forM_ ["case r of {a=x |} -> x", "case r of {a=x | 42} -> x",
         "case r of {a=x, | rest} -> x", "case r of {a=x | rest, b=y} -> x"] $ \source ->
    it ("rejects malformed open pattern " ++ source) $ case parseExpr source of
      Left _ -> pure ()
      Right expression -> expectationFailure (show expression)

  it "rejects a missing required field" $ case inferSource "case {b=1} of {a=x | _} -> x" of
    Left (T.UnificationError T.TRecord {} T.TOpenRecord {}) -> pure ()
    other -> expectationFailure (show other)

  it "does not put removed fields back into the remainder" $
    inferSource "\\r -> case r of {a=_ | rest} -> rest.a" `shouldBe` Left (T.DuplicateRecordField "a")

  it "requires complete payload patterns before any effects execute" $ do
    (code,out) <- captureOutput (runCLI ["-e","print \"unexpected\"; case {a=true,b=1} of {a=true | _} -> 1"])
    code `shouldBe` ExitFailure 1
    out `shouldContain` "{a = false"
    out `shouldNotContain` "unexpected\n"

  it "retains the exact-field requirement for existing record patterns" $ case inferSource
    "case {a=1,b=2} of {a=x} -> x" of
      Left (T.UnificationError T.TRecord {} T.TRecord {}) -> pure ()
      other -> expectationFailure (show other)

  it "preserves rows in exported helpers and excludes remainder names from recursive dependencies" $ withTempDir $ \dir -> do
    let modulePath = dir </> "Rows.kai"
        mainPath = dir </> "main.kai"
    writeFile modulePath (unlines ["// expect: ()", "let dropA = \\r -> case r of {a=_ | rest} -> rest", "export dropA"])
    writeFile mainPath (unlines ["// expect: ()", "import Rows", "letrec rest = case {a=1,b=2} of {a=_ | rest} -> rest.b",
      "print (dropA {a=1,b=true},dropA {a=false,c=2},rest)"])
    captureOutput (runCLI [mainPath]) `shouldReturn` (ExitSuccess,"({b: True}, {c: 2}, 2)\n")

  it "retains a rest-polymorphic helper across REPL inputs" $ do
    (code,out) <- captureOutput $ withStdin
      "let dropA = \\r -> case r of {a=_ | rest} -> rest\ndropA {a=1,b=true}\ndropA {a=false,c=2}\n:quit\n" (runCLI [])
    code `shouldBe` ExitSuccess
    out `shouldContain` "kai> {b: True}\nkai> {c: 2}\nkai> "
    out `shouldNotContain` "Type error"

  it "agrees with all boolean field combinations, with and without extra fields" $ property $
    forAll (listOf (elements recordPatterns)) $ \patterns ->
      let domain = [Map.fromList ([("a",a),("b",b)] ++ [("extra",False) | extra]) |
                    a <- [False,True],b <- [False,True],extra <- [False,True]]
          covered = all (\value -> any (`matches` value) patterns) domain
          warnings = [T.UnreachableAlternative index | (index,p) <- zip [1..] patterns,
            all (\value -> not (matches p value) || any (`matches` value) (take (index-1) patterns)) domain]
          checked = T.checkCoverage Map.empty (T.TOpenRecord (Map.fromList [("a",T.TBool),("b",T.TBool)]) (T.TRowVar "row")) patterns
          coverageMatches = if covered then checked == Right warnings else case checked of
            Left T.NonExhaustivePatterns {} -> True
            _ -> False
          executionMatches value =
            let expected = head ([index | (index,p) <- zip [0..] patterns,matches p value] ++ [-1])
                expression = Case (RecordLit [(name,BoolLit flag) | (name,flag) <- Map.toList value])
                  ([(p,IntLit index) | (index,p) <- zip [0..] patterns] ++ [(PVar "_",IntLit (-1))])
            in evalPure expression == Right (VInt expected)
      in counterexample (show (patterns,checked)) (coverageMatches && all executionMatches domain)

  it "checks a thousand nested open patterns through parsing, inference and evaluation" $ do
    let value = concat (replicate 1000 "{a=") ++ "42" ++ replicate 1000 '}'
        pat = concat (replicate 1000 "{a=") ++ "x" ++ concat (replicate 1000 " | _}")
        source = "case " ++ value ++ " of " ++ pat ++ " -> x"
    completed <- timeout 10000000 $ do
      evaluateCheckedSource source `shouldBe` Right (VInt 42)
      eval (parseExpression source) `shouldReturn` Right (VInt 42)
    completed `shouldBe` Just ()

recordPatterns :: [Pattern]
recordPatterns = [PVar "_"] ++
  [POpenRecord fields "_" | fields <- [[],[("a",PBool True)],[("a",PBool False)],[("b",PBool True)],
    [("b",PBool False)],[("a",PBool True),("b",PBool True)],[("a",PBool False),("b",PBool False)]]] ++
  [PRecord [("a",PBool a),("b",PBool b)] | a <- [False,True],b <- [False,True]]

matches :: Pattern -> Map.Map String Bool -> Bool
matches (PVar _) _ = True
matches (PRecord fields) value = Map.keys value == Map.keys (Map.fromList fields) && matchFields fields value
matches (POpenRecord fields _) value = matchFields fields value
matches _ _ = False

matchFields :: [(String,Pattern)] -> Map.Map String Bool -> Bool
matchFields fields value = all (\(name,pat) -> case (Map.lookup name value,pat) of
  (Just actual,PBool expected) -> actual == expected
  (Just _,PVar _) -> True
  _ -> False) fields
