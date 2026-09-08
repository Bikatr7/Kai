module CoverageSpec where

import qualified Data.Map as Map
import ExampleSpec (withTempDir)
import DocumentationSpec (requireExecutable)
import qualified Evaluator as E
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import TestSupport (inferSource, parseExpression)
import qualified ModuleSystem
import Parser (parseExpr, parseProgram)
import Syntax
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck
import qualified TypeChecker as T

spec :: Spec
spec = describe "Pattern coverage analysis" $ do
  let check = T.checkCoverage Map.empty
      anyValue = PVar "_"
  it "reports a missing boolean with a witness" $
    check T.TBool [PBool True] `shouldBe` Left (T.NonExhaustivePatterns "false")
  it "accepts both boolean alternatives" $
    check T.TBool [PBool False,PBool True] `shouldBe` Right []
  it "reports Nothing for a missing optional case" $
    check (T.TMaybe T.TInt) [PJust anyValue] `shouldBe` Left (T.NonExhaustivePatterns "Nothing")
  it "checks nested optional cases" $
    check (T.TMaybe T.TBool) [PNothing,PJust (PBool True)] `shouldBe`
      Left (T.NonExhaustivePatterns "Just false")
  it "accepts nested optional alternatives" $
    check (T.TMaybe T.TBool) [PNothing,PJust (PBool True),PJust (PBool False)] `shouldBe` Right []
  it "checks both Either payloads" $
    check (T.TEither T.TBool T.TBool) [PLeft anyValue,PRight (PBool True)] `shouldBe`
      Left (T.NonExhaustivePatterns "Right false")
  it "checks combinations across tuple columns" $
    check (T.TTuple [T.TBool,T.TBool]) [PTuple [PBool True,anyValue],PTuple [anyValue,PBool True]]
      `shouldBe` Left (T.NonExhaustivePatterns "(false, false)")
  it "does not confuse per-column coverage with all combinations" $
    check (T.TTuple [T.TBool,T.TBool]) [PTuple [PBool True,PBool True],PTuple [PBool False,PBool False]]
      `shouldBe` Left (T.NonExhaustivePatterns "(false, true)")
  it "normalizes record field order before checking combinations" $
    check (T.TRecord (Map.fromList [("a",T.TBool),("b",T.TBool)]))
      [PRecord [("b",anyValue),("a",PBool True)],PRecord [("a",PBool False),("b",PBool True)]]
      `shouldBe` Left (T.NonExhaustivePatterns "{a = false, b = false}")
  it "does not treat a closed pattern as covering arbitrary extra record fields" $
    check (T.TOpenRecord (Map.singleton "a" T.TInt) (T.TRowVar "row")) [PRecord [("a",anyValue)]]
      `shouldBe` Left (T.NonExhaustivePatterns "_")
  it "allows a catch-all for open records" $
    check (T.TOpenRecord (Map.singleton "a" T.TInt) (T.TRowVar "row")) [PRecord [("a",anyValue)],anyValue]
      `shouldBe` Right []
  it "reports a missing empty list" $
    check (T.TList T.TInt) [PCons anyValue anyValue] `shouldBe` Left (T.NonExhaustivePatterns "[]")
  it "accepts empty and cons list alternatives" $
    check (T.TList T.TInt) [PList [],PCons anyValue anyValue] `shouldBe` Right []
  it "checks list lengths beyond explicit singleton patterns" $
    check (T.TList T.TBool) [PList [],PList [anyValue]] `shouldBe`
      Left (T.NonExhaustivePatterns "false :: false :: []")
  it "checks list element cases along with length" $
    check (T.TList T.TBool) [PList [],PCons (PBool True) anyValue] `shouldBe`
      Left (T.NonExhaustivePatterns "false :: []")
  it "finds an integer missing from literal alternatives" $
    check T.TInt [PInt 0,PInt 1] `shouldBe` Left (T.NonExhaustivePatterns "2")
  it "finds a string missing from literal alternatives" $
    check T.TString [PStr "",PStr "x"] `shouldBe` Left (T.NonExhaustivePatterns "\"xx\"")
  it "accepts catch-alls after literal alternatives" $
    check T.TInt [PInt 0,anyValue] `shouldBe` Right []
  it "accepts the only unit value" $
    check T.TUnit [PUnit] `shouldBe` Right []
  it "accepts a wildcard on an unconstrained type" $
    check (T.TVar "a") [anyValue] `shouldBe` Right []
  it "warns for alternatives after a catch-all without reordering them" $
    check T.TBool [anyValue,PBool True,PBool False] `shouldBe`
      Right [T.UnreachableAlternative 2,T.UnreachableAlternative 3]
  it "warns when several earlier branches jointly cover an alternative" $
    check T.TBool [PBool True,PBool False,anyValue] `shouldBe` Right [T.UnreachableAlternative 3]
  it "warns for a duplicate literal while retaining the final catch-all" $
    check T.TInt [PInt 0,PInt 0,anyValue] `shouldBe` Right [T.UnreachableAlternative 2]
  it "warns for a list pattern covered by a prior cons pattern" $
    check (T.TList T.TInt) [PList [],PCons anyValue anyValue,PList [PInt 1]] `shouldBe`
      Right [T.UnreachableAlternative 3]

  it "agrees with enumerated boolean tuple values and branch reachability" $ property $
    forAll (listOf (elements tuplePatterns)) $ \patterns ->
      let domain = [(False,False),(False,True),(True,False),(True,True)]
          covered = [value | value <- domain,any (`matchesTuple` value) patterns]
          expectedWarnings = [T.UnreachableAlternative index | (index,pat) <- zip [1..] patterns,
            all (\value -> not (matchesTuple pat value) || any (`matchesTuple` value) (take (index-1) patterns)) domain]
          actual = check (T.TTuple [T.TBool,T.TBool]) patterns
      in if length covered == length domain then actual == Right expectedWarnings else
        case actual of
          Left (T.NonExhaustivePatterns witness) -> case parseExpr ("case (false,false) of " ++ witness ++ " -> ()") of
            Right (Case _ [(pat,_)]) -> any (\value -> matchesTuple pat value && value `notElem` covered) domain
              && all (\value -> not (matchesTuple pat value) || value `notElem` covered) domain
            _ -> False
          _ -> False

  it "checks recursive ADTs without expanding wildcard payloads" $ do
    env <- declarationEnv "data Tree a = Leaf a | Node (Tree a) (Tree a)"
    T.checkCoverage env (T.TCustom "Tree" [T.TBool])
      [PConstructor "Leaf" [anyValue],PConstructor "Node" [anyValue,anyValue]] `shouldBe` Right []
    T.checkCoverage env (T.TCustom "Tree" [T.TBool])
      [PConstructor "Leaf" [PBool True],PConstructor "Node" [anyValue,anyValue]] `shouldBe`
        Left (T.NonExhaustivePatterns "Leaf false")

  it "terminates for polymorphically recursive constructor payloads" $ do
    env <- declarationEnv "data Tower a = End | Next (Tower (a -> a))"
    T.checkCoverage env (T.TCustom "Tower" [T.TInt]) [PConstructor "End" [],PConstructor "Next" [anyValue]]
      `shouldBe` Right []

  it "handles a recursive declaration with no nullary constructor" $ do
    env <- declarationEnv "data Loop = Loop Loop"
    T.checkCoverage env (T.TCustom "Loop" []) [PConstructor "Loop" [anyValue]] `shouldBe` Right []

  it "does not disclose hidden constructors in a missing witness" $ withTempDir $ \dir -> do
    writeFile (dir </> "Private.kai") $ unlines
      ["// expect: ()", "data Hidden = Public | Secret", "export Public"]
    loaded <- ModuleSystem.loadModuleTypeEnvIO dir "Private"
    case loaded of
      Left err -> expectationFailure (show err)
      Right env -> do
        T.checkCoverage env (T.TCustom "Hidden" []) [PConstructor "Public" []] `shouldBe`
          Left (T.NonExhaustivePatterns "_")
        T.checkCoverage env (T.TCustom "Hidden" []) [PConstructor "Public" [],anyValue] `shouldBe` Right []

  it "rejects incomplete source matches statically while retaining defensive evaluation" $ do
    let source = "case Nothing of Just value -> value + 1"
    inferSource source `shouldBe` Left (T.NonExhaustivePatterns "Nothing")
    E.evalPure (parseExpression source) `shouldBe` Left (E.TypeError "No matching pattern in case expression")
    E.eval (parseExpression source) `shouldReturn` Left (E.TypeError "No matching pattern in case expression")

  it "rejects an incomplete match before any CLI effects" $ do
    kai <- requireExecutable "kai"
    (status,out,err) <- readProcessWithExitCode kai ["-e", "print \"must not run\"; case Nothing of Just x -> x + 1"] ""
    status `shouldBe` ExitFailure 1
    out `shouldBe` "<expression>:1:23: Type error: Incomplete case; add a branch covering Nothing.\n1 | print \"must not run\"; case Nothing of Just x -> x + 1\n  |                       ^\n"
    err `shouldBe` ""

  it "reports unreachable alternatives on stderr and preserves the selected result" $ do
    kai <- requireExecutable "kai"
    (status,out,err) <- readProcessWithExitCode kai ["-e", "print (case true of _ -> 7 | true -> 99)"] ""
    status `shouldBe` ExitSuccess
    out `shouldBe` "7\n"
    err `shouldBe` "<expression>:1:30: Warning: case alternative 2 is unreachable\n1 | print (case true of _ -> 7 | true -> 99)\n  |                              ^\n"

  it "retains warnings through top-level definitions and imports" $ withTempDir $ \dir -> do
    kai <- requireExecutable "kai"
    let helper = dir </> "WarningHelper.kai"
        mainFile = dir </> "Main.kai"
    writeFile helper "// expect: ()\nlet f = \\x -> case x of _ -> 7 | true -> 99\n"
    writeFile mainFile "// expect: ()\nimport WarningHelper\nprint (f true)\n"
    (status,out,err) <- readProcessWithExitCode kai [mainFile] ""
    status `shouldBe` ExitSuccess
    out `shouldBe` "7\n"
    err `shouldBe` "In module " ++ helper ++ ":\n" ++ helper ++ ":2:34: Warning: case alternative 2 is unreachable\n2 | let f = \\x -> case x of _ -> 7 | true -> 99\n  |                                  ^\n"

  it "rejects incomplete imported functions before importer effects" $ withTempDir $ \dir -> do
    kai <- requireExecutable "kai"
    writeFile (dir </> "Incomplete.kai") "// expect: ()\nlet f = \\x -> case x of Just y -> y\n"
    writeFile (dir </> "Main.kai") "// expect: ()\nprint \"must not run\"\nimport Incomplete\nf (Just 1)\n"
    (status,out,err) <- readProcessWithExitCode kai [dir </> "Main.kai"] ""
    status `shouldBe` ExitFailure 1
    out `shouldBe` (dir </> "Incomplete.kai") ++ ":2:15: Type error: Incomplete case; add a branch covering Nothing.\n2 | let f = \\x -> case x of Just y -> y\n  |               ^\n" ++ (dir </> "Main.kai") ++ ":3:1: while importing module Incomplete\n3 | import Incomplete\n  | ^\n"
    out `shouldNotContain` "must not run"
    err `shouldBe` ""

  it "reports warnings for REPL definitions and type queries without duplicate emission" $ do
    kai <- requireExecutable "kai"
    let input = unlines ["let f = \\x -> case x of _ -> 7 | true -> 99", "f true",
                         ":type \\x -> case x of _ -> 7 | true -> 99", ":quit"]
    (status,out,err) <- readProcessWithExitCode kai ["repl"] input
    status `shouldBe` ExitSuccess
    out `shouldContain` "7"
    err `shouldBe` "<repl>:1:34: Warning: case alternative 2 is unreachable\n1 | let f = \\x -> case x of _ -> 7 | true -> 99\n  |                                  ^\n<repl>:1:26: Warning: case alternative 2 is unreachable\n1 | \\x -> case x of _ -> 7 | true -> 99\n  |                          ^\n"

  it "requires parentheses to separate nested case alternatives" $ do
    inferSource "case Just 1 of Just a -> case Just 2 of Just b -> a + b | Nothing -> 0 | Nothing -> 0"
      `shouldBe` Left (T.NonExhaustivePatterns "Nothing")
    let source = "case Just 1 of Just a -> (case Just 2 of Just b -> a + b | Nothing -> 0) | Nothing -> 0"
    inferSource source `shouldBe` Right T.TInt
    E.evalPure (parseExpression source) `shouldBe` Right (E.VInt 3)

  it "checks a thousand nested tuple patterns through parsing, inference and evaluation" $ do
    let nested leaf = concat (replicate 1000 "((),") ++ leaf ++ replicate 1000 ')'
        source = "case " ++ nested "true" ++ " of " ++ nested "true" ++ " -> 7 | " ++ nested "false" ++ " -> 0"
    inferSource source `shouldBe` Right T.TInt
    E.evalPure (parseExpression source) `shouldBe` Right (E.VInt 7)

  it "checks a thousand nested tuple patterns" $ do
    let nestType 0 = T.TBool
        nestType n = T.TTuple [T.TUnit,nestType (n-1)]
        nestPattern 0 p = p
        nestPattern n p = PTuple [PUnit,nestPattern (n-1) p]
    check (nestType (1000 :: Int)) [nestPattern (1000 :: Int) (PBool True),nestPattern (1000 :: Int) (PBool False)]
      `shouldBe` Right []

  it "checks a thousand-level recursive cons pattern with a catch-all" $
    check (T.TList T.TInt) [foldr PCons (PList []) (replicate 1000 anyValue),anyValue] `shouldBe` Right []

-- Enumerate the values independently of the coverage matrix algorithm.
tuplePatterns :: [Pattern]
tuplePatterns = PVar "_" : [PTuple [a,b] | a <- atoms,b <- atoms]
  where atoms = [PVar "_",PBool False,PBool True]

matchesTuple :: Pattern -> (Bool,Bool) -> Bool
matchesTuple (PVar _) _ = True
matchesTuple (PTuple [a,b]) (x,y) = matchesAtom a x && matchesAtom b y
matchesTuple _ _ = False

matchesAtom :: Pattern -> Bool -> Bool
matchesAtom (PVar _) _ = True
matchesAtom (PBool expected) actual = expected == actual
matchesAtom _ _ = False

declarationEnv :: String -> IO T.TypeEnv
declarationEnv source = case parseProgram source of
  Left err -> error (show err)
  Right program -> do
    result <- T.inferProgramWithEnvIO (\_ name -> pure $ Left $ T.GeneralTypeError ("Unexpected import: " ++ name)) "." Map.empty program
    case result of
      Left err -> error (show err)
      Right (env,_) -> pure env
