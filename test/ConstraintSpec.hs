module ConstraintSpec where

import Control.Monad (forM_)
import CLI (runCLI)
import Evaluator (Value(..), eval)
import ExampleSpec (withTempDir)
import Parser (parseProgram)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck
import TestIO (captureOutput, withStdin)
import TestSupport
import qualified TypeChecker as T

spec :: Spec
spec = describe "Built-in type constraints" $ do
  forM_
    [ ("let append = \\x -> \\y -> x ++ y in (append \"a\" \"b\",append [1] [2])", VTuple [VStr "ab",VList [VInt 1,VInt 2]])
    , ("let append = \\x -> \\y -> x ++ y in let alias = append in alias [1] [2]", VList [VInt 1,VInt 2])
    , ("let append : [a] -> [a] -> [a] = \\x -> \\y -> x ++ y in append [1] [2]", VList [VInt 1,VInt 2])
    , ("let append : Append a => a -> a -> a = \\x -> \\y -> x ++ y in append \"a\" \"b\"", VStr "ab")
    , ("((\\x -> \\y -> x ++ y) : Append a => a -> a -> a) [1] [2]", VList [VInt 1,VInt 2])
    , ("let append = \\x -> \\y -> x ++ y in map (append [1]) [[2],[3]]", VList [VList [VInt 1,VInt 2],VList [VInt 1,VInt 3]])
    , ("let append = \\x -> \\y -> x ++ y in foldl append [] [[1],[2,3]]", VList [VInt 1,VInt 2,VInt 3])
    , ("let append = \\x -> \\y -> x ++ y in null (append [] [])", VBool True)
    , ("let same = \\x -> \\y -> x == y in (same 1 2,same \"a\" \"a\",same [true] [true])", VTuple [VBool False,VBool True,VBool True])
    , ("let same : Eq a => a -> a -> Bool = \\x -> \\y -> x == y in same [1] [1]", VBool True)
    , ("let f : (Eq a, Append a) => a -> a -> Bool = \\x -> \\y -> (discard (x ++ y); x == y) in f [1] [1]", VBool True)
    , ("(\\x -> let same = x == x in same) [1]", VBool True)
    , ("(\\x -> let f = \\y -> x ++ y in f x) [1]", VList [VInt 1,VInt 1])
    , ("(\\x -> let f = \\unit -> x == x in f ()) 1", VBool True)
    , ("let make = \\x -> \\y -> x == y in let same = make [1] in same [1]", VBool True)
    , ("letrec append = \\n -> \\x -> if n == 0 then x else x ++ append (n-1) x in append 2 [1]", VList [VInt 1,VInt 1,VInt 1])
    , ("letrec append : Append a => Int -> a -> a = \\n -> \\x -> if n == 0 then x else x ++ append (n-1) x in append 2 \"a\"", VStr "aaa")
    , ("letrec same : Eq a => Int -> a -> Bool = \\n -> \\x -> if n == 0 then x == x else same (n-1) [x] in same 2 1", VBool True)
    , ("[] == []", VBool True)
    , ("Nothing == Nothing", VBool True)
    , ("Left 1 == Left 1", VBool True)
    , ("let result = [] == [] in result", VBool True)
    , ("let f = \\x -> x == x in f []", VBool True)
    , ("{a=[1],b=Just true} == {b=Just true,a=[1]}", VBool True)
    , ("let same = \\r -> (discard r.a; r == r) in same {a=1,b=true}", VBool True)
    , ("let f : Eq {a:Int | row} => {a:Int | row} -> Bool = \\r -> r == r in f {a=1,b=true}", VBool True)
    , ("head ([\\x -> x] ++ [\\x -> x]) 42", VInt 42)
    , ("letrec f : Int -> Bool = \\x -> x == x in f 1", VBool True)
    , ("UserError \"x\" == UserError \"x\"", VBool True)
    , ("IOError NotFound \"readFile\" (Just \"x\") \"missing\" == IOError NotFound \"readFile\" (Just \"x\") \"missing\"", VBool True)
    ] $ \(source,expected) -> it source $ do
      evaluateCheckedSource source `shouldBe` Right expected
      eval (parseExpression source) `shouldReturn` Right expected

  it "retains Append in the inferred function type" $
    shouldInfer "\\x -> \\y -> x ++ y"
      (T.TQualified [T.Appendable (T.TVar "a")] (T.TFun (T.TVar "a") (T.TFun (T.TVar "a") (T.TVar "a"))))

  it "retains Eq on a parameter used in an inner let" $
    shouldInfer "\\x -> let result = x == x in result"
      (T.TQualified [T.Equality (T.TVar "a")] (T.TFun (T.TVar "a") T.TBool))

  it "retains Eq on returned functions instead of defaulting their argument" $
    shouldInfer "let same = \\x -> x == x in same"
      (T.TQualified [T.Equality (T.TVar "a")] (T.TFun (T.TVar "a") T.TBool))

  it "renders qualified signatures" $ do
    (status,output) <- captureOutput $ withStdin ":type \\x -> (discard (x ++ x); x == x)\n:quit\n" $ runCLI ["repl"]
    status `shouldBe` ExitSuccess
    output `shouldContain` "Append "
    output `shouldContain` "Eq "
    output `shouldContain` ") => "
    output `shouldContain` " -> Bool"

  forM_
    [ "let f = \\x -> x in f == f"
    , "[\\x -> x] == [\\x -> x]"
    , "Just (\\x -> x) == Just (\\x -> x)"
    , "Left (\\x -> x) == Left (\\x -> x)"
    , "(1,\\x -> x) == (1,\\x -> x)"
    , "{f=\\x -> x} == {f=\\x -> x}"
    , "let same = \\x -> x == x in same (\\x -> x)"
    , "(\\x -> let result = x == x in result) (\\x -> x)"
    , "let same = \\r -> (discard r.a; r == r) in same {a=1,f=\\x -> x}"
    , "let f : Eq {a:Int | row} => {a:Int | row} -> Bool = \\r -> r == r in f {a=1,b=\\x -> x}"
    , "let f = \\x -> x == x in f ([] : [Int -> Int])"
    , "letrec f : Eq a => a -> Bool = \\x -> if true then x == x else f (\\unit -> x) in f 1"
    , "let bad = (\\x -> x) == (\\x -> x) in 1"
    , "let f : Eq a => a -> a = \\x -> x in f (\\x -> x)"
    , "let f : Eq a => a -> Bool = \\x -> true in f (\\x -> x)"
    ] $ \source -> it ("statically rejects callable equality: " ++ source) $
      case inferSource source of
        Left (T.UnsatisfiedConstraint (T.Equality T.TFun {})) -> pure ()
        other -> expectationFailure (show other)

  forM_ ["1 ++ 2", "true ++ false", "(\\x -> x) ++ (\\y -> y)",
          "let append = \\x -> \\y -> x ++ y in append 1 2"] $ \source ->
    it ("rejects invalid Append: " ++ source) $
      case inferSource source of
        Left (T.UnsatisfiedConstraint T.Appendable {}) -> pure ()
        other -> expectationFailure (show other)

  it "rejects mixed append operands during unification" $
    inferSource "\"a\" ++ [1]" `shouldBe` Left (T.UnificationError T.TString (T.TList T.TInt))

  forM_ ["let f : a -> a -> a = \\x -> \\y -> x ++ y in f",
         "let f : a -> a -> Bool = \\x -> \\y -> x == y in f",
         "letrec f : a -> Bool = \\x -> x == x in f"] $ \source ->
    it ("rejects a context hidden by an annotation: " ++ source) $
      case inferSource source of
        Left T.MissingConstraint {} -> pure ()
        other -> expectationFailure (show other)

  it "does not default ambiguous Append to strings or lists" $
    case inferSource "discard ((raise (UserError \"x\")) ++ (raise (UserError \"y\")))" of
      Left (T.AmbiguousConstraint T.Appendable {}) -> pure ()
      other -> expectationFailure (show other)

  it "does not default a variable needed by both Eq and Append" $
    case inferSource "let f = \\x -> (discard (x ++ x); x == x) in f (raise (UserError \"x\"))" of
      Left T.AmbiguousConstraint {} -> pure ()
      other -> expectationFailure (show other)

  it "rejects unknown constraint names" $
    inferSource "let f : Magic a => a -> a = \\x -> x in f" `shouldBe`
      Left (T.GeneralTypeError "Unknown constraint: Magic")

  it "rejects nested qualified types" $
    inferSource "\\f : (Eq a => a -> Bool) -> f 1" `shouldBe`
      Left (T.GeneralTypeError "Lambda parameters cannot have qualified types")

  it "matches independent list append and equality expectations" $ property $
    forAll (listOf (choose (-1000,1000 :: Int))) $ \left ->
    forAll (listOf (choose (-1000,1000 :: Int))) $ \right ->
      evaluateCheckedSource ("let append = \\x -> \\y -> x ++ y in let same = \\x -> \\y -> x == y in " ++
        "(append " ++ show left ++ " " ++ show right ++ ", same " ++ show left ++ " " ++ show right ++ ")") ==
        Right (VTuple [VList (map VInt (left ++ right)),VBool (left == right)])

  forM_
    [ ("data Phantom a = Phantom\n(Phantom : Phantom (Int -> Int)) == Phantom", T.TBool)
    , ("data Tree a = Leaf a | Node (Tree a) (Tree a)\nNode (Leaf 1) (Leaf 2) == Node (Leaf 1) (Leaf 2)", T.TBool)
    , ("data Tower a = End | Next (Tower (a -> a))\n(End : Tower (Int -> Int)) == End", T.TBool)
    , ("data Rose a = Rose [Rose a]\n(Rose [] : Rose (Int -> Int)) == Rose []", T.TBool)
    ] $ \(source,expected) -> it ("checks comparable custom data: " ++ source) $
      checkProgram source `shouldBe` Right expected

  forM_
    [ "data Holder a = Hold a\nHold (\\x -> x) == Hold (\\x -> x)"
    , "data Hidden = Safe | Unsafe (Int -> Int)\nSafe == Safe"
    , "data Weird a = Leaf a | Next (Weird (a -> a))\nLeaf 1 == Leaf 1"
    , "data Swap a b = Base a | Recur (Swap b a)\n(Base 1 : Swap Int (Int -> Int)) == Base 1"
    , "data Holder a = Hold (Maybe a)\n(Hold Nothing : Holder (Int -> Int)) == Hold Nothing"
    ] $ \source -> it ("rejects non-comparable data: " ++ source) $
      case checkProgram source of
        Left (T.UnsatisfiedConstraint T.Equality {}) -> pure ()
        other -> expectationFailure (show other)

  it "carries constraints through module exports and rejects private callable payloads" $ withTempDir $ \dir -> do
    writeFile (dir </> "Helpers.kai") $ unlines
      ["// expect: ()", "let append = \\x -> \\y -> x ++ y", "let same = \\x -> \\y -> x == y", "export append, same"]
    writeFile (dir </> "Main.kai") $ unlines
      ["// expect: ()", "import Helpers", "print (append [1] [2], append \"a\" \"b\", same true true)"]
    (status,output) <- captureOutput $ runCLI [dir </> "Main.kai"]
    status `shouldBe` ExitSuccess
    output `shouldBe` "([1, 2], ab, True)\n"
    writeFile (dir </> "Private.kai") $ unlines
      ["// expect: ()", "data Hidden = Safe | Unsafe (Int -> Int)", "let value = Safe", "export value"]
    writeFile (dir </> "Bad.kai") $ unlines
      ["// expect: ()", "import Helpers", "import Private", "print \"must not run\"", "same value value"]
    (badStatus,badOutput) <- captureOutput $ runCLI [dir </> "Bad.kai"]
    badStatus `shouldBe` ExitFailure 1
    badOutput `shouldContain` "Type error: Equality is not supported for"
    badOutput `shouldNotContain` "must not run"
    badOutput `shouldNotContain` "Unsafe"

  it "retains constraints in mutually recursive definitions" $ withTempDir $ \dir -> do
    let path = dir </> "Mutual.kai"
    writeFile path $ unlines
      ["// expect: ()", "letrec even = \\n -> \\x -> if n == 0 then x == x else odd (n-1) x",
       "letrec odd = \\n -> \\x -> if n == 0 then x == x else even (n-1) x", "print (even 4 [1],odd 3 \"a\")"]
    (status,output) <- captureOutput $ runCLI [path]
    status `shouldBe` ExitSuccess
    output `shouldBe` "(True, True)\n"
    appendFile path "even 1 (\\x -> x)\n"
    (badStatus,badOutput) <- captureOutput $ runCLI [path]
    badStatus `shouldBe` ExitFailure 1
    badOutput `shouldContain` "Type error: Equality is not supported for"
    badOutput `shouldNotContain` "(True, True)"

  it "checks qualified annotations in mutually recursive definitions" $ withTempDir $ \dir -> do
    let path = dir </> "QualifiedMutual.kai"
    writeFile path $ unlines
      ["// expect: ()", "letrec even : Eq a => Int -> a -> Bool = \\n -> \\x -> if n == 0 then x == x else odd (n-1) [x]",
       "letrec odd : Eq a => Int -> a -> Bool = \\n -> \\x -> if n == 0 then x == x else even (n-1) [x]", "print (even 4 1,odd 3 true)"]
    (status,output) <- captureOutput $ runCLI [path]
    status `shouldBe` ExitSuccess
    output `shouldBe` "(True, True)\n"

  it "rejects value and row variable kind confusion across the context" $
    inferSource "let f : Eq row => {a:Int | row} -> Bool = \\x -> true in f" `shouldBe`
      Left (T.ConflictingVariableKind "row")

  it "retains constraints across REPL inputs" $ do
    let input = unlines ["let append = \\x -> \\y -> x ++ y", "append \"a\" \"b\"", "append [1] [2]",
                         "let same = \\x -> x == x", "same 1", "same (\\x -> x)", ":quit"]
    (status,output) <- captureOutput $ withStdin input $ runCLI ["repl"]
    status `shouldBe` ExitSuccess
    output `shouldContain` "Append "
    output `shouldContain` "Eq "
    output `shouldContain` "ab"
    output `shouldContain` "[1, 2]"
    output `shouldContain` "True"
    output `shouldContain` "Type error: Equality is not supported for"

  it "solves a thousand nested comparable lists without recursive ADT expansion" $
    inferSource (replicate 1000 '[' ++ "1" ++ replicate 1000 ']' ++ " == " ++
                 replicate 1000 '[' ++ "1" ++ replicate 1000 ']') `shouldBe` Right T.TBool

checkProgram :: String -> Either T.TypeError T.Type
checkProgram source = case parseProgram source of
  Left err -> error (show err)
  Right program -> T.typeCheckProgram program
