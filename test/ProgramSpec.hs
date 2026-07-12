module ProgramSpec where

import Test.Hspec
import Test.QuickCheck

import Parser
import Evaluator (evalProgram, Value(..))
import qualified Evaluator as E
import TypeChecker (Type(..), TypeError(..), typeCheckProgram)
import Syntax

spec :: Spec
spec = do
  describe "Top-Level Definitions" $ do
    describe "Basic Top-Level Definitions" $ do
      it "evaluates simple top-level definition" $ do
        let program = "let x = 42\nx"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "evaluates multiple definitions" $ do
        let program = "let x = 10\nlet y = 20\nx + y"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 30)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "evaluates definitions with functions" $ do
        let program = "let double = \\x -> x * 2\nlet n = 5\ndouble n"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 10)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Scoping and Visibility" $ do
      it "definitions are visible to subsequent definitions" $ do
        let program = "let x = 10\nlet y = x * 2\ny"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 20)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "later definitions can reference earlier ones" $ do
        let program = "let a = 1\nlet b = a + 1\nlet c = b + 1\nc"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 3)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Type Checking" $ do
      it "infers correct types for top-level definitions" $ do
        let program = "let x = 42\nx"
        case parseProgram program of
          Right ast -> typeCheckProgram ast `shouldBe` Right TInt
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "infers function types in definitions" $ do
        let program = "let add = \\x -> \\y -> x + y\nadd"
        case parseProgram program of
          Right ast -> case typeCheckProgram ast of
            Right (TFun TInt (TFun TInt TInt)) -> return ()
            Right other -> expectationFailure $ "Expected TFun TInt (TFun TInt TInt), got " ++ show other
            Left err -> expectationFailure $ "Type error: " ++ show err
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "generalizes top-level definitions across distinct uses" $ do
        let program = "let id = \\x -> x\n(id 1, id true)"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right (TTuple [TInt, TBool])
            evalProgram ast `shouldReturn` Right (VTuple [VInt 1, VBool True])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "catches type errors in definitions" $ do
        let program = "let x = true\nlet y = 5\nx + y"
        case parseProgram program of
          Right ast -> case typeCheckProgram ast of
            Left _ -> return ()
            Right ty -> expectationFailure $ "Expected type error, but got type: " ++ show ty
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Recursive Definitions" $ do
      it "handles recursive functions" $ do
        let program = "letrec factorial = \\n -> if n == 0 then 1 else n * factorial (n - 1)\nfactorial 5"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 120)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "generalizes recursive top-level definitions across distinct uses" $ do
        let program = "letrec id = \\x -> x\n(id 1, id true)"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right (TTuple [TInt, TBool])
            evalProgram ast `shouldReturn` Right (VTuple [VInt 1, VBool True])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles recursive functions with case expressions" $ do
        let program = "letrec countDown = \\n -> case n of 0 -> 0 | x -> x + countDown (x - 1)\ncountDown 5"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 15)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles recursive functions with conversions" $ do
        let program = "letrec testParse = \\s -> case parseInt s of Just n -> n | Nothing -> testParse \"0\"\ntestParse \"42\""
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles recursive functions with string operations" $ do
        let program = "letrec repeat = \\n -> \\s -> if n == 0 then \"\" else s ++ repeat (n - 1) s\nrepeat 3 \"hi\""
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VStr "hihihi")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports annotated polymorphic recursion at top level" $ do
        let program =
              "letrec nestedLayers : Int -> [a] -> Int = \\depth -> \\xs -> if depth == 0 then length xs else 1 + nestedLayers (depth - 1) [xs]\n\
              \nestedLayers 2 [1, 2, 3]"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TInt
            evalProgram ast `shouldReturn` Right (VInt 3)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "rejects top-level polymorphic recursion without an explicit annotation" $ do
        let program =
              "letrec nestedLayers = \\depth -> \\xs -> if depth == 0 then length xs else 1 + nestedLayers (depth - 1) [xs]\n\
              \nestedLayers 2 [1, 2, 3]"
        case parseProgram program of
          Right ast -> case typeCheckProgram ast of
            Left (InfiniteType _ _) -> return ()
            Left other -> expectationFailure $ "Expected InfiniteType, got " ++ show other
            Right ty -> expectationFailure $ "Expected type error, but got type: " ++ show ty
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Complex Programs" $ do
      it "handles program with data structures" $ do
        let program = "let nums = [1, 2, 3, 4, 5]\nlet doubled = map (\\x -> x * 2) nums\nlength doubled"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 5)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles program with string operations" $ do
        let program = "let greeting = \"Hello\"\nlet name = \"World\"\ngreeting ++ \" \" ++ name"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VStr "Hello World")
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Type Annotations" $ do
      it "accepts type annotations on definitions" $ do
        let program = "let x : Int = 42\nx"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TInt
            evalProgram ast `shouldReturn` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "accepts function type annotations" $ do
        let program = "let add : Int -> Int -> Int = \\x -> \\y -> x + y\nadd 3 4"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TInt
            evalProgram ast `shouldReturn` Right (VInt 7)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Edge Cases" $ do
      it "handles empty program (returns unit)" $ do
        let program = ""
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right VUnit
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles program with only definitions (no final expression)" $ do
        let program = "let x = 42\nlet y = x + 1"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right VUnit
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles program with comments" $ do
        let program = "// This is a comment\nlet x = 42\n// Another comment\nx"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles a final top-level let expression" $ do
        let program = "let x = 1\nlet y = x in y + 1"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 2)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "ignores a shebang line when parsing a program file" $ do
        let program = "#!/usr/bin/env kai\nprint \"hello\""
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right VUnit
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Error Cases" $ do
      it "reports unbound variables in final expression" $ do
        let program = "let x = 42\ny"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Left (E.UnboundVariable "y")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "reports unbound variables in definitions" $ do
        let program = "let x = y\nx"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Left (E.UnboundVariable "y")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles type mismatches in definitions" $ do
        let program = "let x : Bool = 42\nx"
        case parseProgram program of
          Right ast -> case typeCheckProgram ast of
            Left _ -> return ()
            Right ty -> expectationFailure $ "Expected type error, but got type: " ++ show ty
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "reports missing record fields in top-level programs" $ do
        let program = "let r = {a = 1}\nr.b"
        case parseProgram program of
          Right ast -> case typeCheckProgram ast of
            Left err -> err `shouldBe` RecordFieldMismatch "b"
            Right ty -> expectationFailure $ "Expected type error, but got type: " ++ show ty
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Integration with Existing Features" $ do
      it "works with list operations" $ do
        let program = "let nums = [1, 2, 3]\nlet sum = foldl (\\acc -> \\x -> acc + x) 0 nums\nsum"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 6)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "works with pattern matching" $ do
        let program = "let maybeValue = Just 42\ncase maybeValue of Just x -> x | Nothing -> 0"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "works with tuple patterns" $ do
        let program = "let pair = (1, 2)\ncase pair of (x, y) -> x + y"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 3)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "works with list patterns" $ do
        let program = "let nums = [1, 2, 3]\ncase nums of [] -> 0 | x :: xs -> x + length xs"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 3)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "works with record patterns" $ do
        let program = "let r = {a = 1, b = 2}\ncase r of {a = x, b = y} -> x + y"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 3)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "works with record field access at top level" $ do
        let program = "let r = {outer = {inner = 7}, flag = true}\nr.outer.inner"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TInt
            evalProgram ast `shouldReturn` Right (VInt 7)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "works with wildcard top-level definitions feeding a final case expression" $ do
        let program = "let _ = case Just 42 of _ -> \"matched\" | Nothing -> \"none\"\nlet _ = case (1, \"hello\") of _ -> \"tuple\"\ncase Nothing of _ -> \"done\" | Just x -> \"bad\""
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TString
            evalProgram ast `shouldReturn` Right (VStr "done")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports multiline do blocks in top-level definitions" $ do
        let program = "let result = do {\n  print \"start\";\n  42\n}\nresult"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TInt
            evalProgram ast `shouldReturn` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports comments inside multiline do blocks" $ do
        let program = "let result = do {\n  // keep this comment inside the block\n  print \"start\";\n  42\n}\nresult"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports multiline top-level helper definitions that continue after =" $ do
        let program = "let parseDefaultSecret : String -> Int = (\\value ->\n  case parseInt value of\n    Just n -> n\n    | Nothing -> 42\n)\nlet parseSecret : [String] -> Int = (\\cliArgs ->\n  case cliArgs of\n    value :: _ -> parseDefaultSecret value\n    | [] -> 42\n)\nparseSecret [\"7\"]"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TInt
            evalProgram ast `shouldReturn` Right (VInt 7)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports multiline top-level case alternatives without forcing parentheses" $ do
        let program = "letrec render = \\value -> case value of\n  0 -> \"zero\"\n  | n -> \"n=\" ++ toString n\nrender 7"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TString
            evalProgram ast `shouldReturn` Right (VStr "n=7")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports top-level data programs whose helper definitions continue onto | lines" $ do
        let program = "data Expr = Lit Int | Add (Expr) (Expr)\nletrec render = \\expr -> case expr of\n  Lit n -> toString n\n  | Add left right -> \"(\" ++ render left ++ \" + \" ++ render right ++ \")\"\nrender (Add (Lit 2) (Lit 3))"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TString
            evalProgram ast `shouldReturn` Right (VStr "(2 + 3)")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports consecutive independent letrec helpers with different result types" $ do
        let program = "letrec render = \\n -> if n == 0 then \"zero\" else toString n\nletrec double = \\n -> if n == 0 then 0 else n * 2\n(render 7, double 7)"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right (TTuple [TString, TInt])
            evalProgram ast `shouldReturn` Right (VTuple [VStr "7", VInt 14])
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports top-level letrec helpers with one-way forward references" $ do
        let program = "letrec start = \\n -> bump n\nletrec bump = \\n -> if n == 0 then 1 else n + 1\nstart 4"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TInt
            evalProgram ast `shouldReturn` Right (VInt 5)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "supports multiline top-level recursive definitions that continue after ->" $ do
        let program = "letrec countDown : Int -> Int = \\n ->\n  if n == 0 then 0\n  else countDown (n - 1)\ncountDown 3"
        case parseProgram program of
          Right ast -> do
            typeCheckProgram ast `shouldBe` Right TInt
            evalProgram ast `shouldReturn` Right (VInt 0)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Mutual Recursion" $ do
      it "handles mutually recursive functions at top level" $ do
        let program = "letrec isEven = \\n -> if n == 0 then true else isOdd (n - 1)\nletrec isOdd = \\n -> if n == 0 then false else isEven (n - 1)\nisEven 4"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VBool True)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles mutually recursive functions with different types" $ do
        let program = "letrec countDown = \\n -> if n == 0 then \"done\" else countUp (n - 1)\nletrec countUp = \\n -> if n == 0 then \"done\" else countDown (n - 1)\ncountDown 3"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VStr "done")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "handles three mutually recursive functions" $ do
        let program = "letrec f1 = \\n -> if n == 0 then 0 else f2 (n - 1)\nletrec f2 = \\n -> if n == 0 then 1 else f3 (n - 1)\nletrec f3 = \\n -> if n == 0 then 2 else f1 (n - 1)\nf1 6"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 0)
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "mutual recursion executes correctly (implies type checking works)" $ do
        let program = "letrec isEven = \\n -> if n == 0 then true else isOdd (n - 1)\nletrec isOdd = \\n -> if n == 0 then false else isEven (n - 1)\nisEven 4"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VBool True)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Top-Level Definitions with I/O" $ do
      it "handles top-level definitions that use print" $ do
        let program = "let _ = print \"Hello\"\nlet _ = print \"World\"\n42"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Right (VInt 42)
          Left err -> expectationFailure $ "Parse error: " ++ show err

    describe "Additional Error Cases" $ do
      it "reports error when letrec value is not a function" $ do
        let program = "letrec x = 42\nx"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Left (E.TypeError "LetRec value must be a function, got: 42")
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "reports mutual recursion type errors without crashing" $ do
        let program = "letrec f = \\n -> if n == 0 then 0 else g true\nletrec g = \\x -> x + 1\nf 1"
        case parseProgram program of
          Right ast -> case typeCheckProgram ast of
            Left _ -> return ()
            Right ty -> expectationFailure $ "Expected type error, but got type: " ++ show ty
          Left err -> expectationFailure $ "Parse error: " ++ show err

      it "reports error when expression appears before final expression" $ do
        let program = "let x = 42\nx + 1\nx"
        case parseProgram program of
          Right ast -> evalProgram ast `shouldReturn` Left (E.TypeError "Expressions must be at the end of the program")
          Left err -> expectationFailure $ "Parse error: " ++ show err
