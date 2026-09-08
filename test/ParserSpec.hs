module ParserSpec where

import Test.Hspec
import Syntax
import Parser
import Control.Monad (forM_)

spec :: Spec
spec = describe "Parser Tests" $ do
  describe "File expression comments" $ do
    forM_ ["// literal text", "  // literal text", "/* literal text */", "#! literal text", "雪"] $ \line ->
      it ("preserves string content " ++ show line) $ do
        let value = "first\n" ++ line ++ "\nlast"
            source = "#!/usr/bin/env kai\n// actual comment\n\"" ++ value ++ "\"\n// final comment\n"
        parseFileExpr source `shouldBe` Right (StrLit value)
        parseFile "example.kai" source `shouldBe` Right (StrLit value)
        parseProgram source `shouldBe` Right (Program [TLExpr (StrLit value)])
    it "accepts nested comments around an expression" $
      parseFileExpr "/* first /* nested */ last */ 42 // end" `shouldBe` Right (IntLit 42)
  
  describe "Number Parsing" $ do
    it "parses positive integers" $ do
      parseExpr "42" `shouldBe` Right (IntLit 42)
    
    it "parses zero" $ do
      parseExpr "0" `shouldBe` Right (IntLit 0)
    
    it "parses multi-digit numbers" $ do
      parseExpr "12345" `shouldBe` Right (IntLit 12345)
  
  describe "Boolean Parsing" $ do
    it "parses true" $ do
      parseExpr "true" `shouldBe` Right (BoolLit True)
    
    it "parses false" $ do
      parseExpr "false" `shouldBe` Right (BoolLit False)
  
  describe "Variable Parsing" $ do
    it "parses single letter variable" $ do
      parseExpr "x" `shouldBe` Right (Var "x")
    
    it "parses multi-letter variable" $ do
      parseExpr "hello" `shouldBe` Right (Var "hello")
    
    it "parses variable with underscores" $ do
      parseExpr "my_var" `shouldBe` Right (Var "my_var")

    it "reserves module keywords and permits a shadowed fix function" $ do
      mapM_
        (\name -> case parseExpr ("let " ++ name ++ " = 1 in 1") of
          Left _ -> return ()
          Right parsed -> expectationFailure $ "Should reject reserved name " ++ name ++ ", got " ++ show parsed)
        ["import", "export"]
      parseExpr "let fix = 1 in fix" `shouldBe` Right (Let "fix" Nothing (IntLit 1) (Var "fix"))

    it "does not split keyword-prefixed identifiers" $ do
      mapM_
        (\name -> parseExpr name `shouldBe` Right (Var name))
        ["nothing", "trueValue", "ifonly", "letdown", "android", "origin"]
  
  describe "Operator Parsing" $ do
    it "parses addition" $ do
      parseExpr "1 + 2" `shouldBe` Right (Add (IntLit 1) (IntLit 2))
    
    it "parses subtraction" $ do
      parseExpr "5 - 3" `shouldBe` Right (Sub (IntLit 5) (IntLit 3))
    
    it "parses multiplication" $ do
      parseExpr "3 * 4" `shouldBe` Right (Mul (IntLit 3) (IntLit 4))
    
    it "parses division" $ do
      parseExpr "8 / 2" `shouldBe` Right (Div (IntLit 8) (IntLit 2))

  describe "String Parsing" $ do
    it "parses empty string" $ do
      parseExpr "\"\"" `shouldBe` Right (StrLit "")

    it "parses simple string" $ do
      parseExpr "\"hello\"" `shouldBe` Right (StrLit "hello")

    it "parses escaped quote (\")" $ do
      parseExpr "\"\\\"\"" `shouldBe` Right (StrLit "\"")

    it "parses escaped backslash (\\)" $ do
      parseExpr "\"\\\\\"" `shouldBe` Right (StrLit "\\")

    it "parses unary minus on literals" $ do
      parseExpr "-5" `shouldBe` Right (IntLit (-5))

    it "parses unary minus on variables" $ do
      parseExpr "-x" `shouldBe` Right (Sub (IntLit 0) (Var "x"))

    it "parses unary minus on parenthesized expr" $ do
      parseExpr "-(1 + 2)" `shouldBe` Right (Sub (IntLit 0) (Add (IntLit 1) (IntLit 2)))
  
  describe "Precedence Parsing" $ do
    it "multiplication before addition" $ do
      parseExpr "1 + 2 * 3" `shouldBe` Right (Add (IntLit 1) (Mul (IntLit 2) (IntLit 3)))
    
    it "parentheses override precedence" $ do
      parseExpr "(1 + 2) * 3" `shouldBe` Right (Mul (Add (IntLit 1) (IntLit 2)) (IntLit 3))
  
  describe "Lambda Parsing" $ do
    it "parses simple lambda" $ do
      parseExpr "\\x -> x" `shouldBe` Right (Lambda "x" Nothing (Var "x"))
    
    it "parses lambda with expression body" $ do
      parseExpr "\\x -> x + 1" `shouldBe` Right (Lambda "x" Nothing (Add (Var "x") (IntLit 1)))
  
  describe "Function Application Parsing" $ do
    it "parses simple application" $ do
      parseExpr "f x" `shouldBe` Right (App (Var "f") (Var "x"))
    
    it "parses lambda application" $ do
      parseExpr "(\\x -> x) 5" `shouldBe` Right (App (Lambda "x" Nothing (Var "x")) (IntLit 5))

    it "parses adjacent constructors as left-associative applications" $ do
      parseExpr "Pair Z Z"
        `shouldBe` Right (App (App (Var "Pair") (Var "Z")) (Var "Z"))

    it "does not treat custom constructor names as Maybe/Either keyword prefixes" $ do
      mapM_
        (\name ->
          parseExpr (name ++ " Z")
            `shouldBe` Right (App (Var name) (Var "Z")))
        ["Justly", "NothingElse", "Leftover", "RightAngle"]

  describe "Built-in Application Precedence" $ do
    mapM_
      (\(source, expected) ->
        it ("parses " ++ source ++ " before the surrounding operator") $ do
          parseExpr source `shouldBe` Right expected)
      [ ("head [1] + 2", Add (App (Var "head") (ListLit [IntLit 1])) (IntLit 2))
      , ("tail [1] ++ [2]", Concat (App (Var "tail") (ListLit [IntLit 1])) (ListLit [IntLit 2]))
      , ("null [] == true", Eq (App (Var "null") (ListLit [])) (BoolLit True))
      , ("fix f + 1", Add (App (Var "fix") (Var "f")) (IntLit 1))
      , ("parseInt \"1\" == Nothing", Eq (App (Var "parseInt") (StrLit "1")) MNothing)
      , ("toString 1 ++ \"!\"", Concat (App (Var "toString") (IntLit 1)) (StrLit "!"))
      , ("show 1 ++ \"!\"", Concat (App (Var "show") (IntLit 1)) (StrLit "!"))
      , ("fst (1, 2) + 3", Add (App (Var "fst") (TupleLit [IntLit 1, IntLit 2])) (IntLit 3))
      , ("snd (1, 2) + 3", Add (App (Var "snd") (TupleLit [IntLit 1, IntLit 2])) (IntLit 3))
      , ("Just 1 == Nothing", Eq (App (Var "Just") (IntLit 1)) MNothing)
      , ("Just 1 :: []", Cons (App (Var "Just") (IntLit 1)) (ListLit []))
      , ("Left \"bad\" == Right 1", Eq (App (Var "Left") (StrLit "bad")) (App (Var "Right") (IntLit 1)))
      , ("discard 1; 2", Seq (App (Var "discard") (IntLit 1)) (IntLit 2))
      , ("print 1; 2", Seq (App (Var "print") (IntLit 1)) (IntLit 2))
      , ( "take 1 [1, 2] ++ [3]"
        , Concat (App (App (Var "take") (IntLit 1)) (ListLit [IntLit 1, IntLit 2])) (ListLit [IntLit 3])
        )
      , ( "foldl (\\acc -> \\x -> acc + x) 0 [1, 2] + 3"
        , Add
            (App (App (App (Var "foldl")
              (Lambda "acc" Nothing (Lambda "x" Nothing (Add (Var "acc") (Var "x")))))
              (IntLit 0))
              (ListLit [IntLit 1, IntLit 2]))
            (IntLit 3)
        )
      ]

    it "allows a parenthesized operator expression as a built-in argument" $ do
      parseExpr "head ([1] ++ [2]) + 3"
        `shouldBe` Right
          (Add
            (App (Var "head") (Concat (ListLit [IntLit 1]) (ListLit [IntLit 2])))
            (IntLit 3))

    it "leaves later application arguments outside a unary built-in" $ do
      parseExpr "head xs fallback"
        `shouldBe` Right (App (App (Var "head") (Var "xs")) (Var "fallback"))
  
  describe "Conditional Parsing" $ do
    it "parses if-then-else" $ do
      parseExpr "if true then 1 else 2" 
        `shouldBe` Right (If (BoolLit True) (IntLit 1) (IntLit 2))
  
  describe "Complex Expression Parsing" $ do
    it "parses nested expressions" $ do
      parseExpr "if (5 > 3) then (2 + 3) else (4 * 1)"
        `shouldBe` Right (If (Gt (IntLit 5) (IntLit 3)) 
                            (Add (IntLit 2) (IntLit 3)) 
                            (Mul (IntLit 4) (IntLit 1)))

    it "parses chained record field access" $ do
      parseExpr "{outer = {inner = 7}}.outer.inner"
        `shouldBe` Right
          (RecordAccess
            (RecordAccess
              (RecordLit [("outer", RecordLit [("inner", IntLit 7)])])
              "outer")
            "inner")

    it "parses do blocks into sequencing expressions" $ do
      parseExpr "do { print \"hello\"; 42 }"
        `shouldBe` Right (Seq (App (Var "print") (StrLit "hello")) (IntLit 42))

    it "parses empty do blocks as unit" $ do
      parseExpr "do {}" `shouldBe` Right UnitLit

    it "parses do blocks with trailing semicolons" $ do
      parseExpr "do { print \"hello\"; 42; }"
        `shouldBe` Right (Seq (App (Var "print") (StrLit "hello")) (IntLit 42))
  
  describe "Parse Errors" $ do
    it "rejects empty input" $ do
      case parseExpr "" of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Should fail to parse empty input"
    
    it "rejects invalid syntax" $ do
      case parseExpr "1 + + 2" of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Should fail to parse invalid syntax"
    
    it "rejects incomplete expressions" $ do
      case parseExpr "if true then" of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Should fail to parse incomplete if"
