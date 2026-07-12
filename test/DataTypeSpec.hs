module DataTypeSpec where

import Test.Hspec
import Control.Exception (bracket)
import qualified Data.Map as Map
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, hPutStr, openTempFile)

import Evaluator (Value(..))
import qualified Evaluator as E
import Parser (parseExpr, parseProgram)
import TypeChecker (Type(..), TypeError(..), typeCheckProgram, typeCheckProgramWithDirIO)
import qualified ModuleSystem
import Syntax

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  tempDir <- getTemporaryDirectory
  bracket
    (do
        (path, handle) <- openTempFile tempDir "kai-data-type"
        hClose handle
        removeFile path
        createDirectory path
        return path)
    removeDirectoryRecursive
    action

spec :: Spec
spec = describe "Custom Data Types" $ do
  it "parses top-level data declarations" $ do
    let source = "data Option a = None | Some a\nSome 7"
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right (Program [TLData "Option" ["a"] [DataConstructor "None" [], DataConstructor "Some" [STVar "a"]], _]) ->
        return ()
      Right other -> expectationFailure $ "Unexpected AST: " ++ show other

  it "keeps adjacent custom constructor field types separate" $ do
    let source = "data Token = Empty | Pair Token Token"
    parseProgram source
      `shouldBe` Right
        (Program
          [ TLData "Token" []
              [ DataConstructor "Empty" []
              , DataConstructor "Pair" [STCustom "Token" [], STCustom "Token" []]
              ]
          ])

  it "uses parentheses to group an applied custom field type" $ do
    let source = "data Wrapped a b = Wrapped (Pair a b) b"
    parseProgram source
      `shouldBe` Right
        (Program
          [ TLData "Wrapped" ["a", "b"]
              [ DataConstructor "Wrapped"
                  [ STCustom "Pair" [STVar "a", STVar "b"]
                  , STVar "b"
                  ]
              ]
          ])

  it "preserves custom type and constructor names that begin with built-in names" $ do
    let source = unlines
          [ "data MaybeTree = NothingElse | Justly Int"
          , "data Wrapper = Wrap MaybeTree MaybeTree"
          , "case Wrap (Justly 4) NothingElse of Wrap (Justly value) NothingElse -> value"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right TInt
        E.evalProgram program `shouldReturn` Right (VInt 4)

  it "parses adjacent nullary constructors as separate pattern arguments" $ do
    let source = "case Pair Z Z of Pair Z Z -> 1 | Pair Z other -> 2"
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right (Program [TLExpr parsed]) ->
        parsed `shouldBe`
          Case
            (App (App (Var "Pair") (Var "Z")) (Var "Z"))
            [ (PConstructor "Pair" [PConstructor "Z" [], PConstructor "Z" []], IntLit 1)
            , (PConstructor "Pair" [PConstructor "Z" [], PVar "other"], IntLit 2)
            ]
      Right other -> expectationFailure $ "Unexpected AST: " ++ show other

  it "parses parenthesized nested constructor patterns without stealing sibling arguments" $ do
    let source = "case Node (Leaf 1) Empty of Node (Leaf value) Empty -> value | Empty -> 0"
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right (Program [TLExpr parsed]) ->
        parsed `shouldBe`
          Case
            (App (App (Var "Node") (App (Var "Leaf") (IntLit 1))) (Var "Empty"))
            [ ( PConstructor "Node"
                  [PConstructor "Leaf" [PVar "value"], PConstructor "Empty" []]
              , Var "value"
              )
            , (PConstructor "Empty" [], IntLit 0)
            ]
      Right other -> expectationFailure $ "Unexpected AST: " ++ show other

  it "type-checks and evaluates adjacent nullary constructor patterns" $ do
    let source = unlines
          [ "data Marker = Z"
          , "data PairValue = Pair Marker Marker"
          , "case Pair Z Z of Pair Z Z -> 1"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right TInt
        E.evalProgram program `shouldReturn` Right (VInt 1)

  it "type-checks and evaluates nested constructor patterns" $ do
    let source = unlines
          [ "data Tree = Empty | Leaf Int | Node Tree Tree"
          , "case Node (Leaf 7) Empty of Node (Leaf value) Empty -> value | _ -> 0"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right TInt
        E.evalProgram program `shouldReturn` Right (VInt 7)

  it "rejects conflicting constructor patterns when arguments share a type parameter" $ do
    let source = unlines
          [ "data Same a = Same a a"
          , "\\x -> case Same x x of Same 1 true -> 1 | _ -> 0"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program ->
        typeCheckProgram program `shouldBe` Left (UnificationError TInt TBool)

  it "accepts consistent constructor patterns when arguments share a type parameter" $ do
    let source = unlines
          [ "data Same a = Same a a"
          , "\\x -> case Same x x of Same 1 2 -> 1 | _ -> 0"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program ->
        typeCheckProgram program `shouldBe` Right (TFun TInt TInt)

  it "evaluates constructor pattern matching" $ do
    let source = "data Option a = None | Some a\ncase Some 7 of None -> 0 | Some x -> x"
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right TInt
        E.evalProgram program `shouldReturn` Right (VInt 7)

  it "treats constructors as polymorphic first-class functions" $ do
    let source = "data Box a = Box a\nlet make = Box\n(make 1, make true)"
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right (TTuple [TCustom "Box" [TInt], TCustom "Box" [TBool]])
        E.evalProgram program `shouldReturn`
          Right (VTuple [VData "Box" [VInt 1], VData "Box" [VBool True]])

  it "maps bare and partially applied constructors in pure and IO evaluation" $ do
    let source = unlines
          [ "data Box a = Box a"
          , "data Pair a b = Pair a b"
          , "(map Box [1, 2], map (Pair 1) [true, false])"
          ]
        expected = VTuple
          [ VList [VData "Box" [VInt 1], VData "Box" [VInt 2]]
          , VList
              [ VData "Pair" [VInt 1, VBool True]
              , VData "Pair" [VInt 1, VBool False]
              ]
          ]
        runtimeEnv = Map.fromList
          [ ("Box", VConstructor "Box" 1 [])
          , ("Pair", VConstructor "Pair" 2 [])
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe`
          Right
            (TTuple
              [ TList (TCustom "Box" [TInt])
              , TList (TCustom "Pair" [TInt, TBool])
              ])
        E.evalProgram program `shouldReturn` Right expected
    case parseExpr "(map Box [1, 2], map (Pair 1) [true, false])" of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right expression -> do
        E.evalPureWithEnv runtimeEnv expression `shouldBe` Right expected
        E.evalWithEnv runtimeEnv expression `shouldReturn` Right expected

  it "folds with a multi-argument constructor in pure and IO evaluation" $ do
    let source = unlines
          [ "data Chain a = Empty | Link (Chain a) a"
          , "foldl Link Empty [1, 2]"
          ]
        empty = VData "Empty" []
        firstLink = VData "Link" [empty, VInt 1]
        expected = VData "Link" [firstLink, VInt 2]
        runtimeEnv = Map.fromList
          [ ("Empty", empty)
          , ("Link", VConstructor "Link" 2 [])
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right (TCustom "Chain" [TInt])
        E.evalProgram program `shouldReturn` Right expected
    case parseExpr "foldl Link Empty [1, 2]" of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right expression -> do
        E.evalPureWithEnv runtimeEnv expression `shouldBe` Right expected
        E.evalWithEnv runtimeEnv expression `shouldReturn` Right expected

  it "reports cyclic constructor fixed points as Kai runtime errors" $ do
    let source = "data Loop = Loop Loop\nfix Loop"
        runtimeEnv = Map.singleton "Loop" (VConstructor "Loop" 1 [])
        expected = Left (E.TypeError "Fixpoint forced before initialization")
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right (TCustom "Loop" [])
        E.evalProgram program `shouldReturn` expected
    case parseExpr "fix Loop" of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right expression -> do
        E.evalPureWithEnv runtimeEnv expression `shouldBe` expected
        E.evalWithEnv runtimeEnv expression `shouldReturn` expected

  it "supports partial application of multi-argument constructors" $ do
    let source = unlines
          [ "data Pair a b = Pair a b"
          , "let withOne = Pair 1"
          , "case withOne true of Pair x y -> if y then x else 0"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right TInt
        E.evalProgram program `shouldReturn` Right (VInt 1)

  it "supports recursive custom data types in letrec programs" $ do
    let source = unlines
          [ "data Tree a = Leaf a | Node (Tree a) (Tree a)"
          , "letrec sumTree = \\tree -> case tree of Leaf x -> x | Node left right -> sumTree left + sumTree right"
          , "sumTree (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right TInt
        E.evalProgram program `shouldReturn` Right (VInt 6)

  it "compares custom data values structurally, including nested fields" $ do
    let source = unlines
          [ "data Tree a = Leaf a | Node (Tree a) (Tree a)"
          , "let first = Node (Leaf 1) (Leaf 2)"
          , "let same = Node (Leaf 1) (Leaf 2)"
          , "let different = Node (Leaf 1) (Leaf 3)"
          , "(first == same, first == different)"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right (TTuple [TBool, TBool])
        E.evalProgram program `shouldReturn`
          Right (VTuple [VBool True, VBool False])

  it "compares nullary and distinct constructors of the same custom type" $ do
    let source = unlines
          [ "data Option a = None | Some a"
          , "let none : Option Int = None"
          , "(none == None, none == Some 1, Some 1 == Some 1)"
          ]
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right (TTuple [TBool, TBool, TBool])
        E.evalProgram program `shouldReturn`
          Right (VTuple [VBool True, VBool False, VBool True])

  it "rejects direct and nested constructor callables in equality" $ do
    let source = unlines
          [ "data Box a = Box a"
          , "data Holder a = Holder a"
          , "(Box == Box, Holder Box == Holder Box)"
          ]
        runtimeEnv = Map.fromList
          [ ("Box", VConstructor "Box" 1 [])
          , ("Holder", VConstructor "Holder" 1 [])
          ]
        expected = Left (E.TypeError "Equality is not defined for callable or recursive reference values")
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right (TTuple [TBool, TBool])
        E.evalProgram program `shouldReturn` expected
    let assertRuntime sourceExpression = case parseExpr sourceExpression of
          Left err -> expectationFailure $ "Parse error: " ++ show err
          Right expression -> do
            E.evalPureWithEnv runtimeEnv expression `shouldBe` expected
            E.evalWithEnv runtimeEnv expression `shouldReturn` expected
    assertRuntime "Box == Box"
    assertRuntime "Holder Box == Holder Box"

  it "rejects ordinary functions nested inside custom data equality" $ do
    let source = unlines
          [ "data Holder a = Holder a"
          , "let identity = \\value -> value"
          , "Holder identity == Holder identity"
          ]
        runtimeEnv = Map.singleton "Holder" (VConstructor "Holder" 1 [])
        expected = Left (E.TypeError "Equality is not defined for callable or recursive reference values")
    case parseProgram source of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right program -> do
        typeCheckProgram program `shouldBe` Right TBool
        E.evalProgram program `shouldReturn` expected
    case parseExpr "let identity = \\value -> value in Holder identity == Holder identity" of
      Left err -> expectationFailure $ "Parse error: " ++ show err
      Right expression -> do
        E.evalPureWithEnv runtimeEnv expression `shouldBe` expected
        E.evalWithEnv runtimeEnv expression `shouldReturn` expected

  it "imports constructors from modules and uses them in patterns" $ do
    withTempDir $ \dir -> do
      writeFile (dir </> "Choice.kai") $ unlines
        [ "data Choice a = Pick a | Skip"
        , "export Pick, Skip"
        ]
      let source = unlines
            [ "import Choice"
            , "let choose = \\value -> case value of Pick x -> x | Skip -> 0"
            , "choose (Pick 9)"
            ]
      case parseProgram source of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right program -> do
          typeResult <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO dir program
          typeResult `shouldBe` Right TInt
          E.evalProgramWithEnv Map.empty dir program `shouldReturn` Right (VInt 9)
