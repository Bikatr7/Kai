module ModuleSpec where

import qualified TypeChecker as T
import Test.Hspec
import ExampleSpec (withTempDir)
import System.FilePath ((</>))
import System.IO (writeFile)

import Parser
import Evaluator (evalProgramWithEnv, Value(..))
import qualified Evaluator as E
import TypeChecker (Type(..), typeCheckProgramWithDirIO)
import Syntax
import qualified Data.Map as Map
import qualified ModuleSystem

spec :: Spec
spec = do
  around withTestModules $ describe "Module System" $ do
        describe "Basic Module Imports" $ do
          it "imports and uses a simple function" $ \directory -> do
            let program = "import Math\nadd 2 3"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 5)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports polymorphic definitions across distinct uses" $ \directory -> do
            let program = "import Poly\n(id 1, id true)"
            case parseProgram program of
              Right ast -> do
                typeResult <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO directory ast
                typeResult `shouldBe` Right (TTuple [TInt, TBool])
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VTuple [VInt 1, VBool True])
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports and uses multiple functions from a module" $ \directory -> do
            let program = "import Math\nmultiply (add 2 3) 4"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 20)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports multiple modules" $ \directory -> do
            let program = "import Math\nimport Util\nadd (double 5) (triple 3)"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 19)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports and uses values from modules" $ \directory -> do
            let program = "import Constants\npi"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                -- Constants module has pi as a string representation
                case result of
                  Right (VStr s) -> s `shouldBe` "3.14159"
                  Right other -> expectationFailure $ "Expected VStr, got " ++ show other
                  Left err -> expectationFailure $ "Runtime error: " ++ show err
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Circular Import Detection" $ do
          it "detects circular imports and reports error" $ \directory -> do
            let program = "import CircularA\n\"test\""
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                case result of
                  Left (E.TypeError err) -> err `shouldContain` "Circular import detected"
                  Left otherErr -> expectationFailure $ "Expected TypeError with circular import message, got: " ++ show otherErr
                  Right _ -> expectationFailure "Expected circular import error but evaluation succeeded"
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "provides helpful error message with loading stack" $ \directory -> do
            let program = "import CircularA\n\"test\""
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                case result of
                  Left (E.TypeError err) -> do
                    err `shouldContain` "CircularA"
                    err `shouldContain` "CircularB"
                    err `shouldContain` "->"
                  Left _ -> expectationFailure "Expected circular import error"
                  Right _ -> expectationFailure "Expected circular import error"
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Explicit Exports" $ do
          it "exports only explicitly exported names" $ \directory -> do
            let program = "import Exported\npublic"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 42)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "does not export non-exported names" $ \directory -> do
            let program = "import Exported\nprivate"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                case result of
                  Left (E.UnboundVariable "private") -> return ()
                  Left otherErr -> expectationFailure $ "Expected UnboundVariable error, got: " ++ show otherErr
                  Right _ -> expectationFailure "Expected UnboundVariable error but evaluation succeeded"
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "exports everything when no export statement" $ \directory -> do
            let program = "import NoExports\navailable"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 99)
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Module Scoping" $ do
          it "imported definitions are available to local definitions" $ \directory -> do
            let program = "import Math\nlet result = add 10 20\nresult"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 30)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "local definitions can shadow imported ones" $ \directory -> do
            let program = "import Math\nlet add = \\x -> \\y -> x - y\nadd 10 5"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 5)  -- Local add shadows imported one
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Recursive Functions in Modules" $ do
          it "imports and uses recursive functions" $ \directory -> do
            let program = "import Recursive\nfactorial 5"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 120)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports mutually recursive functions" $ \directory -> do
            let program = "import Mutual\nisEven 4"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VBool True)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "type-checks imported recursive functions with their concrete types" $ \directory -> do
            let program = "import Recursive\nfactorial true"
            case parseProgram program of
              Right ast -> do
                result <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO directory ast
                result `shouldBe` Left (T.UnificationError T.TInt T.TBool)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports annotated polymorphically recursive functions" $ \directory -> do
            let program = "import PolyRec\nnestedLayers 2 [1, 2, 3]"
            case parseProgram program of
              Right ast -> do
                typeResult <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO directory ast
                typeResult `shouldBe` Right TInt
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 3)
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Complex Module Usage" $ do
          it "uses imported functions in complex expressions" $ \directory -> do
            let program = "import Math\nlet x = add 1 2\nlet y = multiply x 3\ny"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 9)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "combines multiple imported modules" $ \directory -> do
            let program = "import Math\nimport Util\nmultiply (double 2) (triple 3)"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 36)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "uses imported functions with data structures" $ \directory -> do
            let program = "import Math\nlet nums = [1, 2, 3]\nfoldl add 0 nums"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty directory ast
                result `shouldBe` Right (VInt 6)
              Left err -> expectationFailure $ "Parse error: " ++ show err

withTestModules :: (FilePath -> IO ()) -> IO ()
withTestModules action = withTempDir $ \directory -> do
  setupTestModules directory
  action directory

setupTestModules :: FilePath -> IO ()
setupTestModules directory = do
  
  -- Math.kai
  writeFile (directory </> "Math.kai") $ unlines
    [ "let add = \\x -> \\y -> x + y"
    , "let multiply = \\x -> \\y -> x * y"
    , "let subtract = \\x -> \\y -> x - y"
    ]
  
  -- Util.kai
  writeFile (directory </> "Util.kai") $ unlines
    [ "let double = \\x -> x * 2"
    , "let triple = \\x -> x * 3"
    , "let square = \\x -> x * x"
    ]

  -- Poly.kai
  writeFile (directory </> "Poly.kai") $ unlines
    [ "let id = \\x -> x"
    ]
  
  -- Constants.kai
  writeFile (directory </> "Constants.kai") $ unlines
    [ "let pi = \"3.14159\""
    , "let e = \"2.71828\""
    ]
  
  -- Recursive.kai
  writeFile (directory </> "Recursive.kai") $ unlines
    [ "letrec factorial = \\n -> if n == 0 then 1 else n * factorial (n - 1)"
    ]

  -- PolyRec.kai
  writeFile (directory </> "PolyRec.kai") $ unlines
    [ "letrec nestedLayers : Int -> [a] -> Int = \\depth -> \\xs -> if depth == 0 then length xs else 1 + nestedLayers (depth - 1) [xs]"
    ]
  
  -- Mutual.kai
  writeFile (directory </> "Mutual.kai") $ unlines
    [ "letrec isEven = \\n -> if n == 0 then true else isOdd (n - 1)"
    , "letrec isOdd = \\n -> if n == 0 then false else isEven (n - 1)"
    ]

  -- CircularA.kai (imports CircularB)
  writeFile (directory </> "CircularA.kai") $ unlines
    [ "import CircularB"
    , "let a = \"from A\""
    ]

  -- CircularB.kai (imports CircularA - creates cycle)
  writeFile (directory </> "CircularB.kai") $ unlines
    [ "import CircularA"
    , "let b = \"from B\""
    ]

  -- Exported.kai (explicit exports)
  writeFile (directory </> "Exported.kai") $ unlines
    [ "let public = 42"
    , "let private = 99"
    , "export public"
    ]

  -- NoExports.kai (no explicit exports - should export everything)
  writeFile (directory </> "NoExports.kai") $ unlines
    [ "let available = 99"
    ]


  return ()
