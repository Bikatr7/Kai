module ModuleSpec where

import Test.Hspec
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
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
  describe "Module System" $ do
    beforeAll setupTestModules $ do
      afterAll (const cleanupTestModules) $ do
        describe "Basic Module Imports" $ do
          it "imports and uses a simple function" $ do
            let program = "import Math\nadd 2 3"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 5)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports polymorphic definitions across distinct uses" $ do
            let program = "import Poly\n(id 1, id true)"
            case parseProgram program of
              Right ast -> do
                typeResult <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO "test_modules" ast
                typeResult `shouldBe` Right (TTuple [TInt, TBool])
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VTuple [VInt 1, VBool True])
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports and uses multiple functions from a module" $ do
            let program = "import Math\nmultiply (add 2 3) 4"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 20)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports multiple modules" $ do
            let program = "import Math\nimport Util\nadd (double 5) (triple 3)"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 19)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports and uses values from modules" $ do
            let program = "import Constants\npi"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                -- Constants module has pi as a string representation
                case result of
                  Right (VStr s) -> s `shouldBe` "3.14159"
                  Right other -> expectationFailure $ "Expected VStr, got " ++ show other
                  Left err -> expectationFailure $ "Runtime error: " ++ show err
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Circular Import Detection" $ do
          it "detects circular imports and reports error" $ do
            let program = "import CircularA\n\"test\""
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                case result of
                  Left (E.TypeError err) -> err `shouldContain` "Circular import detected"
                  Left otherErr -> expectationFailure $ "Expected TypeError with circular import message, got: " ++ show otherErr
                  Right _ -> expectationFailure "Expected circular import error but evaluation succeeded"
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "provides helpful error message with loading stack" $ do
            let program = "import CircularA\n\"test\""
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                case result of
                  Left (E.TypeError err) -> do
                    err `shouldContain` "CircularA"
                    err `shouldContain` "CircularB"
                    err `shouldContain` "->"
                  Left _ -> expectationFailure "Expected circular import error"
                  Right _ -> expectationFailure "Expected circular import error"
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Explicit Exports" $ do
          it "exports only explicitly exported names" $ do
            let program = "import Exported\npublic"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 42)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "does not export non-exported names" $ do
            let program = "import Exported\nprivate"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                case result of
                  Left (E.UnboundVariable "private") -> return ()
                  Left otherErr -> expectationFailure $ "Expected UnboundVariable error, got: " ++ show otherErr
                  Right _ -> expectationFailure "Expected UnboundVariable error but evaluation succeeded"
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "exports everything when no export statement" $ do
            let program = "import NoExports\navailable"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 99)
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Module Scoping" $ do
          it "imported definitions are available to local definitions" $ do
            let program = "import Math\nlet result = add 10 20\nresult"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 30)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "local definitions can shadow imported ones" $ do
            let program = "import Math\nlet add = \\x -> \\y -> x - y\nadd 10 5"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 5)  -- Local add shadows imported one
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Recursive Functions in Modules" $ do
          it "imports and uses recursive functions" $ do
            let program = "import Recursive\nfactorial 5"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 120)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "imports mutually recursive functions" $ do
            let program = "import Mutual\nisEven 4"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VBool True)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "type-checks imported recursive functions with their concrete types" $ do
            let program = "import Recursive\nfactorial true"
            case parseProgram program of
              Right ast -> do
                result <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO "test_modules" ast
                case result of
                  Left _ -> return ()
                  Right ty -> expectationFailure $ "Expected type error, got: " ++ show ty
              Left err -> expectationFailure $ "Parse error: " ++ show err

        describe "Complex Module Usage" $ do
          it "uses imported functions in complex expressions" $ do
            let program = "import Math\nlet x = add 1 2\nlet y = multiply x 3\ny"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 9)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "combines multiple imported modules" $ do
            let program = "import Math\nimport Util\nmultiply (double 2) (triple 3)"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 36)
              Left err -> expectationFailure $ "Parse error: " ++ show err

          it "uses imported functions with data structures" $ do
            let program = "import Math\nlet nums = [1, 2, 3]\nfoldl add 0 nums"
            case parseProgram program of
              Right ast -> do
                result <- evalProgramWithEnv Map.empty "test_modules" ast
                result `shouldBe` Right (VInt 6)
              Left err -> expectationFailure $ "Parse error: " ++ show err

setupTestModules :: IO ()
setupTestModules = do
  createDirectoryIfMissing True "test_modules"
  
  -- Math.kai
  writeFile "test_modules/Math.kai" $ unlines
    [ "let add = \\x -> \\y -> x + y"
    , "let multiply = \\x -> \\y -> x * y"
    , "let subtract = \\x -> \\y -> x - y"
    ]
  
  -- Util.kai
  writeFile "test_modules/Util.kai" $ unlines
    [ "let double = \\x -> x * 2"
    , "let triple = \\x -> x * 3"
    , "let square = \\x -> x * x"
    ]

  -- Poly.kai
  writeFile "test_modules/Poly.kai" $ unlines
    [ "let id = \\x -> x"
    ]
  
  -- Constants.kai
  writeFile "test_modules/Constants.kai" $ unlines
    [ "let pi = \"3.14159\""
    , "let e = \"2.71828\""
    ]
  
  -- Recursive.kai
  writeFile "test_modules/Recursive.kai" $ unlines
    [ "letrec factorial = \\n -> if n == 0 then 1 else n * factorial (n - 1)"
    ]
  
  -- Mutual.kai
  writeFile "test_modules/Mutual.kai" $ unlines
    [ "letrec isEven = \\n -> if n == 0 then true else isOdd (n - 1)"
    , "letrec isOdd = \\n -> if n == 0 then false else isEven (n - 1)"
    ]

  -- CircularA.kai (imports CircularB)
  writeFile "test_modules/CircularA.kai" $ unlines
    [ "import CircularB"
    , "let a = \"from A\""
    ]

  -- CircularB.kai (imports CircularA - creates cycle)
  writeFile "test_modules/CircularB.kai" $ unlines
    [ "import CircularA"
    , "let b = \"from B\""
    ]

  -- Exported.kai (explicit exports)
  writeFile "test_modules/Exported.kai" $ unlines
    [ "let public = 42"
    , "let private = 99"
    , "export public"
    ]

  -- NoExports.kai (no explicit exports - should export everything)
  writeFile "test_modules/NoExports.kai" $ unlines
    [ "let available = 99"
    ]

cleanupTestModules :: IO ()
cleanupTestModules = do
  removeDirectoryRecursive "test_modules"
  return ()
