module ScriptCheck (checkScriptFile) where

import qualified Data.Map as Map
import Data.List (stripPrefix)
import Data.Char (isSpace)
import Parser (parseExpr, parseLocatedProgram)
import Evaluator (evalPure, evalProgramWithEnv, stripRuntimeLocation)
import TypeChecker (typeCheck, typeCheckProgramWithDirIO)
import Diagnostics (renderTypeError)
import Text.Megaparsec (errorBundlePretty)
import qualified ModuleSystem
import SourceIO (readSourceFile)
import System.FilePath (takeDirectory)

-- The release verifier and Hspec use this same value/type contract. Expected
-- expressions are evaluated purely, so fixture assertions cannot perform IO.
checkScriptFile :: FilePath -> IO (Either String ())
checkScriptFile path = do
  source <- readSourceFile path
  case source of
    Left err -> return $ Left $ "IO error: " ++ show err
    Right content -> case (directive "expect" content, parseLocatedProgram path content) of
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left $ "Parse error: " ++ errorBundlePretty err
      (Right expected, Right program) -> do
        checked <- typeCheckProgramWithDirIO ModuleSystem.loadModuleTypeEnvIO (takeDirectory path) program
        case checked of
          Left err -> return $ Left $ renderTypeError err
          Right ty -> case optionalDirective "expect-type" content of
            Left err -> return $ Left err
            Right expectedType | maybe False (/= show ty) expectedType ->
              return $ Left $ "Expected type " ++ show expectedType ++ ", got " ++ show ty
            Right _ -> do
              result <- evalProgramWithEnv Map.empty (takeDirectory path) program
              return $ case stripPrefix "error " expected of
                Just errorName -> case result of
                  Left err | show (stripRuntimeLocation err) == errorName -> Right ()
                  _ -> Left $ "Expected error " ++ errorName ++ ", got " ++ show result
                Nothing -> do
                  expression <- either (Left . ("Bad expectation: " ++) . show) Right (parseExpr expected)
                  _ <- either (Left . ("Bad expectation type: " ++) . show) Right (typeCheck expression)
                  value <- either (Left . ("Bad expectation: " ++) . show) Right (evalPure expression)
                  case result of
                    Right actual | actual == value -> Right ()
                    _ -> Left $ "Expected " ++ show value ++ ", got " ++ show result

optionalDirective :: String -> String -> Either String (Maybe String)
optionalDirective name source = case [trim value | line <- lines source,
                                      Just value <- [stripPrefix ("// " ++ name ++ ":") line]] of
  [] -> Right Nothing
  [value] | not (null value) -> Right $ Just value
  _ -> Left $ "Invalid or duplicate // " ++ name ++ ": directive"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

directive :: String -> String -> Either String String
directive name source = do
  value <- optionalDirective name source
  maybe (Left $ "Missing // " ++ name ++ ": directive") Right value
