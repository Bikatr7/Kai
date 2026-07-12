module DataDeclarations
  ( constructorResultType
  , constructorScheme
  , dataConstructorsTypeEnv
  , dataConstructorsValueEnv
  ) where

import qualified Data.Map as Map
import Syntax
import Evaluator.Types
import TypeChecker.Types

constructorResultType :: String -> [String] -> Type
constructorResultType typeName typeVars = TCustom typeName (map TVar typeVars)

constructorScheme :: String -> [String] -> DataConstructor -> Scheme
constructorScheme typeName typeVars (DataConstructor constructorName argTypes) =
  let resultType = constructorResultType typeName typeVars
      constructorType = foldr (TFun . syntaxTypeToType) resultType argTypes
  in constructorName `seq` Forall typeVars constructorType

dataConstructorsTypeEnv :: String -> [String] -> [DataConstructor] -> TypeEnv
dataConstructorsTypeEnv typeName typeVars constructors =
  Map.fromList
    [ (constructorName, constructorScheme typeName typeVars constructorDecl)
    | constructorDecl@(DataConstructor constructorName _) <- constructors
    ]

dataConstructorsValueEnv :: [DataConstructor] -> Env
dataConstructorsValueEnv constructors =
  Map.fromList
    [ (constructorName, constructorValue constructorDecl)
    | constructorDecl@(DataConstructor constructorName _) <- constructors
    ]
  where
    constructorValue (DataConstructor constructorName argTypes)
      | null argTypes = VData constructorName []
      | otherwise = VConstructor constructorName (length argTypes) []
