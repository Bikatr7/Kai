module TypeChecker.Types where

import Syntax
import qualified Data.Map as Map
import Control.Monad.State
import Data.Bifunctor (second)
import Control.DeepSeq

data Type
  = TInt
  | TBool
  | TString
  | TUnit
  | TFun Type Type
  | TVar String  -- Type variables for inference
  | TCustom String [Type]
  | TMaybe Type  -- Maybe type for optional values
  | TEither Type Type  -- Either type for error handling
  | TList Type
  | TRecord (Map.Map String Type)
  | TTuple [Type]
  deriving (Show, Eq)

data Scheme = Forall [String] Type
  deriving (Show, Eq)

type TypeEnv = Map.Map String Scheme

type InferFunc = TypeEnv -> Expr -> TypeInfer (Substitution, Type)

-- Substitution maps type variables to types
type Substitution = Map.Map String Type

data TypeError
  = TypeMismatch Type Type
  | ExpectedInt Type
  | ExpectedBool Type
  | ExpectedFunction Type
  | UnboundVariable String
  | InfiniteType String Type
  | UnificationError Type Type
  | RecordFieldMismatch String
  | InvalidWildcard String
  | GeneralTypeError String
  | DuplicatePatternBinding String
  | DuplicateRecordField String
  | InvalidDataDeclaration String
  | ConstructorPatternArity String Int Int
  deriving (Show, Eq)

-- Type inference monad
type TypeInfer = StateT Int (Either TypeError)

-- Convert syntax types to internal types
syntaxTypeToType :: SyntaxType -> Type
syntaxTypeToType STInt = TInt
syntaxTypeToType STBool = TBool
syntaxTypeToType STString = TString
syntaxTypeToType STUnit = TUnit
syntaxTypeToType (STVar name) = TVar name
syntaxTypeToType (STFun t1 t2) = TFun (syntaxTypeToType t1) (syntaxTypeToType t2)
syntaxTypeToType (STMaybe t) = TMaybe (syntaxTypeToType t)
syntaxTypeToType (STEither t1 t2) = TEither (syntaxTypeToType t1) (syntaxTypeToType t2)
syntaxTypeToType (STList t) = TList (syntaxTypeToType t)
syntaxTypeToType (STRecord fields) = TRecord (Map.fromList (map (second syntaxTypeToType) fields))
syntaxTypeToType (STTuple ts) = TTuple (map syntaxTypeToType ts)
syntaxTypeToType (STCustom name args) = TCustom name (map syntaxTypeToType args)

monoScheme :: Type -> Scheme
monoScheme = Forall []

schemeType :: Scheme -> Type
schemeType (Forall _ ty) = ty

instance NFData Type where
  rnf TInt = ()
  rnf TBool = ()
  rnf TString = ()
  rnf TUnit = ()
  rnf (TFun t1 t2) = rnf t1 `seq` rnf t2
  rnf (TVar s) = rnf s
  rnf (TCustom name args) = rnf name `seq` rnf args
  rnf (TMaybe t) = rnf t
  rnf (TEither t1 t2) = rnf t1 `seq` rnf t2
  rnf (TList t) = rnf t
  rnf (TRecord m) = rnf m
  rnf (TTuple ts) = rnf ts

instance NFData Scheme where
  rnf (Forall vars ty) = rnf vars `seq` rnf ty

-- Declaration metadata cannot be addressed by a source-language identifier.
dataTypeKey :: String -> String
dataTypeKey name = "@type:" ++ name

validateSyntaxType :: TypeEnv -> SyntaxType -> Either TypeError Type
validateSyntaxType env syntax = do
  validate syntax
  return (syntaxTypeToType syntax)
  where
    validate (STCustom name args) = case Map.lookup (dataTypeKey name) env of
      Nothing -> Left $ InvalidDataDeclaration ("Unknown type: " ++ name)
      Just (Forall vars _) | length vars /= length args ->
        Left $ InvalidDataDeclaration ("Wrong type arity: " ++ name)
      Just _ -> mapM_ validate args
    validate (STFun a b) = validate a >> validate b
    validate (STMaybe a) = validate a
    validate (STEither a b) = validate a >> validate b
    validate (STList a) = validate a
    validate (STTuple ts) = mapM_ validate ts
    validate (STRecord fields) = do
      uniqueFields (map fst fields)
      mapM_ (validate . snd) fields
    validate _ = Right ()
    uniqueFields [] = Right ()
    uniqueFields (name:names)
      | name `elem` names = Left (DuplicateRecordField name)
      | otherwise = uniqueFields names

constructorKey :: String -> String
constructorKey name = "@constructor:" ++ name
