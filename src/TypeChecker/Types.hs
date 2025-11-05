module TypeChecker.Types where

import Syntax
import qualified Data.Map as Map
import Control.Monad.State
import Data.Bifunctor (second)

data Type
  = TInt
  | TBool
  | TString
  | TUnit
  | TFun Type Type
  | TVar String  -- Type variables for inference
  | TMaybe Type  -- Maybe type for optional values
  | TEither Type Type  -- Either type for error handling
  | TList Type
  | TRecord (Map.Map String Type)
  | TTuple [Type]
  deriving (Show, Eq)

type TypeEnv = Map.Map String Type

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
  deriving (Show, Eq)

-- Type inference monad
type TypeInfer = StateT Int (Either TypeError)

-- Convert syntax types to internal types
syntaxTypeToType :: SyntaxType -> Type
syntaxTypeToType STInt = TInt
syntaxTypeToType STBool = TBool
syntaxTypeToType STString = TString
syntaxTypeToType STUnit = TUnit
syntaxTypeToType (STFun t1 t2) = TFun (syntaxTypeToType t1) (syntaxTypeToType t2)
syntaxTypeToType (STMaybe t) = TMaybe (syntaxTypeToType t)
syntaxTypeToType (STEither t1 t2) = TEither (syntaxTypeToType t1) (syntaxTypeToType t2)
syntaxTypeToType (STList t) = TList (syntaxTypeToType t)
syntaxTypeToType (STRecord fields) = TRecord (Map.fromList (map (second syntaxTypeToType) fields))
syntaxTypeToType (STTuple ts) = TTuple (map syntaxTypeToType ts)
