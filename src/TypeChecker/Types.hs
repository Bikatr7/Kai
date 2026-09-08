module TypeChecker.Types where

import Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Bifunctor (second)
import Control.DeepSeq
import StandardData (standardTypeArity)

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
  | TOpenRecord (Map.Map String Type) Type
  | TRowVar String
  | TRowEmpty
  | TTuple [Type]
  | TQualified [Predicate] Type
  deriving (Show, Eq)

data Predicate = Equality Type | Appendable Type | PredicateAt SourceSpan Predicate

unlocatedPredicate :: Predicate -> Predicate
unlocatedPredicate (PredicateAt _ predicate) = unlocatedPredicate predicate
unlocatedPredicate predicate = predicate

instance Eq Predicate where
  left == right = case (unlocatedPredicate left,unlocatedPredicate right) of
    (Equality a,Equality b) -> a == b
    (Appendable a,Appendable b) -> a == b
    _ -> False

instance Show Predicate where
  showsPrec precedence predicate = case unlocatedPredicate predicate of
    Equality ty -> showParen (precedence > 10) (showString "Equality " . showsPrec 11 ty)
    Appendable ty -> showParen (precedence > 10) (showString "Appendable " . showsPrec 11 ty)
    PredicateAt {} -> error "unlocatedPredicate must remove source wrappers"

predicateType :: Predicate -> Type
predicateType (PredicateAt _ predicate) = predicateType predicate
predicateType (Equality ty) = ty
predicateType (Appendable ty) = ty

qualifiedType :: [Predicate] -> Type -> Type
qualifiedType [] ty = ty
qualifiedType predicates ty = TQualified predicates ty

splitQualified :: Type -> ([Predicate], Type)
splitQualified (TQualified predicates ty) = (predicates, ty)
splitQualified ty = ([], ty)

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
  | KindMismatch Type Type
  | ConflictingVariableKind String
  | UnsatisfiedConstraint Predicate
  | AmbiguousConstraint Predicate
  | MissingConstraint Predicate
  | NonExhaustivePatterns String
  | TypeAt SourceSpan TypeError
  | TypeImport SourceSpan String TypeError
  deriving (Show, Eq)

data TypeWarning = UnreachableAlternative Int | InModule FilePath TypeWarning | WarningAt SourceSpan TypeWarning
  deriving (Show, Eq)

-- Type inference monad
data InferState = InferState
  { nextTypeVariable :: Int
  , pendingPredicates :: [Predicate]
  , inferredWarnings :: [TypeWarning]
  }

initialInferState :: Int -> InferState
initialInferState seed = InferState seed [] []

type TypeInfer = StateT InferState (Either TypeError)

-- Convert syntax types to internal types
syntaxTypeToType :: SyntaxType -> Type
syntaxTypeToType (STQualified predicates ty) = qualifiedType
  [if name == "Eq" then Equality (syntaxTypeToType arg) else Appendable (syntaxTypeToType arg) | (name,arg) <- predicates]
  (syntaxTypeToType ty)
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
syntaxTypeToType (STRecordRow fields row) = TOpenRecord
  (Map.fromList (map (second syntaxTypeToType) fields)) (TRowVar row)
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
  rnf (TOpenRecord fields row) = rnf fields `seq` rnf row
  rnf (TRowVar row) = rnf row
  rnf TRowEmpty = ()
  rnf (TTuple ts) = rnf ts
  rnf (TQualified ps ty) = rnf ps `seq` rnf ty

instance NFData Predicate where
  rnf (PredicateAt location predicate) = rnf location `seq` rnf predicate
  rnf predicate = rnf (predicateType predicate)

instance NFData Scheme where
  rnf (Forall vars ty) = rnf vars `seq` rnf ty

-- Declaration metadata cannot be addressed by a source-language identifier.
dataTypeKey :: String -> String
dataTypeKey name = "@type:" ++ name

validateSyntaxType :: TypeEnv -> SyntaxType -> Either TypeError Type
validateSyntaxType env syntax = do
  case syntax of
    STQualified predicates ty -> do
      mapM_ validatePredicate predicates
      validate ty
    _ -> validate syntax
  let (values, rows) = variableKinds syntax
  case Set.lookupMin (Set.intersection values rows) of
    Just name -> Left (ConflictingVariableKind name)
    Nothing -> pure ()
  return (syntaxTypeToType syntax)
  where
    validatePredicate (name,ty)
      | name `elem` ["Eq", "Append"] = validate ty
      | otherwise = Left $ GeneralTypeError ("Unknown constraint: " ++ name)
    validate (STQualified _ _) = Left $ GeneralTypeError "Qualified types are only allowed on whole expressions or bindings"
    validate (STCustom name args) = case Map.lookup (dataTypeKey name) env of
      Nothing -> case standardTypeArity name of
        Just arity | length args == arity -> mapM_ validate args
        Just _ -> Left $ InvalidDataDeclaration ("Wrong type arity: " ++ name)
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
    validate (STRecordRow fields _) = validate (STRecord fields)
    validate _ = Right ()
    uniqueFields [] = Right ()
    uniqueFields (name:names)
      | name `elem` names = Left (DuplicateRecordField name)
      | otherwise = uniqueFields names

variableKinds :: SyntaxType -> (Set.Set String, Set.Set String)
variableKinds syntax = case syntax of
  STQualified predicates ty -> manyKinds (ty : map snd predicates)
  STVar name -> (Set.singleton name, Set.empty)
  STRecordRow fields row -> mergeKinds (Set.empty, Set.singleton row) (manyKinds (map snd fields))
  STRecord fields -> manyKinds (map snd fields)
  STFun a b -> manyKinds [a,b]
  STMaybe a -> variableKinds a
  STEither a b -> manyKinds [a,b]
  STList a -> variableKinds a
  STTuple ts -> manyKinds ts
  STCustom _ ts -> manyKinds ts
  _ -> (Set.empty, Set.empty)
  where
    manyKinds = foldr (mergeKinds . variableKinds) (Set.empty,Set.empty)
    mergeKinds (a,b) (c,d) = (Set.union a c, Set.union b d)

-- Keep row tails explicit until their substitutions are known. Flattening checks
-- duplicate labels instead of silently discarding a field introduced by a tail.
recordRow :: Type -> Either TypeError (Map.Map String Type, Type)
recordRow (TRecord fields) = Right (fields, TRowEmpty)
recordRow (TOpenRecord fields tailType) = do
  (more, tailRow) <- recordRow tailType
  case Map.lookupMin (Map.intersection fields more) of
    Just (name,_) -> Left (DuplicateRecordField name)
    Nothing -> Right (Map.union fields more, tailRow)
recordRow row@(TRowVar _) = Right (Map.empty,row)
recordRow TRowEmpty = Right (Map.empty,TRowEmpty)
recordRow other = Left (KindMismatch other TRowEmpty)

validateRows :: Type -> Either TypeError ()
validateRows ty = case ty of
  TQualified ps value -> mapM_ validateRows (value : map predicateType ps)
  TOpenRecord {} -> do
    (fields,_) <- recordRow ty
    mapM_ validateRows fields
  TRecord fields -> mapM_ validateRows fields
  TFun a b -> validateRows a >> validateRows b
  TMaybe a -> validateRows a
  TEither a b -> validateRows a >> validateRows b
  TList a -> validateRows a
  TTuple ts -> mapM_ validateRows ts
  TCustom _ ts -> mapM_ validateRows ts
  _ -> Right ()

constructorKey :: String -> String
constructorKey name = "@constructor:" ++ name

locateTypeError :: SourceSpan -> TypeError -> TypeError
locateTypeError _ failure@TypeAt {} = failure
locateTypeError _ failure@TypeImport {} = failure
locateTypeError location failure = TypeAt location failure

stripTypeLocation :: TypeError -> TypeError
stripTypeLocation (TypeAt _ failure) = stripTypeLocation failure
stripTypeLocation (TypeImport _ _ failure) = stripTypeLocation failure
stripTypeLocation failure = failure

predicateFailure :: (Predicate -> TypeError) -> Predicate -> TypeError
predicateFailure constructor (PredicateAt location predicate) = locateTypeError location (predicateFailure constructor predicate)
predicateFailure constructor predicate = constructor predicate

locateWarning :: SourceSpan -> TypeWarning -> TypeWarning
locateWarning _ warning@WarningAt {} = warning
locateWarning _ warning@InModule {} = warning
locateWarning location warning = WarningAt location warning
