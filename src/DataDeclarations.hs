module DataDeclarations
  ( constructorResultType
  , constructorScheme
  , dataConstructorsTypeEnv
  , registerDataDeclaration
  , mergeTypeEnvironments
  , filterTypeExports
  , constructorPatternScheme
  , dataConstructorsValueEnv
  ) where

import qualified Data.Map as Map
import Data.List (isPrefixOf)
import Control.Monad (unless, when)
import Syntax
import Evaluator.Types
import TypeChecker.Types
import TypeChecker.Substitution (alphaEquivalentSchemes)

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

registerDataDeclaration :: TypeEnv -> String -> [String] -> [DataConstructor] -> Either TypeError TypeEnv
registerDataDeclaration env name vars constructors = do
  when (Map.member (dataTypeKey name) env) $
    Left $ InvalidDataDeclaration ("Duplicate type: " ++ name)
  unique "type parameter" vars
  unique "constructor" [n | DataConstructor n _ <- constructors]
  let schemes = dataConstructorsTypeEnv name vars constructors
      metadata = Forall vars (TRecord (Map.map schemeType schemes))
      declaredEnv = Map.insert (dataTypeKey name) metadata env
  mapM_ (validateConstructor declaredEnv) constructors
  let visible = Map.fromList [(constructorKey n, scheme) | (n, scheme) <- Map.toList schemes]
  return $ Map.unions [visible, schemes, declaredEnv]
  where
    unique _ [] = Right ()
    unique label (n:ns)
      | n `elem` ns = Left $ InvalidDataDeclaration ("Duplicate " ++ label ++ ": " ++ n)
      | otherwise = unique label ns
    validateConstructor declaredEnv (DataConstructor n args) = do
      when (Map.member n env || any (Map.member n) declaredConstructors) $
        Left $ InvalidDataDeclaration ("Duplicate constructor: " ++ n)
      mapM_ (validateSyntaxType declaredEnv) args
      mapM_ checkVariables args
    declaredConstructors = [cs | (key, Forall _ (TRecord cs)) <- Map.toList env,
                                  "@type:" `isPrefixOf` key]
    checkVariables (STVar v) = unless (v `elem` vars) $
      Left $ InvalidDataDeclaration ("Unbound type parameter: " ++ v)
    checkVariables (STFun a b) = checkVariables a >> checkVariables b
    checkVariables (STMaybe a) = checkVariables a
    checkVariables (STEither a b) = checkVariables a >> checkVariables b
    checkVariables (STList a) = checkVariables a
    checkVariables (STTuple ts) = mapM_ checkVariables ts
    checkVariables (STRecord fs) = mapM_ (checkVariables . snd) fs
    checkVariables (STCustom _ ts) = mapM_ checkVariables ts
    checkVariables _ = Right ()

-- Type names are global in an import graph. Preserve their full declarations
-- even when the constructors are private, and reject incompatible imports.
mergeTypeEnvironments :: TypeEnv -> TypeEnv -> Either TypeError TypeEnv
mergeTypeEnvironments incoming existing = do
  mapM_ check (Map.toList incoming)
  let merged = Map.union incoming existing
      constructors = [(n, ty) | (key, Forall _ (TRecord cs)) <- Map.toList merged,
                               "@type:" `isPrefixOf` key, (n,ty) <- Map.toList cs]
  checkConstructors constructors
  return merged
  where
    check (key, scheme)
      | "@type:" `isPrefixOf` key = case Map.lookup key existing of
          Just old | not (alphaEquivalentSchemes old scheme) -> Left $ InvalidDataDeclaration ("Conflicting imported type: " ++ drop 6 key)
          _ -> Right ()
      | otherwise = Right ()
    checkConstructors [] = Right ()
    checkConstructors ((name, _):rest) = do
      when (any ((== name) . fst) rest) $
        Left $ InvalidDataDeclaration ("Conflicting imported constructor: " ++ name)
      checkConstructors rest

filterTypeExports :: TypeEnv -> [String] -> TypeEnv
filterTypeExports env [] = env
filterTypeExports env names = Map.filterWithKey keep env
  where
    keep key _
      | "@type:" `isPrefixOf` key = True
      | "@constructor:" `isPrefixOf` key = drop (length "@constructor:") key `elem` names
      | otherwise = key `elem` names

constructorPatternScheme :: TypeEnv -> String -> Maybe Scheme
constructorPatternScheme env name = Map.lookup (constructorKey name) env
