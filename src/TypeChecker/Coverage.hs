module TypeChecker.Coverage (checkCoverage) where

import Data.List (find, nub)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Map as Map
import Syntax (Pattern(..))
import TypeChecker.Types
import TypeChecker.Substitution (applySubst)
import DataDeclarations (standardDataTypeEnv, constructorPatternScheme)

-- Heads distinguish constructor alternatives; variables always mean a wildcard.
-- Record heads include field names because closed patterns require exact labels.
data Head
  = BooleanHead Bool | IntegerHead Int | StringHead String | UnitHead
  | NothingHead | JustHead | LeftHead | RightHead | NilHead | ConsHead
  | TupleHead Int | RecordHead [String] | DataHead String | ExtraFieldsHead [String]
  deriving (Show, Eq)

data Shape = Shape Head [Type] Bool

data Cell = Wild | Construct Head [Cell] | OpenFields (Map.Map String Cell)
  deriving (Show, Eq)

type Matrix = [[Cell]]

checkCoverage :: TypeEnv -> Type -> [Pattern] -> Either TypeError [TypeWarning]
checkCoverage supplied ty patterns = do
  let env = Map.union supplied standardDataTypeEnv
      rows = map ((:[]) . cell) patterns
      missing = uncovered env rows [ty]
  case missing of
    Just [witness] -> Left (NonExhaustivePatterns (renderCell env ty witness))
    Just _ -> Left (NonExhaustivePatterns "_")
    Nothing -> pure [UnreachableAlternative index |
      (index,row) <- zip [1..] rows,
      not (useful env (take (index-1) rows) row [ty])]

cell :: Pattern -> Cell
cell (PVar _) = Wild
cell (PInt n) = Construct (IntegerHead n) []
cell (PBool b) = Construct (BooleanHead b) []
cell (PStr s) = Construct (StringHead s) []
cell PUnit = Construct UnitHead []
cell (PJust p) = Construct JustHead [cell p]
cell PNothing = Construct NothingHead []
cell (PLeft p) = Construct LeftHead [cell p]
cell (PRight p) = Construct RightHead [cell p]
cell (PList []) = Construct NilHead []
cell (PList (p:ps)) = Construct ConsHead [cell p,cell (PList ps)]
cell (PCons a b) = Construct ConsHead [cell a,cell b]
cell (PTuple ps) = Construct (TupleHead (length ps)) (map cell ps)
cell (PRecord fields) = let ordered = Map.toList (Map.fromList fields)
  in Construct (RecordHead (map fst ordered)) (map (cell . snd) ordered)
cell (POpenRecord fields _) = OpenFields (Map.fromList [(name,cell p) | (name,p) <- fields])
cell (PConstructor name ps) = Construct (DataHead name) (map cell ps)

shapes :: TypeEnv -> Type -> Maybe [Shape]
shapes env ty = case ty of
  TBool -> Just [shape (BooleanHead False) [],shape (BooleanHead True) []]
  TUnit -> Just [shape UnitHead []]
  TMaybe a -> Just [shape NothingHead [],shape JustHead [a]]
  TEither a b -> Just [shape LeftHead [a],shape RightHead [b]]
  TList a -> Just [shape NilHead [],shape ConsHead [a,TList a]]
  TTuple values -> Just [shape (TupleHead (length values)) values]
  TRecord fields -> Just [shape (RecordHead (Map.keys fields)) (Map.elems fields)]
  TOpenRecord fields _ -> Just
    [shape (RecordHead (Map.keys fields)) (Map.elems fields),
     Shape (ExtraFieldsHead (Map.keys fields)) (Map.elems fields) False]
  TCustom name values -> case Map.lookup (dataTypeKey name) env of
    Just (Forall variables (TRecord constructors)) ->
      let subst = Map.fromList (zip variables values)
      in Just [Shape (DataHead constructor) (arguments (applySubst subst signature))
                (isJust (constructorPatternScheme env constructor)) |
                (constructor,signature) <- Map.toList constructors]
    _ -> Nothing
  _ -> Nothing
  where
    shape headTag arguments = Shape headTag arguments True
    arguments (TFun argument result) = argument : arguments result
    arguments _ = []

headOf :: Shape -> Head
headOf (Shape headTag _ _) = headTag

argumentsOf :: Shape -> [Type]
argumentsOf (Shape _ arguments _) = arguments

-- An open pattern applies to both the exact known fields and records with an
-- additional tail. Its payload restrictions must be checked in both shapes.
recordArguments :: Head -> Map.Map String Cell -> Maybe [Cell]
recordArguments headTag fields = case headTag of
  RecordHead names -> arguments names
  ExtraFieldsHead names -> arguments names
  _ -> Nothing
  where
    arguments names
      | all (`elem` names) (Map.keys fields) = Just [Map.findWithDefault Wild name fields | name <- names]
      | otherwise = Nothing

presentHeads :: TypeEnv -> Type -> Matrix -> [Head]
presentHeads env ty rows = nub $ concatMap heads rows
  where
    heads (Construct headTag _ : _) = [headTag]
    heads (OpenFields fields : _) =
      [headOf shape | shape <- fromMaybe [] (shapes env ty), isJust (recordArguments (headOf shape) fields)]
    heads _ = []

-- Each specialization consumes a constructor appearing in the finite pattern
-- matrix. Wildcard-only columns take the default path without expanding an ADT.
uncovered :: TypeEnv -> Matrix -> [Type] -> Maybe [Cell]
uncovered _ rows [] = if null rows then Just [] else Nothing
uncovered env rows (ty:rest) =
  let present = presentHeads env ty rows
      defaults = defaultRows rows
  in case shapes env ty of
    Just alternatives | all ((`elem` present) . headOf) alternatives ->
      firstJust [do
        witness <- uncovered env (specialize alternative rows) (argumentsOf alternative ++ rest)
        let (arguments,tailWitness) = splitAt (length (argumentsOf alternative)) witness
        pure (Construct (headOf alternative) arguments : tailWitness)
        | alternative <- alternatives]
    finite -> do
      tailWitness <- uncovered env defaults rest
      let missing = case finite of
            Just alternatives -> case find ((`notElem` present) . headOf) alternatives of
              Just alternative -> Construct (headOf alternative) (map (representative env []) (argumentsOf alternative))
              Nothing -> Wild
            Nothing -> missingLiteral ty present
      pure (missing : tailWitness)

useful :: TypeEnv -> Matrix -> [Cell] -> [Type] -> Bool
useful _ rows [] [] = null rows
useful env rows (Wild:query) (ty:rest) =
  let present = presentHeads env ty rows
  in case shapes env ty of
    Just alternatives | all ((`elem` present) . headOf) alternatives -> any
      (\alternative -> useful env (specialize alternative rows)
        (replicate (length (argumentsOf alternative)) Wild ++ query)
        (argumentsOf alternative ++ rest)) alternatives
    _ -> useful env (defaultRows rows) query rest
useful env rows (Construct headTag args:query) (ty:rest) =
  let alternative = case shapes env ty >>= find ((== headTag) . headOf) of
        Just found -> found
        Nothing -> Shape headTag (replicate (length args) TUnit) True
  in useful env (specialize alternative rows) (args ++ query) (argumentsOf alternative ++ rest)
useful env rows (OpenFields fields:query) (ty:rest) = any
  (\alternative -> case recordArguments (headOf alternative) fields of
    Just arguments -> useful env (specialize alternative rows)
      (arguments ++ query) (argumentsOf alternative ++ rest)
    Nothing -> False)
  (fromMaybe [] (shapes env ty))
useful _ _ _ _ = False

defaultRows :: Matrix -> Matrix
defaultRows rows = [rest | Wild:rest <- rows]

specialize :: Shape -> Matrix -> Matrix
specialize (Shape headTag arguments _) = mapMaybe go
  where
    go (Wild:rest) = Just (replicate (length arguments) Wild ++ rest)
    go (Construct tag fields:rest) | tag == headTag = Just (fields ++ rest)
    go (OpenFields fields:rest) = (++ rest) <$> recordArguments headTag fields
    go _ = Nothing

missingLiteral :: Type -> [Head] -> Cell
missingLiteral TInt present = Construct (IntegerHead missing) []
  where missing = head [n | n <- [0..], IntegerHead n `notElem` present]
missingLiteral TString present = Construct (StringHead missing) []
  where missing = head [replicate n 'x' | n <- [0..], StringHead (replicate n 'x') `notElem` present]
missingLiteral _ _ = Wild

-- Prefer finite, readable values for missing payloads. Recursive custom types
-- stop at a wildcard when no finite constructor has been reached yet.
representative :: TypeEnv -> [String] -> Type -> Cell
representative env visited ty = case ty of
  TCustom name _ | name `elem` visited -> Wild
  _ -> case shapes env ty of
    Just alternatives -> case find (null . argumentsOf) alternatives of
      Just empty -> Construct (headOf empty) []
      Nothing -> case alternatives of
        first:_ -> let nextVisited = case ty of TCustom name _ -> name:visited; _ -> visited
                   in Construct (headOf first) (map (representative env nextVisited) (argumentsOf first))
        [] -> Wild
    Nothing -> missingLiteral ty []

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:rest) = firstJust rest
firstJust (value:_) = value

-- Privacy is checked again while rendering, including nested missing values.
-- A wildcard is the actionable witness when a hidden constructor is required.
renderCell :: TypeEnv -> Type -> Cell -> String
renderCell _ _ Wild = "_"
renderCell _ _ OpenFields {} = "_"
renderCell env ty (Construct headTag arguments) =
  let candidate = shapes env ty >>= find ((== headTag) . headOf)
      fields = maybe (replicate (length arguments) TUnit) argumentsOf candidate
      rendered = zipWith (renderCell env) fields arguments
      atom value = if any (`elem` value) " :" then "(" ++ value ++ ")" else value
  in case candidate of
    Just (Shape _ _ False) -> "_"
    _ -> case (headTag,rendered) of
      (BooleanHead False,_) -> "false"
      (BooleanHead True,_) -> "true"
      (IntegerHead n,_) -> show n
      (StringHead value,_) -> quote value
      (UnitHead,_) -> "()"
      (NothingHead,_) -> "Nothing"
      (JustHead,[value]) -> "Just " ++ atom value
      (LeftHead,[value]) -> "Left " ++ atom value
      (RightHead,[value]) -> "Right " ++ atom value
      (NilHead,_) -> "[]"
      (ConsHead,[a,b]) -> atom a ++ " :: " ++ b
      (TupleHead _,values) -> "(" ++ comma values ++ ")"
      (RecordHead names,values) -> "{" ++ comma (zipWith (\name value -> name ++ " = " ++ value) names values) ++ "}"
      (DataHead name,values) -> unwords (name : map atom values)
      _ -> "_"
  where
    comma [] = ""
    comma [value] = value
    comma (value:rest) = value ++ ", " ++ comma rest
    quote value = "\"" ++ concatMap escape value ++ "\""
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape c = [c]
