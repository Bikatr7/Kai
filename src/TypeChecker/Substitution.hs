module TypeChecker.Substitution where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import TypeChecker.Types
import Syntax (SyntaxType)

freshTVar :: TypeInfer Type
freshTVar = do
  n <- gets nextTypeVariable
  modify $ \state -> state { nextTypeVariable = n + 1 }
  return $ TVar ("t" ++ show n)

freshRowVar :: TypeInfer Type
freshRowVar = do
  n <- gets nextTypeVariable
  modify $ \state -> state { nextTypeVariable = n + 1 }
  pure $ TRowVar ("t" ++ show n)

freshTVarAvoiding :: Set.Set String -> TypeInfer Type
freshTVarAvoiding avoid = do
  n <- gets nextTypeVariable
  modify $ \state -> state { nextTypeVariable = n + 1 }
  let candidate = "t" ++ show n
  if candidate `Set.member` avoid
    then freshTVarAvoiding avoid
    else return $ TVar candidate

applySubst :: Substitution -> Type -> Type
applySubst sub (TVar name) = case Map.lookup name sub of
  Just t -> applySubst sub t  -- Apply recursively in case of chains
  Nothing -> TVar name
applySubst sub (TRowVar name) = case Map.lookup name sub of
  Just row -> applySubst sub row
  Nothing -> TRowVar name
applySubst sub (TFun t1 t2) = TFun (applySubst sub t1) (applySubst sub t2)
applySubst sub (TCustom name args) = TCustom name (map (applySubst sub) args)
applySubst sub (TMaybe t) = TMaybe (applySubst sub t)
applySubst sub (TEither t1 t2) = TEither (applySubst sub t1) (applySubst sub t2)
applySubst sub (TList t) = TList (applySubst sub t)
applySubst sub (TRecord fields) = TRecord (Map.map (applySubst sub) fields)
applySubst sub (TOpenRecord fields row) =
  let resolved = TOpenRecord (Map.map (applySubst sub) fields) (applySubst sub row)
  in case recordRow resolved of
    Right (allFields,TRowEmpty) -> TRecord allFields
    Right (allFields,tailRow) -> TOpenRecord allFields tailRow
    Left _ -> resolved -- validateRows reports duplicate labels at inference boundaries
applySubst sub (TQualified ps ty) = qualifiedType (map (applyPredicate sub) ps) (applySubst sub ty)
applySubst sub (TTuple ts) = TTuple (map (applySubst sub) ts)
applySubst _ t = t

applySubstScheme :: Substitution -> Scheme -> Scheme
applySubstScheme sub (Forall vars ty) =
  let filteredSubst = foldr Map.delete sub vars
  in Forall vars (applySubst filteredSubst ty)

applySubstEnv :: Substitution -> TypeEnv -> TypeEnv
applySubstEnv sub = Map.map (applySubstScheme sub)

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 =
  let s2' = Map.map (applySubst s1) s2
  in s2' `Map.union` s1

composeSubstList :: [Substitution] -> Substitution
composeSubstList [] = Map.empty
composeSubstList [s] = s
composeSubstList subs =
  let applyAllPrevious acc sub =
        Map.map (applySubst acc) sub `Map.union` acc
  in foldl applyAllPrevious Map.empty subs

freeTypeVars :: Type -> Set.Set String
freeTypeVars (TVar name) = Set.singleton name
freeTypeVars (TRowVar name) = Set.singleton name
freeTypeVars (TFun t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2
freeTypeVars (TCustom _ args) = Set.unions (map freeTypeVars args)
freeTypeVars (TMaybe t) = freeTypeVars t
freeTypeVars (TEither t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2
freeTypeVars (TList t) = freeTypeVars t
freeTypeVars (TRecord fields) = Set.unions (map freeTypeVars (Map.elems fields))
freeTypeVars (TOpenRecord fields row) = freeTypeVars row `Set.union` Set.unions (map freeTypeVars (Map.elems fields))
freeTypeVars (TQualified ps ty) = Set.unions (freeTypeVars ty : map (freeTypeVars . predicateType) ps)
freeTypeVars (TTuple ts) = Set.unions (map freeTypeVars ts)
freeTypeVars _ = Set.empty

freeTypeVarsScheme :: Scheme -> Set.Set String
freeTypeVarsScheme (Forall vars ty) = freeTypeVars ty `Set.difference` Set.fromList vars

freeTypeVarsEnv :: TypeEnv -> Set.Set String
freeTypeVarsEnv env = Set.unions (map freeTypeVarsScheme (Map.elems env))

generalize :: TypeEnv -> Type -> Scheme
generalize env ty =
  let vars = Set.toList $ freeTypeVars ty `Set.difference` freeTypeVarsEnv env
  in Forall vars ty

instantiate :: Scheme -> TypeInfer Type
instantiate scheme = do
  qualified <- instantiateRaw scheme
  let (predicates,ty) = splitQualified qualified
  addPredicates predicates
  return ty

instantiateRaw :: Scheme -> TypeInfer Type
instantiateRaw (Forall vars ty) = do
  let avoid0 = freeTypeVars ty
  freshVars <- go avoid0 vars
  let substitution = Map.fromList $ zipWith preserveKind vars freshVars
  return $ applySubst substitution ty
  where
    preserveKind name (TVar fresh)
      | name `Set.member` rowVariables ty = (name,TRowVar fresh)
    preserveKind name fresh = (name,fresh)
    go _ [] = return []
    go avoid (_ : rest) = do
      freshVar <- freshTVarAvoiding avoid
      let freshName = case freshVar of
            TVar name -> name
            _ -> error "freshTVarAvoiding returned a non-type-variable"
      otherVars <- go (Set.insert freshName avoid) rest
      return (freshVar : otherVars)

schemeIsInstanceOf :: Scheme -> Scheme -> TypeInfer Bool
schemeIsInstanceOf instanceScheme generalScheme = do
  generalType <- instantiateRaw generalScheme
  let rigidType = skolemizeScheme instanceScheme
  return $ case matchType generalType rigidType of
    Left _ -> False
    Right _ -> True

skolemizeScheme :: Scheme -> Type
skolemizeScheme (Forall vars ty) =
  applySubst
    (Map.fromList [(var, if var `Set.member` rowVariables ty
      then TRowVar (skolemPrefix ++ var) else TVar (skolemPrefix ++ var)) | var <- vars])
    ty

skolemPrefix :: String
skolemPrefix = "__skolem__"

matchType :: Type -> Type -> Either TypeError Substitution
matchType = matchTypeWith Map.empty

matchTypeWith :: Substitution -> Type -> Type -> Either TypeError Substitution
matchTypeWith subst t1 t2 =
  case (applySubst subst t1, t2) of
    (TVar a, t)
      | isBareRow t -> Left $ KindMismatch (TVar a) t
      | TVar a == t -> Right subst
      | occursInType a t -> Left $ InfiniteType a t
      | otherwise -> Right $ Map.insert a t subst
    (TRowVar a,t) -> do
      _ <- recordRow t
      if TRowVar a == t then Right subst
        else if occursInType a t then Left (InfiniteType a t)
        else Right $ Map.insert a t subst
    (TRowEmpty,TRowEmpty) -> Right subst
    (TInt, TInt) -> Right subst
    (TBool, TBool) -> Right subst
    (TString, TString) -> Right subst
    (TUnit, TUnit) -> Right subst
    (TFun a1 r1, TFun a2 r2) -> do
      s1 <- matchTypeWith subst a1 a2
      matchTypeWith s1 r1 r2
    (TCustom name1 args1, TCustom name2 args2)
      | name1 /= name2 || length args1 /= length args2 ->
          Left $ UnificationError (TCustom name1 args1) (TCustom name2 args2)
      | otherwise ->
          foldl matchPair (Right subst) (zip args1 args2)
    (TMaybe t1', TMaybe t2') -> matchTypeWith subst t1' t2'
    (TEither a1 b1, TEither a2 b2) -> do
      s1 <- matchTypeWith subst a1 a2
      matchTypeWith s1 b1 b2
    (TList t1', TList t2') -> matchTypeWith subst t1' t2'
    (TRecord f1, TRecord f2)
      | Map.keys f1 /= Map.keys f2 -> Left $ UnificationError (TRecord f1) (TRecord f2)
      | otherwise ->
          foldl matchPair (Right subst) (zip (Map.elems f1) (Map.elems f2))
    (left@TOpenRecord {}, right@TOpenRecord {}) -> matchRows subst left right
    (left@TOpenRecord {}, right@TRecord {}) -> matchRows subst left right
    (left@TRecord {}, right@TOpenRecord {}) -> matchRows subst left right
    (TTuple ts1, TTuple ts2)
      | length ts1 /= length ts2 -> Left $ UnificationError (TTuple ts1) (TTuple ts2)
      | otherwise ->
          foldl matchPair (Right subst) (zip ts1 ts2)
    (matched, expected) -> Left $ UnificationError matched expected
  where
    isBareRow TRowVar {} = True
    isBareRow TRowEmpty = True
    isBareRow _ = False
    matchPair acc (leftTy, rightTy) = acc >>= \s -> matchTypeWith s leftTy rightTy

matchRows :: Substitution -> Type -> Type -> Either TypeError Substitution
matchRows subst left right = do
  (leftFields,leftTail) <- recordRow left
  (rightFields,rightTail) <- recordRow right
  if not (Map.keysSet leftFields `Set.isSubsetOf` Map.keysSet rightFields)
    then Left (UnificationError left right)
    else do
      next <- foldM (\s (a,b) -> matchTypeWith s a b) subst
        (Map.elems (Map.intersectionWith (,) leftFields rightFields))
      let extra = Map.difference rightFields leftFields
          remainder = if Map.null extra then rightTail else TOpenRecord extra rightTail
      case leftTail of
        TRowEmpty | Map.null extra && rightTail == TRowEmpty -> Right next
                 | otherwise -> Left (UnificationError left right)
        _ -> matchTypeWith next leftTail remainder

occursInType :: String -> Type -> Bool
occursInType name (TVar name') = name == name'
occursInType name (TRowVar name') = name == name'
occursInType name (TFun t1 t2) = occursInType name t1 || occursInType name t2
occursInType name (TCustom _ args) = any (occursInType name) args
occursInType name (TMaybe t) = occursInType name t
occursInType name (TEither t1 t2) = occursInType name t1 || occursInType name t2
occursInType name (TList t) = occursInType name t
occursInType name (TRecord fields) = any (occursInType name) (Map.elems fields)
occursInType name (TOpenRecord fields row) = any (occursInType name) (Map.elems fields) || occursInType name row
occursInType name (TQualified ps ty) = any (occursInType name) (ty : map predicateType ps)
occursInType name (TTuple ts) = any (occursInType name) ts
occursInType _ _ = False

-- Each annotation owns its variables. Instantiate before unification so source
-- names (including t0, t1, ...) cannot capture inference-generated variables.
inferAnnotation :: TypeEnv -> SyntaxType -> TypeInfer Type
inferAnnotation env syntax = do
  ty <- lift $ validateSyntaxType env syntax
  case ty of
    TQualified {} -> lift $ Left $ GeneralTypeError "Lambda parameters cannot have qualified types"
    _ -> instantiateRaw (generalize Map.empty ty)

-- Declaration parameters are positional; their spelling is not type identity.
alphaEquivalentSchemes :: Scheme -> Scheme -> Bool
alphaEquivalentSchemes left right = canonical left == canonical right
  where
    canonical (Forall vars ty) =
      (length vars, applySubst (Map.fromList [(name, if name `Set.member` rowVariables ty
        then TRowVar ("@bound" ++ show i) else TVar ("@bound" ++ show i)) |
        (name,i) <- zip vars [0 :: Int ..]]) ty)

rowVariables :: Type -> Set.Set String
rowVariables ty = case ty of
  TQualified ps value -> Set.unions (rowVariables value : map (rowVariables . predicateType) ps)
  TRowVar name -> Set.singleton name
  TOpenRecord fields row -> Set.unions (rowVariables row : map rowVariables (Map.elems fields))
  TRecord fields -> Set.unions (map rowVariables (Map.elems fields))
  TFun a b -> Set.union (rowVariables a) (rowVariables b)
  TMaybe a -> rowVariables a
  TEither a b -> Set.union (rowVariables a) (rowVariables b)
  TList a -> rowVariables a
  TTuple ts -> Set.unions (map rowVariables ts)
  TCustom _ ts -> Set.unions (map rowVariables ts)
  _ -> Set.empty

applyPredicate :: Substitution -> Predicate -> Predicate
applyPredicate sub (PredicateAt location predicate) = PredicateAt location (applyPredicate sub predicate)
applyPredicate sub (Equality ty) = Equality (applySubst sub ty)
applyPredicate sub (Appendable ty) = Appendable (applySubst sub ty)

addPredicates :: [Predicate] -> TypeInfer ()
addPredicates predicates = modify $ \state -> state
  { pendingPredicates = predicates ++ pendingPredicates state }

capturePredicates :: TypeInfer a -> TypeInfer (a, [Predicate])
capturePredicates action = do
  previous <- gets pendingPredicates
  modify $ \state -> state { pendingPredicates = [] }
  result <- action
  predicates <- gets pendingPredicates
  modify $ \state -> state { pendingPredicates = previous }
  pure (result, predicates)

inferQualifiedAnnotation :: TypeEnv -> SyntaxType -> TypeInfer ([Predicate], Type)
inferQualifiedAnnotation env syntax = do
  ty <- lift $ validateSyntaxType env syntax
  splitQualified <$> instantiateRaw (generalize Map.empty ty)
