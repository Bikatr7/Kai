module TypeChecker.Substitution where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import TypeChecker.Types
import Syntax (SyntaxType)

freshTVar :: TypeInfer Type
freshTVar = do
  n <- get
  put (n + 1)
  return $ TVar ("t" ++ show n)

freshTVarAvoiding :: Set.Set String -> TypeInfer Type
freshTVarAvoiding avoid = do
  n <- get
  put (n + 1)
  let candidate = "t" ++ show n
  if candidate `Set.member` avoid
    then freshTVarAvoiding avoid
    else return $ TVar candidate

applySubst :: Substitution -> Type -> Type
applySubst sub (TVar name) = case Map.lookup name sub of
  Just t -> applySubst sub t  -- Apply recursively in case of chains
  Nothing -> TVar name
applySubst sub (TFun t1 t2) = TFun (applySubst sub t1) (applySubst sub t2)
applySubst sub (TCustom name args) = TCustom name (map (applySubst sub) args)
applySubst sub (TMaybe t) = TMaybe (applySubst sub t)
applySubst sub (TEither t1 t2) = TEither (applySubst sub t1) (applySubst sub t2)
applySubst sub (TList t) = TList (applySubst sub t)
applySubst sub (TRecord fields) = TRecord (Map.map (applySubst sub) fields)
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
freeTypeVars (TFun t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2
freeTypeVars (TCustom _ args) = Set.unions (map freeTypeVars args)
freeTypeVars (TMaybe t) = freeTypeVars t
freeTypeVars (TEither t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2
freeTypeVars (TList t) = freeTypeVars t
freeTypeVars (TRecord fields) = Set.unions (map freeTypeVars (Map.elems fields))
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
instantiate (Forall vars ty) = do
  let avoid0 = freeTypeVars ty
  freshVars <- go avoid0 vars
  let substitution = Map.fromList $ zip vars freshVars
  return $ applySubst substitution ty
  where
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
  generalType <- instantiate generalScheme
  let rigidType = skolemizeScheme instanceScheme
  return $ case matchType generalType rigidType of
    Left _ -> False
    Right _ -> True

skolemizeScheme :: Scheme -> Type
skolemizeScheme (Forall vars ty) =
  applySubst
    (Map.fromList [(var, TVar (skolemPrefix ++ var)) | var <- vars])
    ty

skolemPrefix :: String
skolemPrefix = "__skolem__"

matchType :: Type -> Type -> Either TypeError Substitution
matchType = matchTypeWith Map.empty

matchTypeWith :: Substitution -> Type -> Type -> Either TypeError Substitution
matchTypeWith subst t1 t2 =
  case (applySubst subst t1, t2) of
    (TVar a, t)
      | TVar a == t -> Right subst
      | occursInType a t -> Left $ InfiniteType a t
      | otherwise -> Right $ Map.insert a t subst
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
    (TTuple ts1, TTuple ts2)
      | length ts1 /= length ts2 -> Left $ UnificationError (TTuple ts1) (TTuple ts2)
      | otherwise ->
          foldl matchPair (Right subst) (zip ts1 ts2)
    (matched, expected) -> Left $ UnificationError matched expected
  where
    matchPair acc (leftTy, rightTy) = acc >>= \s -> matchTypeWith s leftTy rightTy

occursInType :: String -> Type -> Bool
occursInType name (TVar name') = name == name'
occursInType name (TFun t1 t2) = occursInType name t1 || occursInType name t2
occursInType name (TCustom _ args) = any (occursInType name) args
occursInType name (TMaybe t) = occursInType name t
occursInType name (TEither t1 t2) = occursInType name t1 || occursInType name t2
occursInType name (TList t) = occursInType name t
occursInType name (TRecord fields) = any (occursInType name) (Map.elems fields)
occursInType name (TTuple ts) = any (occursInType name) ts
occursInType _ _ = False

-- Each annotation owns its variables. Instantiate before unification so source
-- names (including t0, t1, ...) cannot capture inference-generated variables.
inferAnnotation :: TypeEnv -> SyntaxType -> TypeInfer Type
inferAnnotation env syntax = do
  ty <- lift $ validateSyntaxType env syntax
  instantiate (generalize Map.empty ty)

-- Declaration parameters are positional; their spelling is not type identity.
alphaEquivalentSchemes :: Scheme -> Scheme -> Bool
alphaEquivalentSchemes left right = canonical left == canonical right
  where
    canonical (Forall vars ty) =
      (length vars, applySubst (Map.fromList (zip vars [TVar ("@bound" ++ show i) | i <- [0 :: Int ..]])) ty)
