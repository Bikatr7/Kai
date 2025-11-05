module TypeChecker.Substitution where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import TypeChecker.Types

freshTVar :: TypeInfer Type
freshTVar = do
  n <- get
  put (n + 1)
  return $ TVar ("t" ++ show n)

applySubst :: Substitution -> Type -> Type
applySubst sub (TVar name) = case Map.lookup name sub of
  Just t -> applySubst sub t  -- Apply recursively in case of chains
  Nothing -> TVar name
applySubst sub (TFun t1 t2) = TFun (applySubst sub t1) (applySubst sub t2)
applySubst sub (TMaybe t) = TMaybe (applySubst sub t)
applySubst sub (TEither t1 t2) = TEither (applySubst sub t1) (applySubst sub t2)
applySubst sub (TList t) = TList (applySubst sub t)
applySubst sub (TRecord fields) = TRecord (Map.map (applySubst sub) fields)
applySubst sub (TTuple ts) = TTuple (map (applySubst sub) ts)
applySubst _ t = t

applySubstEnv :: Substitution -> TypeEnv -> TypeEnv
applySubstEnv sub = Map.map (applySubst sub)

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
freeTypeVars (TMaybe t) = freeTypeVars t
freeTypeVars (TEither t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2
freeTypeVars (TList t) = freeTypeVars t
freeTypeVars (TRecord fields) = Set.unions (map freeTypeVars (Map.elems fields))
freeTypeVars _ = Set.empty

freeTypeVarsEnv :: TypeEnv -> Set.Set String
freeTypeVarsEnv env = Set.unions (map freeTypeVars (Map.elems env))
