module TypeChecker where

import Syntax
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except

-- Types with type variables for proper inference
data Type 
  = TInt 
  | TBool 
  | TFun Type Type
  | TVar String  -- Type variables for inference
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
  deriving (Show, Eq)

-- Type inference monad
type TypeInfer = StateT Int (Either TypeError)

-- Generate fresh type variable
freshTVar :: TypeInfer Type
freshTVar = do
  n <- get
  put (n + 1)
  return $ TVar ("t" ++ show n)

-- Apply substitution to a type
applySubst :: Substitution -> Type -> Type
applySubst sub (TVar name) = case Map.lookup name sub of
  Just t -> applySubst sub t  -- Apply recursively in case of chains
  Nothing -> TVar name
applySubst sub (TFun t1 t2) = TFun (applySubst sub t1) (applySubst sub t2)
applySubst _ t = t

-- Apply substitution to type environment
applySubstEnv :: Substitution -> TypeEnv -> TypeEnv
applySubstEnv sub = Map.map (applySubst sub)

-- Compose two substitutions
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1

-- Get free type variables in a type
freeTypeVars :: Type -> Set.Set String
freeTypeVars (TVar name) = Set.singleton name
freeTypeVars (TFun t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2
freeTypeVars _ = Set.empty

-- Get free type variables in type environment
freeTypeVarsEnv :: TypeEnv -> Set.Set String
freeTypeVarsEnv env = Set.unions (map freeTypeVars (Map.elems env))

-- Occurs check to prevent infinite types
occurs :: String -> Type -> Bool
occurs name (TVar name') = name == name'
occurs name (TFun t1 t2) = occurs name t1 || occurs name t2
occurs _ _ = False

-- Unification algorithm
unify :: Type -> Type -> Either TypeError Substitution
unify (TVar a) t
  | t == TVar a = Right Map.empty
  | occurs a t = Left $ InfiniteType a t
  | otherwise = Right $ Map.singleton a t
unify t (TVar a) = unify (TVar a) t
unify TInt TInt = Right Map.empty
unify TBool TBool = Right Map.empty
unify (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  return $ composeSubst s2 s1
unify t1 t2 = Left $ UnificationError t1 t2

-- Main type inference function
infer :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
infer _ (IntLit _) = return (Map.empty, TInt)
infer _ (BoolLit _) = return (Map.empty, TBool)

infer env (Var x) = case Map.lookup x env of
  Just t -> return (Map.empty, t)
  Nothing -> lift $ Left $ UnboundVariable x

infer env (Add e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applySubstEnv s1 env) e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubst s4 (composeSubst s3 (composeSubst s2 s1))
  return (finalSubst, TInt)

infer env (Sub e1 e2) = infer env (Add e1 e2)  -- Same logic as addition
infer env (Mul e1 e2) = infer env (Add e1 e2)  -- Same logic as addition
infer env (Div e1 e2) = infer env (Add e1 e2)  -- Same logic as addition

infer env (And e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applySubstEnv s1 env) e2
  s3 <- lift $ unify (applySubst s2 t1) TBool
  s4 <- lift $ unify (applySubst s3 t2) TBool
  let finalSubst = composeSubst s4 (composeSubst s3 (composeSubst s2 s1))
  return (finalSubst, TBool)

infer env (Or e1 e2) = infer env (And e1 e2)  -- Same logic as And

infer env (Not e) = do
  (s1, t1) <- infer env e
  s2 <- lift $ unify t1 TBool
  return (composeSubst s2 s1, TBool)

infer env (Eq e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applySubstEnv s1 env) e2
  s3 <- lift $ unify (applySubst s2 t1) (applySubst s2 t2)
  let finalSubst = composeSubst s3 (composeSubst s2 s1)
  return (finalSubst, TBool)

infer env (Lt e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applySubstEnv s1 env) e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubst s4 (composeSubst s3 (composeSubst s2 s1))
  return (finalSubst, TBool)

infer env (Gt e1 e2) = infer env (Lt e1 e2)  -- Same logic as Lt

infer env (If c t e) = do
  (s1, tc) <- infer env c
  s2 <- lift $ unify tc TBool
  let s12 = composeSubst s2 s1
  (s3, tt) <- infer (applySubstEnv s12 env) t
  (s4, te) <- infer (applySubstEnv (composeSubst s3 s12) env) e
  s5 <- lift $ unify (applySubst s4 tt) te
  let finalSubst = composeSubst s5 (composeSubst s4 (composeSubst s3 s12))
  return (finalSubst, applySubst s5 te)

-- Lambda with proper type inference
infer env (Lambda param body) = do
  paramType <- freshTVar
  let env' = Map.insert param paramType env
  (s1, bodyType) <- infer env' body
  let finalParamType = applySubst s1 paramType
  return (s1, TFun finalParamType bodyType)

-- Function application with proper type inference
infer env (App fun arg) = do
  resultType <- freshTVar
  (s1, funType) <- infer env fun
  (s2, argType) <- infer (applySubstEnv s1 env) arg
  s3 <- lift $ unify (applySubst s2 funType) (TFun argType resultType)
  let finalSubst = composeSubst s3 (composeSubst s2 s1)
  return (finalSubst, applySubst s3 resultType)

-- Public interface
typeCheck :: Expr -> Either TypeError Type
typeCheck expr = case evalStateT (infer Map.empty expr) 0 of
  Left err -> Left err
  Right (subst, ty) -> Right $ applySubst subst ty

-- Legacy interface for compatibility
typeCheckWithEnv :: TypeEnv -> Expr -> Either TypeError Type  
typeCheckWithEnv env expr = case evalStateT (infer env expr) 0 of
  Left err -> Left err
  Right (subst, ty) -> Right $ applySubst subst ty