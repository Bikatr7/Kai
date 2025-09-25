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
  | TString
  | TUnit
  | TFun Type Type
  | TVar String  -- Type variables for inference
  | TMaybe Type  -- Maybe type for optional values
  | TEither Type Type  -- Either type for error handling
  | TList Type
  | TRecord (Map.Map String Type)
  deriving (Show, Eq)

type TypeEnv = Map.Map String Type

-- Substitution maps type variables to types
type Substitution = Map.Map String Type

syntaxTypeToType :: SyntaxType -> Type
syntaxTypeToType STInt = TInt
syntaxTypeToType STBool = TBool
syntaxTypeToType STString = TString
syntaxTypeToType STUnit = TUnit
syntaxTypeToType (STFun t1 t2) = TFun (syntaxTypeToType t1) (syntaxTypeToType t2)
syntaxTypeToType (STMaybe t) = TMaybe (syntaxTypeToType t)
syntaxTypeToType (STEither t1 t2) = TEither (syntaxTypeToType t1) (syntaxTypeToType t2)
syntaxTypeToType (STList t) = TList (syntaxTypeToType t)
syntaxTypeToType (STRecord fields) = TRecord (Map.fromList (map (\(s, t) -> (s, syntaxTypeToType t)) fields))

data TypeError
  = TypeMismatch Type Type
  | ExpectedInt Type
  | ExpectedBool Type
  | ExpectedFunction Type
  | UnboundVariable String
  | InfiniteType String Type
  | UnificationError Type Type
  | RecordFieldMismatch String
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
applySubst sub (TMaybe t) = TMaybe (applySubst sub t)
applySubst sub (TEither t1 t2) = TEither (applySubst sub t1) (applySubst sub t2)
applySubst sub (TList t) = TList (applySubst sub t)
applySubst sub (TRecord fields) = TRecord (Map.map (applySubst sub) fields)
applySubst _ t = t

-- Apply substitution to type environment
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
  -- Merge all substitutions into one map, applying earlier ones to later ones
  let applyAllPrevious acc sub = 
        Map.map (applySubst acc) sub `Map.union` acc
  in foldl applyAllPrevious Map.empty subs

-- Get free type variables in a type
freeTypeVars :: Type -> Set.Set String
freeTypeVars (TVar name) = Set.singleton name
freeTypeVars (TFun t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2
freeTypeVars (TMaybe t) = freeTypeVars t
freeTypeVars (TEither t1 t2) = freeTypeVars t1 `Set.union` freeTypeVars t2
freeTypeVars (TList t) = freeTypeVars t
freeTypeVars (TRecord fields) = Set.unions (map freeTypeVars (Map.elems fields))
freeTypeVars _ = Set.empty

-- Get free type variables in type environment
freeTypeVarsEnv :: TypeEnv -> Set.Set String
freeTypeVarsEnv env = Set.unions (map freeTypeVars (Map.elems env))

-- Occurs check to prevent infinite types
occurs :: String -> Type -> Bool
occurs name (TVar name') = name == name'
occurs name (TFun t1 t2) = occurs name t1 || occurs name t2
occurs name (TMaybe t) = occurs name t
occurs name (TEither t1 t2) = occurs name t1 || occurs name t2
occurs name (TList t) = occurs name t
occurs name (TRecord fields) = any (occurs name) (Map.elems fields)
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
unify TString TString = Right Map.empty
unify TUnit TUnit = Right Map.empty
unify (TFun a1 r1) (TFun a2 r2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  return $ composeSubst s2 s1
unify (TMaybe t1) (TMaybe t2) = unify t1 t2
unify (TEither a1 b1) (TEither a2 b2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 b1) (applySubst s1 b2)
  return $ composeSubst s2 s1
unify (TList t1) (TList t2) = unify t1 t2
unify (TRecord f1) (TRecord f2) = do
    if Map.keys f1 /= Map.keys f2
        then Left $ UnificationError (TRecord f1) (TRecord f2)
        else do
            let subs = Map.intersectionWith unify f1 f2
            let errors = [e | Left e <- Map.elems subs]
            if not (null errors)
                then Left (head errors)
                else do
                    let successes = [s | Right s <- Map.elems subs]
                    return $ composeSubstList successes
unify t1 t2 = Left $ UnificationError t1 t2

-- Main type inference function
infer :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
infer _ (IntLit _) = return (Map.empty, TInt)
infer _ (BoolLit _) = return (Map.empty, TBool)
infer _ (StrLit _) = return (Map.empty, TString)
infer _ UnitLit = return (Map.empty, TUnit)
infer _ Input = return (Map.empty, TString)

infer env (Var x) = case Map.lookup x env of
  Just t -> return (Map.empty, t)
  Nothing -> lift $ Left $ UnboundVariable x

infer env (Add e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TInt)

infer env (Sub e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TInt)

infer env (Mul e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TInt)

infer env (Div e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TInt)

infer env (Concat e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  -- Check if both types are strings
  s3 <- lift $ unify (applySubst s2 t1) (applySubst s2 t2)
  case applySubst s3 t1 of
    -- String concatenation
    TString -> return (s3, TString)
    -- List concatenation
    TList _ -> return (s3, applySubst s3 t1)
    -- Other types not supported
    _ -> lift $ throwError $ UnificationError (applySubst s3 t1) (applySubst s3 t2)

infer env (Print e) = do
  (s, _) <- infer env e
  return (s, TUnit)  -- Print returns Unit

infer env (And e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TBool
  s4 <- lift $ unify (applySubst s3 t2) TBool
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TBool)

infer env (Or e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TBool
  s4 <- lift $ unify (applySubst s3 t2) TBool
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TBool)

infer env (Not e) = do
  (s1, t1) <- infer env e
  s2 <- lift $ unify t1 TBool
  return (composeSubst s2 s1, TBool)

infer env (Eq e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) (applySubst s2 t2)
  let finalSubst = composeSubstList [s1, s2, s3]
  return (finalSubst, TBool)

infer env (Lt e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TBool)

infer env (Gt e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer env e2
  s3 <- lift $ unify (applySubst s2 t1) TInt
  s4 <- lift $ unify (applySubst s3 t2) TInt
  let finalSubst = composeSubstList [s1, s2, s3, s4]
  return (finalSubst, TBool)

infer env (If c t e) = do
  (s1, tc) <- infer env c
  s2 <- lift $ unify tc TBool
  let s12 = composeSubst s2 s1
  (s3, tt) <- infer env t
  (s4, te) <- infer env e
  s5 <- lift $ unify (applySubst s4 tt) te
  let finalSubst = composeSubstList [s12, s3, s4, s5]
  return (finalSubst, applySubst s5 te)

-- Lambda with proper type inference (with optional type annotation)
infer env (Lambda param maybeType body) = do
  paramType <- case maybeType of
    Just sType -> return $ syntaxTypeToType sType
    Nothing -> freshTVar
  let env' = Map.insert param paramType env
  (s1, bodyType) <- infer env' body
  let finalParamType = applySubst s1 paramType
  return (s1, TFun finalParamType bodyType)

-- Function application with proper type inference
infer env (App fun arg) = do
  resultType <- freshTVar
  (s1, funType) <- infer env fun
  (s2, argType) <- infer env arg
  s3 <- lift $ unify (applySubst s2 funType) (TFun argType resultType)
  let finalSubst = composeSubstList [s1, s2, s3]
  return (finalSubst, applySubst s3 resultType)

-- Let binding: let x = val in body (with optional type annotation)
infer env (Let var maybeType val body) = do
  (s1, valType) <- infer env val
  finalValType <- case maybeType of
    Just sType -> do
      let annotatedType = syntaxTypeToType sType
      s2 <- lift $ unify valType annotatedType
      return $ applySubst s2 annotatedType
    Nothing -> return valType
  let env' = Map.insert var finalValType env
  (s2, bodyType) <- infer env' body
  let finalSubst = composeSubst s2 s1
  return (finalSubst, bodyType)

-- Recursive let binding: letrec x = val in body (with optional type annotation)
infer env (LetRec var maybeType val body) = do
  -- Create a fresh type variable for the recursive binding
  recType <- case maybeType of
    Just sType -> return $ syntaxTypeToType sType
    Nothing -> freshTVar
  let env' = Map.insert var recType env
  (s1, valType) <- infer env' val
  s2 <- lift $ unify (applySubst s1 recType) (applySubst s1 valType)
  let combinedSubst = composeSubst s2 s1
  let finalEnv = Map.insert var (applySubst combinedSubst recType) env
  (s3, bodyType) <- infer finalEnv body
  let finalSubst = composeSubst s3 combinedSubst
  return (finalSubst, bodyType)

-- Type annotation: check that expression matches annotated type
infer env (TypeAnnotation e sType) = do
  let annotatedType = syntaxTypeToType sType
  (s, exprType) <- infer env e
  s2 <- lift $ unify exprType annotatedType
  let finalSubst = composeSubst s2 s
  return (finalSubst, applySubst finalSubst annotatedType)

-- Built-in conversion functions
infer env (ParseInt e) = do
  (s, eType) <- infer env e
  s2 <- lift $ unify eType TString
  let finalSubst = composeSubst s2 s
  return (finalSubst, TMaybe TInt)

infer env (ToString e) = do
  (s, eType) <- infer env e
  s2 <- lift $ unify eType TInt
  let finalSubst = composeSubst s2 s
  return (finalSubst, TString)

infer env (Show e) = do
  (s, _) <- infer env e  -- Accept any type cause reasons
  return (s, TString)

-- Maybe/Either constructors
infer env (MJust e) = do
  (s, eType) <- infer env e
  return (s, TMaybe eType)

infer _ MNothing = do
  tyVar <- freshTVar
  return (Map.empty, TMaybe tyVar)

infer env (ELeft e) = do
  (s, eType) <- infer env e
  tyVar <- freshTVar
  return (s, TEither eType tyVar)

infer env (ERight e) = do
  (s, eType) <- infer env e
  tyVar <- freshTVar
  return (s, TEither tyVar eType)

infer env (ListLit es) = do
    elemType <- freshTVar
    subs <- forM es (\e -> do
        (s, t) <- infer env e
        s' <- lift $ unify t elemType
        return (composeSubst s' s))
    let finalSubst = composeSubstList subs
    return (finalSubst, TList (applySubst finalSubst elemType))

infer env (Cons h t) = do
    (s1, hType) <- infer env h
    (s2, tType) <- infer env t
    s3 <- lift $ unify (applySubst s2 tType) (TList (applySubst s2 hType))
    let finalSubst = composeSubstList [s1, s2, s3]
    return (finalSubst, applySubst s3 tType)

infer env (Head e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify eType (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst elemType)

infer env (Tail e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify eType (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst eType)

infer env (Null e) = do
    (s, eType) <- infer env e
    elemType <- freshTVar
    s' <- lift $ unify eType (TList elemType)
    let finalSubst = composeSubst s' s
    return (finalSubst, TBool)

infer env (RecordLit fields) = do
    let inferField (name, e) = do
            (s, t) <- infer env e
            return (s, (name, t))
    (subs, typedFields) <- unzip <$> mapM inferField fields
    let finalSubst = composeSubstList subs
    return (finalSubst, TRecord (Map.fromList (map (\(n, t) -> (n, applySubst finalSubst t)) typedFields)))

infer env (RecordAccess r field) = do
    (s, rType) <- infer env r
    fieldType <- freshTVar
    s' <- lift $ unify rType (TRecord (Map.singleton field fieldType))
    let finalSubst = composeSubst s' s
    return (finalSubst, applySubst finalSubst fieldType)

-- Case expression
infer env (Case scrutinee patterns) = do
  (s1, scrutType) <- infer env scrutinee
  resultType <- freshTVar
  (s2, _) <- inferPatterns env (applySubst s1 scrutType) resultType patterns
  let finalSubst = composeSubst s2 s1
  return (finalSubst, applySubst finalSubst resultType)
  where
    inferPatterns _ _ _ [] = return (Map.empty, TUnit)
    inferPatterns env scrutType resultType ((pat, expr) : rest) = do
      (patSubst, patEnv) <- inferPattern pat scrutType
      let newEnv = Map.union patEnv env
      (exprSubst, exprType) <- infer newEnv expr
      unifySubst <- lift $ unify (applySubst exprSubst resultType) exprType
      let combinedSubst = composeSubstList [patSubst, exprSubst, unifySubst]
      (restSubst, _) <- inferPatterns env (applySubst combinedSubst scrutType) (applySubst combinedSubst resultType) rest
      return (composeSubst restSubst combinedSubst, TUnit)

-- Pattern type inference
inferPattern :: Pattern -> Type -> TypeInfer (Substitution, TypeEnv)
inferPattern (PVar name) ty = return (Map.empty, Map.singleton name ty)
inferPattern (PInt _) ty = do
  s <- lift $ unify ty TInt
  return (s, Map.empty)
inferPattern (PBool _) ty = do
  s <- lift $ unify ty TBool
  return (s, Map.empty)
inferPattern (PStr _) ty = do
  s <- lift $ unify ty TString
  return (s, Map.empty)
inferPattern PUnit ty = do
  s <- lift $ unify ty TUnit
  return (s, Map.empty)
inferPattern (PJust pat) ty = do
  tyVar <- freshTVar
  s1 <- lift $ unify ty (TMaybe tyVar)
  (s2, env) <- inferPattern pat (applySubst s1 tyVar)
  return (composeSubst s2 s1, env)
inferPattern PNothing ty = do
  tyVar <- freshTVar
  s <- lift $ unify ty (TMaybe tyVar)
  return (s, Map.empty)
inferPattern (PLeft pat) ty = do
  tyVar1 <- freshTVar
  tyVar2 <- freshTVar
  s1 <- lift $ unify ty (TEither tyVar1 tyVar2)
  (s2, env) <- inferPattern pat (applySubst s1 tyVar1)
  return (composeSubst s2 s1, env)
inferPattern (PRight pat) ty = do
  tyVar1 <- freshTVar
  tyVar2 <- freshTVar
  s1 <- lift $ unify ty (TEither tyVar1 tyVar2)
  (s2, env) <- inferPattern pat (applySubst s1 tyVar2)
  return (composeSubst s2 s1, env)
inferPattern (PList []) ty = do
    elemType <- freshTVar
    s <- lift $ unify ty (TList elemType)
    return (s, Map.empty)
inferPattern (PList pats) ty = do
    elemType <- freshTVar
    s1 <- lift $ unify ty (TList elemType)
    let inferPat p = inferPattern p elemType
    (subs, envs) <- unzip <$> mapM inferPat pats
    return (composeSubstList (s1:subs), Map.unions envs)
inferPattern (PCons h t) ty = do
    elemType <- freshTVar
    s1 <- lift $ unify ty (TList elemType)
    (s2, hEnv) <- inferPattern h elemType
    (s3, tEnv) <- inferPattern t (TList elemType)
    return (composeSubstList [s1, s2, s3], Map.union hEnv tEnv)
inferPattern (PRecord fields) ty = do
    let inferField (name, p) = do
            fieldType <- freshTVar
            (s, env) <- inferPattern p fieldType
            return (s, env, (name, fieldType))
    (subs, envs, typedFields) <- unzip3 <$> mapM inferField fields
    s' <- lift $ unify ty (TRecord (Map.fromList typedFields))
    return (composeSubstList (s':subs), Map.unions envs)

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