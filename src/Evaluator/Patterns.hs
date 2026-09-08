{-# LANGUAGE FlexibleContexts #-}
module Evaluator.Patterns where

import Evaluator.Types
import Control.Monad.Except (MonadError, throwError)
import Evaluator.Helpers (evalInIO)
import Syntax
import qualified Data.Map as Map

matchPattern :: Pattern -> Value -> Maybe Env
matchPattern (PVar name) val = Just $ if name == "_" then Map.empty else Map.singleton name val
matchPattern (PInt n) (VInt m) = if n == m then Just Map.empty else Nothing
matchPattern (PBool b) (VBool c) = if b == c then Just Map.empty else Nothing
matchPattern (PStr s) (VStr t) = if s == t then Just Map.empty else Nothing
matchPattern PUnit VUnit = Just Map.empty
matchPattern (PJust pat) (VJust val) = matchPattern pat val
matchPattern PNothing VNothing = Just Map.empty
matchPattern (PLeft pat) (VLeft val) = matchPattern pat val
matchPattern (PRight pat) (VRight val) = matchPattern pat val
matchPattern (PList []) (VList []) = Just Map.empty
matchPattern (PList (p:ps)) (VList (v:vs)) = do
    env1 <- matchPattern p v
    env2 <- matchPattern (PList ps) (VList vs)
    return $ Map.union env1 env2
matchPattern (PCons ph pt) (VList (v:vs)) = do
    env1 <- matchPattern ph v
    env2 <- matchPattern pt (VList vs)
    return $ Map.union env1 env2
matchPattern (PRecord pfs) (VRecord vfs) = do
    let pfsMap = Map.fromList pfs
    if Map.keysSet pfsMap == Map.keysSet vfs
        then do
            let pvs = Map.intersectionWith (,) pfsMap vfs
            envs <- mapM (uncurry matchPattern) (Map.elems pvs)
            return $ Map.unions envs
        else Nothing
matchPattern (POpenRecord fields rest) (VRecord values) = do
    bindings <- mapM (\(name, pat) -> Map.lookup name values >>= matchPattern pat) fields
    let remaining = VRecord (foldr (Map.delete . fst) values fields)
        restBinding = if rest == "_" then Map.empty else Map.singleton rest remaining
    pure $ Map.unions (restBinding : bindings)
matchPattern (PTuple pats) (VTuple vals)
  | length pats == length vals = do
      envs <- sequence [matchPattern p v | (p, v) <- zip pats vals]
      return $ Map.unions envs
  | otherwise = Nothing
matchPattern (PConstructor name pats) (VData valueName values)
  | name == valueName && length pats == length values = do
      envs <- sequence [matchPattern pat value | (pat, value) <- zip pats values]
      return $ Map.unions envs
  | otherwise = Nothing
matchPattern _ _ = Nothing

evalPatterns :: MonadError RuntimeError m => Eval m -> Eval m
evalPatterns eval env (Case scrutinee patterns) = do
  val <- eval env scrutinee
  tryPatterns env val patterns
  where
    tryPatterns _ _ [] = throwError $ TypeError "No matching pattern in case expression"
    tryPatterns scope val ((pat, expr) : rest) = do
      case matchPattern pat val of
        Nothing -> tryPatterns scope val rest
        Just bindings ->
          let newEnv = Map.union bindings scope
          in eval newEnv expr
evalPatterns _ _ _ = error "evalPatterns called on non-pattern expression"

evalPatternsIO :: EvalFuncIO -> Env -> Expr -> IO (Either RuntimeError Value)
evalPatternsIO = evalInIO evalPatterns
