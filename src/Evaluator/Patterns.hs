module Evaluator.Patterns where

import Evaluator.Types
import Syntax
import qualified Data.Map as Map
import Control.Monad (foldM)

type EvalFunc = Env -> Expr -> Either RuntimeError Value

matchPattern :: Pattern -> Value -> Maybe Env
matchPattern (PVar name) val = Just $ Map.singleton name val
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
matchPattern (PTuple pats) (VTuple vals)
  | length pats == length vals = do
      envs <- sequence [matchPattern p v | (p, v) <- zip pats vals]
      return $ Map.unions envs
  | otherwise = Nothing
matchPattern _ _ = Nothing

evalPatterns :: EvalFunc -> Env -> Expr -> Either RuntimeError Value
evalPatterns eval env (Case scrutinee patterns) = do
  val <- eval env scrutinee
  tryPatterns env val patterns
  where
    tryPatterns _ _ [] = Left $ TypeError "No matching pattern in case expression"
    tryPatterns env val ((pat, expr) : rest) = do
      case matchPattern pat val of
        Nothing -> tryPatterns env val rest
        Just bindings ->
          let newEnv = Map.union bindings env
          in eval newEnv expr
evalPatterns _ _ _ = error "evalPatterns called on non-pattern expression"
