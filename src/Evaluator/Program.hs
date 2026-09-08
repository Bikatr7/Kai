module Evaluator.Program (evaluateTopLevels) where

import qualified Data.Map as Map
import Data.Bifunctor (first)
import DataDeclarations (dataConstructorsValueEnv)
import Evaluator.Types
import Evaluator.Helpers (bindResult)
import Evaluator.Recursion (initializeRecursiveBindings)
import Syntax
import TopLevelRecursion (collectConsecutiveLetrecs)

evaluateTopLevels :: (Env -> Expr -> IO (Either RuntimeError Value)) -> (String -> IO (Either RuntimeError Env)) -> Env -> Program -> IO (Either RuntimeError (Env, Value))
evaluateTopLevels evaluate load initial (Program levels) = go initial levels
  where
    go env [] = return $ Right (env, VUnit)
    go env (TLAt location level:rest) =
      first (locateRuntimeError location UnitLit) <$> go env (level:rest)
    go env (TLExpr expression : rest) =
      bindResult (evaluate env expression) $ \value ->
        if null rest then return $ Right (env, value) else go env rest
    go env (TLImport name : rest) =
      bindResult (load name) $ \imported -> go (Map.union imported env) rest
    go env (TLExport _ : rest) = go env rest
    go env (TLData _ _ constructors : rest) =
      go (Map.union (dataConstructorsValueEnv constructors) env) rest
    go env definitions@(TLDef name _ expression : rest) = case expression of
      LetRec {} ->
        let (recursive, remaining) = collectConsecutiveLetrecs definitions
            bindings = [(var, value) | TLDef var _ (LetRec _ _ value _) <- recursive]
        in bindResult (initializeRecursiveBindings evaluate env bindings) $ \next -> go next remaining
      _ -> bindResult (evaluate env expression) $ \value ->
        go (if name == "_" then env else Map.insert name value env) rest
