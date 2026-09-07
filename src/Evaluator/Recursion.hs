module Evaluator.Recursion (initializeRecursiveBindings) where

import qualified Data.Map as Map
import Data.IORef (newIORef, writeIORef)
import Evaluator.Types
import Evaluator.Helpers (bindResult)
import Syntax (Expr)

-- Allocate the whole block first so closures can refer forward. Evaluate and
-- publish each initializer in source order, stopping on the first failure.
initializeRecursiveBindings :: (Env -> Expr -> IO (Either RuntimeError Value)) -> Env -> [(String, Expr)] -> IO (Either RuntimeError Env)
initializeRecursiveBindings evaluate env bindings = do
  refs <- mapM (newIORef . VUninitialized . fst) bindings
  let recursive = Map.union (Map.fromList (zip (map fst bindings) (map VRef refs))) env
      initialize [] = return $ Right recursive
      initialize ((expression, ref) : rest) =
        bindResult (evaluate recursive expression) $ \value -> do
          writeIORef ref value
          initialize rest
  initialize (zip (map snd bindings) refs)
