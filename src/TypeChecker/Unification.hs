module TypeChecker.Unification where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (lift)
import Text.Read (readMaybe)
import TypeChecker.Types
import TypeChecker.Substitution

occurs :: String -> Type -> Bool
occurs = occursInType

-- Standalone callers get their own fresh supply. Inference uses unifyInfer so
-- row variables share the same supply as the rest of the expression/module.
unify :: Type -> Type -> Either TypeError Substitution
unify left right = evalStateT (unifyInfer left right) (initialInferState (unificationSeed [left,right]))

unificationSeed :: [Type] -> Int
unificationSeed types = maximum (0 : [n + 1 |
  't':digits <- Set.toList (Set.unions (map freeTypeVars types)), Just n <- [readMaybe digits]])

unifyInfer :: Type -> Type -> TypeInfer Substitution
unifyInfer (TVar a) t
  | isRowTail t = lift $ Left $ KindMismatch (TVar a) t
  | t == TVar a = pure Map.empty
  | occurs a t = lift $ Left $ InfiniteType a t
  | otherwise = pure $ Map.singleton a t
unifyInfer t (TVar a) = unifyInfer (TVar a) t
unifyInfer (TRowVar a) t = do
  _ <- lift $ recordRow t
  if t == TRowVar a then pure Map.empty
    else if occurs a t then lift $ Left $ InfiniteType a t
    else pure $ Map.singleton a t
unifyInfer t (TRowVar a) = unifyInfer (TRowVar a) t
unifyInfer TRowEmpty TRowEmpty = pure Map.empty
unifyInfer TInt TInt = pure Map.empty
unifyInfer TBool TBool = pure Map.empty
unifyInfer TString TString = pure Map.empty
unifyInfer TUnit TUnit = pure Map.empty
unifyInfer (TFun a1 r1) (TFun a2 r2) = unifyPairsInfer [(a1,a2),(r1,r2)]
unifyInfer (TCustom name1 args1) (TCustom name2 args2)
  | name1 /= name2 || length args1 /= length args2 =
      lift $ Left $ UnificationError (TCustom name1 args1) (TCustom name2 args2)
  | otherwise = unifyPairsInfer (zip args1 args2)
unifyInfer (TMaybe t1) (TMaybe t2) = unifyInfer t1 t2
unifyInfer (TEither a1 b1) (TEither a2 b2) = unifyPairsInfer [(a1,a2),(b1,b2)]
unifyInfer (TList t1) (TList t2) = unifyInfer t1 t2
unifyInfer left@TRecord {} right@TRecord {} = unifyRecords left right
unifyInfer left@TOpenRecord {} right@TRecord {} = unifyRecords left right
unifyInfer left@TRecord {} right@TOpenRecord {} = unifyRecords left right
unifyInfer left@TOpenRecord {} right@TOpenRecord {} = unifyRecords left right
unifyInfer TRowEmpty (TRecord fields) | Map.null fields = pure Map.empty
unifyInfer (TRecord fields) TRowEmpty | Map.null fields = pure Map.empty
unifyInfer (TTuple ts1) (TTuple ts2)
  | length ts1 /= length ts2 = lift $ Left $ UnificationError (TTuple ts1) (TTuple ts2)
  | otherwise = unifyPairsInfer (zip ts1 ts2)
unifyInfer t1 t2 = lift $ Left $ UnificationError t1 t2

isRowTail :: Type -> Bool
isRowTail TRowVar {} = True
isRowTail TRowEmpty = True
isRowTail _ = False

unifyRecords :: Type -> Type -> TypeInfer Substitution
unifyRecords left right = do
  (first,_) <- lift $ recordRow left
  (second,_) <- lift $ recordRow right
  common <- unifyPairsInfer (Map.elems (Map.intersectionWith (,) first second))
  (first',tail1) <- lift $ recordRow (applySubst common left)
  (second',tail2) <- lift $ recordRow (applySubst common right)
  let only1 = Map.difference first' second'
      only2 = Map.difference second' first'
      row fields tailRow = if Map.null fields then tailRow else TOpenRecord fields tailRow
      mismatch = lift $ Left $ UnificationError (applySubst common left) (applySubst common right)
  rest <- case (tail1,tail2) of
    (TRowEmpty,TRowEmpty)
      | Map.null only1 && Map.null only2 -> pure Map.empty
      | otherwise -> mismatch
    (TRowVar _,TRowEmpty)
      | Map.null only1 -> unifyInfer tail1 (row only2 TRowEmpty)
      | otherwise -> mismatch
    (TRowEmpty,TRowVar _)
      | Map.null only2 -> unifyInfer tail2 (row only1 TRowEmpty)
      | otherwise -> mismatch
    (TRowVar a,TRowVar b)
      | a == b -> if Map.null only1 && Map.null only2 then pure Map.empty else mismatch
      | Map.null only1 && Map.null only2 -> unifyInfer tail1 tail2
      | otherwise -> do
          shared <- freshRowVar
          unifyPairsInfer [(tail1,row only2 shared),(tail2,row only1 shared)]
    _ -> mismatch
  let final = composeSubst rest common
  lift $ mapM_ (validateRows . applySubst final) [left,right]
  pure final

unifyPairs :: [(Type, Type)] -> Either TypeError Substitution
unifyPairs pairs = evalStateT (unifyPairsInfer pairs)
  (initialInferState (unificationSeed (concatMap (\(a,b) -> [a,b]) pairs)))

unifyPairsInfer :: [(Type, Type)] -> TypeInfer Substitution
unifyPairsInfer = go Map.empty
  where
    go subst [] = pure subst
    go subst ((t1,t2):rest) = do
      next <- unifyInfer (applySubst subst t1) (applySubst subst t2)
      go (composeSubst next subst) rest
