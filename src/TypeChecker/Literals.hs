module TypeChecker.Literals where

import qualified Data.Map as Map
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution
import StandardLibrary (standardTypeEnv)
import Control.Applicative ((<|>))

inferLiteral :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferLiteral _ (IntLit _) = return (Map.empty, TInt)
inferLiteral _ (BoolLit _) = return (Map.empty, TBool)
inferLiteral _ (StrLit _) = return (Map.empty, TString)
inferLiteral _ UnitLit = return (Map.empty, TUnit)
inferLiteral _ Input = return (Map.empty, TString)
inferLiteral _ Args = return (Map.empty, TList TString)
inferLiteral _ GetCurrentDirectory = return (Map.empty, TString)

inferLiteral _ _ = error "inferLiteral called on non-literal expression"

inferVariable :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferVariable env (Var x) = case Map.lookup x env <|> Map.lookup x standardTypeEnv of
  Just scheme -> do
    ty <- instantiate scheme
    return (Map.empty, ty)
  Nothing -> lift $ Left $ UnboundVariable x

inferVariable _ _ = error "inferVariable called on non-variable expression"
