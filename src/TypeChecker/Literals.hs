module TypeChecker.Literals where

import qualified Data.Map as Map
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Syntax (Expr(..))
import TypeChecker.Types
import TypeChecker.Substitution

inferLiteral :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferLiteral _ (IntLit _) = return (Map.empty, TInt)
inferLiteral _ (BoolLit _) = return (Map.empty, TBool)
inferLiteral _ (StrLit _) = return (Map.empty, TString)
inferLiteral _ UnitLit = return (Map.empty, TUnit)
inferLiteral _ Input = return (Map.empty, TString)
inferLiteral _ Args = return (Map.empty, TList TString)

inferVariable :: TypeEnv -> Expr -> TypeInfer (Substitution, Type)
inferVariable env (Var x) = case Map.lookup x env of
  Just t -> return (Map.empty, t)
  Nothing -> lift $ Left $ UnboundVariable x
