module Evaluator (
    Value(..),
    Env,
    RuntimeError(..),
    eval,
    evalWithEnv,
    evalPure,
    evalPureWithEnv,
    evalProgram,
    evalProgramWithEnv
) where

import ModuleSystem (filterByExports, loadModule, ModuleInfo(..))

import Syntax
import Evaluator.Program (evaluateTopLevels)
import qualified Data.Map as Map
import Data.IORef
import Evaluator.Types
import Evaluator.Helpers (bindResult)
import Evaluator.Literals
import Evaluator.Arithmetic
import Evaluator.BooleanOps
import Evaluator.ControlFlow
import Evaluator.Functions
import Evaluator.Bindings
import Evaluator.DataStructures
import Evaluator.StringOps
import Evaluator.Conversions
import Evaluator.IOOps
import Evaluator.Patterns
import qualified Evaluator.DataStructures as DataIO
import qualified Evaluator.StringOps as StrIO
import qualified Evaluator.Conversions as ConvIO
import qualified Evaluator.Patterns as PatIO

eval :: Expr -> IO (Either RuntimeError Value)
eval = evalWithEnv Map.empty

evalPure :: Expr -> Either RuntimeError Value
evalPure = evalPureWithEnv Map.empty

evalPureWithEnv :: Env -> Expr -> Either RuntimeError Value
evalPureWithEnv env expr = case expr of
  IntLit _ -> evalLiteral expr
  BoolLit _ -> evalLiteral expr
  StrLit _ -> evalLiteral expr
  UnitLit -> evalLiteral expr
  Input -> evalIOPure evalPureWithEnv env expr
  Args -> evalIOPure evalPureWithEnv env expr
  GetCurrentDirectory -> evalIOPure evalPureWithEnv env expr
  Var x -> case Map.lookup x env of
    Just (VUninitialized name) -> Left $ UninitializedRecursion name
    Just v -> Right v
    Nothing -> Left $ UnboundVariable x
  Add _ _ -> evalArithmetic evalPureWithEnv env expr
  Sub _ _ -> evalArithmetic evalPureWithEnv env expr
  Mul _ _ -> evalArithmetic evalPureWithEnv env expr
  Div _ _ -> evalArithmetic evalPureWithEnv env expr
  Concat _ _ -> evalArithmetic evalPureWithEnv env expr
  And _ _ -> evalBooleanOps evalPureWithEnv env expr
  Or _ _ -> evalBooleanOps evalPureWithEnv env expr
  Not _ -> evalBooleanOps evalPureWithEnv env expr
  Eq _ _ -> evalBooleanOps evalPureWithEnv env expr
  Lt _ _ -> evalBooleanOps evalPureWithEnv env expr
  Gt _ _ -> evalBooleanOps evalPureWithEnv env expr
  If _ _ _ -> evalBooleanOps evalPureWithEnv env expr
  Seq _ _ -> evalControlFlow evalPureWithEnv env expr
  Lambda _ _ _ -> evalFunctions evalPureWithEnv env expr
  App _ _ -> evalFunctions evalPureWithEnv env expr
  Fix e -> do
    functionValue <- evalPureWithEnv env e
    if isCallableValue functionValue
      then case applyCallable evalPureWithEnv functionValue fixPlaceholder of
          Left err -> Left err
          Right probeValue
            | containsFixPlaceholder probeValue -> Left uninitializedFixError
            | otherwise ->
                let fixedResult = applyCallable evalPureWithEnv functionValue fixedValue
                    fixedValue = case fixedResult of
                      Right value -> value
                      Left _ -> fixPlaceholder
                in fixedResult
      else Left $ TypeError "Fix expects a function"
  Let _ _ _ _ -> evalBindings evalPureWithEnv env expr
  LetRec _ _ _ _ -> evalBindings evalPureWithEnv env expr
  TypeAnnotation _ _ -> evalBindings evalPureWithEnv env expr
  ListLit _ -> evalDataStructures evalPureWithEnv env expr
  Cons _ _ -> evalDataStructures evalPureWithEnv env expr
  Head _ -> evalDataStructures evalPureWithEnv env expr
  Tail _ -> evalDataStructures evalPureWithEnv env expr
  Null _ -> evalDataStructures evalPureWithEnv env expr
  RecordLit _ -> evalDataStructures evalPureWithEnv env expr
  RecordAccess _ _ -> evalDataStructures evalPureWithEnv env expr
  TupleLit _ -> evalDataStructures evalPureWithEnv env expr
  Fst _ -> evalDataStructures evalPureWithEnv env expr
  Snd _ -> evalDataStructures evalPureWithEnv env expr
  Map _ _ -> evalDataStructures evalPureWithEnv env expr
  Filter _ _ -> evalDataStructures evalPureWithEnv env expr
  Foldl _ _ _ -> evalDataStructures evalPureWithEnv env expr
  Length _ -> evalDataStructures evalPureWithEnv env expr
  Reverse _ -> evalDataStructures evalPureWithEnv env expr
  Take _ _ -> evalDataStructures evalPureWithEnv env expr
  Drop _ _ -> evalDataStructures evalPureWithEnv env expr
  Zip _ _ -> evalDataStructures evalPureWithEnv env expr
  Split _ _ -> evalStringOps evalPureWithEnv env expr
  Join _ _ -> evalStringOps evalPureWithEnv env expr
  Trim _ -> evalStringOps evalPureWithEnv env expr
  Replace _ _ _ -> evalStringOps evalPureWithEnv env expr
  StrLength _ -> evalStringOps evalPureWithEnv env expr
  ParseInt _ -> evalConversions evalPureWithEnv env expr
  ToString _ -> evalConversions evalPureWithEnv env expr
  Show _ -> evalConversions evalPureWithEnv env expr
  Discard _ -> evalConversions evalPureWithEnv env expr
  MJust _ -> evalConversions evalPureWithEnv env expr
  MNothing -> evalConversions evalPureWithEnv env expr
  ELeft _ -> evalConversions evalPureWithEnv env expr
  ERight _ -> evalConversions evalPureWithEnv env expr
  Print _ -> evalIOPure evalPureWithEnv env expr
  ReadFile _ -> evalIOPure evalPureWithEnv env expr
  WriteFile _ _ -> evalIOPure evalPureWithEnv env expr
  AppendFile _ _ -> evalIOPure evalPureWithEnv env expr
  FileExists _ -> evalIOPure evalPureWithEnv env expr
  ListDirectory _ -> evalIOPure evalPureWithEnv env expr
  CreateDirectory _ -> evalIOPure evalPureWithEnv env expr
  RemoveDirectory _ -> evalIOPure evalPureWithEnv env expr
  SetCurrentDirectory _ -> evalIOPure evalPureWithEnv env expr
  System _ -> evalIOPure evalPureWithEnv env expr
  GetEnv _ -> evalIOPure evalPureWithEnv env expr
  SetEnv _ _ -> evalIOPure evalPureWithEnv env expr
  Exit _ -> evalIOPure evalPureWithEnv env expr
  Case _ _ -> evalPatterns evalPureWithEnv env expr

evalWithEnv :: Env -> Expr -> IO (Either RuntimeError Value)
evalWithEnv env expr = case expr of
  Input -> evalIOWithEnv evalWithEnv env expr
  Args -> evalIOWithEnv evalWithEnv env expr
  Print _ -> evalIOWithEnv evalWithEnv env expr
  ReadFile _ -> evalIOWithEnv evalWithEnv env expr
  WriteFile _ _ -> evalIOWithEnv evalWithEnv env expr
  AppendFile _ _ -> evalIOWithEnv evalWithEnv env expr
  FileExists _ -> evalIOWithEnv evalWithEnv env expr
  ListDirectory _ -> evalIOWithEnv evalWithEnv env expr
  CreateDirectory _ -> evalIOWithEnv evalWithEnv env expr
  RemoveDirectory _ -> evalIOWithEnv evalWithEnv env expr
  GetCurrentDirectory -> evalIOWithEnv evalWithEnv env expr
  SetCurrentDirectory _ -> evalIOWithEnv evalWithEnv env expr
  System _ -> evalIOWithEnv evalWithEnv env expr
  GetEnv _ -> evalIOWithEnv evalWithEnv env expr
  SetEnv _ _ -> evalIOWithEnv evalWithEnv env expr
  Exit _ -> evalIOWithEnv evalWithEnv env expr
  Var x -> case Map.lookup x env of
    Nothing -> return $ Left $ UnboundVariable x
    Just value -> resolveCallableIO value
  Lambda _ _ _ -> evalFunctionsIO evalWithEnv env expr
  App _ _ -> evalFunctionsIO evalWithEnv env expr
  Fix e ->
    bindResult (evalWithEnv env e) $ \functionValue ->
      bindResult (resolveCallableIO functionValue) $ \callable ->
        if isCallableValue callable
        then do
          recRef <- newIORef fixPlaceholder
          result <- applyCallableIO evalWithEnv callable (VRef recRef)
          case result of
            Right finalVal
              | containsFixPlaceholder finalVal -> return $ Left uninitializedFixError
              | otherwise -> do
                  writeIORef recRef finalVal
                  return $ Right finalVal
            Left err -> return $ Left err
        else return $ Left $ TypeError "Fix expects a function"
  LetRec {} -> evalBindingsIO evalWithEnv env expr
  Add _ _ -> evalArithmeticIO evalWithEnv env expr
  Sub _ _ -> evalArithmeticIO evalWithEnv env expr
  Mul _ _ -> evalArithmeticIO evalWithEnv env expr
  Div _ _ -> evalArithmeticIO evalWithEnv env expr
  Concat _ _ -> evalArithmeticIO evalWithEnv env expr
  And _ _ -> evalBooleanOpsIO evalWithEnv env expr
  Or _ _ -> evalBooleanOpsIO evalWithEnv env expr
  Not _ -> evalBooleanOpsIO evalWithEnv env expr
  Eq _ _ -> evalBooleanOpsIO evalWithEnv env expr
  Lt _ _ -> evalBooleanOpsIO evalWithEnv env expr
  Gt _ _ -> evalBooleanOpsIO evalWithEnv env expr
  If _ _ _ -> evalBooleanOpsIO evalWithEnv env expr
  Seq _ _ -> evalControlFlowIO evalWithEnv env expr
  Let _ _ _ _ -> evalBindingsIO evalWithEnv env expr
  TypeAnnotation _ _ -> evalBindingsIO evalWithEnv env expr
  ListLit _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Cons _ _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Head _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Tail _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Null _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  RecordLit _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  RecordAccess _ _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  TupleLit _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Fst _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Snd _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Map _ _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Filter _ _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Foldl _ _ _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Length _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Reverse _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Take _ _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Drop _ _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Zip _ _ -> DataIO.evalDataStructuresIO evalWithEnv env expr
  Split _ _ -> StrIO.evalStringOpsIO evalWithEnv env expr
  Join _ _ -> StrIO.evalStringOpsIO evalWithEnv env expr
  Trim _ -> StrIO.evalStringOpsIO evalWithEnv env expr
  Replace _ _ _ -> StrIO.evalStringOpsIO evalWithEnv env expr
  StrLength _ -> StrIO.evalStringOpsIO evalWithEnv env expr
  ParseInt _ -> ConvIO.evalConversionsIO evalWithEnv env expr
  ToString _ -> ConvIO.evalConversionsIO evalWithEnv env expr
  Show _ -> ConvIO.evalConversionsIO evalWithEnv env expr
  Discard _ -> ConvIO.evalConversionsIO evalWithEnv env expr
  MJust _ -> ConvIO.evalConversionsIO evalWithEnv env expr
  MNothing -> ConvIO.evalConversionsIO evalWithEnv env expr
  ELeft _ -> ConvIO.evalConversionsIO evalWithEnv env expr
  ERight _ -> ConvIO.evalConversionsIO evalWithEnv env expr
  Case _ _ -> PatIO.evalPatternsIO evalWithEnv env expr
  _ -> return $ evalPureWithEnv env expr

fixPlaceholder :: Value
fixPlaceholder = VData "\0kai-fix-uninitialized" []

uninitializedFixError :: RuntimeError
uninitializedFixError = TypeError "Fixpoint forced before initialization"

containsFixPlaceholder :: Value -> Bool
containsFixPlaceholder value = case value of
  VData name values -> name == "\0kai-fix-uninitialized" || any containsFixPlaceholder values
  VConstructor _ _ values -> any containsFixPlaceholder values
  VJust inner -> containsFixPlaceholder inner
  VLeft inner -> containsFixPlaceholder inner
  VRight inner -> containsFixPlaceholder inner
  VList values -> any containsFixPlaceholder values
  VRecord fields -> any containsFixPlaceholder (Map.elems fields)
  VTuple values -> any containsFixPlaceholder values
  VRef _ -> True
  _ -> False

evalProgram :: Program -> IO (Either RuntimeError Value)
evalProgram = evalProgramWithEnv Map.empty "."

evalProgramWithEnv :: Env -> FilePath -> Program -> IO (Either RuntimeError Value)
evalProgramWithEnv env currentDir program = fmap snd <$>
  evaluateTopLevels evalWithEnv load env program
  where
    load name = do
      result <- loadModule evalWithEnv currentDir name []
      return $ case result of
        Left err -> Left $ TypeError $ "Failed to import module " ++ name ++ ": " ++ err
        Right info -> Right $ filterByExports (moduleEnv info) (moduleExports info)
