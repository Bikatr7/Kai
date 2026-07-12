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

import ModuleSystem (loadModule, ModuleInfo(..))

import Syntax
import DataDeclarations (dataConstructorsValueEnv)
import TopLevelRecursion (collectConsecutiveLetrecs, dependencyOrderedLetrecGroups)
import qualified Data.Map as Map
import Data.IORef
import Evaluator.Types
import Evaluator.Helpers (bindResult, traverseResults)
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
import qualified Evaluator.Arithmetic as ArithIO
import qualified Evaluator.BooleanOps as BoolIO
import qualified Evaluator.ControlFlow as CtrlIO
import qualified Evaluator.Functions as FuncIO
import qualified Evaluator.Bindings as BindIO
import qualified Evaluator.DataStructures as DataIO
import qualified Evaluator.StringOps as StrIO
import qualified Evaluator.Conversions as ConvIO
import qualified Evaluator.Patterns as PatIO

filterByExports :: Map.Map String a -> [String] -> Map.Map String a
filterByExports env [] = env
filterByExports env exports = Map.filterWithKey (\k _ -> k `elem` exports) env

evalLiteralPure :: Expr -> Value
evalLiteralPure (IntLit n) = VInt n
evalLiteralPure (BoolLit b) = VBool b
evalLiteralPure (StrLit s) = VStr s
evalLiteralPure UnitLit = VUnit
evalLiteralPure _ = error "evalLiteralPure called on non-literal expression"

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
  RecordAccess r field ->
    case evalPureWithEnv env r of
      Right (VRecord m) -> case Map.lookup field m of
        Just fv -> Right fv
        Nothing -> Left $ RecordFieldNotFound field
      Right _ -> Left $ TypeError "Record access expects a record"
      Left err -> Left err
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
  Var x -> do
    let lookupResult = Map.lookup x env
    case lookupResult of
      Just (VRef ref) -> do
        val <- readIORef ref
        return $ Right val
      Just v -> return $ Right v
      Nothing -> return $ Left $ UnboundVariable x
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
  LetRec var _maybeType val body -> do
    recValueRef <- newIORef (VFun "_placeholder" (IntLit 0) Map.empty)
    let env' = Map.insert var (VRef recValueRef) env
    valResult <- evalWithEnv env' val
    case valResult of
      Left err -> return $ Left err
      Right recValue -> do
        writeIORef recValueRef recValue
        evalWithEnv env' body
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
evalProgramWithEnv env currentDir (Program topLevels) = do
    importResult <- processImports env currentDir topLevels
    case importResult of
        Left err -> return $ Left err
        Right (importedEnv, remaining) -> do
            go importedEnv remaining
  where
    processImports :: Env -> FilePath -> [TopLevel] -> IO (Either RuntimeError (Env, [TopLevel]))
    processImports env _ [] = return $ Right (env, [])
    processImports env currentDir (TLImport moduleName : rest) = do
        moduleResult <- loadModule evalWithEnv currentDir moduleName []
        case moduleResult of
            Left err -> return $ Left $ TypeError $ "Failed to import module " ++ moduleName ++ ": " ++ err
            Right moduleInfo -> do
                let exportedEnv = filterByExports (moduleEnv moduleInfo) (moduleExports moduleInfo)
                let mergedEnv = Map.union exportedEnv env
                remainingResult <- processImports mergedEnv currentDir rest
                case remainingResult of
                    Left err -> return $ Left err
                    Right (finalEnv, remaining') -> return $ Right (finalEnv, remaining')
    processImports env currentDir (other : rest) = do
        remainingResult <- processImports env currentDir rest
        case remainingResult of
            Left err -> return $ Left err
            Right (finalEnv, remaining') -> return $ Right (finalEnv, other : remaining')
    
    go env [] = return $ Right VUnit
    go env (TLExpr expr : rest) = do
      result <- evalWithEnv env expr
      case result of
        Left err -> return $ Left err
        Right val -> do
          case rest of
            [] -> return $ Right val
            _ -> return $ Left $ TypeError "Expressions must be at the end of the program"
    go env (TLImport _ : rest) = go env rest
    go env (TLExport _ : rest) = go env rest
    go env (TLData _ _ constructors : rest) = do
      let env' = Map.union (dataConstructorsValueEnv constructors) env
      go env' rest
    go env (TLDef var maybeType expr : rest) = do
      case expr of
        LetRec _ _ _ _ -> do
          let (letrecs, remaining) = collectConsecutiveLetrecs (TLDef var maybeType expr : rest)
          processLetrecGroups env (dependencyOrderedLetrecGroups letrecs) remaining
        _ -> do
          result <- evalWithEnv env expr
          case result of
            Left err -> return $ Left err
            Right val -> do
              let env' = Map.insert var val env
              go env' rest

    processLetrecGroups :: Env -> [[TopLevel]] -> [TopLevel] -> IO (Either RuntimeError Value)
    processLetrecGroups env [] remaining = go env remaining
    processLetrecGroups env (group : groups) remaining = do
      groupResult <- processMutualRecursion env group
      case groupResult of
        Left err -> return $ Left err
        Right newEnv -> processLetrecGroups newEnv groups remaining

    processMutualRecursion :: Env -> [TopLevel] -> IO (Either RuntimeError Env)
    processMutualRecursion env letrecs = do
      refs <- mapM (\_ -> newIORef (VFun "_placeholder" (IntLit 0) Map.empty)) letrecs
      let refMap = Map.fromList $ zipWith (\topLevel ref ->
            case topLevel of
              TLDef var _ _ -> (var, VRef ref)
              _ -> error "processMutualRecursion: expected TLDef") letrecs refs
      let mutualEnv = Map.union refMap env
      let evalLetrec topLevel = case topLevel of
            TLDef _ _ expr -> case expr of
              LetRec _ _ recVal _ -> evalWithEnv mutualEnv recVal
              _ -> return $ Left $ TypeError $ "Expected LetRec expression, got: " ++ show expr
            _ -> return $ Left $ TypeError "Expected TLDef with LetRec"
      bindResult (traverseResults evalLetrec letrecs) $ \recValues -> do
          let checkAndUpdate (recValue, ref) =
                case recValue of
                  VFun _ _ _ -> do
                    writeIORef ref recValue
                    return $ Right ()
                  _ -> return $ Left $ TypeError ("LetRec value must be a function, got: " ++ show recValue)
          bindResult (traverseResults checkAndUpdate (zip recValues refs)) $ \_ ->
            return $ Right mutualEnv
