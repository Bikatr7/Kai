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
import qualified Data.Map as Map
import Data.IORef
import System.IO (getLine, readFile, writeFile)
import Control.Monad (foldM)
import Control.Exception (try, SomeException)
import Data.Char (isSpace)
import Data.Either (rights)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Evaluator.Types
import Evaluator.Helpers (parseIntString, showValue)
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
  Case _ _ -> evalPatterns evalPureWithEnv env expr

evalWithEnv :: Env -> Expr -> IO (Either RuntimeError Value)
evalWithEnv env expr = case expr of
  Input -> evalIOWithEnv evalWithEnv env expr
  Args -> evalIOWithEnv evalWithEnv env expr
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
  Fix e -> do
    fResult <- evalWithEnv env e
    case fResult of
      Right fVal -> case fVal of
        VFun param body closure -> do
          recRef <- newIORef (error "fix not initialized")
          let recVal = VRef recRef
          let env' = Map.insert param recVal closure
          result <- evalWithEnv env' body
          case result of
            Right finalVal -> do
              writeIORef recRef finalVal
              return $ Right finalVal
            Left err -> return $ Left err
        _ -> return $ Left $ TypeError "Fix expects a function"
      Left err -> return $ Left err
  LetRec var _maybeType val body -> do
    recValueRef <- newIORef (VFun "_placeholder" (IntLit 0) Map.empty)
    let env' = Map.insert var (VRef recValueRef) env
    valResult <- evalWithEnv env' val
    case valResult of
      Left err -> return $ Left err
      Right recValue -> do
        writeIORef recValueRef recValue
        evalWithEnv env' body
  Print e -> do
    let result = evalPureWithEnv env e
    case result of
      Left err -> return $ Left err
      Right v -> do
        case v of
          VInt n -> print n
          VBool b -> print b
          VStr s -> putStrLn s
          VUnit -> putStrLn "()"
          VFun {} -> putStrLn "<function>"
          VJust val -> putStrLn $ "Just " ++ showValue val
          VNothing -> putStrLn "Nothing"
          VLeft val -> putStrLn $ "Left " ++ showValue val
          VRight val -> putStrLn $ "Right " ++ showValue val
          VList l -> putStrLn $ showValue (VList l)
          VRecord r -> putStrLn $ showValue (VRecord r)
        return $ Right VUnit
  ReadFile path -> do
    let pathResult = evalPureWithEnv env path
    case pathResult of
        Left err -> return $ Left err
        Right (VStr p) -> do
          result <- try (readFile p) :: IO (Either SomeException String)
          case result of
            Right contents -> return $ Right $ VStr contents
            Left _ -> return $ Left $ TypeError $ "readFile: could not read file '" ++ p ++ "'"
        Right _ -> return $ Left $ TypeError "readFile: path must be a string"
  WriteFile path content -> do
    let pathResult = evalPureWithEnv env path
    let contentResult = evalPureWithEnv env content
    case (pathResult, contentResult) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right (VStr p), Right (VStr c)) -> do
          result <- try (writeFile p c) :: IO (Either SomeException ())
          case result of
            Right _ -> return $ Right VUnit
            Left _ -> return $ Left $ TypeError $ "writeFile: could not write to file '" ++ p ++ "'"
        (Right (VStr _), Right _) -> return $ Left $ TypeError "writeFile: content must be a string"
        (Right _, Right _) -> return $ Left $ TypeError "writeFile: path must be a string"
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
  RecordAccess r field -> do
    result <- evalWithEnv env r
    case result of
      Right (VRecord fields) -> return $ case Map.lookup field fields of
        Just val -> Right val
        Nothing -> Left $ UnboundVariable field
      Right _ -> return $ Left $ TypeError "Cannot access field on non-record value"
      Left err -> return $ Left err
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
    go env (TLDef var maybeType expr : rest) = do
      case expr of
        LetRec _ _ _ _ -> do
          let (letrecs, remaining) = collectConsecutiveLetrecs (TLDef var maybeType expr : rest)
          processMutualRecursion env letrecs remaining
        _ -> do
          result <- evalWithEnv env expr
          case result of
            Left err -> return $ Left err
            Right val -> do
              let env' = Map.insert var val env
              go env' rest
    
    collectConsecutiveLetrecs :: [TopLevel] -> ([TopLevel], [TopLevel])
    collectConsecutiveLetrecs [] = ([], [])
    collectConsecutiveLetrecs (TLDef var maybeType expr : rest) =
      case expr of
        LetRec _ _ _ _ ->
          let (moreLetrecs, remaining) = collectConsecutiveLetrecs rest
          in (TLDef var maybeType expr : moreLetrecs, remaining)
        _ -> ([], TLDef var maybeType expr : rest)
    collectConsecutiveLetrecs (other : rest) = ([], other : rest)
    
    processMutualRecursion :: Env -> [TopLevel] -> [TopLevel] -> IO (Either RuntimeError Value)
    processMutualRecursion env letrecs remaining = do
      refs <- mapM (\_ -> newIORef (VFun "_placeholder" (IntLit 0) Map.empty)) letrecs
      let refMap = Map.fromList $ zipWith (\topLevel ref ->
            case topLevel of
              TLDef var _ _ -> (var, VRef ref)
              _ -> error "processMutualRecursion: expected TLDef") letrecs refs
      let mutualEnv = Map.union refMap env
      results <- mapM (\topLevel ->
        case topLevel of
          TLDef var _ expr ->
            case expr of
              LetRec _ _ recVal _ -> evalWithEnv mutualEnv recVal
              _ -> return $ Left $ TypeError $ "Expected LetRec expression, got: " ++ show expr
          _ -> return $ Left $ TypeError "Expected TLDef with LetRec") letrecs
      let findError = foldr (\result acc -> case result of Left err -> Left err; Right _ -> acc) (Right ()) results
      case findError of
        Left err -> return $ Left err
        Right _ -> do
          let recValues = rights results
          let checkAndUpdate (recValue, ref) =
                case recValue of
                  VFun _ _ _ -> do
                    writeIORef ref recValue
                    return $ Right ()
                  _ -> return $ Left $ TypeError ("LetRec value must be a function, got: " ++ show recValue)
          updateResults <- mapM checkAndUpdate (zip recValues refs)
          let checkUpdates = foldr (\result acc -> case result of Left err -> Left err; Right _ -> acc) (Right ()) updateResults
          case checkUpdates of
            Left err -> return $ Left err
            Right _ -> do
              go mutualEnv remaining
