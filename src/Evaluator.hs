module Evaluator (
    Value(..),
    Env,
    RuntimeError(..),
    eval,
    evalWithEnv,
    evalPure,
    evalPureWithEnv
) where

import Syntax
import qualified Data.Map as Map
import Data.IORef
import System.IO (getLine, readFile, writeFile)
import Control.Monad (foldM)
import Control.Exception (try, SomeException)
import Data.Char (isSpace)
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


evalLiteralPure :: Expr -> Value
evalLiteralPure (IntLit n) = VInt n
evalLiteralPure (BoolLit b) = VBool b
evalLiteralPure (StrLit s) = VStr s
evalLiteralPure UnitLit = VUnit
evalLiteralPure _ = error "evalLiteralPure called on non-literal expression"

-- Evaluate expression (public interface - now IO-capable)
eval :: Expr -> IO (Either RuntimeError Value)
eval = evalWithEnv Map.empty

-- Pure eval for testing (no Input support)
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
  LetRec var _maybeType val body -> do
    let testEnv = Map.insert var (VFun "_placeholder" (IntLit 0) env) env
    let testResult = evalPureWithEnv testEnv val
    case testResult of
      Left err -> return $ Left $ TypeError $ "LetRec definition failed: " ++ show err
      Right _ -> do
        recValueRef <- newIORef (VFun "_placeholder" (IntLit 0) env)
        let env' = Map.insert var (VRef recValueRef) env
        let valResult = evalPureWithEnv env' val
        case valResult of
          Right recValue -> do
            writeIORef recValueRef recValue
            return $ evalPureWithEnv env' body
          Left err -> return $ Left err
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
  _ -> return $ evalPureWithEnv env expr
