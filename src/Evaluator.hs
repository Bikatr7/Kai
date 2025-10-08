module Evaluator where

import Syntax
import qualified Data.Map as Map
import System.IO (hFlush, stdout, getLine)
import Data.IORef
import Data.List (intercalate)
import Control.Monad (foldM)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import qualified System.IO as IO
import Control.Exception (try, SomeException)

-- Values include functions (closures)
data Value
  = VInt Int
  | VBool Bool
  | VStr String
  | VUnit
  | VFun String Expr Env  -- parameter, body, captured environment
  | VJust Value           -- Just constructor
  | VNothing              -- Nothing constructor
  | VLeft Value           -- Left constructor
  | VRight Value          -- Right constructor
  | VList [Value]
  | VRecord (Map.Map String Value)
  | VTuple [Value]
  | VRef (IORef Value)    -- Mutable reference for recursive bindings

instance Show Value where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VStr s) = show s
  show VUnit = "()"
  show (VFun param body env) = "<function " ++ param ++ ">"
  show (VJust v) = "Just (" ++ show v ++ ")"
  show VNothing = "Nothing"
  show (VLeft v) = "Left (" ++ show v ++ ")"
  show (VRight v) = "Right (" ++ show v ++ ")"
  show (VList vs) = show vs
  show (VRecord m) = show m
  show (VTuple vs) = "(" ++ intercalate ", " (map show vs) ++ ")"
  show (VRef _) = "<ref>"

instance Eq Value where
  (VInt n1) == (VInt n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  (VStr s1) == (VStr s2) = s1 == s2
  VUnit == VUnit = True
  (VJust v1) == (VJust v2) = v1 == v2
  VNothing == VNothing = True
  (VLeft v1) == (VLeft v2) = v1 == v2
  (VRight v1) == (VRight v2) = v1 == v2
  (VList vs1) == (VList vs2) = vs1 == vs2
  (VRecord m1) == (VRecord m2) = m1 == m2
  (VTuple vs1) == (VTuple vs2) = vs1 == vs2
  (VRef _) == (VRef _) = False  -- References are never equal
  (VFun {}) == (VFun {}) = False  -- Functions are never equal
  _ == _ = False

-- Runtime environment for variables
type Env = Map.Map String Value

data RuntimeError
  = DivByZero
  | TypeError String
  | UnboundVariable String
  | RecordFieldNotFound String
  deriving (Show, Eq)

-- Evaluate expression (public interface - now IO-capable)
eval :: Expr -> IO (Either RuntimeError Value)
eval = evalWithEnv Map.empty

-- Pure eval for testing (no Input support)
evalPure :: Expr -> Either RuntimeError Value
evalPure = evalPureWithEnv Map.empty

evalPureWithEnv :: Env -> Expr -> Either RuntimeError Value
evalPureWithEnv _ (IntLit n) = Right $ VInt n
evalPureWithEnv _ (BoolLit b) = Right $ VBool b
evalPureWithEnv _ (StrLit s) = Right $ VStr s
evalPureWithEnv _ UnitLit = Right VUnit
evalPureWithEnv _ Input = Left $ TypeError "Input not available in pure evaluation"
evalPureWithEnv _ Args = Left $ TypeError "Args not available in pure evaluation"
evalPureWithEnv env (Var x) =
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left $ UnboundVariable x
evalPureWithEnv env (Add e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
    _ -> Left $ TypeError "Addition requires integer operands"
evalPureWithEnv env (Sub e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
    _ -> Left $ TypeError "Subtraction requires integer operands"
evalPureWithEnv env (Mul e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 * n2)
    _ -> Left $ TypeError "Multiplication requires integer operands"
evalPureWithEnv env (Div e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VInt _, VInt 0) -> Left DivByZero
    (VInt n1, VInt n2) -> Right $ VInt (n1 `div` n2)
    _ -> Left $ TypeError "Division requires integer operands"
evalPureWithEnv env (Concat e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VStr s1, VStr s2) -> Right $ VStr (s1 ++ s2)
    (VList l1, VList l2) -> Right $ VList (l1 ++ l2)
    _ -> Left $ TypeError "Concatenation requires string or list operands"
evalPureWithEnv env (And e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> Right $ VBool (b1 && b2)
    _ -> Left $ TypeError "AND requires boolean operands"
evalPureWithEnv env (Or e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> Right $ VBool (b1 || b2)
    _ -> Left $ TypeError "OR requires boolean operands"
-- Sequencing: evaluate e1, discard result, return e2's value
evalPureWithEnv env (Seq e1 e2) = do
  _ <- evalPureWithEnv env e1  -- Evaluate first expression but discard result
  evalPureWithEnv env e2       -- Return second expression's result
evalPureWithEnv env (Not e) = do
  v <- evalPureWithEnv env e
  case v of
    VBool b -> Right $ VBool (not b)
    _ -> Left $ TypeError "NOT requires a boolean operand"
evalPureWithEnv env (Eq e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 == n2)
    (VBool b1, VBool b2) -> Right $ VBool (b1 == b2)
    (VStr s1, VStr s2) -> Right $ VBool (s1 == s2)
    (VUnit, VUnit) -> Right $ VBool True
    (VNothing, VNothing) -> Right $ VBool True
    (VJust v1', VJust v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
    (VLeft v1', VLeft v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
    (VRight v1', VRight v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
    (VList l1, VList l2) -> Right $ VBool (l1 == l2)
    (VRecord r1, VRecord r2) -> Right $ VBool (r1 == r2)
    (VTuple vs1, VTuple vs2) -> Right $ VBool (vs1 == vs2)
    _ -> Left $ TypeError "Equality comparison requires operands of the same type"
evalPureWithEnv env (Lt e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 < n2)
    _ -> Left $ TypeError "Less than comparison requires integer operands"
evalPureWithEnv env (Gt e1 e2) = do
  v1 <- evalPureWithEnv env e1
  v2 <- evalPureWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 > n2)
    _ -> Left $ TypeError "Greater than comparison requires integer operands"
evalPureWithEnv env (If c t e) = do
  vc <- evalPureWithEnv env c
  case vc of
    VBool True -> evalPureWithEnv env t
    VBool False -> evalPureWithEnv env e
    _ -> Left $ TypeError "If condition must be a boolean"
evalPureWithEnv env (Print e) = do
  _ <- evalPureWithEnv env e
  Right VUnit
evalPureWithEnv env (Lambda param _maybeType body) =
  Right $ VFun param body env
evalPureWithEnv env (App fun arg) = do
  funVal <- evalPureWithEnv env fun
  argVal <- evalPureWithEnv env arg
  case funVal of
    VFun param body closureEnv ->
      let env' = Map.insert param argVal closureEnv
      in evalPureWithEnv env' body
    _ -> Left $ TypeError "Cannot apply non-function value"
evalPureWithEnv env (Let var _maybeType val body) = do
  valResult <- evalPureWithEnv env val
  let env' = if var == "_" then env else Map.insert var valResult env
  evalPureWithEnv env' body
evalPureWithEnv env (LetRec var _maybeType val body) = do
  let testEnv = Map.insert var (VFun "_placeholder" (IntLit 0) env) env
  case evalPureWithEnv testEnv val of
    Left err -> Left $ TypeError $ "LetRec definition failed: " ++ show err
    Right _ -> do 
      let env' = Map.insert var recValue env
          recValue = case evalPureWithEnv env' val of
                       Right v -> v
                       Left err -> VFun "_error" (IntLit 0) env
      evalPureWithEnv env' body
evalPureWithEnv env (TypeAnnotation e _type) = evalPureWithEnv env e
evalPureWithEnv env (ParseInt e) = do
  result <- evalPureWithEnv env e
  case result of
    VStr s -> case parseIntString s of
      Just n -> Right $ VJust (VInt n)
      Nothing -> Right VNothing
    _ -> Left $ TypeError "parseInt requires string argument"
evalPureWithEnv env (ToString e) = do
  result <- evalPureWithEnv env e
  case result of
    VInt n -> Right $ VStr (show n)
    _ -> Left $ TypeError "toString requires integer argument"
evalPureWithEnv env (Show e) = do
  result <- evalPureWithEnv env e
  Right $ VStr (showValue result)
evalPureWithEnv env (MJust e) = do
  result <- evalPureWithEnv env e
  Right $ VJust result
evalPureWithEnv _ MNothing = Right VNothing
evalPureWithEnv env (ELeft e) = do
  result <- evalPureWithEnv env e
  Right $ VLeft result
evalPureWithEnv env (ERight e) = do
  result <- evalPureWithEnv env e
  Right $ VRight result
evalPureWithEnv env (ListLit es) = do
    vs <- mapM (evalPureWithEnv env) es
    Right $ VList vs
evalPureWithEnv env (Cons h t) = do
    vh <- evalPureWithEnv env h
    vt <- evalPureWithEnv env t
    case vt of
        VList l -> Right $ VList (vh:l)
        _ -> Left $ TypeError "Cons expects a list as its second argument"
evalPureWithEnv env (Head e) = do
    v <- evalPureWithEnv env e
    case v of
        VList (h:_) -> Right h
        VList [] -> Left $ TypeError "Head of an empty list"
        _ -> Left $ TypeError "Head expects a list"
evalPureWithEnv env (Tail e) = do
    v <- evalPureWithEnv env e
    case v of
        VList (_:t) -> Right $ VList t
        VList [] -> Left $ TypeError "Tail of an empty list"
        _ -> Left $ TypeError "Tail expects a list"
evalPureWithEnv env (Null e) = do
    v <- evalPureWithEnv env e
    case v of
        VList l -> Right $ VBool (null l)
        _ -> Left $ TypeError "Null expects a list"
evalPureWithEnv env (RecordLit fields) = do
    let evalField (name, e) = do
            v <- evalPureWithEnv env e
            return (name, v)
    evaledFields <- mapM evalField fields
    Right $ VRecord (Map.fromList evaledFields)
evalPureWithEnv env (RecordAccess r field) = do
    v <- evalPureWithEnv env r
    case v of
        VRecord m -> case Map.lookup field m of
            Just fv -> Right fv
            Nothing -> Left $ RecordFieldNotFound field
        _ -> Left $ TypeError "Record access expects a record"

-- Tuple literal
evalPureWithEnv env (TupleLit exprs) = do
    vals <- mapM (evalPureWithEnv env) exprs
    Right $ VTuple vals

-- fst and snd
evalPureWithEnv env (Fst e) = do
    v <- evalPureWithEnv env e
    case v of
        VTuple (v1:_) -> Right v1
        VTuple [] -> Left $ TypeError "fst: empty tuple"
        _ -> Left $ TypeError "fst: expected a tuple"

evalPureWithEnv env (Snd e) = do
    v <- evalPureWithEnv env e
    case v of
        VTuple (_:v2:_) -> Right v2
        VTuple _ -> Left $ TypeError "snd: tuple must have at least 2 elements"
        _ -> Left $ TypeError "snd: expected a tuple"

-- List functions
evalPureWithEnv env (Map f lst) = do
    fVal <- evalPureWithEnv env f
    lstVal <- evalPureWithEnv env lst
    case (fVal, lstVal) of
        (VFun param body closureEnv, VList vs) -> do
            results <- mapM (\v -> evalPureWithEnv (Map.insert param v closureEnv) body) vs
            Right $ VList results
        (_, VList _) -> Left $ TypeError "map: first argument must be a function"
        (VFun {}, _) -> Left $ TypeError "map: second argument must be a list"
        _ -> Left $ TypeError "map: invalid arguments"

evalPureWithEnv env (Filter f lst) = do
    fVal <- evalPureWithEnv env f
    lstVal <- evalPureWithEnv env lst
    case (fVal, lstVal) of
        (VFun param body closureEnv, VList vs) -> do
            results <- mapM (\v -> do
                r <- evalPureWithEnv (Map.insert param v closureEnv) body
                case r of
                    VBool b -> Right (v, b)
                    _ -> Left $ TypeError "filter: predicate must return a boolean") vs
            Right $ VList [v | (v, True) <- results]
        (_, VList _) -> Left $ TypeError "filter: first argument must be a function"
        (VFun {}, _) -> Left $ TypeError "filter: second argument must be a list"
        _ -> Left $ TypeError "filter: invalid arguments"

evalPureWithEnv env (Foldl f acc lst) = do
    fVal <- evalPureWithEnv env f
    accVal <- evalPureWithEnv env acc
    lstVal <- evalPureWithEnv env lst
    case (fVal, lstVal) of
        (VFun param1 body1 closureEnv, VList vs) -> do
            let foldStep currAcc v = do
                    -- First apply f to currAcc, which returns a function
                    case evalPureWithEnv (Map.insert param1 currAcc closureEnv) body1 of
                        Right (VFun param2 body2 closureEnv2) ->
                            evalPureWithEnv (Map.insert param2 v closureEnv2) body2
                        Right _ -> Left $ TypeError "foldl: function must take two arguments"
                        Left err -> Left err
            foldM foldStep accVal vs
        (_, VList _) -> Left $ TypeError "foldl: first argument must be a function"
        (VFun {}, _) -> Left $ TypeError "foldl: third argument must be a list"
        _ -> Left $ TypeError "foldl: invalid arguments"

evalPureWithEnv env (Length lst) = do
    lstVal <- evalPureWithEnv env lst
    case lstVal of
        VList vs -> Right $ VInt (length vs)
        _ -> Left $ TypeError "length: argument must be a list"

evalPureWithEnv env (Reverse lst) = do
    lstVal <- evalPureWithEnv env lst
    case lstVal of
        VList vs -> Right $ VList (reverse vs)
        _ -> Left $ TypeError "reverse: argument must be a list"

evalPureWithEnv env (Take n lst) = do
    nVal <- evalPureWithEnv env n
    lstVal <- evalPureWithEnv env lst
    case (nVal, lstVal) of
        (VInt count, VList vs) -> Right $ VList (take count vs)
        (VInt _, _) -> Left $ TypeError "take: second argument must be a list"
        (_, VList _) -> Left $ TypeError "take: first argument must be an integer"
        _ -> Left $ TypeError "take: invalid arguments"

evalPureWithEnv env (Drop n lst) = do
    nVal <- evalPureWithEnv env n
    lstVal <- evalPureWithEnv env lst
    case (nVal, lstVal) of
        (VInt count, VList vs) -> Right $ VList (drop count vs)
        (VInt _, _) -> Left $ TypeError "drop: second argument must be a list"
        (_, VList _) -> Left $ TypeError "drop: first argument must be an integer"
        _ -> Left $ TypeError "drop: invalid arguments"

evalPureWithEnv env (Zip l1 l2) = do
    l1Val <- evalPureWithEnv env l1
    l2Val <- evalPureWithEnv env l2
    case (l1Val, l2Val) of
        (VList vs1, VList vs2) -> Right $ VList [VTuple [v1, v2] | (v1, v2) <- zip vs1 vs2]
        (VList _, _) -> Left $ TypeError "zip: second argument must be a list"
        (_, VList _) -> Left $ TypeError "zip: first argument must be a list"
        _ -> Left $ TypeError "zip: invalid arguments"

-- String functions
evalPureWithEnv env (Split delim str) = do
    delimVal <- evalPureWithEnv env delim
    strVal <- evalPureWithEnv env str
    case (delimVal, strVal) of
        (VStr d, VStr s) -> Right $ VList (map VStr (splitOn d s))
        (VStr _, _) -> Left $ TypeError "split: second argument must be a string"
        (_, VStr _) -> Left $ TypeError "split: first argument must be a string"
        _ -> Left $ TypeError "split: invalid arguments"

evalPureWithEnv env (Join delim lst) = do
    delimVal <- evalPureWithEnv env delim
    lstVal <- evalPureWithEnv env lst
    case (delimVal, lstVal) of
        (VStr d, VList vs) -> do
            strs <- mapM extractString vs
            Right $ VStr (intercalate d strs)
        (VStr _, _) -> Left $ TypeError "join: second argument must be a list"
        (_, VList _) -> Left $ TypeError "join: first argument must be a string"
        _ -> Left $ TypeError "join: invalid arguments"
  where
    extractString (VStr s) = Right s
    extractString _ = Left $ TypeError "join: list must contain only strings"

evalPureWithEnv env (Trim str) = do
    strVal <- evalPureWithEnv env str
    case strVal of
        VStr s -> Right $ VStr (dropWhile isSpace $ dropWhileEnd isSpace s)
        _ -> Left $ TypeError "trim: argument must be a string"
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

evalPureWithEnv env (Replace old new str) = do
    oldVal <- evalPureWithEnv env old
    newVal <- evalPureWithEnv env new
    strVal <- evalPureWithEnv env str
    case (oldVal, newVal, strVal) of
        (VStr o, VStr n, VStr s) -> Right $ VStr (replaceAll o n s)
        (VStr _, VStr _, _) -> Left $ TypeError "replace: third argument must be a string"
        (VStr _, _, VStr _) -> Left $ TypeError "replace: second argument must be a string"
        (_, VStr _, VStr _) -> Left $ TypeError "replace: first argument must be a string"
        _ -> Left $ TypeError "replace: invalid arguments"
  where
    replaceAll old new str = intercalate new (splitOn old str)

evalPureWithEnv env (StrLength str) = do
    strVal <- evalPureWithEnv env str
    case strVal of
        VStr s -> Right $ VInt (length s)
        _ -> Left $ TypeError "strLength: argument must be a string"

-- File I/O (not available in pure evaluation)
evalPureWithEnv _ (ReadFile _) = Left $ TypeError "readFile not available in pure evaluation"
evalPureWithEnv _ (WriteFile _ _) = Left $ TypeError "writeFile not available in pure evaluation"

evalPureWithEnv env (Case scrutinee patterns) = do
  val <- evalPureWithEnv env scrutinee
  tryPatterns env val patterns
  where
    tryPatterns _ _ [] = Left $ TypeError "No matching pattern in case expression"
    tryPatterns env val ((pat, expr) : rest) = do
      case matchPattern pat val of
        Nothing -> tryPatterns env val rest
        Just bindings ->
          let newEnv = Map.union bindings env
          in evalPureWithEnv newEnv expr

-- Evaluate with environment (now IO-capable)
evalWithEnv :: Env -> Expr -> IO (Either RuntimeError Value)
evalWithEnv _ (IntLit n) = return $ Right $ VInt n
evalWithEnv _ (BoolLit b) = return $ Right $ VBool b
evalWithEnv _ (StrLit s) = return $ Right $ VStr s
evalWithEnv _ UnitLit = return $ Right VUnit
evalWithEnv _ Input = Right . VStr <$> getLine
evalWithEnv env Args =
  case Map.lookup "__args__" env of
    Just v -> return $ Right v
    Nothing -> return $ Right $ VList []  -- Default to empty list if no args

evalWithEnv env (Var x) = do
  case Map.lookup x env of
    Just (VRef ref) -> do
      val <- readIORef ref
      return $ Right val
    Just v -> return $ Right v
    Nothing -> return $ Left $ UnboundVariable x

evalWithEnv env (Add e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
      _ -> Left $ TypeError "Addition requires integer operands"

evalWithEnv env (Sub e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
      _ -> Left $ TypeError "Subtraction requires integer operands"

evalWithEnv env (Mul e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VInt (n1 * n2)
      _ -> Left $ TypeError "Multiplication requires integer operands"

evalWithEnv env (Div e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt _, VInt 0) -> Left DivByZero
      (VInt n1, VInt n2) -> Right $ VInt (n1 `div` n2)
      _ -> Left $ TypeError "Division requires integer operands"

evalWithEnv env (Concat e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VStr s1, VStr s2) -> Right $ VStr (s1 ++ s2)
      (VList l1, VList l2) -> Right $ VList (l1 ++ l2)
      _ -> Left $ TypeError "Concatenation requires string or list operands"

evalWithEnv env (And e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VBool b1, VBool b2) -> Right $ VBool (b1 && b2)
      _ -> Left $ TypeError "AND requires boolean operands"

evalWithEnv env (Or e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VBool b1, VBool b2) -> Right $ VBool (b1 || b2)
      _ -> Left $ TypeError "OR requires boolean operands"

-- Sequencing with IO: evaluate e1 for side effects, discard result, return e2's value
evalWithEnv env (Seq e1 e2) = do
  r1 <- evalWithEnv env e1  -- Evaluate first for side effects
  r2 <- evalWithEnv env e2  -- Evaluate second
  return $ do
    _ <- r1  -- Check first succeeded but discard result
    r2       -- Return second's result

evalWithEnv env (Not e) = do
  r <- evalWithEnv env e
  return $ do
    v <- r
    case v of
      VBool b -> Right $ VBool (not b)
      _ -> Left $ TypeError "NOT requires a boolean operand"

evalWithEnv env (Eq e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 == n2)
      (VBool b1, VBool b2) -> Right $ VBool (b1 == b2)
      (VStr s1, VStr s2) -> Right $ VBool (s1 == s2)
      (VUnit, VUnit) -> Right $ VBool True
      (VNothing, VNothing) -> Right $ VBool True
      (VJust v1', VJust v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
      (VLeft v1', VLeft v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
      (VRight v1', VRight v2') -> if v1' == v2' then Right $ VBool True else Right $ VBool False
      (VList l1, VList l2) -> Right $ VBool (l1 == l2)
      (VRecord r1, VRecord r2) -> Right $ VBool (r1 == r2)
      (VTuple vs1, VTuple vs2) -> Right $ VBool (vs1 == vs2)
      _ -> Left $ TypeError "Equality comparison requires operands of the same type"

evalWithEnv env (Lt e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 < n2)
      _ -> Left $ TypeError "Less than comparison requires integer operands"

evalWithEnv env (Gt e1 e2) = do
  r1 <- evalWithEnv env e1
  r2 <- evalWithEnv env e2
  return $ do
    v1 <- r1
    v2 <- r2
    case (v1, v2) of
      (VInt n1, VInt n2) -> Right $ VBool (n1 > n2)
      _ -> Left $ TypeError "Greater than comparison requires integer operands"

evalWithEnv env (If c t e) = do
  rc <- evalWithEnv env c
  case rc of
    Left err -> return $ Left err
    Right (VBool True) -> evalWithEnv env t
    Right (VBool False) -> evalWithEnv env e
    Right _ -> return $ Left $ TypeError "If condition must be a boolean"

-- Print: evaluate subexpr, print human-readable, return Unit
evalWithEnv env (Print e) = do
  result <- evalWithEnv env e
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

-- Lambda creates a closure capturing the current environment (ignore type annotation)
evalWithEnv env (Lambda param _maybeType body) =
  return $ Right $ VFun param body env

-- Function application substitutes argument and evaluates body
evalWithEnv env (App fun arg) = do
  funResult <- evalWithEnv env fun
  argResult <- evalWithEnv env arg
  case (funResult, argResult) of
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err
    (Right (VFun param body closureEnv), Right argVal) ->
      let env' = Map.insert param argVal closureEnv
      in evalWithEnv env' body
    (Right _, Right _) -> return $ Left $ TypeError "Cannot apply non-function value"

-- Let binding: evaluate value, extend environment, evaluate body (ignore type annotation)
evalWithEnv env (Let var _maybeType val body) = do
  valResult <- evalWithEnv env val
  case valResult of
    Left err -> return $ Left err
    Right valValue ->
      -- Don't add wildcard variables to the environment
      let env' = if var == "_" then env else Map.insert var valValue env
      in evalWithEnv env' body

-- Recursive let binding: create recursive environment with fixed point (ignore type annotation)
evalWithEnv env (LetRec var _maybeType val body) = do
  let testEnv = Map.insert var (VFun "_placeholder" (IntLit 0) env) env
  testResult <- evalWithEnv testEnv val
  case testResult of
    Left err -> return $ Left $ TypeError $ "LetRec definition failed: " ++ show err
    Right _ -> do
      recValueRef <- newIORef (VFun "_placeholder" (IntLit 0) env)
      let env' = Map.insert var (VRef recValueRef) env

      valResult <- evalWithEnv env' val
      case valResult of
        Right recValue -> do
          writeIORef recValueRef recValue
          evalWithEnv env' body
        Left err -> return $ Left err

-- Type annotation: just evaluate the expression (ignore type)
evalWithEnv env (TypeAnnotation e _type) = evalWithEnv env e

-- Built-in conversion functions (basic implementation)
evalWithEnv env (ParseInt e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right (VStr s) -> case parseIntString s of
      Just n -> Right $ VJust (VInt n)
      Nothing -> Right VNothing
    Right _ -> Left $ TypeError "parseInt requires string argument"

evalWithEnv env (ToString e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right (VInt n) -> Right $ VStr (show n)
    Right _ -> Left $ TypeError "toString requires integer argument"

evalWithEnv env (Show e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right val -> Right $ VStr (showValue val)

-- Maybe/Either constructors
evalWithEnv env (MJust e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right val -> Right $ VJust val

evalWithEnv _ MNothing = return $ Right VNothing

evalWithEnv env (ELeft e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right val -> Right $ VLeft val

evalWithEnv env (ERight e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right val -> Right $ VRight val

evalWithEnv env (ListLit exprs) = do
  results <- mapM (evalWithEnv env) exprs
  return $ case sequence results of
    Left err -> Left err
    Right vals -> Right $ VList vals

evalWithEnv env (RecordLit fields) = do
  evaledFields <- mapM evalField fields
  return $ case sequence evaledFields of
    Left err -> Left err
    Right vals -> Right $ VRecord (Map.fromList vals)
  where
    evalField (name, expr) = do
      result <- evalWithEnv env expr
      return $ case result of
        Left err -> Left err
        Right val -> Right (name, val)

evalWithEnv env (RecordAccess record field) = do
  result <- evalWithEnv env record
  return $ case result of
    Left err -> Left err
    Right v -> case v of
      VRecord m -> case Map.lookup field m of
        Just fv -> Right fv
        Nothing -> Left $ TypeError $ "Record field '" ++ field ++ "' not found"
      _ -> Left $ TypeError "Record access on non-record value"

-- Tuple literal
evalWithEnv env (TupleLit exprs) = do
  results <- mapM (evalWithEnv env) exprs
  return $ case sequence results of
    Left err -> Left err
    Right vals -> Right $ VTuple vals

-- fst and snd
evalWithEnv env (Fst e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right v -> case v of
      VTuple (v1:_) -> Right v1
      VTuple [] -> Left $ TypeError "fst: empty tuple"
      _ -> Left $ TypeError "fst: expected a tuple"

evalWithEnv env (Snd e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right v -> case v of
      VTuple (_:v2:_) -> Right v2
      VTuple _ -> Left $ TypeError "snd: tuple must have at least 2 elements"
      _ -> Left $ TypeError "snd: expected a tuple"

-- List functions
evalWithEnv env (Map f lst) = do
    fResult <- evalWithEnv env f
    lstResult <- evalWithEnv env lst
    case (fResult, lstResult) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right (VFun param body closureEnv), Right (VList vs)) -> do
            results <- mapM (\v -> evalWithEnv (Map.insert param v closureEnv) body) vs
            case sequence results of
                Left err -> return $ Left err
                Right vals -> return $ Right $ VList vals
        (Right _, Right (VList _)) -> return $ Left $ TypeError "map: first argument must be a function"
        (Right (VFun {}), Right _) -> return $ Left $ TypeError "map: second argument must be a list"
        _ -> return $ Left $ TypeError "map: invalid arguments"

evalWithEnv env (Filter f lst) = do
    fResult <- evalWithEnv env f
    lstResult <- evalWithEnv env lst
    case (fResult, lstResult) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right (VFun param body closureEnv), Right (VList vs)) -> do
            results <- mapM (\v -> do
                r <- evalWithEnv (Map.insert param v closureEnv) body
                return $ case r of
                    Right (VBool b) -> Right (v, b)
                    Right _ -> Left $ TypeError "filter: predicate must return a boolean"
                    Left err -> Left err) vs
            case sequence results of
                Left err -> return $ Left err
                Right pairs -> return $ Right $ VList [v | (v, True) <- pairs]
        (Right _, Right (VList _)) -> return $ Left $ TypeError "filter: first argument must be a function"
        (Right (VFun {}), Right _) -> return $ Left $ TypeError "filter: second argument must be a list"
        _ -> return $ Left $ TypeError "filter: invalid arguments"

evalWithEnv env (Foldl f acc lst) = do
    fResult <- evalWithEnv env f
    accResult <- evalWithEnv env acc
    lstResult <- evalWithEnv env lst
    case (fResult, accResult, lstResult) of
        (Left err, _, _) -> return $ Left err
        (_, Left err, _) -> return $ Left err
        (_, _, Left err) -> return $ Left err
        (Right (VFun param1 body1 closureEnv), Right accVal, Right (VList vs)) -> do
            let foldStep currAcc v = do
                    -- First apply f to currAcc, which returns a function
                    r1 <- evalWithEnv (Map.insert param1 currAcc closureEnv) body1
                    case r1 of
                        Right (VFun param2 body2 closureEnv2) ->
                            evalWithEnv (Map.insert param2 v closureEnv2) body2
                        Right _ -> return $ Left $ TypeError "foldl: function must take two arguments"
                        Left err -> return $ Left err
            result <- foldM (\acc v -> do
                r <- foldStep acc v
                case r of
                    Right val -> return val
                    Left err -> return $ VInt 0) accVal vs  -- Error handling placeholder
            -- Check if we hit an error
            case result of
                VInt 0 | null vs -> return $ Right accVal
                _ -> return $ Right result
        (Right _, Right _, Right (VList _)) -> return $ Left $ TypeError "foldl: first argument must be a function"
        (Right (VFun {}), Right _, Right _) -> return $ Left $ TypeError "foldl: third argument must be a list"
        _ -> return $ Left $ TypeError "foldl: invalid arguments"

evalWithEnv env (Length lst) = do
    lstResult <- evalWithEnv env lst
    return $ case lstResult of
        Left err -> Left err
        Right (VList vs) -> Right $ VInt (length vs)
        Right _ -> Left $ TypeError "length: argument must be a list"

evalWithEnv env (Reverse lst) = do
    lstResult <- evalWithEnv env lst
    return $ case lstResult of
        Left err -> Left err
        Right (VList vs) -> Right $ VList (reverse vs)
        Right _ -> Left $ TypeError "reverse: argument must be a list"

evalWithEnv env (Take n lst) = do
    nResult <- evalWithEnv env n
    lstResult <- evalWithEnv env lst
    return $ case (nResult, lstResult) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (VInt count), Right (VList vs)) -> Right $ VList (take count vs)
        (Right (VInt _), Right _) -> Left $ TypeError "take: second argument must be a list"
        (Right _, Right (VList _)) -> Left $ TypeError "take: first argument must be an integer"
        _ -> Left $ TypeError "take: invalid arguments"

evalWithEnv env (Drop n lst) = do
    nResult <- evalWithEnv env n
    lstResult <- evalWithEnv env lst
    return $ case (nResult, lstResult) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (VInt count), Right (VList vs)) -> Right $ VList (drop count vs)
        (Right (VInt _), Right _) -> Left $ TypeError "drop: second argument must be a list"
        (Right _, Right (VList _)) -> Left $ TypeError "drop: first argument must be an integer"
        _ -> Left $ TypeError "drop: invalid arguments"

evalWithEnv env (Zip l1 l2) = do
    l1Result <- evalWithEnv env l1
    l2Result <- evalWithEnv env l2
    return $ case (l1Result, l2Result) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (VList vs1), Right (VList vs2)) -> Right $ VList [VTuple [v1, v2] | (v1, v2) <- zip vs1 vs2]
        (Right (VList _), Right _) -> Left $ TypeError "zip: second argument must be a list"
        (Right _, Right (VList _)) -> Left $ TypeError "zip: first argument must be a list"
        _ -> Left $ TypeError "zip: invalid arguments"

-- String functions
evalWithEnv env (Split delim str) = do
    delimResult <- evalWithEnv env delim
    strResult <- evalWithEnv env str
    return $ case (delimResult, strResult) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (VStr d), Right (VStr s)) -> Right $ VList (map VStr (splitOn d s))
        (Right (VStr _), Right _) -> Left $ TypeError "split: second argument must be a string"
        (Right _, Right (VStr _)) -> Left $ TypeError "split: first argument must be a string"
        _ -> Left $ TypeError "split: invalid arguments"

evalWithEnv env (Join delim lst) = do
    delimResult <- evalWithEnv env delim
    lstResult <- evalWithEnv env lst
    return $ case (delimResult, lstResult) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right (VStr d), Right (VList vs)) -> do
            let strs = mapM extractString vs
            case strs of
                Right ss -> Right $ VStr (intercalate d ss)
                Left err -> Left err
        (Right (VStr _), Right _) -> Left $ TypeError "join: second argument must be a list"
        (Right _, Right (VList _)) -> Left $ TypeError "join: first argument must be a string"
        _ -> Left $ TypeError "join: invalid arguments"
  where
    extractString (VStr s) = Right s
    extractString _ = Left $ TypeError "join: list must contain only strings"

evalWithEnv env (Trim str) = do
    strResult <- evalWithEnv env str
    return $ case strResult of
        Left err -> Left err
        Right (VStr s) -> Right $ VStr (dropWhile isSpace $ dropWhileEnd isSpace s)
        Right _ -> Left $ TypeError "trim: argument must be a string"
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

evalWithEnv env (Replace old new str) = do
    oldResult <- evalWithEnv env old
    newResult <- evalWithEnv env new
    strResult <- evalWithEnv env str
    return $ case (oldResult, newResult, strResult) of
        (Left err, _, _) -> Left err
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
        (Right (VStr o), Right (VStr n), Right (VStr s)) ->
            Right $ VStr (replaceAll o n s)
        (Right (VStr _), Right (VStr _), Right _) ->
            Left $ TypeError "replace: third argument must be a string"
        (Right (VStr _), Right _, Right (VStr _)) ->
            Left $ TypeError "replace: second argument must be a string"
        (Right _, Right (VStr _), Right (VStr _)) ->
            Left $ TypeError "replace: first argument must be a string"
        _ -> Left $ TypeError "replace: invalid arguments"
  where
    replaceAll old new str = intercalate new (splitOn old str)

evalWithEnv env (StrLength str) = do
    strResult <- evalWithEnv env str
    return $ case strResult of
        Left err -> Left err
        Right (VStr s) -> Right $ VInt (length s)
        Right _ -> Left $ TypeError "strLength: argument must be a string"

-- File I/O
evalWithEnv env (ReadFile path) = do
    pathResult <- evalWithEnv env path
    case pathResult of
        Left err -> return $ Left err
        Right (VStr p) -> do
            result <- try (IO.readFile p) :: IO (Either SomeException String)
            case result of
                Right contents -> return $ Right $ VStr contents
                Left _ -> return $ Left $ TypeError $ "readFile: could not read file '" ++ p ++ "'"
        Right _ -> return $ Left $ TypeError "readFile: path must be a string"

evalWithEnv env (WriteFile path content) = do
    pathResult <- evalWithEnv env path
    contentResult <- evalWithEnv env content
    case (pathResult, contentResult) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right (VStr p), Right (VStr c)) -> do
            result <- try (IO.writeFile p c) :: IO (Either SomeException ())
            case result of
                Right _ -> return $ Right VUnit
                Left _ -> return $ Left $ TypeError $ "writeFile: could not write to file '" ++ p ++ "'"
        (Right (VStr _), Right _) -> return $ Left $ TypeError "writeFile: content must be a string"
        (Right _, Right _) -> return $ Left $ TypeError "writeFile: path must be a string"

evalWithEnv env (Head e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right v -> case v of
      VList (x:_) -> Right x
      VList [] -> Left $ TypeError "Cannot take head of empty list"
      _ -> Left $ TypeError "Head requires list operand"

evalWithEnv env (Tail e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right v -> case v of
      VList (_:xs) -> Right $ VList xs
      VList [] -> Left $ TypeError "Cannot take tail of empty list"
      _ -> Left $ TypeError "Tail requires list operand"

evalWithEnv env (Null e) = do
  result <- evalWithEnv env e
  return $ case result of
    Left err -> Left err
    Right v -> case v of
      VList [] -> Right $ VBool True
      VList _ -> Right $ VBool False
      _ -> Left $ TypeError "Null requires list operand"

evalWithEnv env (Cons h t) = do
  hResult <- evalWithEnv env h
  tResult <- evalWithEnv env t
  return $ do
    hVal <- hResult
    tVal <- tResult
    case tVal of
      VList xs -> Right $ VList (hVal : xs)
      _ -> Left $ TypeError "Cons requires list as second operand"

-- Case expression for pattern matching
evalWithEnv env (Case scrutinee patterns) = do
  scrutResult <- evalWithEnv env scrutinee
  case scrutResult of
    Left err -> return $ Left err
    Right val -> tryPatterns env val patterns
  where
    tryPatterns _ _ [] = return $ Left $ TypeError "No matching pattern in case expression"
    tryPatterns env val ((pat, expr) : rest) = do
      case matchPattern pat val of
        Nothing -> tryPatterns env val rest
        Just bindings ->
          let newEnv = Map.union bindings env
          in evalWithEnv newEnv expr


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

-- Helper function to parse integer strings (basic implementation)
parseIntString :: String -> Maybe Int
parseIntString s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

-- Helper function to show values
showValue :: Value -> String
showValue (VInt n) = show n
showValue (VBool b) = show b
showValue (VStr s) = s
showValue VUnit = "()"
showValue (VFun {}) = "<function>"
showValue (VJust v) = "Just " ++ showValue v
showValue VNothing = "Nothing"
showValue (VLeft v) = "Left " ++ showValue v
showValue (VRight v) = "Right " ++ showValue v
showValue (VList l) = "[" ++ intercalate ", " (map showValue l) ++ "]"
showValue (VRecord r) = "{" ++ concatMap (\(k,v) -> k ++ ": " ++ showValue v) (Map.toList r) ++ "}"
showValue (VTuple vs) = "(" ++ intercalate ", " (map showValue vs) ++ ")"