module Evaluator where

import Syntax
import qualified Data.Map as Map
import System.IO (hFlush, stdout, getLine)

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
  deriving (Show, Eq)

-- Runtime environment for variables
type Env = Map.Map String Value

data RuntimeError
  = DivByZero
  | TypeError String
  | UnboundVariable String
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
    _ -> Left $ TypeError "Concatenation requires string operands"

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
  Right VUnit  -- Pure version doesn't actually print

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
  let env' = Map.insert var valResult env
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

evalWithEnv env (Var x) =
  return $ case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left $ UnboundVariable x

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
      _ -> Left $ TypeError "Concatenation requires string operands"

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
      let env' = Map.insert var valValue env
      in evalWithEnv env' body

-- Recursive let binding: create recursive environment with fixed point (ignore type annotation)
evalWithEnv env (LetRec var _maybeType val body) = do
  let env' = Map.insert var (VFun "_placeholder" val env) env
  valResult <- evalWithEnv env' val
  case valResult of
    Left err -> return $ Left err
    Right recValue ->
      let finalEnv = Map.insert var recValue env
      in evalWithEnv finalEnv body

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