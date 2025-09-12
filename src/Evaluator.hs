module Evaluator where

import Syntax
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

-- Values include functions (closures)
data Value
  = VInt Int
  | VBool Bool
  | VStr String
  | VUnit
  | VFun String Expr Env  -- parameter, body, captured environment
  deriving (Show, Eq)

-- Runtime environment for variables
type Env = Map.Map String Value

data RuntimeError
  = DivByZero
  | TypeError String
  | UnboundVariable String
  deriving (Show, Eq)

-- Evaluate expression (public interface)
eval :: Expr -> Either RuntimeError Value
eval = evalWithEnv Map.empty

-- Evaluate with environment
evalWithEnv :: Env -> Expr -> Either RuntimeError Value
evalWithEnv _ (IntLit n) = Right $ VInt n
evalWithEnv _ (BoolLit b) = Right $ VBool b
evalWithEnv _ (StrLit s) = Right $ VStr s
evalWithEnv _ UnitLit = Right VUnit

evalWithEnv env (Var x) = 
  case Map.lookup x env of
    Just v -> Right v
    Nothing -> Left $ UnboundVariable x

evalWithEnv env (Add e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 + n2)
    _ -> Left $ TypeError "Addition requires integer operands"

evalWithEnv env (Sub e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 - n2)
    _ -> Left $ TypeError "Subtraction requires integer operands"

evalWithEnv env (Mul e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VInt (n1 * n2)
    _ -> Left $ TypeError "Multiplication requires integer operands"

evalWithEnv env (Div e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VInt _, VInt 0) -> Left DivByZero
    (VInt n1, VInt n2) -> Right $ VInt (n1 `div` n2)
    _ -> Left $ TypeError "Division requires integer operands"

evalWithEnv env (Concat e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VStr s1, VStr s2) -> Right $ VStr (s1 ++ s2)
    _ -> Left $ TypeError "Concatenation requires string operands"

evalWithEnv env (And e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> Right $ VBool (b1 && b2)
    _ -> Left $ TypeError "AND requires boolean operands"

evalWithEnv env (Or e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VBool b1, VBool b2) -> Right $ VBool (b1 || b2)
    _ -> Left $ TypeError "OR requires boolean operands"

evalWithEnv env (Not e) = do
  v <- evalWithEnv env e
  case v of
    VBool b -> Right $ VBool (not b)
    _ -> Left $ TypeError "NOT requires a boolean operand"

evalWithEnv env (Eq e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 == n2)
    (VBool b1, VBool b2) -> Right $ VBool (b1 == b2)
    _ -> Left $ TypeError "Equality comparison requires operands of the same type"

evalWithEnv env (Lt e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 < n2)
    _ -> Left $ TypeError "Less than comparison requires integer operands"

evalWithEnv env (Gt e1 e2) = do
  v1 <- evalWithEnv env e1
  v2 <- evalWithEnv env e2
  case (v1, v2) of
    (VInt n1, VInt n2) -> Right $ VBool (n1 > n2)
    _ -> Left $ TypeError "Greater than comparison requires integer operands"

evalWithEnv env (If c t e) = do
  vc <- evalWithEnv env c
  case vc of
    VBool True -> evalWithEnv env t
    VBool False -> evalWithEnv env e
    _ -> Left $ TypeError "If condition must be a boolean"

-- Print: evaluate subexpr, print human-readable, return Unit
evalWithEnv env (Print e) = do
  v <- evalWithEnv env e
  case v of
    VInt n -> print n `seq` hFlush stdout `seq` Right VUnit
    VBool b -> print b `seq` hFlush stdout `seq` Right VUnit
    VStr s -> putStrLn s `seq` hFlush stdout `seq` Right VUnit
    VUnit -> putStrLn "()" `seq` hFlush stdout `seq` Right VUnit
    VFun {} -> putStr "<function>\n" `seq` hFlush stdout `seq` Right VUnit

-- Lambda creates a closure capturing the current environment
evalWithEnv env (Lambda param body) = 
  Right $ VFun param body env

-- Function application substitutes argument and evaluates body
evalWithEnv env (App fun arg) = do
  funVal <- evalWithEnv env fun
  argVal <- evalWithEnv env arg
  case funVal of
    VFun param body closureEnv -> 
      let env' = Map.insert param argVal closureEnv
      in evalWithEnv env' body
    _ -> Left $ TypeError "Cannot apply non-function value"

-- Let binding: evaluate value, extend environment, evaluate body
evalWithEnv env (Let var val body) = do
  valResult <- evalWithEnv env val
  let env' = Map.insert var valResult env
  evalWithEnv env' body

-- Recursive let binding: create recursive environment with fixed point
evalWithEnv env (LetRec var val body) = do
  let env' = Map.insert var recValue env
      recValue = case evalWithEnv env' val of
                   Right v -> v
                   Left err -> error $ "LetRec evaluation failed: " ++ show err
  evalWithEnv env' body

-- IO-based evaluator for handling Print statements
evalWithEnvIO :: Env -> Expr -> IO (Either RuntimeError Value)
evalWithEnvIO env (Print e) = do
  result <- evalWithEnvIO env e
  case result of
    Left err -> return $ Left err
    Right val -> do
      case val of
        VInt n -> print n
        VBool b -> print b
        VStr s -> putStrLn s
        VUnit -> putStrLn "()"
        VFun {} -> putStrLn "<function>"
      return $ Right VUnit

evalWithEnvIO env (Let var val body) = do
  valResult <- evalWithEnvIO env val
  case valResult of
    Left err -> return $ Left err
    Right valValue -> do
      let env' = Map.insert var valValue env
      evalWithEnvIO env' body

evalWithEnvIO env (LetRec var val body) = do
  let env' = Map.insert var recValue env
      recValue = case evalWithEnv env' val of
                   Right v -> v
                   Left err -> error $ "LetRec evaluation failed: " ++ show err
  evalWithEnvIO env' body

evalWithEnvIO env expr = return $ evalWithEnv env expr