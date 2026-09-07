module Evaluator.Helpers where

import Evaluator.Types
import qualified Data.Map as Map
import Data.List (intercalate)
import Syntax (isKaiInt)
import Control.Monad.Except (ExceptT(..), runExceptT)

-- Lift the evaluator callback into the same error monad used by pure operations.
-- ExceptT preserves left-to-right effects and stops immediately on a language error.
evalInIO :: (Eval (ExceptT RuntimeError IO) -> Eval (ExceptT RuntimeError IO)) -> EvalFuncIO -> EvalFuncIO
evalInIO operation evaluate env expression =
  runExceptT $ operation (\scope value -> ExceptT $ evaluate scope value) env expression

bindResult :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
bindResult action next = do
  result <- action
  case result of
    Left err -> return $ Left err
    Right value -> next value

parseIntString :: String -> Maybe Int
parseIntString s = case reads s of
  [(n, "")] | isKaiInt n -> Just (fromIntegral (n :: Integer))
  _ -> Nothing

showValue :: Value -> String
showValue (VInt n) = show n
showValue (VBool b) = show b
showValue (VStr s) = s
showValue VUnit = "()"
showValue (VFun {}) = "<function>"
showValue (VConstructor name _ []) = name
showValue (VConstructor name _ values) = name ++ "(" ++ intercalate ", " (map showValue values) ++ ")"
showValue (VData name []) = name
showValue (VData name values) = name ++ "(" ++ intercalate ", " (map showValue values) ++ ")"
showValue (VJust v) = "Just " ++ showValue v
showValue VNothing = "Nothing"
showValue (VLeft v) = "Left " ++ showValue v
showValue (VRight v) = "Right " ++ showValue v
showValue (VList l) = "[" ++ intercalate ", " (map showValue l) ++ "]"
showValue (VRecord r) = "{" ++ intercalate ", " [k ++ ": " ++ showValue v | (k,v) <- Map.toList r] ++ "}"
showValue (VRef _) = "<ref>"
showValue (VUninitialized name) = "<uninitialized " ++ name ++ ">"
showValue (VTuple vs) = "(" ++ intercalate ", " (map showValue vs) ++ ")"

extractString :: Value -> Either RuntimeError String
extractString (VStr s) = Right s
extractString _ = Left $ TypeError "join: list must contain only strings"
