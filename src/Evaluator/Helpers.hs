module Evaluator.Helpers where

import Evaluator.Types
import qualified Data.Map as Map
import Data.List (intercalate)

parseIntString :: String -> Maybe Int
parseIntString s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

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

extractString :: Value -> Either RuntimeError String
extractString (VStr s) = Right s
extractString _ = Left $ TypeError "join: list must contain only strings"
