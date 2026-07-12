module Evaluator.Helpers where

import Evaluator.Types
import qualified Data.Map as Map
import Data.List (intercalate)
import Syntax (isKaiInt)

bindResult :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
bindResult action next = do
  result <- action
  case result of
    Left err -> return $ Left err
    Right value -> next value

traverseResults :: (a -> IO (Either e b)) -> [a] -> IO (Either e [b])
traverseResults action = go []
  where
    go values [] = return $ Right $ reverse values
    go values (item:items) =
      bindResult (action item) $ \value -> go (value:values) items

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
showValue (VRecord r) = "{" ++ concatMap (\(k,v) -> k ++ ": " ++ showValue v) (Map.toList r) ++ "}"
showValue (VTuple vs) = "(" ++ intercalate ", " (map showValue vs) ++ ")"

extractString :: Value -> Either RuntimeError String
extractString (VStr s) = Right s
extractString _ = Left $ TypeError "join: list must contain only strings"
