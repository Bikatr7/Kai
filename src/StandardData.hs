module StandardData (standardDataDeclarations, standardTypeArity) where

import Syntax
import Data.List (find)

-- These declarations have one identity in every script, module and REPL.
standardDataDeclarations :: [(String, [String], [DataConstructor])]
standardDataDeclarations =
  [ ("IOErrorKind", [], [DataConstructor name [] | name <-
      ["NotFound", "PermissionDenied", "AlreadyExists", "InvalidPath",
       "InvalidEncoding", "ResourceBusy", "OtherIO"]])
  , ("Error", [],
      [ DataConstructor "DivisionByZero" []
      , DataConstructor "ArithmeticOverflow" []
      , DataConstructor "EmptyList" [STString]
      , DataConstructor "EndOfInput" []
      , DataConstructor "IOError" [STCustom "IOErrorKind" [], STString, STMaybe STString, STString]
      , DataConstructor "UserError" [STString]
      ])
  ]

standardTypeArity :: String -> Maybe Int
standardTypeArity name = case find (\(n,_,_) -> n == name) standardDataDeclarations of
  Just (_,parameters,_) -> Just (length parameters)
  Nothing -> Nothing
