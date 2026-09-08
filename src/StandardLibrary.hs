module StandardLibrary (standardTypeEnv, standardValueEnv, builtinDefinitions) where

import qualified Data.Map as Map
import Syntax
import Evaluator.Types (Env, Value(..))
import TypeChecker.Types (Type(..), TypeEnv)
import TypeChecker.Substitution (generalize)
import DataDeclarations (standardDataTypeEnv, standardDataValueEnv)

-- One source of truth for callable names, types, and primitive bodies. The
-- parser treats these names exactly like user functions. Internal parameters
-- cannot be written in Kai and therefore cannot capture a user's binding.
builtinDefinitions :: [(String, Type, Expr)]
builtinDefinitions =
  [ unary "print" a TUnit Print
  , unary "discard" a TUnit Discard
  , unary "parseInt" TString (TMaybe TInt) ParseInt
  , unary "toString" TInt TString ToString
  , unary "show" a TString Show
  , unary "fix" (TFun a a) a Fix
  , unary "head" (TList a) a Head
  , unary "tail" (TList a) (TList a) Tail
  , unary "null" (TList a) TBool Null
  , unary "length" (TList a) TInt Length
  , unary "reverse" (TList a) (TList a) Reverse
  , unary "fst" (TTuple [a,b]) a Fst
  , unary "snd" (TTuple [a,b]) b Snd
  , binary "map" (TFun a b) (TList a) (TList b) Map
  , binary "filter" (TFun a TBool) (TList a) (TList a) Filter
  , ternary "foldl" (TFun b (TFun a b)) b (TList a) b Foldl
  , binary "take" TInt (TList a) (TList a) Take
  , binary "drop" TInt (TList a) (TList a) Drop
  , binary "zip" (TList a) (TList b) (TList (TTuple [a,b])) Zip
  , binary "split" TString TString (TList TString) Split
  , binary "join" TString (TList TString) TString Join
  , unary "trim" TString TString Trim
  , ternary "replace" TString TString TString TString Replace
  , unary "strLength" TString TInt StrLength
  , unary "readFile" TString TString ReadFile
  , binary "writeFile" TString TString TUnit WriteFile
  , binary "appendFile" TString TString TUnit AppendFile
  , unary "fileExists" TString TBool FileExists
  , unary "listDirectory" TString (TList TString) ListDirectory
  , unary "createDirectory" TString TUnit CreateDirectory
  , unary "removeDirectory" TString TUnit RemoveDirectory
  , unary "setCurrentDirectory" TString TUnit SetCurrentDirectory
  , unary "system" TString TInt System
  , unary "getEnv" TString (TMaybe TString) GetEnv
  , binary "setEnv" TString TString TUnit SetEnv
  , unary "exit" TInt a Exit
  , unary "Just" a (TMaybe a) MJust
  , unary "Left" a (TEither a b) ELeft
  , unary "Right" b (TEither a b) ERight
  , unary "attempt" (TFun TUnit a) (TEither (TCustom "Error" []) a) Attempt
  , unary "raise" (TCustom "Error" []) a Raise
  , unary "readLine" TUnit (TMaybe TString) ReadLine
  , unary "headMaybe" (TList a) (TMaybe a) HeadMaybe
  , unary "tailMaybe" (TList a) (TMaybe (TList a)) TailMaybe
  ]
  where
    a = TVar "a"
    b = TVar "b"

unary :: String -> Type -> Type -> (Expr -> Expr) -> (String, Type, Expr)
unary name input output build =
  (name, TFun input output, Lambda "@arg0" Nothing (build (Var "@arg0")))

binary :: String -> Type -> Type -> Type -> (Expr -> Expr -> Expr) -> (String, Type, Expr)
binary name first second output build =
  (name, TFun first (TFun second output),
   Lambda "@arg0" Nothing (Lambda "@arg1" Nothing (build (Var "@arg0") (Var "@arg1"))))

ternary :: String -> Type -> Type -> Type -> Type -> (Expr -> Expr -> Expr -> Expr) -> (String, Type, Expr)
ternary name first second third output build =
  (name, TFun first (TFun second (TFun third output)),
   Lambda "@arg0" Nothing (Lambda "@arg1" Nothing (Lambda "@arg2" Nothing
     (build (Var "@arg0") (Var "@arg1") (Var "@arg2")))))

standardTypeEnv :: TypeEnv
standardTypeEnv = Map.union standardDataTypeEnv $
  Map.fromList [(name, generalize Map.empty ty) | (name,ty,_) <- builtinDefinitions]

standardValueEnv :: Env
standardValueEnv = Map.union standardDataValueEnv $
  Map.fromList [(name, closure body) | (name,_,body) <- builtinDefinitions]
  where
    closure (Lambda name _ body) = VFun name body Map.empty
    closure _ = error "Standard library definition must be a function"
