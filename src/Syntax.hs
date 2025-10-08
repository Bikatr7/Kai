module Syntax where

data SyntaxType
  = STInt
  | STBool
  | STString
  | STUnit
  | STFun SyntaxType SyntaxType
  | STMaybe SyntaxType
  | STEither SyntaxType SyntaxType
  | STList SyntaxType
  | STRecord [(String, SyntaxType)]
  | STTuple [SyntaxType]
  deriving (Show, Eq)

data Expr
  = IntLit Int
  | BoolLit Bool
  | StrLit String
  | UnitLit
  | Input
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Concat Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Seq Expr Expr
  | Eq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | If Expr Expr Expr
  | Print Expr
  | Lambda String (Maybe SyntaxType) Expr
  | App Expr Expr
  | Let String (Maybe SyntaxType) Expr Expr
  | LetRec String (Maybe SyntaxType) Expr Expr
  | TypeAnnotation Expr SyntaxType
  -- Built-in conversion functions
  | ParseInt Expr
  | ToString Expr
  | Show Expr
  -- Maybe/Either constructors
  | MJust Expr
  | MNothing
  | ELeft Expr
  | ERight Expr
  -- Pattern matching
  | Case Expr [(Pattern, Expr)]
  -- Lists
  | ListLit [Expr]
  | Cons Expr Expr
  | Head Expr
  | Tail Expr
  | Null Expr
  -- Records
  | RecordLit [(String, Expr)]
  | RecordAccess Expr String
  -- Tuples
  | TupleLit [Expr]
  | Fst Expr
  | Snd Expr
  -- List functions
  | Map Expr Expr
  | Filter Expr Expr
  | Foldl Expr Expr Expr
  | Length Expr
  | Reverse Expr
  | Take Expr Expr
  | Drop Expr Expr
  | Zip Expr Expr
  -- String functions
  | Split Expr Expr
  | Join Expr Expr
  | Trim Expr
  | Replace Expr Expr Expr
  | StrLength Expr
  -- File I/O
  | ReadFile Expr
  | WriteFile Expr Expr
  -- Command-line arguments
  | Args
  deriving (Show, Eq)

-- Patterns for case expressions
data Pattern
  = PVar String
  | PInt Int
  | PBool Bool
  | PStr String
  | PUnit
  | PJust Pattern
  | PNothing
  | PLeft Pattern
  | PRight Pattern
  | PList [Pattern]
  | PCons Pattern Pattern
  | PRecord [(String, Pattern)]
  | PTuple [Pattern]
  deriving (Show, Eq) 
