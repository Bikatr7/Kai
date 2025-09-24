module Syntax where

data SyntaxType
  = STInt
  | STBool
  | STString
  | STUnit
  | STFun SyntaxType SyntaxType
  | STMaybe SyntaxType    -- Maybe type for optional values
  | STEither SyntaxType SyntaxType  -- Either type for error handling
  deriving (Show, Eq)

data Expr
  = IntLit Int            -- Integer literals
  | BoolLit Bool          -- Boolean literals
  | StrLit String         -- String literals
  | UnitLit               -- Unit literal ()
  | Input                 -- Read a line of input
  | Var String            -- Variables
  | Add Expr Expr         -- Addition
  | Sub Expr Expr         -- Subtraction
  | Mul Expr Expr         -- Multiplication
  | Div Expr Expr         -- Division
  | Concat Expr Expr      -- String concatenation (++)
  | And Expr Expr         -- Logical AND
  | Or Expr Expr          -- Logical OR
  | Not Expr              -- Logical NOT
  | Eq Expr Expr          -- Equality
  | Lt Expr Expr          -- Less than
  | Gt Expr Expr          -- Greater than
  | If Expr Expr Expr     -- If-then-else
  | Print Expr            -- Print expression (prints at runtime, returns ())
  | Lambda String (Maybe SyntaxType) Expr    -- Lambda function (\x : Type -> expr) or (\x -> expr)
  | App Expr Expr         -- Function application
  | Let String (Maybe SyntaxType) Expr Expr  -- Let binding (let x : Type = val in expr) or (let x = val in expr)
  | LetRec String (Maybe SyntaxType) Expr Expr -- Recursive let binding with optional type annotation
  | TypeAnnotation Expr SyntaxType -- Explicit type annotation (expr : Type)
  -- Built-in conversion functions
  | ParseInt Expr         -- parseInt : String -> Maybe Int
  | ToString Expr         -- toString : Int -> String
  | Show Expr             -- show : a -> String
  -- Maybe/Either constructors
  | MJust Expr            -- Just constructor for Maybe
  | MNothing              -- Nothing constructor for Maybe
  | ELeft Expr            -- Left constructor for Either
  | ERight Expr           -- Right constructor for Either
  -- Pattern matching
  | Case Expr [(Pattern, Expr)]  -- case expr of pattern -> expr
  deriving (Show, Eq)

-- Patterns for case expressions
data Pattern
  = PVar String           -- Variable pattern
  | PInt Int              -- Integer literal pattern
  | PBool Bool            -- Boolean literal pattern
  | PStr String           -- String literal pattern
  | PUnit                 -- Unit pattern
  | PJust Pattern         -- Just pattern
  | PNothing              -- Nothing pattern
  | PLeft Pattern         -- Left pattern
  | PRight Pattern        -- Right pattern
  deriving (Show, Eq) 
