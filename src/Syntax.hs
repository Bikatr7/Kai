module Syntax where

import Control.DeepSeq
import qualified Data.Text as Text

kaiIntMin :: Integer
kaiIntMin = -2147483648

kaiIntMax :: Integer
kaiIntMax = 2147483647

isKaiInt :: Integer -> Bool
isKaiInt value = value >= kaiIntMin && value <= kaiIntMax

data SyntaxType
  = STInt
  | STBool
  | STString
  | STUnit
  | STVar String
  | STFun SyntaxType SyntaxType
  | STMaybe SyntaxType
  | STEither SyntaxType SyntaxType
  | STList SyntaxType
  | STRecord [(String, SyntaxType)]
  | STRecordRow [(String, SyntaxType)] String
  | STTuple [SyntaxType]
  | STCustom String [SyntaxType]
  | STQualified [(String, SyntaxType)] SyntaxType
  deriving (Show, Eq)

-- Excerpts use strict Text so forcing an AST does not repeatedly traverse a
-- shared source line for every expression on that line.
data SourceSpan = SourceSpan
  { spanFile :: FilePath
  , spanLine :: Int
  , spanColumn :: Int
  , spanEndLine :: Int
  , spanEndColumn :: Int
  , spanExcerpt :: Text.Text
  } deriving (Show, Eq)

instance NFData SourceSpan where
  rnf (SourceSpan file line column endLine endColumn excerpt) =
    rnf file `seq` rnf line `seq` rnf column `seq` rnf endLine `seq` rnf endColumn `seq` rnf excerpt

exprSpan :: Expr -> Maybe SourceSpan
exprSpan (Located location _) = Just location
exprSpan _ = Nothing

unlocatedExpr :: Expr -> Expr
unlocatedExpr (Located _ expression) = unlocatedExpr expression
unlocatedExpr expression = expression

unlocatedTopLevel :: TopLevel -> TopLevel
unlocatedTopLevel (TLAt _ level) = unlocatedTopLevel level
unlocatedTopLevel level = level

-- Compose spans while preserving the source line of the leftmost expression.
spanBinary :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
spanBinary constructor left right = case (exprSpan left,exprSpan right) of
  (Just start,Just end) -> Located
    (start {spanEndLine = spanEndLine end, spanEndColumn = spanEndColumn end}) (constructor left right)
  _ -> constructor left right

data Expr
  = Located SourceSpan Expr
  | IntLit Int
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
  | Discard Expr
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
  -- Fixpoint combinator for recursion
  | Fix Expr
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
  | AppendFile Expr Expr
  | FileExists Expr
  | ListDirectory Expr
  | CreateDirectory Expr
  | RemoveDirectory Expr
  | GetCurrentDirectory
  | SetCurrentDirectory Expr
  | System Expr
  | GetEnv Expr
  | SetEnv Expr Expr
  | Exit Expr
  | Attempt Expr
  | Raise Expr
  | ReadLine Expr
  | HeadMaybe Expr
  | TailMaybe Expr
  -- Command-line arguments
  | Args
  deriving (Show, Eq)

data DataConstructor = DataConstructor String [SyntaxType]
  deriving (Show, Eq)

-- Top-level definitions for modules
data TopLevel
  = TLAt SourceSpan TopLevel
  | TLDef String (Maybe SyntaxType) Expr      -- let x = expr
  | TLExpr Expr                               -- top-level expression
  | TLData String [String] [DataConstructor]  -- data Tree a = Leaf a | Node (Tree a) (Tree a)
  | TLImport String                           -- import ModuleName
  | TLExport [String]                         -- export name1, name2, ...
  deriving (Show, Eq)

data Program = Program [TopLevel]
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
  | PConstructor String [Pattern]
  deriving (Show, Eq)

instance NFData SyntaxType where
  rnf STInt = ()
  rnf STBool = ()
  rnf STString = ()
  rnf STUnit = ()
  rnf (STVar name) = rnf name
  rnf (STFun t1 t2) = rnf t1 `seq` rnf t2
  rnf (STMaybe t) = rnf t
  rnf (STEither t1 t2) = rnf t1 `seq` rnf t2
  rnf (STList t) = rnf t
  rnf (STRecord fs) = rnf fs
  rnf (STRecordRow fs row) = rnf fs `seq` rnf row
  rnf (STTuple ts) = rnf ts
  rnf (STCustom name args) = rnf name `seq` rnf args
  rnf (STQualified predicates ty) = rnf predicates `seq` rnf ty

instance NFData Expr where
  rnf (Located location expression) = rnf location `seq` rnf expression
  rnf (IntLit n) = rnf n
  rnf (BoolLit b) = rnf b
  rnf (StrLit s) = rnf s
  rnf UnitLit = ()
  rnf Input = ()
  rnf (Var s) = rnf s
  rnf (Add e1 e2) = rnf e1 `seq` rnf e2
  rnf (Sub e1 e2) = rnf e1 `seq` rnf e2
  rnf (Mul e1 e2) = rnf e1 `seq` rnf e2
  rnf (Div e1 e2) = rnf e1 `seq` rnf e2
  rnf (Concat e1 e2) = rnf e1 `seq` rnf e2
  rnf (And e1 e2) = rnf e1 `seq` rnf e2
  rnf (Or e1 e2) = rnf e1 `seq` rnf e2
  rnf (Not e) = rnf e
  rnf (Seq e1 e2) = rnf e1 `seq` rnf e2
  rnf (Eq e1 e2) = rnf e1 `seq` rnf e2
  rnf (Lt e1 e2) = rnf e1 `seq` rnf e2
  rnf (Gt e1 e2) = rnf e1 `seq` rnf e2
  rnf (If c t e) = rnf c `seq` rnf t `seq` rnf e
  rnf (Print e) = rnf e
  rnf (Discard e) = rnf e
  rnf (Lambda s mt e) = rnf s `seq` rnf mt `seq` rnf e
  rnf (App e1 e2) = rnf e1 `seq` rnf e2
  rnf (Let s mt e1 e2) = rnf s `seq` rnf mt `seq` rnf e1 `seq` rnf e2
  rnf (LetRec s mt e1 e2) = rnf s `seq` rnf mt `seq` rnf e1 `seq` rnf e2
  rnf (TypeAnnotation e t) = rnf e `seq` rnf t
  rnf (ParseInt e) = rnf e
  rnf (ToString e) = rnf e
  rnf (Show e) = rnf e
  rnf (MJust e) = rnf e
  rnf MNothing = ()
  rnf (ELeft e) = rnf e
  rnf (ERight e) = rnf e
  rnf (Case e ps) = rnf e `seq` rnf ps
  rnf (ListLit es) = rnf es
  rnf (Cons e1 e2) = rnf e1 `seq` rnf e2
  rnf (Head e) = rnf e
  rnf (Tail e) = rnf e
  rnf (Null e) = rnf e
  rnf (Fix e) = rnf e
  rnf (RecordLit fs) = rnf fs
  rnf (RecordAccess e s) = rnf e `seq` rnf s
  rnf (TupleLit es) = rnf es
  rnf (Fst e) = rnf e
  rnf (Snd e) = rnf e
  rnf (Map e1 e2) = rnf e1 `seq` rnf e2
  rnf (Filter e1 e2) = rnf e1 `seq` rnf e2
  rnf (Foldl e1 e2 e3) = rnf e1 `seq` rnf e2 `seq` rnf e3
  rnf (Length e) = rnf e
  rnf (Reverse e) = rnf e
  rnf (Take e1 e2) = rnf e1 `seq` rnf e2
  rnf (Drop e1 e2) = rnf e1 `seq` rnf e2
  rnf (Zip e1 e2) = rnf e1 `seq` rnf e2
  rnf (Split e1 e2) = rnf e1 `seq` rnf e2
  rnf (Join e1 e2) = rnf e1 `seq` rnf e2
  rnf (Trim e) = rnf e
  rnf (Replace e1 e2 e3) = rnf e1 `seq` rnf e2 `seq` rnf e3
  rnf (StrLength e) = rnf e
  rnf (ReadFile e) = rnf e
  rnf (WriteFile e1 e2) = rnf e1 `seq` rnf e2
  rnf (AppendFile e1 e2) = rnf e1 `seq` rnf e2
  rnf (FileExists e) = rnf e
  rnf (ListDirectory e) = rnf e
  rnf (CreateDirectory e) = rnf e
  rnf (RemoveDirectory e) = rnf e
  rnf GetCurrentDirectory = ()
  rnf (SetCurrentDirectory e) = rnf e
  rnf (System e) = rnf e
  rnf (GetEnv e) = rnf e
  rnf (SetEnv e1 e2) = rnf e1 `seq` rnf e2
  rnf (Exit e) = rnf e
  rnf (Attempt e) = rnf e
  rnf (Raise e) = rnf e
  rnf (ReadLine e) = rnf e
  rnf (HeadMaybe e) = rnf e
  rnf (TailMaybe e) = rnf e
  rnf Args = ()

instance NFData DataConstructor where
  rnf (DataConstructor name tys) = rnf name `seq` rnf tys

instance NFData TopLevel where
  rnf (TLAt location level) = rnf location `seq` rnf level
  rnf (TLDef s mt e) = rnf s `seq` rnf mt `seq` rnf e
  rnf (TLExpr e) = rnf e
  rnf (TLData name vars ctors) = rnf name `seq` rnf vars `seq` rnf ctors
  rnf (TLImport s) = rnf s
  rnf (TLExport ss) = rnf ss

instance NFData Program where
  rnf (Program tls) = rnf tls

instance NFData Pattern where
  rnf (PVar s) = rnf s
  rnf (PInt n) = rnf n
  rnf (PBool b) = rnf b
  rnf (PStr s) = rnf s
  rnf PUnit = ()
  rnf (PJust p) = rnf p
  rnf PNothing = ()
  rnf (PLeft p) = rnf p
  rnf (PRight p) = rnf p
  rnf (PList ps) = rnf ps
  rnf (PCons p1 p2) = rnf p1 `seq` rnf p2
  rnf (PRecord fs) = rnf fs
  rnf (PTuple ps) = rnf ps
  rnf (PConstructor name pats) = rnf name `seq` rnf pats
