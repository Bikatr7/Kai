module Parser.Builtins where

import Text.Megaparsec
import Syntax
import Parser.Lexer

type ExprParser = Parser Expr

printExpr :: ExprParser -> Parser Expr
printExpr expr = do
  keyword "print"
  Print <$> expr

inputExpr :: Parser Expr
inputExpr = keyword "input" >> return Input

argsExpr :: Parser Expr
argsExpr = keyword "args" >> return Args

parseIntExpr :: ExprParser -> Parser Expr
parseIntExpr expr = do
  keyword "parseInt"
  ParseInt <$> expr

toStringExpr :: ExprParser -> Parser Expr
toStringExpr expr = do
  keyword "toString"
  ToString <$> expr

showExpr :: ExprParser -> Parser Expr
showExpr expr = do
  keyword "show"
  Show <$> expr

headExpr :: ExprParser -> Parser Expr
headExpr expr = keyword "head" >> Head <$> expr

tailExpr :: ExprParser -> Parser Expr
tailExpr expr = keyword "tail" >> Tail <$> expr

nullExpr :: ExprParser -> Parser Expr
nullExpr expr = keyword "null" >> Null <$> expr

fstExpr :: ExprParser -> Parser Expr
fstExpr expr = keyword "fst" >> Fst <$> expr

sndExpr :: ExprParser -> Parser Expr
sndExpr expr = keyword "snd" >> Snd <$> expr

mapExpr :: ExprParser -> Parser Expr
mapExpr atom = do
  keyword "map"
  f <- atom
  Map f <$> atom

filterExpr :: ExprParser -> Parser Expr
filterExpr atom = do
  keyword "filter"
  f <- atom
  Filter f <$> atom

foldlExpr :: ExprParser -> Parser Expr
foldlExpr atom = do
  keyword "foldl"
  f <- atom
  acc <- atom
  Foldl f acc <$> atom

lengthExpr :: ExprParser -> Parser Expr
lengthExpr atom = keyword "length" >> Length <$> atom

reverseExpr :: ExprParser -> Parser Expr
reverseExpr atom = keyword "reverse" >> Reverse <$> atom

takeExpr :: ExprParser -> Parser Expr
takeExpr atom = do
  keyword "take"
  n <- atom
  Take n <$> atom

dropExpr :: ExprParser -> Parser Expr
dropExpr atom = do
  keyword "drop"
  n <- atom
  Drop n <$> atom

zipExpr :: ExprParser -> Parser Expr
zipExpr atom = do
  keyword "zip"
  l1 <- atom
  Zip l1 <$> atom

splitExpr :: ExprParser -> Parser Expr
splitExpr atom = do
  keyword "split"
  delim <- atom
  Split delim <$> atom

joinExpr :: ExprParser -> Parser Expr
joinExpr atom = do
  keyword "join"
  delim <- atom
  Join delim <$> atom

trimExpr :: ExprParser -> Parser Expr
trimExpr atom = keyword "trim" >> Trim <$> atom

replaceExpr :: ExprParser -> Parser Expr
replaceExpr atom = do
  keyword "replace"
  old <- atom
  new <- atom
  Replace old new <$> atom

strLengthExpr :: ExprParser -> Parser Expr
strLengthExpr atom = keyword "strLength" >> StrLength <$> atom

readFileExpr :: ExprParser -> Parser Expr
readFileExpr atom = keyword "readFile" >> ReadFile <$> atom

writeFileExpr :: ExprParser -> Parser Expr
writeFileExpr atom = do
  keyword "writeFile"
  path <- atom
  WriteFile path <$> atom

justExpr :: ExprParser -> Parser Expr
justExpr expr = do
  symbol "Just"
  MJust <$> expr

nothingExpr :: Parser Expr
nothingExpr = symbol "Nothing" >> return MNothing

leftExpr :: ExprParser -> Parser Expr
leftExpr expr = do
  symbol "Left"
  ELeft <$> expr

rightExpr :: ExprParser -> Parser Expr
rightExpr expr = do
  symbol "Right"
  ERight <$> expr
