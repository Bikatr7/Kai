module Parser.ComplexExpr where

import Text.Megaparsec
import Syntax
import Parser.Lexer
import Parser.Literals
import Parser.Types
import Parser.Patterns

type ExprParser = Parser Expr

lambdaExpr :: ExprParser -> Parser Expr
lambdaExpr expr = do
  symbol "\\"
  param <- identifier
  maybeType <- optional $ do
    symbol ":"
    syntaxType
  symbol "->"
  Lambda param maybeType <$> expr

ifExpr :: ExprParser -> Parser Expr
ifExpr expr = do
  symbol "if"
  cond <- expr
  symbol "then"
  thenExpr <- expr
  symbol "else"
  If cond thenExpr <$> expr

letExpr :: ExprParser -> Parser Expr
letExpr expr = do
  symbol "let"
  var <- identifier
  maybeType <- optional $ do
    symbol ":"
    syntaxType
  symbol "="
  val <- expr
  symbol "in"
  Let var maybeType val <$> expr

letRecExpr :: ExprParser -> Parser Expr
letRecExpr expr = do
  symbol "letrec"
  var <- identifier
  maybeType <- optional $ do
    symbol ":"
    syntaxType
  symbol "="
  val <- expr
  symbol "in"
  LetRec var maybeType val <$> expr

caseExpr :: ExprParser -> Parser Expr
caseExpr expr = do
  symbol "case"
  scrutinee <- expr
  symbol "of"
  patterns <- sepBy1 (casePattern expr) (symbol "|")
  return $ Case scrutinee patterns

casePattern :: ExprParser -> Parser (Pattern, Expr)
casePattern expr = do
  pat <- patternParser
  symbol "->"
  e <- expr
  return (pat, e)
