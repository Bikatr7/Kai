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
  _ <- symbol "\\"
  param <- identifier
  maybeType <- optional $ do
    _ <- symbol ":"
    syntaxTypeApplication
  _ <- symbol "->"
  Lambda param maybeType <$> expr

ifExpr :: ExprParser -> Parser Expr
ifExpr expr = do
  keyword "if"
  cond <- expr
  keyword "then"
  thenExpr <- expr
  keyword "else"
  If cond thenExpr <$> expr

letExpr :: ExprParser -> Parser Expr
letExpr expr = do
  keyword "let"
  var <- identifier
  maybeType <- optional $ do
    _ <- symbol ":"
    syntaxType
  _ <- symbol "="
  val <- expr
  keyword "in"
  Let var maybeType val <$> expr

letRecExpr :: ExprParser -> Parser Expr
letRecExpr expr = do
  keyword "letrec"
  var <- identifier
  maybeType <- optional $ do
    _ <- symbol ":"
    syntaxType
  _ <- symbol "="
  val <- expr
  keyword "in"
  LetRec var maybeType val <$> expr

caseExpr :: ExprParser -> Parser Expr
caseExpr expr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  patterns <- sepBy1 (casePattern expr) (symbol "|")
  return $ Case scrutinee patterns

casePattern :: ExprParser -> Parser (Pattern, Expr)
casePattern expr = do
  pat <- patternParser
  _ <- symbol "->"
  e <- expr
  return (pat, e)

blockExpr :: ExprParser -> Parser Expr
blockExpr entryExpr = do
  keyword "do"
  exprs <- braces (sepEndBy entryExpr (symbol ";"))
  return $ mkBlock exprs
  where
    mkBlock [] = UnitLit
    mkBlock [e] = e
    mkBlock (e:es) = Seq e (mkBlock es)
