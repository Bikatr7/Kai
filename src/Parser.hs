module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad.Combinators.Expr
import Syntax

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

boolean :: Parser Bool
boolean = (symbol "true" >> return True) <|> (symbol "false" >> return False)

expr :: Parser Expr
expr = makeExprParser term operatorTable

term :: Parser Expr
term = choice
  [ IntLit . fromInteger <$> integer
  , BoolLit <$> boolean
  , parens expr
  , ifExpr
  ]

ifExpr :: Parser Expr
ifExpr = do
  symbol "if"
  cond <- expr
  symbol "then"
  thenExpr <- expr
  symbol "else"
  elseExpr <- expr
  return $ If cond thenExpr elseExpr

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (Not <$ symbol "not") ]
  , [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/")
    ]
  , [ InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-")
    ]
  , [ InfixN (Lt <$ symbol "<")
    , InfixN (Gt <$ symbol ">")
    , InfixN (Eq <$ symbol "==")
    ]
  , [ InfixL (And <$ symbol "and") ]
  , [ InfixL (Or <$ symbol "or") ]
  ]

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (sc *> expr <* eof) ""

parseFile :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseFile filename = parse (sc *> expr <* eof) filename