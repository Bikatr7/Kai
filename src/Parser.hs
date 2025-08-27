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

keywords :: [String]
keywords = ["true", "false", "if", "then", "else", "and", "or", "not"]

-- Parse non-negative integers only; unary '-' is handled as a prefix operator
integer :: Parser Integer
integer = lexeme L.decimal

boolean :: Parser Bool
boolean = choice
  [ symbol "true" >> return True
  , symbol "false" >> return False
  ]

-- Parse identifiers (excluding keywords)
identifier :: Parser String
identifier = lexeme $ do
  name <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  if name `elem` keywords
    then fail $ "keyword " ++ show name ++ " cannot be used as identifier"
    else return name

-- Main expression parser
expr :: Parser Expr
expr = makeExprParser appExpr operatorTable

-- Function application (higher precedence than operators)
appExpr :: Parser Expr
appExpr = do
  first <- atom
  rest <- many atom
  return $ foldl App first rest

-- Atomic expressions
atom :: Parser Expr
atom = choice
  [ IntLit . fromInteger <$> integer
  , BoolLit <$> boolean  
  , lambdaExpr
  , ifExpr
  -- Use try so keywords like 'then', 'and', 'or' don't cause a consuming failure maybe?
  , try (Var <$> identifier)
  , parens expr
  ]

-- Lambda expression: \x -> expr
lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  param <- identifier
  symbol "->"
  body <- expr
  return $ Lambda param body

-- If-then-else expression
ifExpr :: Parser Expr
ifExpr = do
  symbol "if"
  cond <- expr
  symbol "then"
  thenExpr <- expr
  symbol "else"
  elseExpr <- expr
  return $ If cond thenExpr elseExpr

-- Operator precedence table (function application handled separately)
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (Not <$ symbol "not")
    , Prefix ((\e -> Sub (IntLit 0) e) <$ symbol "-") -- unary minus
    ]
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

-- Parse expression from string
parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (sc *> expr <* eof) ""

-- Parse expression from file
parseFile :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseFile filename = parse (sc *> expr <* eof) filename
