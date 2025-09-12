module Parser 
  ( parseExpr
  , parseStatements  
  , parseFileExpr
  , parseFile
  , Parser
  , ParseErrorBundle
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Char (isAlphaNum)
import Control.Monad.Combinators.Expr
import Control.Monad (void)
import Syntax
import Data.List (lines)
import System.IO (putStrLn)

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
keywords = ["true", "false", "if", "then", "else", "and", "or", "not", "print", "let", "letrec", "in"]

stringLit :: Parser String
stringLit = lexeme $ char '"' *> many charChunk <* char '"'
  where
    charChunk = escaped <|> normal
    escaped = do
      _ <- char '\\'
      c <- anySingle
      case c of
        '"' -> pure '"'
        '\\' -> pure '\\'
        'n' -> pure '\n'
        _ -> fail $ "UnknownEscape '" ++ [c] ++ "' (supported: \\\" \\\\ \\n)"
    normal = satisfy (\c -> c /= '"' && c /= '\\')

-- Parse signed integers (sign must be adjacent to digits) with bounds check
integer :: Parser Int
integer = lexeme $ do
  s <- optionalSign
  n <- (L.decimal :: Parser Integer)
  let val = s * n
  if val >= fromIntegral (minBound :: Int) && val <= fromIntegral (maxBound :: Int)
    then pure (fromIntegral val)
    else fail $ "Integer literal " ++ show val ++ " is outside Int bounds"

-- Helper: consume '+' or '-' only if immediately followed by a digit
optionalSign :: Parser Integer
optionalSign =
  (try (char '-' <* lookAhead digitChar) >> pure (-1))
  <|> (try (char '+' <* lookAhead digitChar) >> pure 1)
  <|> pure 1

boolean :: Parser Bool
boolean = choice
  [ symbol "true" >> return True
  , symbol "false" >> return False
  ]

unit :: Parser ()
unit = void (symbol "()")

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
  [ IntLit <$> integer
  , BoolLit <$> boolean  
  , StrLit <$> stringLit
  , UnitLit <$ unit
  , printExpr
  , lambdaExpr
  , ifExpr
  , letRecExpr
  , letExpr
  -- Use try so keywords like 'then', 'and', 'or' don't cause a consuming failure maybe?
  , try (Var <$> identifier)
  , parens expr
  ]

-- Print expression: print expr
printExpr :: Parser Expr
printExpr = do
  symbol "print"
  Print <$> expr

-- Lambda expression: \x -> expr
lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  param <- identifier
  symbol "->"
  Lambda param <$> expr

-- If-then-else expression
ifExpr :: Parser Expr
ifExpr = do
  symbol "if"
  cond <- expr
  symbol "then"
  thenExpr <- expr
  symbol "else"
  If cond thenExpr <$> expr

-- Let expression: let x = val in expr
letExpr :: Parser Expr
letExpr = do
  symbol "let"
  var <- identifier
  symbol "="
  val <- expr
  symbol "in"
  Let var val <$> expr

-- LetRec expression: letrec x = val in expr
letRecExpr :: Parser Expr
letRecExpr = do
  symbol "letrec"
  var <- identifier
  symbol "="
  val <- expr
  symbol "in"
  LetRec var val <$> expr

-- Operator precedence table (Haskell-aligned, function application handled separately)
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix (Not <$ symbol "not")
    , Prefix ( Sub (IntLit 0)
             <$ try (char '-' <* notFollowedBy digitChar <* sc)
             )
    ]
  , [ InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/")
    ]
  , [ InfixL (Add <$ try (char '+' <* notFollowedBy (char '+') <* sc))
    , InfixL (Sub <$ symbol "-")
    ]
  , [ InfixR (symbol "++" >> return Concat)  -- right-assoc like Haskell
    ]
  , [ InfixN (Lt <$ symbol "<")
    , InfixN (Gt <$ symbol ">")
    , InfixN (Eq <$ symbol "==")
    ]
  , [ InfixR (And <$ symbol "and") ]  -- right-assoc like Haskell &&
  , [ InfixR (Or <$ symbol "or") ]    -- right-assoc like Haskell ||
  ]

-- Parse expression from string
parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (sc *> expr <* eof) ""

-- Parse multiple statements (expressions) from string
statements :: Parser [Expr]
statements = many (expr <* sc <* (eol <|> eof))
  where
    eol = try (some (char '\n') *> notFollowedBy (char '\r'))

-- Parse statements from string by splitting lines
parseStatements :: String -> Either (ParseErrorBundle String Void) [Expr]
parseStatements content = 
  let contentLines = filter (not . null . dropWhile (== ' ')) $ lines content
      nonCommentLines = filter (not . isComment) contentLines
      isComment line = "//" `isPrefixOf` dropWhile (== ' ') line
      isPrefixOf prefix str = take (length prefix) str == prefix
      parseLine = parse (sc *> expr <* eof) ""
  in case mapM parseLine nonCommentLines of
       Left err -> Left err
       Right exprs -> Right exprs

-- Parse single expression from multi-line content (ignoring comments)
parseFileExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseFileExpr content = 
  let cleanContent = unlines $ filter (not . isComment) $ lines content
      isComment line = "//" `isPrefixOf` dropWhile (== ' ') line
      isPrefixOf prefix str = take (length prefix) str == prefix
  in parse (sc *> expr <* eof) "" cleanContent

-- Parse expression from file
parseFile :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseFile = parse (sc *> expr <* eof)

-- Parse statements from file
parseFileStatements :: String -> String -> Either (ParseErrorBundle String Void) [Expr]
parseFileStatements = parse (sc *> statements <* eof)
