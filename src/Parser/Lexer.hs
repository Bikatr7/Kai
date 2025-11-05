module Parser.Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Char (isAlphaNum)

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

keyword :: String -> Parser ()
keyword kw = lexeme $ try $ do
  _ <- string kw
  notFollowedBy (alphaNumChar <|> char '_')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

keywords :: [String]
keywords = ["true", "false", "if", "then", "else", "and", "or", "not", "print", "let", "letrec", "in", "input", "Int", "Bool", "String", "Unit", "parseInt", "toString", "show", "Maybe", "Either", "Just", "Nothing", "Left", "Right", "case", "of", "head", "tail", "null", "fst", "snd", "map", "filter", "foldl", "length", "reverse", "take", "drop", "zip", "split", "join", "trim", "replace", "strLength", "readFile", "writeFile", "args"]
