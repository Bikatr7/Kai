module Parser.Literals where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Data.Char (isLower, isUpper)
import Parser.Lexer
import Syntax (isKaiInt)

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

integer :: Parser Int
integer = lexeme $ do
  s <- optionalSign
  n <- (L.decimal :: Parser Integer)
  let val = s * n
  if isKaiInt val
    then pure (fromIntegral val)
    else fail $ "Integer literal " ++ show val ++ " is outside 32-bit signed Int bounds"

optionalSign :: Parser Integer
optionalSign =
  (try (char '-' <* lookAhead digitChar) >> pure (-1))
  <|> (try (char '+' <* lookAhead digitChar) >> pure 1)
  <|> pure 1

boolean :: Parser Bool
boolean = choice
  [ keyword "true" >> return True
  , keyword "false" >> return False
  ]

unit :: Parser ()
unit = void (symbol "()")

identifier :: Parser String
identifier = lexeme $ do
  name <- wildcard <|> regularIdentifier
  if name `elem` keywords
    then fail $ "keyword " ++ show name ++ " cannot be used as identifier"
    else return name
  where
    wildcard = string "_"
    regularIdentifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

constructorIdentifier :: Parser String
constructorIdentifier = lexeme $ do
  name <- (:) <$> satisfy isUpper <*> many (alphaNumChar <|> char '_')
  if name `elem` keywords
    then fail $ "keyword " ++ show name ++ " cannot be used as constructor name"
    else return name

lowerIdentifier :: Parser String
lowerIdentifier = lexeme $ do
  name <- (:) <$> satisfy isLower <*> many (alphaNumChar <|> char '_')
  if name `elem` keywords
    then fail $ "keyword " ++ show name ++ " cannot be used as identifier"
    else return name
