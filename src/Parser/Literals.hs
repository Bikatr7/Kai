module Parser.Literals where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Parser.Lexer

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
  if val >= fromIntegral (minBound :: Int) && val <= fromIntegral (maxBound :: Int)
    then pure (fromIntegral val)
    else fail $ "Integer literal " ++ show val ++ " is outside Int bounds"

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

identifier :: Parser String
identifier = lexeme $ do
  name <- wildcard <|> regularIdentifier
  if name `elem` keywords
    then fail $ "keyword " ++ show name ++ " cannot be used as identifier"
    else return name
  where
    wildcard = string "_"
    regularIdentifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
