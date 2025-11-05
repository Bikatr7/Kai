module Parser.Patterns where

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Syntax
import Parser.Lexer
import Parser.Literals

patternParser :: Parser Pattern
patternParser = makeExprParser patternTerm patternOperatorTable

patternTerm :: Parser Pattern
patternTerm = choice
  [ PInt <$> integer
  , PBool <$> boolean
  , PStr <$> stringLit
  , PUnit <$ unit
  , try listPattern
  , try recordPattern
  , justPattern
  , nothingPattern
  , leftPattern
  , rightPattern
  , PVar <$> identifier
  , try parensOrTuplePattern
  ]

parensOrTuplePattern :: Parser Pattern
parensOrTuplePattern = do
  symbol "("
  pats <- sepBy patternParser (symbol ",")
  symbol ")"
  case pats of
    [p] -> return p
    _   -> return (PTuple pats)

patternOperatorTable :: [[Operator Parser Pattern]]
patternOperatorTable = [ [ InfixR (PCons <$ symbol "::") ] ]

justPattern :: Parser Pattern
justPattern = do
  symbol "Just"
  PJust <$> patternTerm

nothingPattern :: Parser Pattern
nothingPattern = symbol "Nothing" >> return PNothing

leftPattern :: Parser Pattern
leftPattern = do
  symbol "Left"
  PLeft <$> patternTerm

rightPattern :: Parser Pattern
rightPattern = do
  symbol "Right"
  PRight <$> patternTerm

listPattern :: Parser Pattern
listPattern = PList <$> brackets (sepBy patternParser (symbol ","))

recordPattern :: Parser Pattern
recordPattern = PRecord <$> braces (sepBy recordPatternField (symbol ","))

recordPatternField :: Parser (String, Pattern)
recordPatternField = do
  name <- identifier
  symbol "="
  p <- patternParser
  return (name, p)
