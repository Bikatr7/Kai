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
  , try constructorPattern
  , PVar <$> identifier
  , try parensOrTuplePattern
  ]

parensOrTuplePattern :: Parser Pattern
parensOrTuplePattern = do
  _ <- symbol "("
  pats <- sepBy patternParser (symbol ",")
  _ <- symbol ")"
  case pats of
    [p] -> return p
    _   -> return (PTuple pats)

patternOperatorTable :: [[Operator Parser Pattern]]
patternOperatorTable = [ [ InfixR (PCons <$ symbol "::") ] ]

justPattern :: Parser Pattern
justPattern = do
  keyword "Just"
  PJust <$> patternArgument

nothingPattern :: Parser Pattern
nothingPattern = keyword "Nothing" >> return PNothing

leftPattern :: Parser Pattern
leftPattern = do
  keyword "Left"
  PLeft <$> patternArgument

rightPattern :: Parser Pattern
rightPattern = do
  keyword "Right"
  PRight <$> patternArgument

constructorPattern :: Parser Pattern
constructorPattern = do
  name <- constructorIdentifier
  args <- many patternArgument
  return $ PConstructor name args

patternArgument :: Parser Pattern
patternArgument = choice
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
  , PConstructor <$> constructorIdentifier <*> pure []
  , PVar <$> identifier
  , try parensOrTuplePattern
  ]

listPattern :: Parser Pattern
listPattern = PList <$> brackets (sepBy patternParser (symbol ","))

recordPattern :: Parser Pattern
recordPattern = braces $ do
  fields <- sepBy recordPatternField (symbol ",")
  rest <- optional (symbol "|" *> identifier)
  pure $ maybe (PRecord fields) (POpenRecord fields) rest

recordPatternField :: Parser (String, Pattern)
recordPatternField = do
  name <- identifier
  _ <- symbol "="
  p <- patternParser
  return (name, p)
