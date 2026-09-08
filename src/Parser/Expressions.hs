module Parser.Expressions where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Syntax
import Parser.Lexer
import Parser.Literals
import Parser.Types
import Parser.Builtins
import Parser.ComplexExpr
import Parser.Source

expr :: Parser Expr
expr = buildExpr True

exprNoSeq :: Parser Expr
exprNoSeq = buildExpr False

buildExpr :: Bool -> Parser Expr
buildExpr = buildExpression Nothing

buildExpression :: Maybe SourceInfo -> Bool -> Parser Expr
buildExpression source allowSeq = exprParser
  where
    exprParser = located source $ makeExprParser appParser (operatorTableWith source allowSeq)

    appParser = do
      first <- postfixParser
      rest <- many (application postfixParser)
      return $ foldl (flip ($)) first rest

    postfixParser = located source $ do
      first <- atomParser
      fields <- many (recordAccessWith source)
      return $ foldl (flip ($)) first fields

    atomParser = located source $ choice
      [ UnitLit <$ unit
      , IntLit <$> integer
      , try (parensOrTuple exprParser)
      , BoolLit <$> boolean
      , StrLit <$> stringLit
      , builtinExpr
      , complexExprWithBlock source exprParser
      , try (listLitExpr exprParser)
      , try (recordLitExpr exprParser)
      , try (Var <$> constructorIdentifier)
      , try (Var <$> identifier)
      , try (typeAnnotationExpr exprParser)
      ]

builtinExpr :: Parser Expr
builtinExpr = choice
  [ inputExpr
  , argsExpr
  , getCurrentDirectoryExpr
  , nothingExpr
  ]

complexExprWithBlock :: Maybe SourceInfo -> Parser Expr -> Parser Expr
complexExprWithBlock source exprParser = choice
  [ lambdaExpr exprParser
  , ifExpr exprParser
  , letRecExpr exprParser
  , letExpr exprParser
  , caseExprWith source exprParser
  , blockExpr (buildExpression source False)
  ]

recordAccess :: Parser (Expr -> Expr)
recordAccess = recordAccessWith Nothing

recordAccessWith :: Maybe SourceInfo -> Parser (Expr -> Expr)
recordAccessWith source = do
  _ <- symbol "."
  field <- identifier
  case source of
    Nothing -> return (`RecordAccess` field)
    Just _ -> do
      end <- getSourcePos
      pure $ \receiver -> case exprSpan receiver of
        Just start -> Located (start {spanEndLine = unPos (sourceLine end), spanEndColumn = unPos (sourceColumn end)})
          (RecordAccess receiver field)
        Nothing -> RecordAccess receiver field

application :: Parser Expr -> Parser (Expr -> Expr)
application atomParser = do
  notFollowedBy (char '+' <|> char '-')
  arg <- atomParser
  return (\fun -> spanBinary App fun arg)

parensOrTuple :: Parser Expr -> Parser Expr
parensOrTuple exprParser = do
  _ <- symbol "("
  exprs <- sepBy exprParser (symbol ",")
  _ <- symbol ")"
  case exprs of
    [e] -> return e
    _   -> return (TupleLit exprs)

listLitExpr :: Parser Expr -> Parser Expr
listLitExpr exprParser = ListLit <$> brackets (sepBy exprParser (symbol ","))

recordLitExpr :: Parser Expr -> Parser Expr
recordLitExpr exprParser = RecordLit <$> braces (sepBy (recordField exprParser) (symbol ","))

recordField :: Parser Expr -> Parser (String, Expr)
recordField exprParser = do
  name <- identifier
  _ <- symbol "="
  e <- exprParser
  return (name, e)

typeAnnotationExpr :: Parser Expr -> Parser Expr
typeAnnotationExpr exprParser = do
  _ <- symbol "("
  e <- exprParser
  _ <- symbol ":"
  t <- syntaxType
  _ <- symbol ")"
  return $ TypeAnnotation e t

operatorTable :: Bool -> [[Operator Parser Expr]]
operatorTable = operatorTableWith Nothing

operatorTableWith :: Maybe SourceInfo -> Bool -> [[Operator Parser Expr]]
operatorTableWith source allowSeq =
  [ [Prefix (foldr (.) id <$> some (prefixOperatorWith source))]
  , [ InfixL (spanBinary Mul <$ symbol "*")
    , InfixL (spanBinary Div <$ symbol "/")
    ]
  , [ InfixL (spanBinary Add <$ try (char '+' <* notFollowedBy (char '+') <* sc))
    , InfixL (spanBinary Sub <$ symbol "-")
    ]
  , [ InfixR (spanBinary Cons <$ symbol "::") ]
  , [ InfixR (spanBinary Concat <$ symbol "++") ]
  , [ InfixN (spanBinary Lt <$ symbol "<")
    , InfixN (spanBinary Gt <$ symbol ">")
    , InfixN (spanBinary Eq <$ symbol "==")
    ]
  , [ InfixR (spanBinary And <$ keyword "and") ]
  , [ InfixR (spanBinary Or <$ keyword "or") ]
  ] ++ [[InfixR (spanBinary Seq <$ symbol ";")] | allowSeq]

prefixOperator :: Parser (Expr -> Expr)
prefixOperator = prefixOperatorWith Nothing

prefixOperatorWith :: Maybe SourceInfo -> Parser (Expr -> Expr)
prefixOperatorWith Nothing = (Not <$ keyword "not") <|>
  (Sub (IntLit 0) <$ try (char '-' <* notFollowedBy digitChar <* sc))
prefixOperatorWith (Just info) = do
  start <- getSourcePos
  constructor <- prefixOperatorWith Nothing
  end <- getSourcePos
  let operator = sourceSpan info start end
  pure $ \argument -> case exprSpan argument of
    Just finish -> Located (operator {spanEndLine = spanEndLine finish, spanEndColumn = spanEndColumn finish})
      (constructor argument)
    Nothing -> Located operator (constructor argument)
