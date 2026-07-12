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

expr :: Parser Expr
expr = buildExpr True

exprNoSeq :: Parser Expr
exprNoSeq = buildExpr False

buildExpr :: Bool -> Parser Expr
buildExpr allowSeq = exprParser
  where
    exprParser = makeExprParser appParser (operatorTable allowSeq)

    appParser = do
      first <- atomParser
      rest <- many (recordAccess <|> application atomParser)
      return $ foldl (flip ($)) first rest

    atomParser = choice
      [ UnitLit <$ unit
      , IntLit <$> integer
      , try (parensOrTuple exprParser)
      , BoolLit <$> boolean
      , StrLit <$> stringLit
      , builtinExpr atomParser
      , complexExprWithBlock exprParser
      , try (listLitExpr exprParser)
      , try (recordLitExpr exprParser)
      , try (Var <$> constructorIdentifier)
      , try (Var <$> identifier)
      , try (typeAnnotationExpr exprParser)
      ]

builtinExpr :: Parser Expr -> Parser Expr
builtinExpr atomParser = choice
  [ printExpr atomParser
  , discardExpr atomParser
  , inputExpr
  , argsExpr
  , parseIntExpr atomParser
  , toStringExpr atomParser
  , showExpr atomParser
  , headExpr atomParser
  , tailExpr atomParser
  , nullExpr atomParser
  , fixExpr atomParser
  , fstExpr atomParser
  , sndExpr atomParser
  , mapExpr atomParser
  , filterExpr atomParser
  , foldlExpr atomParser
  , lengthExpr atomParser
  , reverseExpr atomParser
  , takeExpr atomParser
  , dropExpr atomParser
  , zipExpr atomParser
  , splitExpr atomParser
  , joinExpr atomParser
  , trimExpr atomParser
  , replaceExpr atomParser
  , strLengthExpr atomParser
  , readFileExpr atomParser
  , writeFileExpr atomParser
  , appendFileExpr atomParser
  , fileExistsExpr atomParser
  , listDirectoryExpr atomParser
  , createDirectoryExpr atomParser
  , removeDirectoryExpr atomParser
  , getCurrentDirectoryExpr
  , setCurrentDirectoryExpr atomParser
  , systemExpr atomParser
  , getEnvExpr atomParser
  , setEnvExpr atomParser
  , exitExpr atomParser
  , justExpr atomParser
  , nothingExpr
  , leftExpr atomParser
  , rightExpr atomParser
  ]

complexExprWithBlock :: Parser Expr -> Parser Expr
complexExprWithBlock exprParser = choice
  [ lambdaExpr exprParser
  , ifExpr exprParser
  , letRecExpr exprParser
  , letExpr exprParser
  , caseExpr exprParser
  , blockExpr exprNoSeq
  ]

recordAccess :: Parser (Expr -> Expr)
recordAccess = do
  symbol "."
  field <- identifier
  return (`RecordAccess` field)

application :: Parser Expr -> Parser (Expr -> Expr)
application atomParser = do
  arg <- atomParser
  return (`App` arg)

parensOrTuple :: Parser Expr -> Parser Expr
parensOrTuple exprParser = do
  symbol "("
  exprs <- sepBy exprParser (symbol ",")
  symbol ")"
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
  symbol "="
  e <- exprParser
  return (name, e)

typeAnnotationExpr :: Parser Expr -> Parser Expr
typeAnnotationExpr exprParser = do
  symbol "("
  e <- exprParser
  symbol ":"
  t <- syntaxType
  symbol ")"
  return $ TypeAnnotation e t

operatorTable :: Bool -> [[Operator Parser Expr]]
operatorTable allowSeq =
  [ [ Prefix (Not <$ keyword "not")
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
  , [ InfixR (Cons <$ symbol "::") ]
  , [ InfixR (Concat <$ symbol "++") ]
  , [ InfixN (Lt <$ symbol "<")
    , InfixN (Gt <$ symbol ">")
    , InfixN (Eq <$ symbol "==")
    ]
  , [ InfixR (And <$ keyword "and") ]
  , [ InfixR (Or <$ keyword "or") ]
  ] ++ [[InfixR (Seq <$ symbol ";")] | allowSeq]
