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
expr = makeExprParser appExpr operatorTable

appExpr :: Parser Expr
appExpr = do
  first <- atom
  rest <- many (recordAccess <|> application)
  return $ foldl (flip ($)) first rest

recordAccess :: Parser (Expr -> Expr)
recordAccess = do
  symbol "."
  field <- identifier
  return (`RecordAccess` field)

application :: Parser (Expr -> Expr)
application = do
  arg <- atom
  return (`App` arg)

atom :: Parser Expr
atom = choice
  [ UnitLit <$ unit
  , IntLit <$> integer
  , try parensOrTuple
  , BoolLit <$> boolean
  , StrLit <$> stringLit
  , builtinExpr
  , complexExpr
  , try listLitExpr
  , try recordLitExpr
  , try (Var <$> identifier)
  , try typeAnnotationExpr
  ]

builtinExpr :: Parser Expr
builtinExpr = choice
  [ printExpr expr
  , inputExpr
  , argsExpr
  , parseIntExpr expr
  , toStringExpr expr
  , showExpr expr
  , headExpr expr
  , tailExpr expr
  , nullExpr expr
  , fstExpr expr
  , sndExpr expr
  , mapExpr atom
  , filterExpr atom
  , foldlExpr atom
  , lengthExpr atom
  , reverseExpr atom
  , takeExpr atom
  , dropExpr atom
  , zipExpr atom
  , splitExpr atom
  , joinExpr atom
  , trimExpr atom
  , replaceExpr atom
  , strLengthExpr atom
  , readFileExpr atom
  , writeFileExpr atom
  , justExpr expr
  , nothingExpr
  , leftExpr expr
  , rightExpr expr
  ]

complexExpr :: Parser Expr
complexExpr = choice
  [ lambdaExpr expr
  , ifExpr expr
  , letRecExpr expr
  , letExpr expr
  , caseExpr expr
  ]

parensOrTuple :: Parser Expr
parensOrTuple = do
  symbol "("
  exprs <- sepBy expr (symbol ",")
  symbol ")"
  case exprs of
    [e] -> return e
    _   -> return (TupleLit exprs)

listLitExpr :: Parser Expr
listLitExpr = ListLit <$> brackets (sepBy expr (symbol ","))

recordLitExpr :: Parser Expr
recordLitExpr = RecordLit <$> braces (sepBy recordField (symbol ","))

recordField :: Parser (String, Expr)
recordField = do
  name <- identifier
  symbol "="
  e <- expr
  return (name, e)

typeAnnotationExpr :: Parser Expr
typeAnnotationExpr = do
  symbol "("
  e <- expr
  symbol ":"
  t <- syntaxType
  symbol ")"
  return $ TypeAnnotation e t

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
  , [ InfixR (Cons <$ symbol "::") ]
  , [ InfixR (Concat <$ symbol "++") ]
  , [ InfixN (Lt <$ symbol "<")
    , InfixN (Gt <$ symbol ">")
    , InfixN (Eq <$ symbol "==")
    ]
  , [ InfixR (And <$ symbol "and") ]
  , [ InfixR (Or <$ symbol "or") ]
  , [ InfixR (Seq <$ symbol ";") ]
  ]
