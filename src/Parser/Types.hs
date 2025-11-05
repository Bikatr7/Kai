module Parser.Types where

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Syntax
import Parser.Lexer
import Parser.Literals

syntaxType :: Parser SyntaxType
syntaxType = makeExprParser atomType typeOperatorTable
  where
    atomType = choice
      [ STInt <$ symbol "Int"
      , STBool <$ symbol "Bool"
      , STString <$ symbol "String"
      , STUnit <$ symbol "Unit"
      , maybeType
      , eitherType
      , listType
      , recordType
      , parens syntaxType
      ]

    maybeType = do
      symbol "Maybe"
      STMaybe <$> atomType

    eitherType = do
      symbol "Either"
      t1 <- atomType
      STEither t1 <$> atomType

    listType = STList <$> brackets syntaxType

    recordType = STRecord <$> braces (sepBy recordTypeField (symbol ","))

    recordTypeField = do
      name <- identifier
      symbol ":"
      ty <- syntaxType
      return (name, ty)

    typeOperatorTable = [ [ InfixR (STFun <$ symbol "->") ] ]
