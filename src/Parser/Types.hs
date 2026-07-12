module Parser.Types where

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Syntax
import Parser.Lexer
import Parser.Literals (constructorIdentifier, identifier, lowerIdentifier)

syntaxType :: Parser SyntaxType
syntaxType = makeExprParser typeApplication typeOperatorTable
  where
    typeApplication = do
      headType <- syntaxTypeAtom
      case headType of
        STCustom name [] -> STCustom name <$> many syntaxTypeAtom
        _ -> return headType

    typeOperatorTable = [ [ InfixR (STFun <$ symbol "->") ] ]

syntaxTypeAtom :: Parser SyntaxType
syntaxTypeAtom = choice
  [ STInt <$ keyword "Int"
  , STBool <$ keyword "Bool"
  , STString <$ keyword "String"
  , STUnit <$ keyword "Unit"
  , maybeType
  , eitherType
  , listType
  , recordType
  , STCustom <$> constructorIdentifier <*> pure []
  , STVar <$> lowerIdentifier
  , parens syntaxType
  ]
  where
    maybeType = do
      keyword "Maybe"
      STMaybe <$> syntaxTypeAtom

    eitherType = do
      keyword "Either"
      t1 <- syntaxTypeAtom
      STEither t1 <$> syntaxTypeAtom

    listType = STList <$> brackets syntaxType

    recordType = STRecord <$> braces (sepBy recordTypeField (symbol ","))

    recordTypeField = do
      name <- identifier
      symbol ":"
      ty <- syntaxType
      return (name, ty)
