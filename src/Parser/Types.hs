module Parser.Types where

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Syntax
import Parser.Lexer
import Parser.Literals (constructorIdentifier, identifier, lowerIdentifier)

syntaxType :: Parser SyntaxType
syntaxType = makeExprParser syntaxTypeApplication [[InfixR (STFun <$ symbol "->")]]

-- Lambda parameter annotations stop before the lambda arrow. Function-valued
-- parameters use parentheses, e.g. \f : (Int -> Int) -> f 1.
syntaxTypeApplication :: Parser SyntaxType
syntaxTypeApplication = do
  headType <- syntaxTypeAtom
  case headType of
    STCustom name [] -> STCustom name <$> many syntaxTypeAtom
    _ -> return headType

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
  , tupleOrParensType
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
      _ <- symbol ":"
      ty <- syntaxType
      return (name, ty)

tupleOrParensType :: Parser SyntaxType
tupleOrParensType = do
  types <- parens (sepBy syntaxType (symbol ","))
  return $ case types of
    [] -> STUnit
    [ty] -> ty
    tys -> STTuple tys
