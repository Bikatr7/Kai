module Parser
  ( parseExpr
  , parseStatements
  , parseFileExpr
  , parseFile
  , parseProgram
  , Parser
  , ParseErrorBundle
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators (sepBy)
import Data.Void
import Data.List (lines, isPrefixOf)
import Data.Char (isSpace)
import Syntax (Expr(..), TopLevel(..), Program(..))
import qualified Parser.Lexer as Lexer
import Parser.Expressions
import Parser.Literals (identifier)
import Parser.Lexer (symbol)
import Parser.Types (syntaxType)

type Parser = Parsec Void String

statements :: Parser [Expr]
statements = many (expr <* Lexer.sc <* (eol <|> eof))
  where
    eol = try (some (char '\n') *> notFollowedBy (char '\r'))

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (Lexer.sc *> expr <* eof) ""

parseStatements :: String -> Either (ParseErrorBundle String Void) [Expr]
parseStatements content =
  let contentLines = filter (not . null . dropWhile (== ' ')) $ lines content
      nonCommentLines = filter (not . isComment) contentLines
      isComment line = "//" `isPrefixOf` dropWhile (== ' ') line
      isPrefixOf prefix str = take (length prefix) str == prefix
      parseLine = parse (Lexer.sc *> expr <* eof) ""
  in case mapM parseLine nonCommentLines of
       Left err -> Left err
       Right exprs -> Right exprs

parseFileExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseFileExpr content =
  let cleanContent = unlines $ filter (not . isComment) $ lines content
      isComment line = "//" `isPrefixOf` dropWhile (== ' ') line
      isPrefixOf prefix str = take (length prefix) str == prefix
  in parse (Lexer.sc *> expr <* eof) "" cleanContent

parseFile :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseFile = parse (Lexer.sc *> expr <* eof)

parseFileStatements :: String -> String -> Either (ParseErrorBundle String Void) [Expr]
parseFileStatements = parse (Lexer.sc *> statements <* eof)

topLevel :: Parser TopLevel
topLevel = choice
  [ try $ do
      symbol "import"
      moduleName <- identifier
      return $ TLImport moduleName
  , try $ do
      symbol "export"
      names <- sepBy identifier (symbol ",")
      return $ TLExport names
  , try $ do
      symbol "let"
      var <- identifier
      maybeType <- optional $ do
        symbol ":"
        syntaxType
      symbol "="
      val <- expr
      return $ TLDef var maybeType val
  , try $ do
      symbol "letrec"
      var <- identifier
      maybeType <- optional $ do
        symbol ":"
        syntaxType
      symbol "="
      val <- expr
      return $ TLDef var maybeType (LetRec var maybeType val (Var var))
  , TLExpr <$> expr
  ]

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram content =
  let lines = filter (not . isCommentOrEmpty) $ splitLines content
      isCommentOrEmpty line = null (trim line) || "//" `isPrefixOf` (trim line)
      trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
      splitLines [] = []
      splitLines xs = let (line, rest) = break (== '\n') xs
                      in line : case rest of
                        [] -> []
                        (_:ys) -> splitLines ys
  in case mapM (parse (Lexer.sc *> topLevel <* eof) "") lines of
       Left err -> Left err
       Right topLevels -> Right $ Program topLevels