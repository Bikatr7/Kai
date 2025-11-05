module Parser
  ( parseExpr
  , parseStatements
  , parseFileExpr
  , parseFile
  , Parser
  , ParseErrorBundle
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List (lines, isPrefixOf)
import Syntax (Expr)
import qualified Parser.Lexer as Lexer
import Parser.Expressions

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