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
import Data.Char (isAlphaNum, isSpace)
import Syntax (Expr(..), TopLevel(..), Program(..))
import qualified Parser.Lexer as Lexer
import Parser.Expressions
import Parser.Literals (identifier)
import Parser.Lexer (symbol)
import Parser.Types (syntaxType)

type Parser = Parsec Void String

stripShebang :: String -> String
stripShebang content = case lines content of
  firstLine : rest | "#!" `isPrefixOf` firstLine -> unlines rest
  _ -> content

data SplitMode
  = SplitNormal
  | SplitString Bool
  | SplitLineComment
  | SplitBlockComment

splitTopLevelChunks :: String -> [String]
splitTopLevelChunks input = reverse $ finalize depthParens depthBrackets depthBraces mode current chunks
  where
    (depthParens, depthBrackets, depthBraces, mode, current, chunks) = go 0 0 0 SplitNormal [] [] input

    go dp db dbr currentMode currentChunk acc [] = (dp, db, dbr, currentMode, currentChunk, acc)
    go dp db dbr SplitNormal currentChunk acc ('/':'/':rest) =
      go dp db dbr SplitLineComment (' ' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc ('/':'*':rest) =
      go dp db dbr SplitBlockComment (' ' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc ('"':rest) =
      go dp db dbr (SplitString False) ('"' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc ('(':rest) =
      go (dp + 1) db dbr SplitNormal ('(' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc (')':rest) =
      go (max 0 (dp - 1)) db dbr SplitNormal (')' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc ('[':rest) =
      go dp (db + 1) dbr SplitNormal ('[' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc (']':rest) =
      go dp (max 0 (db - 1)) dbr SplitNormal (']' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc ('{':rest) =
      go dp db (dbr + 1) SplitNormal ('{' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc ('}':rest) =
      go dp db (max 0 (dbr - 1)) SplitNormal ('}' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc ('\r':rest) =
      go dp db dbr SplitNormal currentChunk acc rest
    go dp db dbr SplitNormal currentChunk acc ('\n':rest)
      | dp == 0 && db == 0 && dbr == 0 =
          go dp db dbr SplitNormal [] (finishChunk currentChunk acc) rest
      | otherwise =
          go dp db dbr SplitNormal ('\n' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc (c:rest) =
      go dp db dbr SplitNormal (c : currentChunk) acc rest

    go dp db dbr (SplitString True) currentChunk acc (c:rest) =
      go dp db dbr (SplitString False) (c : currentChunk) acc rest
    go dp db dbr (SplitString False) currentChunk acc ('\\':rest) =
      go dp db dbr (SplitString True) ('\\' : currentChunk) acc rest
    go dp db dbr (SplitString False) currentChunk acc ('"':rest) =
      go dp db dbr SplitNormal ('"' : currentChunk) acc rest
    go dp db dbr (SplitString False) currentChunk acc (c:rest) =
      go dp db dbr (SplitString False) (c : currentChunk) acc rest

    go dp db dbr SplitLineComment currentChunk acc ('\r':rest) =
      go dp db dbr SplitLineComment currentChunk acc rest
    go dp db dbr SplitLineComment currentChunk acc ('\n':rest)
      | dp == 0 && db == 0 && dbr == 0 =
          go dp db dbr SplitNormal [] (finishChunk currentChunk acc) rest
      | otherwise =
          go dp db dbr SplitNormal ('\n' : currentChunk) acc rest
    go dp db dbr SplitLineComment currentChunk acc (_:rest) =
      go dp db dbr SplitLineComment currentChunk acc rest

    go dp db dbr SplitBlockComment currentChunk acc ('*':'/':rest) =
      go dp db dbr SplitNormal currentChunk acc rest
    go dp db dbr SplitBlockComment currentChunk acc ('\r':rest) =
      go dp db dbr SplitBlockComment currentChunk acc rest
    go dp db dbr SplitBlockComment currentChunk acc ('\n':rest) =
      go dp db dbr SplitBlockComment ('\n' : currentChunk) acc rest
    go dp db dbr SplitBlockComment currentChunk acc (_:rest) =
      go dp db dbr SplitBlockComment currentChunk acc rest

    finishChunk chunk acc =
      let cleaned = reverse chunk
      in if all isSpace cleaned then acc else cleaned : acc

    finalize _ _ _ _ = finishChunk

statements :: Parser [Expr]
statements = many (expr <* Lexer.sc <* (eol <|> eof))
  where
    eol = try (some (char '\n') *> notFollowedBy (char '\r'))

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (Lexer.sc *> expr <* eof) ""

parseStatements :: String -> Either (ParseErrorBundle String Void) [Expr]
parseStatements content =
  let contentChunks = filter (not . null . dropWhile isSpace) $ splitTopLevelChunks (stripShebang content)
      isPrefixOf prefix str = take (length prefix) str == prefix
      parseLine = parse (Lexer.sc *> expr <* eof) ""
  in case mapM parseLine contentChunks of
       Left err -> Left err
       Right exprs -> Right exprs

parseFileExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseFileExpr content =
  let cleanContent = unlines $ filter (not . isComment) $ lines (stripShebang content)
      isComment line = "//" `isPrefixOf` dropWhile isSpace line
      isPrefixOf prefix str = take (length prefix) str == prefix
  in parse (Lexer.sc *> expr <* eof) "" cleanContent

parseFile :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseFile sourceName = parse (Lexer.sc *> expr <* eof) sourceName . stripShebang

parseFileStatements :: String -> String -> Either (ParseErrorBundle String Void) [Expr]
parseFileStatements sourceName = parse (Lexer.sc *> statements <* eof) sourceName . stripShebang

topLevelImport :: Parser TopLevel
topLevelImport = do
  symbol "import"
  TLImport <$> identifier

topLevelExport :: Parser TopLevel
topLevelExport = do
  symbol "export"
  names <- sepBy identifier (symbol ",")
  return $ TLExport names

topLevelLetDef :: Parser TopLevel
topLevelLetDef = do
  symbol "let"
  var <- identifier
  maybeType <- optional $ do
    symbol ":"
    syntaxType
  symbol "="
  TLDef var maybeType <$> expr

topLevelLetRecDef :: Parser TopLevel
topLevelLetRecDef = do
  symbol "letrec"
  var <- identifier
  maybeType <- optional $ do
    symbol ":"
    syntaxType
  symbol "="
  (\val -> TLDef var maybeType (LetRec var maybeType val (Var var))) <$> expr

topLevelExpr :: Parser TopLevel
topLevelExpr = TLExpr <$> expr

startsWithKeyword :: String -> String -> Bool
startsWithKeyword kw input =
  case dropWhile isSpace input of
    rest | kw `isPrefixOf` rest ->
      case drop (length kw) rest of
        c:_ -> not (isAlphaNum c || c == '_')
        [] -> True
    _ -> False

parseTopLevelChunk :: String -> Either (ParseErrorBundle String Void) TopLevel
parseTopLevelChunk chunk
  | startsWithKeyword "import" chunk = parse (Lexer.sc *> topLevelImport <* eof) "" chunk
  | startsWithKeyword "export" chunk = parse (Lexer.sc *> topLevelExport <* eof) "" chunk
  | startsWithKeyword "letrec" chunk =
      case parse (Lexer.sc *> topLevelLetRecDef <* eof) "" chunk of
        Right topLevel -> Right topLevel
        Left _ -> parse (Lexer.sc *> topLevelExpr <* eof) "" chunk
  | startsWithKeyword "let" chunk =
      case parse (Lexer.sc *> topLevelLetDef <* eof) "" chunk of
        Right topLevel -> Right topLevel
        Left _ -> parse (Lexer.sc *> topLevelExpr <* eof) "" chunk
  | otherwise = parse (Lexer.sc *> topLevelExpr <* eof) "" chunk

coalesceProgramChunks :: [String] -> Either (ParseErrorBundle String Void) [String]
coalesceProgramChunks = go []
  where
    go acc [] = Right (reverse acc)
    go acc (chunk:rest) = consume chunk rest
      where
        consume current remaining =
          case parseTopLevelChunk current of
            Right _ -> go (current : acc) remaining
            Left err ->
              case remaining of
                [] -> Left err
                next:more -> consume (current ++ "\n" ++ next) more

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram content =
  let primitiveChunks = filter (not . null . dropWhile isSpace) $ splitTopLevelChunks (stripShebang content)
  in case coalesceProgramChunks primitiveChunks of
       Left err -> Left err
       Right chunks ->
         case mapM parseTopLevelChunk chunks of
           Left err -> Left err
           Right topLevels -> Right $ Program topLevels
