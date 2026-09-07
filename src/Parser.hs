module Parser
  ( parseExpr
  , parseStatements
  , parseFileExpr
  , parseFile
  , parseProgram
  , Parser
  , ParseErrorBundle
  ) where

import Text.Megaparsec hiding (chunk, sourceName)
import Data.Void
import Data.List (isPrefixOf)
import Data.Char (isAlphaNum, isSpace)
import Syntax (Expr(..), TopLevel(..), Program(..), DataConstructor(..))
import qualified Parser.Lexer as Lexer
import Parser.Expressions
import Parser.Literals (identifier, constructorIdentifier, lowerIdentifier)
import Parser.Lexer (keyword, symbol)
import Parser.Types (syntaxType, syntaxTypeAtom)

type Parser = Parsec Void String

stripShebang :: String -> String
stripShebang content = case lines content of
  firstLine : rest | "#!" `isPrefixOf` firstLine -> unlines rest
  _ -> content

data SplitMode
  = SplitNormal
  | SplitString Bool
  | SplitLineComment
  | SplitBlockComment Int

splitTopLevelChunks :: String -> [String]
splitTopLevelChunks input = reverse $ finalize depthParens depthBrackets depthBraces mode current chunks
  where
    (depthParens, depthBrackets, depthBraces, mode, current, chunks) = go (0 :: Int) (0 :: Int) (0 :: Int) SplitNormal [] [] input

    go dp db dbr currentMode currentChunk acc [] = (dp, db, dbr, currentMode, currentChunk, acc)
    go dp db dbr SplitNormal currentChunk acc ('/':'/':rest) =
      go dp db dbr SplitLineComment (' ' : currentChunk) acc rest
    go dp db dbr SplitNormal currentChunk acc ('/':'*':rest) =
      go dp db dbr (SplitBlockComment 1) (' ' : currentChunk) acc rest
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

    go dp db dbr (SplitBlockComment depth) currentChunk acc ('/':'*':rest) =
      go dp db dbr (SplitBlockComment (depth + 1)) currentChunk acc rest
    go dp db dbr (SplitBlockComment depth) currentChunk acc ('*':'/':rest) =
      go dp db dbr (if depth == 1 then SplitNormal else SplitBlockComment (depth - 1)) currentChunk acc rest
    go dp db dbr (SplitBlockComment depth) currentChunk acc ('\n':rest) =
      go dp db dbr (SplitBlockComment depth) ('\n' : currentChunk) acc rest
    go dp db dbr (SplitBlockComment depth) currentChunk acc (_:rest) =
      go dp db dbr (SplitBlockComment depth) currentChunk acc rest

    finishChunk chunk acc =
      let cleaned = reverse chunk
      in if all isSpace cleaned then acc else cleaned : acc

    finalize _ _ _ (SplitBlockComment _) chunk acc = finishChunk (reverse " /*" ++ chunk) acc
    finalize _ _ _ _ chunk acc = finishChunk chunk acc

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (Lexer.sc *> expr <* eof) ""

parseStatements :: String -> Either (ParseErrorBundle String Void) [Expr]
parseStatements content =
  let contentChunks = filter (not . null . dropWhile isSpace) $ splitTopLevelChunks (stripShebang content)
      parseLine = parse (Lexer.sc *> expr <* eof) ""
  in mapM parseLine contentChunks

parseFileExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseFileExpr = parseExpr . stripShebang

parseFile :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseFile sourceName = parse (Lexer.sc *> expr <* eof) sourceName . stripShebang

topLevelImport :: Parser TopLevel
topLevelImport = do
  keyword "import"
  TLImport <$> identifier

topLevelExport :: Parser TopLevel
topLevelExport = do
  keyword "export"
  names <- sepBy identifier (symbol ",")
  return $ TLExport names

topLevelDataDecl :: Parser TopLevel
topLevelDataDecl = do
  keyword "data"
  typeName <- constructorIdentifier
  typeVars <- many lowerIdentifier
  _ <- symbol "="
  constructors <- sepBy1 dataConstructorDecl (symbol "|")
  return $ TLData typeName typeVars constructors

dataConstructorDecl :: Parser DataConstructor
dataConstructorDecl = do
  constructorName <- constructorIdentifier
  argTypes <- many syntaxTypeAtom
  return $ DataConstructor constructorName argTypes

topLevelLetDef :: Parser TopLevel
topLevelLetDef = do
  keyword "let"
  var <- identifier
  maybeType <- optional $ do
    _ <- symbol ":"
    syntaxType
  _ <- symbol "="
  TLDef var maybeType <$> expr

topLevelLetRecDef :: Parser TopLevel
topLevelLetRecDef = do
  keyword "letrec"
  var <- identifier
  maybeType <- optional $ do
    _ <- symbol ":"
    syntaxType
  _ <- symbol "="
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
  | startsWithKeyword "data" chunk = parse (Lexer.sc *> topLevelDataDecl <* eof) "" chunk
  | startsWithKeyword "letrec" chunk =
      case parse (Lexer.sc *> topLevelLetRecDef <* eof) "" chunk of
        Right topLevel -> Right topLevel
        Left _ -> parse (Lexer.sc *> topLevelExpr <* eof) "" chunk
  | startsWithKeyword "let" chunk =
      case parse (Lexer.sc *> topLevelLetDef <* eof) "" chunk of
        Right topLevel -> Right topLevel
        Left _ -> parse (Lexer.sc *> topLevelExpr <* eof) "" chunk
  | otherwise = parse (Lexer.sc *> topLevelExpr <* eof) "" chunk

parseTopLevels :: [String] -> Either (ParseErrorBundle String Void) [TopLevel]
parseTopLevels = go []
  where
    go acc [] = Right (reverse acc)
    go acc (chunk:rest) = consume chunk rest
      where
        consume current remaining =
          case remaining of
            next:more
              | startsWithContinuationPipe next || startsWithKeyword "in" next ->
                  consume (current ++ "\n" ++ next) more
            _ ->
              case parseTopLevelChunk current of
                Right parsed -> go (parsed : acc) remaining
                Left err ->
                  case remaining of
                    [] -> Left err
                    next:more -> consume (current ++ "\n" ++ next) more

    startsWithContinuationPipe chunk =
      case dropWhile isSpace chunk of
        '|' : _ -> True
        _ -> False

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram content =
  let primitiveChunks = filter (not . null . dropWhile isSpace) $ splitTopLevelChunks (stripShebang content)
  in Program <$> parseTopLevels primitiveChunks
