module Parser
  ( parseLocatedExpr
  , parseLocatedProgram
  , parseExpr
  , parseStatements
  , parseFileExpr
  , parseFile
  , parseProgram
  , Parser
  , ParseErrorBundle
  ) where

import Text.Megaparsec hiding (chunk, sourceName)
import Data.Void
import Data.Bifunctor (first)
import Data.List (isPrefixOf)
import Data.Char (isAlphaNum, isSpace)
import Syntax (Expr(..), TopLevel(..), Program(..), DataConstructor(..))
import qualified Parser.Lexer as Lexer
import Parser.Expressions
import Parser.Source
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
topLevelLetDef = topLevelLetDefWith expr

topLevelLetDefWith :: Parser Expr -> Parser TopLevel
topLevelLetDefWith expression = do
  keyword "let"
  var <- identifier
  maybeType <- optional $ do
    _ <- symbol ":"
    syntaxType
  _ <- symbol "="
  TLDef var maybeType <$> expression

topLevelLetRecDef :: Parser TopLevel
topLevelLetRecDef = topLevelLetRecDefWith expr

topLevelLetRecDefWith :: Parser Expr -> Parser TopLevel
topLevelLetRecDefWith expression = do
  keyword "letrec"
  var <- identifier
  maybeType <- optional $ do
    _ <- symbol ":"
    syntaxType
  _ <- symbol "="
  (\val -> TLDef var maybeType (LetRec var maybeType val (Var var))) <$> expression

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

-- Source-preserving entry points used by diagnostics. The original entry points
-- retain their unannotated AST contract for library callers and parser tests.
parseLocatedExpr :: FilePath -> String -> Either (ParseErrorBundle String Void) Expr
parseLocatedExpr file input =
  parse (Lexer.sc *> buildExpression (Just (sourceInfo file input)) True <* eof) file input

parseLocatedProgram :: FilePath -> String -> Either (ParseErrorBundle String Void) Program
parseLocatedProgram file input = Program <$> go (sourceChunks prepared)
  where
    info = sourceInfo file input
    expression = buildExpression (Just info) True
    prepared = case input of
      '#':'!':rest -> "  " ++ map (const ' ') (takeWhile (/= '\n') rest) ++ dropWhile (/= '\n') rest
      _ -> input
    parser = Lexer.sc *> locatedTopLevel info (choice
      [try (topLevelImport <* eof), try (topLevelExport <* eof), try (topLevelDataDecl <* eof),
       try (topLevelLetRecDefWith expression <* eof), try (topLevelLetDefWith expression <* eof),
       TLExpr <$> expression]) <* eof
    trivia chunkText = case parse (Lexer.sc *> getInput) file chunkText of
      Right remaining -> remaining
      Left _ -> chunkText
    emptyChunk (_,text) = null (trivia text)
    go [] = Right []
    go (first:rest) | emptyChunk first = go rest
    go ((line,text):rest) = consume line text rest
    consume line current remaining =
      let (blankChunks,following) = span emptyChunk remaining
          continues text = case trivia text of
            '|':_ -> True
            other -> startsWithKeyword "in" other
      in case following of
        (_,next):more | continues next -> consume line (current ++ concatMap snd blankChunks ++ next) more
        _ -> case first (rebase line) (parse (atSourceLine line parser) file current) of
          Right level -> (level:) <$> go remaining
          Left err -> case remaining of
            [] -> Left err
            (_,next):more -> consume line (current ++ next) more

-- Megaparsec's failure bundle starts from the runner's initial position, which
-- precedes the parser-level position update used for successful AST spans.
rebase :: Int -> ParseErrorBundle String Void -> ParseErrorBundle String Void
rebase line bundle = bundle { bundlePosState = positions
  { pstateSourcePos = (pstateSourcePos positions) { sourceLine = mkPos line } } }
  where positions = bundlePosState bundle
