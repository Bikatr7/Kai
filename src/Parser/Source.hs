module Parser.Source where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import Text.Megaparsec
import Parser.Lexer (Parser)
import Syntax

data SourceInfo = SourceInfo FilePath (IntMap.IntMap Text.Text)

sourceInfo :: FilePath -> String -> SourceInfo
sourceInfo file input = SourceInfo file (IntMap.fromList (zip [1..] (map (Text.dropWhileEnd (== '\r') . Text.pack) (lines input))))

sourceSpan :: SourceInfo -> SourcePos -> SourcePos -> SourceSpan
sourceSpan (SourceInfo file sourceLines) start end = SourceSpan file
  (unPos (sourceLine start)) (unPos (sourceColumn start))
  (unPos (sourceLine end)) (unPos (sourceColumn end))
  (IntMap.findWithDefault Text.empty (unPos (sourceLine start)) sourceLines)

located :: Maybe SourceInfo -> Parser Expr -> Parser Expr
located Nothing parser = parser
located (Just info) parser = do
  start <- getSourcePos
  expression <- parser
  end <- getSourcePos
  let location = sourceSpan info start end
  pure $ case expression of
    Located previous inner | sameExtent previous location -> Located location inner
    _ -> Located location expression

sameExtent :: SourceSpan -> SourceSpan -> Bool
sameExtent left right =
  spanLine left == spanLine right && spanColumn left == spanColumn right &&
  spanEndLine left == spanEndLine right && spanEndColumn left == spanEndColumn right

locatedTopLevel :: SourceInfo -> Parser TopLevel -> Parser TopLevel
locatedTopLevel info parser = do
  start <- getSourcePos
  level <- parser
  end <- getSourcePos
  pure $ TLAt (sourceSpan info start end) level

-- Set the starting position of a raw source slice without padding or deleting
-- lines. Megaparsec still owns token offsets within the slice and parse errors.
atSourceLine :: Int -> Parser a -> Parser a
atSourceLine line parser = do
  updateParserState $ \state -> state
    { statePosState = (statePosState state)
        { pstateSourcePos = (pstateSourcePos (statePosState state))
            { sourceLine = mkPos line } } }
  parser

-- Split only at physical top-level newlines. Keep every character, including
-- comments and CRLF, so offsets and excerpts still refer to the original file.
data ScanMode = Normal | Quoted Bool | LineComment | BlockComment Int

sourceChunks :: String -> [(Int,String)]
sourceChunks input = slices 0 1 input (boundaries Normal 0 0 0 0 1 input)
  where
    slices _ line remaining [] = [(line,remaining) | not (null remaining)]
    slices previous line remaining ((offset,nextLine):rest) =
      let (current,next) = splitAt (offset-previous) remaining
      in (line,current) : slices offset nextLine next rest
    boundaries _ _ _ _ _ _ [] = []
    boundaries Normal dp db dc offset line ('/':'/':rest) = boundaries LineComment dp db dc (offset+2) line rest
    boundaries Normal dp db dc offset line ('/':'*':rest) = boundaries (BlockComment 1) dp db dc (offset+2) line rest
    boundaries (BlockComment depth) dp db dc offset line ('/':'*':rest) =
      boundaries (BlockComment (depth+1)) dp db dc (offset+2) line rest
    boundaries (BlockComment depth) dp db dc offset line ('*':'/':rest) =
      boundaries (if depth == 1 then Normal else BlockComment (depth-1)) dp db dc (offset+2) line rest
    boundaries mode dp db dc offset line ('\n':rest) =
      let nextMode = case mode of LineComment -> Normal; Quoted True -> Quoted False; _ -> mode
          split = case mode of Normal -> True; LineComment -> True; _ -> False
          next = boundaries nextMode dp db dc (offset+1) (line+1) rest
      in if split && dp == 0 && db == 0 && dc == 0 then (offset+1,line+1):next else next
    boundaries mode dp db dc offset line (c:rest) =
      let (nextMode,parens,brackets,braces) = case mode of
            Quoted True -> (Quoted False,dp,db,dc)
            Quoted False | c == '\\' -> (Quoted True,dp,db,dc)
                         | c == '"' -> (Normal,dp,db,dc)
            Normal | c == '"' -> (Quoted False,dp,db,dc)
                   | c == '(' -> (Normal,dp+1,db,dc)
                   | c == ')' -> (Normal,max 0 (dp-1),db,dc)
                   | c == '[' -> (Normal,dp,db+1,dc)
                   | c == ']' -> (Normal,dp,max 0 (db-1),dc)
                   | c == '{' -> (Normal,dp,db,dc+1)
                   | c == '}' -> (Normal,dp,db,max 0 (dc-1))
            _ -> (mode,dp,db,dc)
      in boundaries nextMode parens brackets braces (offset+1) line rest
