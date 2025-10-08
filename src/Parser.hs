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
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Char (isAlphaNum)
import Control.Monad.Combinators.Expr
import Control.Monad (void)
import Syntax
import Data.List (lines)
import System.IO (putStrLn)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyword :: String -> Parser ()
keyword kw = lexeme $ try $ do
  _ <- string kw
  notFollowedBy (alphaNumChar <|> char '_')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

keywords :: [String]
keywords = ["true", "false", "if", "then", "else", "and", "or", "not", "print", "let", "letrec", "in", "input", "Int", "Bool", "String", "Unit", "parseInt", "toString", "show", "Maybe", "Either", "Just", "Nothing", "Left", "Right", "case", "of", "head", "tail", "null", "fst", "snd", "map", "filter", "foldl", "length", "reverse", "take", "drop", "zip", "split", "join", "trim", "replace", "strLength", "readFile", "writeFile", "args"]

stringLit :: Parser String
stringLit = lexeme $ char '"' *> many charChunk <* char '"'
  where
    charChunk = escaped <|> normal
    escaped = do
      _ <- char '\\'
      c <- anySingle
      case c of
        '"' -> pure '"'
        '\\' -> pure '\\'
        'n' -> pure '\n'
        _ -> fail $ "UnknownEscape '" ++ [c] ++ "' (supported: \\\" \\\\ \\n)"
    normal = satisfy (\c -> c /= '"' && c /= '\\')

-- Parse signed integers (sign must be adjacent to digits) with bounds check
integer :: Parser Int
integer = lexeme $ do
  s <- optionalSign
  n <- (L.decimal :: Parser Integer)
  let val = s * n
  if val >= fromIntegral (minBound :: Int) && val <= fromIntegral (maxBound :: Int)
    then pure (fromIntegral val)
    else fail $ "Integer literal " ++ show val ++ " is outside Int bounds"

-- Helper: consume '+' or '-' only if immediately followed by a digit
optionalSign :: Parser Integer
optionalSign =
  (try (char '-' <* lookAhead digitChar) >> pure (-1))
  <|> (try (char '+' <* lookAhead digitChar) >> pure 1)
  <|> pure 1

boolean :: Parser Bool
boolean = choice
  [ symbol "true" >> return True
  , symbol "false" >> return False
  ]

unit :: Parser ()
unit = void (symbol "()")

identifier :: Parser String
identifier = lexeme $ do
  name <- wildcard <|> regularIdentifier
  if name `elem` keywords
    then fail $ "keyword " ++ show name ++ " cannot be used as identifier"
    else return name
  where
    wildcard = string "_"
    regularIdentifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

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

-- Main expression parser
expr :: Parser Expr
expr = makeExprParser appExpr operatorTable

-- Function application and record access (higher precedence than operators)
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

-- Atomic expressions
atom :: Parser Expr
atom = choice
  [ UnitLit <$ unit
  , IntLit <$> integer
  , try parensOrTuple
  , BoolLit <$> boolean
  , StrLit <$> stringLit
  , printExpr
  , inputExpr
  , argsExpr
  , parseIntExpr
  , toStringExpr
  , showExpr
  , headExpr
  , tailExpr
  , nullExpr
  , fstExpr
  , sndExpr
  , mapExpr
  , filterExpr
  , foldlExpr
  , lengthExpr
  , reverseExpr
  , takeExpr
  , dropExpr
  , zipExpr
  , splitExpr
  , joinExpr
  , trimExpr
  , replaceExpr
  , strLengthExpr
  , readFileExpr
  , writeFileExpr
  , lambdaExpr
  , ifExpr
  , letRecExpr
  , letExpr
  , caseExpr
  , justExpr
  , nothingExpr
  , leftExpr
  , rightExpr
  , try listLitExpr
  , try recordLitExpr
  , try (Var <$> identifier)
  , try typeAnnotationExpr
  ]

-- Print expression: print expr
printExpr :: Parser Expr
printExpr = do
  keyword "print"
  Print <$> expr

-- Input expression: input
inputExpr :: Parser Expr
inputExpr = keyword "input" >> return Input

-- Command-line arguments: args
argsExpr :: Parser Expr
argsExpr = keyword "args" >> return Args

-- Built-in conversion functions
parseIntExpr :: Parser Expr
parseIntExpr = do
  keyword "parseInt"
  ParseInt <$> expr

toStringExpr :: Parser Expr
toStringExpr = do
  keyword "toString"
  ToString <$> expr

showExpr :: Parser Expr
showExpr = do
  keyword "show"
  Show <$> expr

-- List functions
headExpr :: Parser Expr
headExpr = keyword "head" >> Head <$> expr

tailExpr :: Parser Expr
tailExpr = keyword "tail" >> Tail <$> expr

nullExpr :: Parser Expr
nullExpr = keyword "null" >> Null <$> expr

fstExpr :: Parser Expr
fstExpr = keyword "fst" >> Fst <$> expr

sndExpr :: Parser Expr
sndExpr = keyword "snd" >> Snd <$> expr

-- Advanced list functions
mapExpr :: Parser Expr
mapExpr = do
  keyword "map"
  f <- atom
  Map f <$> atom

filterExpr :: Parser Expr
filterExpr = do
  keyword "filter"
  f <- atom
  Filter f <$> atom

foldlExpr :: Parser Expr
foldlExpr = do
  keyword "foldl"
  f <- atom
  acc <- atom
  Foldl f acc <$> atom

lengthExpr :: Parser Expr
lengthExpr = keyword "length" >> Length <$> atom

reverseExpr :: Parser Expr
reverseExpr = keyword "reverse" >> Reverse <$> atom

takeExpr :: Parser Expr
takeExpr = do
  keyword "take"
  n <- atom
  Take n <$> atom

dropExpr :: Parser Expr
dropExpr = do
  keyword "drop"
  n <- atom
  Drop n <$> atom

zipExpr :: Parser Expr
zipExpr = do
  keyword "zip"
  l1 <- atom
  Zip l1 <$> atom

-- String functions
splitExpr :: Parser Expr
splitExpr = do
  keyword "split"
  delim <- atom
  Split delim <$> atom

joinExpr :: Parser Expr
joinExpr = do
  keyword "join"
  delim <- atom
  Join delim <$> atom

trimExpr :: Parser Expr
trimExpr = keyword "trim" >> Trim <$> atom

replaceExpr :: Parser Expr
replaceExpr = do
  keyword "replace"
  old <- atom
  new <- atom
  Replace old new <$> atom

strLengthExpr :: Parser Expr
strLengthExpr = keyword "strLength" >> StrLength <$> atom

-- File I/O functions
readFileExpr :: Parser Expr
readFileExpr = keyword "readFile" >> ReadFile <$> atom

writeFileExpr :: Parser Expr
writeFileExpr = do
  keyword "writeFile"
  path <- atom
  WriteFile path <$> atom

-- Parenthesized expression or tuple literal
parensOrTuple :: Parser Expr
parensOrTuple = do
  symbol "("
  exprs <- sepBy expr (symbol ",")
  symbol ")"
  case exprs of
    [e] -> return e  -- Single expression in parens
    _   -> return (TupleLit exprs)  -- Tuple (including empty tuple if needed)

-- List literal
listLitExpr :: Parser Expr
listLitExpr = ListLit <$> brackets (sepBy expr (symbol ","))

-- Record literal
recordLitExpr :: Parser Expr
recordLitExpr = RecordLit <$> braces (sepBy recordField (symbol ","))

recordField :: Parser (String, Expr)
recordField = do
  name <- identifier
  symbol "="
  e <- expr
  return (name, e)

-- Type annotation expression: (expr : Type)
typeAnnotationExpr :: Parser Expr
typeAnnotationExpr = do
  symbol "("
  e <- expr
  symbol ":"
  t <- syntaxType
  symbol ")"
  return $ TypeAnnotation e t

-- Lambda expression: \x -> expr or \x : Type -> expr
lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  param <- identifier
  maybeType <- optional $ do
    symbol ":"
    syntaxType
  symbol "->"
  Lambda param maybeType <$> expr


-- If-then-else expression
ifExpr :: Parser Expr
ifExpr = do
  symbol "if"
  cond <- expr
  symbol "then"
  thenExpr <- expr
  symbol "else"
  If cond thenExpr <$> expr

-- Let expression: let x = val in expr or let x : Type = val in expr
letExpr :: Parser Expr
letExpr = do
  symbol "let"
  var <- identifier
  maybeType <- optional $ do
    symbol ":"
    syntaxType
  symbol "="
  val <- expr
  symbol "in"
  Let var maybeType val <$> expr

-- LetRec expression: letrec x = val in expr or letrec x : Type = val in expr
letRecExpr :: Parser Expr
letRecExpr = do
  symbol "letrec"
  var <- identifier
  maybeType <- optional $ do
    symbol ":"
    syntaxType
  symbol "="
  val <- expr
  symbol "in"
  LetRec var maybeType val <$> expr

-- Operator precedence table (Haskell-aligned, function application handled separately)
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
  , [ InfixR (Seq <$ symbol ";") ]  -- Lowest precedence for sequencing
  ]

-- Parse expression from string
parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (sc *> expr <* eof) ""

-- Parse multiple statements (expressions) from string
statements :: Parser [Expr]
statements = many (expr <* sc <* (eol <|> eof))
  where
    eol = try (some (char '\n') *> notFollowedBy (char '\r'))

-- Parse statements from string by splitting lines
parseStatements :: String -> Either (ParseErrorBundle String Void) [Expr]
parseStatements content = 
  let contentLines = filter (not . null . dropWhile (== ' ')) $ lines content
      nonCommentLines = filter (not . isComment) contentLines
      isComment line = "//" `isPrefixOf` dropWhile (== ' ') line
      isPrefixOf prefix str = take (length prefix) str == prefix
      parseLine = parse (sc *> expr <* eof) ""
  in case mapM parseLine nonCommentLines of
       Left err -> Left err
       Right exprs -> Right exprs

-- Parse single expression from multi-line content (ignoring comments)
parseFileExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseFileExpr content = 
  let cleanContent = unlines $ filter (not . isComment) $ lines content
      isComment line = "//" `isPrefixOf` dropWhile (== ' ') line
      isPrefixOf prefix str = take (length prefix) str == prefix
  in parse (sc *> expr <* eof) "" cleanContent

-- Parse expression from file
parseFile :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseFile = parse (sc *> expr <* eof)

-- Parse statements from file
parseFileStatements :: String -> String -> Either (ParseErrorBundle String Void) [Expr]
parseFileStatements = parse (sc *> statements <* eof)

-- Maybe/Either constructors
justExpr :: Parser Expr
justExpr = do
  symbol "Just"
  MJust <$> expr

nothingExpr :: Parser Expr
nothingExpr = symbol "Nothing" >> return MNothing

leftExpr :: Parser Expr
leftExpr = do
  symbol "Left"
  ELeft <$> expr

rightExpr :: Parser Expr
rightExpr = do
  symbol "Right"
  ERight <$> expr

-- Case expression: case expr of pattern -> expr | pattern -> expr
caseExpr :: Parser Expr
caseExpr = do
  symbol "case"
  scrutinee <- expr
  symbol "of"
  patterns <- sepBy1 casePattern (symbol "|")
  return $ Case scrutinee patterns

-- Pattern in case expression
casePattern :: Parser (Pattern, Expr)
casePattern = do
  pat <- patternParser
  symbol "->"
  expr <- expr
  return (pat, expr)

-- Pattern parser
patternParser :: Parser Pattern
patternParser = makeExprParser patternTerm patternOperatorTable

patternTerm :: Parser Pattern
patternTerm = choice
  [ PInt <$> integer
  , PBool <$> boolean
  , PStr <$> stringLit
  , PUnit <$ unit
  , try listPattern
  , try recordPattern
  , justPattern
  , nothingPattern
  , leftPattern
  , rightPattern
  , PVar <$> identifier
  , try parensOrTuplePattern
  ]

parensOrTuplePattern :: Parser Pattern
parensOrTuplePattern = do
  symbol "("
  pats <- sepBy patternParser (symbol ",")
  symbol ")"
  case pats of
    [p] -> return p
    _   -> return (PTuple pats)  -- Tuple pattern

patternOperatorTable :: [[Operator Parser Pattern]]
patternOperatorTable = [ [ InfixR (PCons <$ symbol "::") ] ]

justPattern :: Parser Pattern
justPattern = do
  symbol "Just"
  PJust <$> patternTerm

nothingPattern :: Parser Pattern
nothingPattern = symbol "Nothing" >> return PNothing

leftPattern :: Parser Pattern
leftPattern = do
  symbol "Left"
  PLeft <$> patternTerm

rightPattern :: Parser Pattern
rightPattern = do
  symbol "Right"
  PRight <$> patternTerm

listPattern :: Parser Pattern
listPattern = PList <$> brackets (sepBy patternParser (symbol ","))

recordPattern :: Parser Pattern
recordPattern = PRecord <$> braces (sepBy recordPatternField (symbol ","))

recordPatternField :: Parser (String, Pattern)
recordPatternField = do
  name <- identifier
  symbol "="
  p <- patternParser
  return (name, p)