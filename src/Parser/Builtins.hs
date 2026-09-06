module Parser.Builtins where

import Text.Megaparsec
import Syntax
import Parser.Lexer

type ExprParser = Parser Expr

-- Internal names cannot occur in user identifiers. Supplied arguments are
-- bound before returning a partial application, preserving strict evaluation.
builtin :: String -> ([Expr] -> Expr) -> Int -> ExprParser -> Parser Expr
builtin name build arity atom = do
  keyword name
  supplied <- arguments arity
  let names = ["@" ++ name ++ show i | i <- [0 .. arity - 1]]
      body = build (map Var names)
      partial = foldr (`Lambda` Nothing) body (drop (length supplied) names)
  return $ if length supplied == arity then build supplied
    else foldr (\(n,e) rest -> Let n Nothing e rest) partial (zip names supplied)
  where
    arguments 0 = pure []
    arguments n = do
      next <- optional (try atom)
      case next of
        Nothing -> pure []
        Just e -> (e :) <$> arguments (n - 1)

unary :: String -> (Expr -> Expr) -> ExprParser -> Parser Expr
unary name build = builtin name (\es -> case es of [a] -> build a; _ -> error "unary arity") 1
binary :: String -> (Expr -> Expr -> Expr) -> ExprParser -> Parser Expr
binary name build = builtin name (\es -> case es of [a,b] -> build a b; _ -> error "binary arity") 2
ternary :: String -> (Expr -> Expr -> Expr -> Expr) -> ExprParser -> Parser Expr
ternary name build = builtin name (\es -> case es of [a,b,c] -> build a b c; _ -> error "ternary arity") 3

printExpr :: ExprParser -> Parser Expr
printExpr = unary "print" Print

discardExpr :: ExprParser -> Parser Expr
discardExpr = unary "discard" Discard

parseIntExpr :: ExprParser -> Parser Expr
parseIntExpr = unary "parseInt" ParseInt

toStringExpr :: ExprParser -> Parser Expr
toStringExpr = unary "toString" ToString

showExpr :: ExprParser -> Parser Expr
showExpr = unary "show" Show

headExpr :: ExprParser -> Parser Expr
headExpr = unary "head" Head

tailExpr :: ExprParser -> Parser Expr
tailExpr = unary "tail" Tail

nullExpr :: ExprParser -> Parser Expr
nullExpr = unary "null" Null

fixExpr :: ExprParser -> Parser Expr
fixExpr = unary "fix" Fix

fstExpr :: ExprParser -> Parser Expr
fstExpr = unary "fst" Fst

sndExpr :: ExprParser -> Parser Expr
sndExpr = unary "snd" Snd

lengthExpr :: ExprParser -> Parser Expr
lengthExpr = unary "length" Length

reverseExpr :: ExprParser -> Parser Expr
reverseExpr = unary "reverse" Reverse

trimExpr :: ExprParser -> Parser Expr
trimExpr = unary "trim" Trim

strLengthExpr :: ExprParser -> Parser Expr
strLengthExpr = unary "strLength" StrLength

readFileExpr :: ExprParser -> Parser Expr
readFileExpr = unary "readFile" ReadFile

fileExistsExpr :: ExprParser -> Parser Expr
fileExistsExpr = unary "fileExists" FileExists

listDirectoryExpr :: ExprParser -> Parser Expr
listDirectoryExpr = unary "listDirectory" ListDirectory

createDirectoryExpr :: ExprParser -> Parser Expr
createDirectoryExpr = unary "createDirectory" CreateDirectory

removeDirectoryExpr :: ExprParser -> Parser Expr
removeDirectoryExpr = unary "removeDirectory" RemoveDirectory

setCurrentDirectoryExpr :: ExprParser -> Parser Expr
setCurrentDirectoryExpr = unary "setCurrentDirectory" SetCurrentDirectory

systemExpr :: ExprParser -> Parser Expr
systemExpr = unary "system" System

getEnvExpr :: ExprParser -> Parser Expr
getEnvExpr = unary "getEnv" GetEnv

exitExpr :: ExprParser -> Parser Expr
exitExpr = unary "exit" Exit

justExpr :: ExprParser -> Parser Expr
justExpr = unary "Just" MJust

leftExpr :: ExprParser -> Parser Expr
leftExpr = unary "Left" ELeft

rightExpr :: ExprParser -> Parser Expr
rightExpr = unary "Right" ERight

mapExpr :: ExprParser -> Parser Expr
mapExpr = binary "map" Map

filterExpr :: ExprParser -> Parser Expr
filterExpr = binary "filter" Filter

takeExpr :: ExprParser -> Parser Expr
takeExpr = binary "take" Take

dropExpr :: ExprParser -> Parser Expr
dropExpr = binary "drop" Drop

zipExpr :: ExprParser -> Parser Expr
zipExpr = binary "zip" Zip

splitExpr :: ExprParser -> Parser Expr
splitExpr = binary "split" Split

joinExpr :: ExprParser -> Parser Expr
joinExpr = binary "join" Join

writeFileExpr :: ExprParser -> Parser Expr
writeFileExpr = binary "writeFile" WriteFile

appendFileExpr :: ExprParser -> Parser Expr
appendFileExpr = binary "appendFile" AppendFile

setEnvExpr :: ExprParser -> Parser Expr
setEnvExpr = binary "setEnv" SetEnv

foldlExpr :: ExprParser -> Parser Expr
foldlExpr = ternary "foldl" Foldl

replaceExpr :: ExprParser -> Parser Expr
replaceExpr = ternary "replace" Replace

inputExpr :: Parser Expr
inputExpr = keyword "input" >> pure Input

argsExpr :: Parser Expr
argsExpr = keyword "args" >> pure Args

nothingExpr :: Parser Expr
nothingExpr = keyword "Nothing" >> pure MNothing

getCurrentDirectoryExpr :: Parser Expr
getCurrentDirectoryExpr = keyword "getCurrentDirectory" >> pure GetCurrentDirectory
