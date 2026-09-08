module Parser.Builtins where

import Syntax
import Parser.Lexer

-- These legacy zero-argument expressions retain their evaluation behavior.
-- Callable builtins are ordinary variables supplied by StandardLibrary.
inputExpr :: Parser Expr
inputExpr = keyword "input" >> pure Input

argsExpr :: Parser Expr
argsExpr = keyword "args" >> pure Args

nothingExpr :: Parser Expr
nothingExpr = keyword "Nothing" >> pure MNothing

getCurrentDirectoryExpr :: Parser Expr
getCurrentDirectoryExpr = keyword "getCurrentDirectory" >> pure GetCurrentDirectory
