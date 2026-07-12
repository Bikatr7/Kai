# Kai Language Specification (v0.0.4.4)

This document provides a comprehensive technical specification of the Kai programming language in its current state. It serves as the authoritative reference for language semantics, syntax, and behavior.

**Version**: 0.0.4.4
**Released**: 2026-07-11
**Last Updated**: 2026-07-11

**Note**: Kai uses a modular architecture with focused Parser, TypeChecker, Evaluator, REPL, and module-loading components. Performance benchmarks are available via `stack bench`.

## Table of Contents

- [Overview](#overview)
- [Lexical Structure](#lexical-structure)
- [Types](#types)
- [Expressions](#expressions)
- [Functions](#functions)
- [Type System](#type-system)
- [Top-Level Programs](#top-level-programs)
- [Built-in Functions](#built-in-functions)
- [I/O Operations](#io-operations)
- [Error Handling](#error-handling)
- [Evaluation Model](#evaluation-model)

## Overview

Kai is a functional-first scripting language with static typing, implemented in Haskell. The language features:

- **Evaluation**: Strict (call-by-value) evaluation
- **Type System**: Unification-based type inference with occurs check, generalized let-polymorphism, and explicitly annotated polymorphic recursion
- **Recursion**: `letrec` plus a typed `fix : (a -> a) -> a` combinator
- **Paradigm**: Expression-oriented with immutable data by default
- **Data Modeling**: Built-in lists/records/tuples/Maybe/Either plus user-defined algebraic data types
- **Interactive Workflow**: CLI file/expression execution plus a REPL with multiline input and `:type`, `:load`, `:reload`, `:quit`
- **File Format**: Single-file scripts with `.kai` extension, or multi-file modules with imports; top-level newlines split expressions only outside nested forms

## Lexical Structure

### Comments

```kai
// Line comments start with double slash
/* Block comments are enclosed in /* */ */
```

- A leading shebang line such as `#!/usr/bin/env kai` is ignored when parsing files.

### Literals

#### Integer Literals
- Range: `-2³¹` to `2³¹-1` (32-bit signed integers)
- Format: `42`, `-17`, `0`
- Overflow: Integer literals outside the valid range cause parse errors
- Arithmetic results outside the valid range cause `IntegerOverflow`

#### Boolean Literals
- `true` and `false`

#### String Literals
- Enclosed in double quotes: `"hello"`
- Supported escapes: `\"`, `\\`, `\n`
- Unknown escapes cause parse errors with helpful messages

#### Unit Literal
- `()` represents the unit value with type `Unit`

### Identifiers
- Start with letter, followed by letters, digits, or underscores
- Cannot be reserved keywords
- Special identifier: `_` (wildcard) can be used in let bindings to discard values

### Reserved Keywords
`true`, `false`, `if`, `then`, `else`, `and`, `or`, `not`, `print`, `discard`, `let`, `letrec`, `in`, `do`, `data`, `import`, `export`, `input`, `args`, `Int`, `Bool`, `String`, `Unit`, `parseInt`, `toString`, `show`, `fix`, `Maybe`, `Either`, `Just`, `Nothing`, `Left`, `Right`, `case`, `of`, `head`, `tail`, `null`, `fst`, `snd`, `map`, `filter`, `foldl`, `length`, `reverse`, `take`, `drop`, `zip`, `split`, `join`, `trim`, `replace`, `strLength`, `readFile`, `writeFile`, `appendFile`, `fileExists`, `listDirectory`, `createDirectory`, `removeDirectory`, `getCurrentDirectory`, `setCurrentDirectory`, `system`, `getEnv`, `setEnv`, `exit`

**Note**: `_` is not a keyword but has special meaning as a wildcard identifier in let bindings.

## Types

Kai has a static type system with the following base types:

### Base Types
- `Int`: 32-bit signed integers
- `Bool`: Boolean values (`true` or `false`)
- `String`: String values with escape sequence support
- `Unit`: Unit type with single value `()`

### Composite Types
- `[T]`: List of type T (e.g., `[Int]`, `[String]`)
- `(T1, T2, ...)`: Tuple of types (e.g., `(Int, String)`, `(Bool, Int, String)`)
- `{field1: T1, field2: T2, ...}`: Record with named fields
- `TypeName T1 ... Tn`: User-defined algebraic data types declared with `data`

### Error Handling Types
- `Maybe T`: Optional values: `Just value` or `Nothing`
- `Either T U`: Error propagation: `Left error` or `Right value`

### Function Types
- `T₁ -> T₂`: Function from type T₁ to type T₂
- Right-associative: `Int -> Int -> Bool` = `Int -> (Int -> Bool)`

### Type Annotations
Optional Haskell-style type annotations:
```kai
let x : Int = 42
\x : String -> length x
let add : Int -> Int -> Int = \x -> \y -> x + y
```

## Expressions

All constructs in Kai are expressions that evaluate to values.

### Arithmetic Expressions
- `+`, `-`, `*`, `/` (integer division)
- Unary minus: `-expr`
- Division by zero causes runtime error

### Boolean Expressions
- `and`, `or` (right-associative)
- `not` (prefix)

### Comparison Expressions
- `==`, `<`, `>` (non-associative)
- Equality is structural for integers, booleans, strings, unit, custom data,
  `Maybe`, `Either`, lists, records, and tuples. Different constructors of the
  same custom, `Maybe`, or `Either` type compare as `false`.
- Callable values and recursive runtime references are not comparable. Equality
  returns a `TypeError` when either operand contains one, including inside a
  composite value.

### String Operations
- `++` (concatenation, right-associative)

### Expression Sequencing
- `;` (sequencing, right-associative) - evaluates first expression for side effects, returns second expression

### Do Blocks
- `do { expr1; expr2; expr3 }` - ergonomic block syntax for sequencing
- Entries are separated by semicolons
- `do {}` evaluates to `()`
- `do { expr1; expr2; expr3 }` desugars to `expr1; expr2; expr3`

### List Operations
- `[elem1, elem2, ...]` - list literals
- `++` - list concatenation (right-associative)
- `::` - cons operator (right-associative): `elem :: list`
- `head list` - first element
- `tail list` - list without first element
- `null list` - check if list is empty

### Record Operations
- `{field1 = val1, field2 = val2, ...}` - record literals
- `record.field` - field access
- `==` - structural equality

### Tuple Operations
- `(val1, val2, ...)` - tuple literals (2 or more elements)
- `fst tuple` - first element of a 2-tuple
- `snd tuple` - second element of a 2-tuple
- `==` - structural equality

**Note**: Empty tuples `()` are the Unit value, and single-element tuples like `(x)` are just parenthesized expressions.

### Conditional Expressions
```kai
if condition then expr1 else expr2
```

### Case Expressions (Pattern Matching)
```kai
case expression of pattern -> expr | pattern -> expr
```

**Patterns**:
- `x` - variable pattern (binds value to variable)
- `_` - wildcard pattern (matches any value without binding)
- `Just x` - matches Maybe values
- `Nothing` - matches empty Maybe
- `Left x` - matches Either left values
- `Right x` - matches Either right values
- `Constructor p1 ... pn` - matches user-defined constructors
- `[]` - matches empty list
- `x :: xs` - matches non-empty list (head and tail)
- `{field1 = pattern1, field2 = pattern2, ...}` - matches records
- `(pattern1, pattern2, ...)` - matches tuples

**Example**:
```kai
case parseInt "42" of Just x -> x | Nothing -> 0
```

### Variable Binding
```kai
let var = value in body
let var : Type = value in body
let _ = value in body    -- Wildcard binding (discards value)
```

### Recursive Binding
```kai
letrec var = value in body
letrec var : Type = value in body
```

**Note**: Wildcards (`_`) are not allowed in `letrec` bindings as they cannot be meaningfully recursive.

### Expression Sequencing
```kai
expr1; expr2         -- Evaluate expr1, discard result, return expr2
expr1; expr2; expr3  -- Right-associative: expr1; (expr2; expr3)
```

### Do Blocks
```kai
do {}                        -- ()
do { print "start"; 42 }     -- Prints then returns 42
do { expr1; expr2; expr3; }  -- Optional trailing semicolon
```

### Lambda Expressions
```kai
\param -> body
\param : Type -> body
```

### Function Application
- Left-associative: `f x y` = `(f x) y`
- Higher precedence than infix operators
- Lambdas and non-nullary data constructors are callable values. Constructors
  can be partially applied and used by higher-order operations such as `map`
  and `foldl`.

### Type Annotation Expressions
```kai
(expr : Type)
```

## Top-Level Programs

Kai program files may contain top-level definitions, data declarations, imports, exports, and an optional final expression.

### Top-Level Definitions
```kai
let value = expr
let value : Type = expr
letrec recursive = expr
letrec recursive : Type = expr
```

### Custom Data Types
```kai
data Option a = None | Some a
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

Each constructor becomes a value in scope:
- Nullary constructors behave like values
- Constructors with arguments behave like functions and can be partially applied

### Modules
```kai
import ModuleName
export value, helper, Constructor
```

Module resolution supports both `ModuleName.kai` and `ModuleName/ModuleName.kai`.

## Functions

### First-Class Functions
Functions are first-class values that can be:
- Passed as arguments
- Returned from other functions
- Stored in variables

### Closures
Lambda expressions capture their lexical environment:
```kai
let makeAdder = \n -> \x -> x + n in
let add5 = makeAdder 5 in
add5 10  // => 15
```

### Fixed Points
`fix : (a -> a) -> a` computes a fixed point for a callable value. Productive
recursive functions can be expressed directly; a function or constructor fixed
point that forces itself before producing a value fails with
`TypeError "Fixpoint forced before initialization"`.

```kai
let factorial = fix (\self -> \n -> if n == 0 then 1 else n * self (n - 1)) in
factorial 5  // => 120
```

## Type System

### Type Inference
Kai uses unification-based type inference:
- Types are inferred without explicit annotations
- Type annotations are optional but checked when provided
- Function signatures can contain inferred type variables
- Let, letrec, top-level, and imported definitions are generalized over free type variables
- Recursive bindings can recurse polymorphically when they have explicit type annotations
- Unannotated recursive bindings remain monomorphic

### Unification
- Occurs check prevents infinite types
- Type variables are unified across expressions
- Substitutions are composed efficiently

### Type Errors
- `TypeMismatch T1 T2`: Expected T1 but got T2
- `ExpectedBool T`: Expected Bool in conditional
- `ExpectedFunction T`: Tried to apply non-function
- `UnboundVariable x`: Variable not in scope
- `InfiniteType x T`: Occurs check failure

## Built-in Functions

### Type Conversion Functions
```kai
parseInt : String -> Maybe Int  // "42" -> Just 42; invalid or out-of-range -> Nothing
toString : Int -> String        // 42 -> "42"
show : a -> String             // Any value to string representation
discard : a -> Unit             // Evaluates and discards any value, returns ()
```

### Recursion Function
```kai
fix : (a -> a) -> a             // Typed fixed-point combinator
```

### List Functions
```kai
// Basic operations
head : [a] -> a            // First element (runtime error if empty)
tail : [a] -> [a]          // List without first element
null : [a] -> Bool         // Check if list is empty
length : [a] -> Int        // Number of elements in list

// Higher-order functions
map : (a -> b) -> [a] -> [b]                    // Apply function to each element
filter : (a -> Bool) -> [a] -> [a]              // Keep elements matching predicate
foldl : (b -> a -> b) -> b -> [a] -> b          // Left fold over list

// List manipulation
reverse : [a] -> [a]       // Reverse list order
take : Int -> [a] -> [a]   // Take first n elements
drop : Int -> [a] -> [a]   // Drop first n elements
zip : [a] -> [b] -> [(a, b)]  // Combine two lists into list of tuples
```

### String Functions
```kai
split : String -> String -> [String]       // Split string by delimiter
join : String -> [String] -> String        // Join strings with delimiter
trim : String -> String                     // Remove leading/trailing whitespace
replace : String -> String -> String -> String  // replace pattern replacement string
strLength : String -> Int                   // Length of string
```

### Tuple Functions
```kai
fst : (a, b) -> a          // First element of pair
snd : (a, b) -> b          // Second element of pair
```

### I/O Functions
```kai
// Console I/O
print : a -> Unit           // Print value and return ()
input : String              // Read line from stdin

// File I/O
readFile : String -> String              // Read entire file as string
writeFile : String -> String -> Unit     // Write string to file
appendFile : String -> String -> Unit    // Append string to file
fileExists : String -> Bool              // Check whether a file exists
listDirectory : String -> [String]       // List directory entries
createDirectory : String -> Unit         // Create a directory
removeDirectory : String -> Unit         // Remove an empty directory
getCurrentDirectory : String             // Current working directory
setCurrentDirectory : String -> Unit     // Change current working directory
system : String -> Int                   // Run shell command and return exit code
getEnv : String -> Maybe String          // Read environment variable
setEnv : String -> String -> Unit        // Set environment variable
exit : Int -> a                          // Exit the current program with a code

// Command-line arguments
args : [String]             // List of command-line arguments passed to script or REPL session
```

## I/O Operations

### Standard Input
- `input` reads a complete line from stdin
- Returns string value including any whitespace
- No prompt is displayed

### Standard Output
- `print expr` evaluates expr, prints its value, returns `()`
- Output format matches value representation

### File I/O
- `readFile path` reads entire file as string
  - Returns file contents as a string
  - Runtime error if file cannot be read
- `writeFile path content` writes string to file
  - Creates file if it doesn't exist, overwrites if it does
  - Returns `()` (Unit)
  - Runtime error if file cannot be written
- `appendFile path content` appends string to file
- `fileExists path` checks whether a file exists
- `listDirectory path` returns directory entries as strings
- `createDirectory path` creates a new directory
- `removeDirectory path` removes an empty directory
- `getCurrentDirectory` returns the current working directory
- `setCurrentDirectory path` changes the current working directory
- `system command` executes a shell command and returns its exit code
- `getEnv name` returns `Just value` or `Nothing`
- `setEnv name value` updates an environment variable
- `exit code` stops the current evaluation with the provided exit code

**Example**:
```kai
let content = "Hello, world!" in
do {
  writeFile "output.txt" content;
  let read = readFile "output.txt" in
    print read
}
```

### Command-Line Arguments and REPL Sessions
- `args` evaluates to list of command-line arguments
- Arguments passed after script filename or after `kai repl`
- Empty list if no arguments provided

**Example**:
```bash
$ kai script.kai foo bar baz
```

```kai
let firstArg = head args in  // "foo"
let numArgs = length args in  // 3
print (show args)  // ["foo", "bar", "baz"]
```

### Interactive Programs
Programs can combine input/output for interaction:
```kai
let name = input in
print ("Hello, " ++ name)
```

## Error Handling

### Parse Errors
- Invalid syntax causes immediate parse failure
- Error messages indicate location and expected tokens

### Type Errors
- Static type checking occurs before evaluation
- Type mismatches expressible by Kai's type system are rejected before evaluation
- Runtime-only constraints, such as non-comparable callable values, still report typed runtime errors

### Runtime Errors
- Division by zero: `DivByZero`
- Signed 32-bit arithmetic overflow: `IntegerOverflow`
- Unbound variable references: `UnboundVariable "var_name"`
- Missing record fields: `RecordFieldNotFound "field_name"`
- Type errors in runtime contexts: `TypeError "message"`
- Host I/O failures, including stdin EOF and invalid file/environment operations, are converted to `TypeError` values rather than escaping as Haskell exceptions

### Error Handling with Types
- **Maybe types**: `Just value | Nothing` for optional values
- **Either types**: `Left error | Right value` for error propagation
- **Case expressions**: Pattern matching for handling Maybe/Either gracefully
- **Safe conversion functions**: `parseInt : String -> Maybe Int` returns `Nothing` for invalid input

### Error Recovery
- `Maybe` and `Either` values support explicit recovery through pattern matching
- Unhandled runtime errors stop the current evaluation before later operands or sequenced effects run
- Programs must be syntactically and type-correct to start, but may still encounter documented runtime errors

## Evaluation Model

### Evaluation Strategy
- **Strict evaluation**: Arguments evaluated before function application
- **Call-by-value**: Values passed to functions, not expressions

### Evaluation Order
- Left-to-right evaluation of function applications
- A runtime error stops evaluation before later operands or sequenced effects run
- Conditional expressions evaluate condition first
- Let bindings evaluate value before body

### Environment Model
- Lexical scoping with static binding
- Variables bound in let/letrec extend inner environment
- Function parameters shadow outer variables

## Operator Precedence

From highest to lowest precedence:

1. **Function Application** (left-associative)
2. **Field Access**: `.field` (left-associative)
3. **Prefix Operators**: `not`, unary `-`
4. **Multiplicative**: `*`, `/` (left-associative)
5. **Additive**: `+`, `-` (left-associative)
6. **Cons**: `::` (right-associative)
7. **Concatenation**: `++` (right-associative)
8. **Comparison**: `==`, `<`, `>` (non-associative)
9. **Logical AND**: `and` (right-associative)
10. **Logical OR**: `or` (right-associative)
11. **Sequencing**: `;` (right-associative, lowest precedence)

## Language Limitations (Current)

- **No error recovery**: Single parse/type error stops execution
- **Minimal REPL ergonomics**: No history, completion, or editor integration yet
- **Limited standard library depth**: Core file/process/env helpers exist, but line-oriented I/O, JSON/HTTP, and packaging are still missing
- **Polymorphic recursion requires explicit annotations**: Unannotated recursive bindings remain monomorphic

## Grammar Summary

```bnf
Program ::= TopLevel*

TopLevel ::= 'data' ConstructorIdent Ident* '=' ConstructorDecl ('|' ConstructorDecl)*
           | 'let' Ident (':' Type)? '=' Expr
           | 'letrec' Ident (':' Type)? '=' Expr
           | 'import' Ident
           | 'export' Ident (',' Ident)*
           | Expr

ConstructorDecl ::= ConstructorIdent TypeAtom*

Expr ::= 'let' Ident (':' Type)? '=' Expr 'in' Expr
       | 'letrec' Ident (':' Type)? '=' Expr 'in' Expr
       | 'if' Expr 'then' Expr 'else' Expr
       | 'case' Expr 'of' Pattern '->' Expr ('|' Pattern '->' Expr)*
       | 'do' '{' (Expr (';' Expr)* ';'?)? '}'
       | '\' Ident (':' Type)? '->' Expr
       | '(' Expr ':' Type ')'
       | SeqExpr

SeqExpr ::= OrExpr (';' SeqExpr)?
OrExpr ::= AndExpr ('or' OrExpr)?
AndExpr ::= CmpExpr ('and' AndExpr)?
CmpExpr ::= ConcatExpr ('==' | '<' | '>') ConcatExpr | ConcatExpr
ConcatExpr ::= ConsExpr ('++' ConcatExpr)?
ConsExpr ::= AddExpr ('::' ConsExpr)?
AddExpr ::= AddExpr ('+' | '-') MulExpr | MulExpr
MulExpr ::= MulExpr ('*' | '/') UnaryExpr | UnaryExpr
UnaryExpr ::= ('not' | '-') UnaryExpr | AppExpr
AppExpr ::= AppExpr ('.' Ident | Atom) | Atom

Atom ::= Integer | Boolean | String | ListLit | RecordLit | TupleLit
       | '(' Expr ')' | Ident | ConstructorIdent | '()' | 'input'
       | UnaryBuiltin Atom | BinaryBuiltin Atom Atom
       | TernaryBuiltin Atom Atom Atom | NullaryBuiltin

UnaryBuiltin ::= 'print' | 'discard' | 'parseInt' | 'toString' | 'show' | 'fix'
               | 'head' | 'tail' | 'null' | 'fst' | 'snd'
               | 'length' | 'reverse' | 'trim' | 'strLength'
               | 'Just' | 'Left' | 'Right' | 'readFile'
               | 'fileExists' | 'listDirectory' | 'createDirectory'
               | 'removeDirectory' | 'setCurrentDirectory' | 'system'
               | 'getEnv' | 'exit'
BinaryBuiltin ::= 'map' | 'filter' | 'take' | 'drop' | 'zip' | 'split' | 'join'
                | 'writeFile' | 'appendFile' | 'setEnv'
TernaryBuiltin ::= 'foldl' | 'replace'
NullaryBuiltin ::= 'input' | 'args' | 'Nothing' | 'getCurrentDirectory'

ListLit ::= '[' (Expr (',' Expr)*)? ']'
RecordLit ::= '{' (Ident '=' Expr (',' Ident '=' Expr)*)? '}'
TupleLit ::= '(' Expr ',' Expr (',' Expr)* ')'

Pattern ::= Integer | Boolean | String | '()' | Ident
          | 'Just' Pattern | 'Nothing'
          | 'Left' Pattern | 'Right' Pattern
          | ConstructorIdent PatternAtom*
          | '[' ']' | Pattern '::' Pattern
          | '{' (Ident '=' Pattern (',' Ident '=' Pattern)*)? '}'
          | '(' Pattern (',' Pattern)+ ')'

PatternAtom ::= Integer | Boolean | String | '()' | Ident | ConstructorIdent
              | '[' (Pattern (',' Pattern)*)? ']'
              | '{' (Ident '=' Pattern (',' Ident '=' Pattern)*)? '}'
              | '(' Pattern ')'

Type ::= TypeApplication ('->' Type)?
TypeApplication ::= TypeAtom TypeAtom*
TypeAtom ::= 'Int' | 'Bool' | 'String' | 'Unit' | Ident | ConstructorIdent
           | '[' Type ']' | '{' (Ident ':' Type (',' Ident ':' Type)*)? '}'
           | 'Maybe' TypeAtom | 'Either' TypeAtom TypeAtom | '(' Type ')'

Ident ::= [a-zA-Z][a-zA-Z0-9_]* | '_'
ConstructorIdent ::= [A-Z][a-zA-Z0-9_]*
Integer ::= [+-]?[0-9]+
Boolean ::= 'true' | 'false'
String ::= '"' StringChar* '"'
```

## Implementation Notes

- **Architecture**: Modular parser, type-checker, evaluator, module, CLI, and REPL components
- **Parser**: Megaparsec with operator precedence parsing across multiple specialized modules
- **Type Checker**: Algorithm W with unification, split across specialized inference modules
- **Evaluator**: Direct AST interpretation with closure environments, dual pure/IO evaluation paths
- **Performance**: Optimized for deeply nested expressions (1000+ levels), comprehensive benchmarking suite available
- **Benchmarks**: Criterion (speed) and Weigh (memory) profiling with regression detection

This specification documents the released Kai v0.0.4.4 language. Updates to language features should be reflected in this document immediately.
