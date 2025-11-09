# Kai Language Specification (v0.0.4)

This document provides a comprehensive technical specification of the Kai programming language in its current state. It serves as the authoritative reference for language semantics, syntax, and behavior.

**Version**: 0.0.4
**Last Updated**: 2025-11-06

**Note**: Kai uses a modular architecture with 28 focused submodules across Parser, TypeChecker, and Evaluator components. Performance benchmarks are available via `stack bench`.

## Table of Contents

- [Overview](#overview)
- [Lexical Structure](#lexical-structure)
- [Types](#types)
- [Expressions](#expressions)
- [Functions](#functions)
- [Type System](#type-system)
- [Built-in Functions](#built-in-functions)
- [I/O Operations](#io-operations)
- [Error Handling](#error-handling)
- [Evaluation Model](#evaluation-model)

## Overview

Kai is a functional-first scripting language with static typing, implemented in Haskell. The language features:

- **Evaluation**: Strict (call-by-value) evaluation
- **Type System**: Hindley-Milner type inference with unification and occurs check
- **Paradigm**: Expression-oriented with immutable data by default
- **File Format**: Single-file scripts with `.kai` extension

## Lexical Structure

### Comments

```kai
// Line comments start with double slash
/* Block comments are enclosed in /* */ */
```

### Literals

#### Integer Literals
- Range: `-2³¹` to `2³¹-1` (32-bit signed integers)
- Format: `42`, `-17`, `0`
- Overflow: Integer literals outside the valid range cause parse errors

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
`true`, `false`, `if`, `then`, `else`, `and`, `or`, `not`, `print`, `let`, `letrec`, `in`, `input`, `args`, `Int`, `Bool`, `String`, `Unit`, `parseInt`, `toString`, `show`, `Maybe`, `Either`, `Just`, `Nothing`, `Left`, `Right`, `case`, `of`, `head`, `tail`, `null`, `fst`, `snd`, `map`, `filter`, `foldl`, `length`, `reverse`, `take`, `drop`, `zip`, `split`, `join`, `trim`, `replace`, `strLength`, `readFile`, `writeFile`

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
- Equality works on all types with same type

### String Operations
- `++` (concatenation, right-associative)

### Expression Sequencing
- `;` (sequencing, right-associative) - evaluates first expression for side effects, returns second expression

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
- `Just x` - matches Maybe values
- `Nothing` - matches empty Maybe
- `Left x` - matches Either left values
- `Right x` - matches Either right values
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

### Lambda Expressions
```kai
\param -> body
\param : Type -> body
```

### Function Application
- Left-associative: `f x y` = `(f x) y`
- Higher precedence than infix operators

### Type Annotation Expressions
```kai
(expr : Type)
```

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

## Type System

### Type Inference
Kai uses Hindley-Milner type inference:
- Types are inferred without explicit annotations
- Type annotations are optional but checked when provided
- Polymorphic functions are supported

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
parseInt : String -> Maybe Int  // "42" -> Just 42, "abc" -> Nothing
toString : Int -> String        // 42 -> "42"
show : a -> String             // Any value to string representation
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

// Command-line arguments
args : [String]             // List of command-line arguments passed to script
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

**Example**:
```kai
let content = "Hello, world!" in
let _ = writeFile "output.txt" content in
let read = readFile "output.txt" in
print read
```

### Command-Line Arguments
- `args` evaluates to list of command-line arguments
- Arguments passed after script filename
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
- All type mismatches caught at compile time

### Runtime Errors
- Division by zero: `DivByZero`
- Unbound variable references: `UnboundVariable "var_name"`
- Type errors in runtime contexts: `TypeError "message"`

### Error Handling with Types
- **Maybe types**: `Just value | Nothing` for optional values
- **Either types**: `Left error | Right value` for error propagation
- **Case expressions**: Pattern matching for handling Maybe/Either gracefully
- **Safe conversion functions**: `parseInt : String -> Maybe Int` returns `Nothing` for invalid input

### Error Recovery
- Graceful error handling through pattern matching
- Programs can continue execution after handling errors appropriately
- Programs must be syntactically and type-correct to run

## Evaluation Model

### Evaluation Strategy
- **Strict evaluation**: Arguments evaluated before function application
- **Call-by-value**: Values passed to functions, not expressions

### Evaluation Order
- Left-to-right evaluation of function applications
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

- **Single-file scripts**: No module system or imports
- **Basic I/O**: No file operations, only stdin/stdout
- **No error recovery**: Single parse/type error stops execution
- **No REPL**: Command-line only execution
- **Limited standard library**: Only built-in conversion and list functions
- **No custom data types**: Only built-in lists, records, Maybe, Either
- **No polymorphic recursion**: Type inference limitations with complex recursive types
- **Wildcard limitations**: `_` only allowed in `let` bindings, not in `letrec` or pattern matching

## Grammar Summary

```bnf
Program ::= Expr

Expr ::= 'let' Ident (':' Type)? '=' Expr 'in' Expr
       | 'letrec' Ident (':' Type)? '=' Expr 'in' Expr
       | 'if' Expr 'then' Expr 'else' Expr
       | 'case' Expr 'of' Pattern '->' Expr ('|' Pattern '->' Expr)*
       | '\' Ident (':' Type)? '->' Expr
       | '(' Expr ':' Type ')'
       | SeqExpr

SeqExpr ::= SeqExpr ';' OrExpr | OrExpr
OrExpr ::= OrExpr 'or' AndExpr | AndExpr
AndExpr ::= AndExpr 'and' CmpExpr | CmpExpr
CmpExpr ::= AddExpr ('==' | '<' | '>') AddExpr | AddExpr
AddExpr ::= AddExpr ('+' | '-') ConsExpr | ConsExpr
ConsExpr ::= ConcatExpr ('::' ConsExpr)? | ConcatExpr
ConcatExpr ::= ConcatExpr '++' MulExpr | MulExpr
MulExpr ::= MulExpr ('*' | '/') UnaryExpr | UnaryExpr
UnaryExpr ::= ('not' | '-') UnaryExpr | AppExpr
AppExpr ::= AppExpr ('.' Ident | Atom) | Atom

Atom ::= Integer | Boolean | String | ListLit | RecordLit
       | '(' Expr ')' | Ident | '()' | 'input'
       | 'print' | 'parseInt' | 'toString' | 'show'
       | 'head' | 'tail' | 'null'
       | 'Just' | 'Nothing' | 'Left' | 'Right'

ListLit ::= '[' (Expr (',' Expr)*)? ']'
RecordLit ::= '{' (Ident '=' Expr (',' Ident '=' Expr)*)? '}'

Pattern ::= Integer | Boolean | String | '()' | Ident
          | 'Just' Pattern | 'Nothing'
          | 'Left' Pattern | 'Right' Pattern
          | '[' ']' | Pattern '::' Pattern
          | '{' (Ident '=' Pattern (',' Ident '=' Pattern)*)? '}'

Type ::= 'Int' | 'Bool' | 'String' | 'Unit'
       | '[' Type ']' | '{' (Ident ':' Type (',' Ident ':' Type)*)? '}'
       | 'Maybe' Type | 'Either' Type Type
       | Type '->' Type | '(' Type ')'

Ident ::= [a-zA-Z][a-zA-Z0-9_]* | '_'
Integer ::= [+-]?[0-9]+
Boolean ::= 'true' | 'false'
String ::= '"' StringChar* '"'
```

## Implementation Notes

- **Architecture**: Modular design with 28 focused submodules across Parser (7 modules), TypeChecker (12 modules), and Evaluator (13 modules)
- **Parser**: Megaparsec with operator precedence parsing across multiple specialized modules
- **Type Checker**: Algorithm W with unification, split across specialized inference modules
- **Evaluator**: Direct AST interpretation with closure environments, dual pure/IO evaluation paths
- **Performance**: Optimized for deeply nested expressions (1000+ levels), comprehensive benchmarking suite available
- **Benchmarks**: Criterion (speed) and Weigh (memory) profiling with regression detection

This specification documents Kai v0.0.4. Updates to language features should be reflected in this document immediately.