# Kai Language Specification (v0.0.3.2)

This document provides a comprehensive technical specification of the Kai programming language in its current state. It serves as the authoritative reference for language semantics, syntax, and behavior.

**Version**: 0.0.3.2
**Last Updated**: 2025-01-24

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

### Reserved Keywords
`true`, `false`, `if`, `then`, `else`, `and`, `or`, `not`, `print`, `let`, `letrec`, `in`, `input`, `Int`, `Bool`, `String`, `Unit`, `parseInt`, `toString`, `show`

## Types

Kai has a static type system with the following base types:

### Base Types
- `Int`: 32-bit signed integers
- `Bool`: Boolean values (`true` or `false`)
- `String`: String values with escape sequence support
- `Unit`: Unit type with single value `()`

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

**Example**:
```kai
case parseInt "42" of Just x -> x | Nothing -> 0
```

### Variable Binding
```kai
let var = value in body
let var : Type = value in body
```

### Recursive Binding
```kai
letrec var = value in body
letrec var : Type = value in body
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
parseInt : String -> Int     // "42" -> 42
toString : Int -> String     // 42 -> "42"
show : a -> String          // Any value to string representation
```

### I/O Functions
```kai
print : a -> Unit           // Print value and return ()
input : String              // Read line from stdin
```

## I/O Operations

### Standard Input
- `input` reads a complete line from stdin
- Returns string value including any whitespace
- No prompt is displayed

### Standard Output
- `print expr` evaluates expr, prints its value, returns `()`
- Output format matches value representation

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
2. **Prefix Operators**: `not`, unary `-`
3. **Multiplicative**: `*`, `/` (left-associative)
4. **Additive**: `+`, `-` (left-associative)
5. **Concatenation**: `++` (right-associative)
6. **Comparison**: `==`, `<`, `>` (non-associative)
7. **Logical AND**: `and` (right-associative)
8. **Logical OR**: `or` (right-associative)

## Language Limitations (Current)

- **Single-file scripts**: No module system or imports
- **Limited data structures**: No lists, records, or algebraic data types
- **No pattern matching**: Only conditional expressions
- **Basic I/O**: No file operations, only stdin/stdout
- **No error recovery**: Single parse/type error stops execution
- **No REPL**: Command-line only execution
- **No standard library**: Only built-in conversion functions

## Grammar Summary

```bnf
Program ::= Expr

Expr ::= 'let' Ident (':' Type)? '=' Expr 'in' Expr
       | 'letrec' Ident (':' Type)? '=' Expr 'in' Expr
       | 'if' Expr 'then' Expr 'else' Expr
       | '\' Ident (':' Type)? '->' Expr
       | '(' Expr ':' Type ')'
       | OrExpr

OrExpr ::= OrExpr 'or' AndExpr | AndExpr
AndExpr ::= AndExpr 'and' CmpExpr | CmpExpr
CmpExpr ::= AddExpr ('==' | '<' | '>') AddExpr | AddExpr
AddExpr ::= AddExpr ('+' | '-') ConcatExpr | ConcatExpr
ConcatExpr ::= ConcatExpr '++' MulExpr | MulExpr
MulExpr ::= MulExpr ('*' | '/') UnaryExpr | UnaryExpr
UnaryExpr ::= ('not' | '-') UnaryExpr | AppExpr
AppExpr ::= AppExpr Atom | Atom

Atom ::= Integer | Boolean | String | '(' Expr ')'
       | Ident | '()' | 'input'
       | 'print' | 'parseInt' | 'toString' | 'show'

Type ::= 'Int' | 'Bool' | 'String' | 'Unit'
       | Type '->' Type | '(' Type ')'

Ident ::= [a-zA-Z][a-zA-Z0-9_]*
Integer ::= [+-]?[0-9]+
Boolean ::= 'true' | 'false'
String ::= '"' StringChar* '"'
```

## Implementation Notes

- **Parser**: Megaparsec with operator precedence parsing
- **Type Checker**: Algorithm W with unification
- **Evaluator**: Direct AST interpretation with closure environments
- **Performance**: Optimized for deeply nested expressions (1000+ levels)

This specification documents Kai v0.0.3.2. Updates to language features should be reflected in this document immediately.