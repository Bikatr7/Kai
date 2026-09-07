# Kai Language Specification (v0.0.4.6)

This document provides a comprehensive technical specification of the Kai programming language in its current state. It serves as the authoritative reference for language semantics, syntax, and behavior.

**Version**: 0.0.4.6
**Last Updated**: 2026-09-06

**Note**: Kai uses a modular architecture with focused Parser, TypeChecker, Evaluator, REPL, and module-loading components. Performance benchmarks are available via `stack bench`.

## Table of Contents

- [Overview](#overview)
- [Lexical Structure](#lexical-structure)
- [Types](#types)
- [Expressions](#expressions)
- [Top-Level Programs](#top-level-programs)
- [Functions](#functions)
- [Type System](#type-system)
- [Built-in Functions](#built-in-functions)
- [I/O Operations](#io-operations)
- [Error Handling](#error-handling)
- [Evaluation Model](#evaluation-model)
- [Operator Precedence](#operator-precedence)
- [Language Limitations](#language-limitations-current)
- [Grammar Summary](#grammar-summary)
- [Implementation Notes](#implementation-notes)

## Overview

Kai is a functional-first scripting language with static typing, implemented in Haskell. The language features:

- **Evaluation**: Strict (call-by-value) evaluation
- **Type System**: Unification-based type inference with occurs check, generalized let-polymorphism, and explicitly annotated polymorphic recursion
- **Recursion**: `letrec` plus a typed `fix : (a -> a) -> a` combinator
- **Paradigm**: Expression-oriented with immutable data by default
- **Data Modeling**: Built-in lists/records/tuples/Maybe/Either plus user-defined algebraic data types
- **Interactive Workflow**: CLI file/expression execution plus a REPL with multiline input and `:type`, `:load`, `:reload`, `:quit`
- **File Format**: UTF-8 scripts with `.kai` extension, or UTF-8 modules with imports; top-level newlines split expressions only outside nested forms

## Lexical Structure

### Comments

```kai
// Line comments start with double slash
/* Block comments are enclosed in /* */ */
```

- Block comments may nest. An unterminated comment is a parse error.

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
- Start with a letter, followed by letters, digits, or underscores; Unicode letters are supported
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
\x : String -> strLength x
let add : Int -> Int -> Int = \x -> \y -> x + y
```

## Expressions

Expressions evaluate to values. Program files can also contain top-level
definitions, data declarations, imports, and exports.

Type variables are scoped to one annotation. Repeated occurrences inside that
annotation refer to the same type; variables in separate annotations are independent.
Names such as `a` and `t0` have the same meaning.

Function types in lambda parameter annotations need parentheses, for example
`\f : (Int -> Int) -> f 42`. Tuple annotations include `(Int, Bool)`; `()` is also a unit type annotation.

### Arithmetic Expressions
- `+`, `-`, `*`, `/` (integer division)
- Unary minus: `-expr`
- Spacing does not change infix arithmetic: `7-2`, `7 -2`, and `7 - 2` all mean subtraction. Parenthesize signed arguments to ordinary functions: `f (-2)`.
- Division by zero causes runtime error

### Boolean Expressions
- `and`, `or` (right-associative)
- `not` (prefix)
- Both operands of `and` and `or` evaluate from left to right; neither operator short-circuits. Use `if` to avoid evaluating an unselected branch.

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

Record literals evaluate fields in source order. If a literal repeats a field,
the last value is retained. Record patterns and type annotations reject duplicate
field names instead.

### Tuple Operations
- `(val1, val2, ...)` - tuple literals (2 or more elements)
- `fst tuple` - first element of a 2-tuple
- `snd tuple` - second element of a 2-tuple
- `==` - structural equality

**Note**: Empty tuples `()` are the Unit value, and single-element tuples like `(x)` are just parenthesized expressions.

### Conditional Expressions
```text
if condition then expr1 else expr2
```

### Case Expressions (Pattern Matching)
```text
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
- `[p1, p2, ...]` - matches a list of exactly that length
- `x :: xs` - matches non-empty list (head and tail)
- `{field1 = pattern1, field2 = pattern2, ...}` - matches records
- `(pattern1, pattern2, ...)` - matches tuples

A name may be bound only once within a pattern, including nested patterns.
Constructor patterns must supply exactly the declared number of fields, otherwise
type checking reports `ConstructorPatternArity name expected actual`. Function-valued
constructor fields each count as one field. Repeated `_` is allowed. Duplicate
record pattern fields are rejected. These
fail with `DuplicatePatternBinding` or `DuplicateRecordField` during type checking.
Branches are tried in order. Matching a nested constructor with fields requires
parentheses, for example `Just (Box value)`. A case with no matching branch returns
`TypeError "No matching pattern in case expression"`; exhaustiveness is not checked
statically.

**Example**:
```kai
case parseInt "42" of Just x -> x | Nothing -> 0
```

### Variable Binding
```text
let var = value in body
let var : Type = value in body
let _ = value in body    // Wildcard binding (discards value)
```

### Recursive Binding
```text
letrec var = value in body
letrec var : Type = value in body
```

Initializers run strictly. Constants are allowed, but reading a recursive binding
before initialization returns `UninitializedRecursion name`. Consecutive top-level
`letrec` definitions allocate references together, then initialize in source order,
stopping at the first error. Closures may refer to later bindings in that block.
The pure evaluator preserves the same initializer errors.

**Note**: Wildcards (`_`) are not allowed in `letrec` bindings as they cannot be meaningfully recursive.

### Expression Sequencing
```text
expr1; expr2         // Evaluate expr1, discard result, return expr2
expr1; expr2; expr3  // Right-associative: expr1; (expr2; expr3)
```

### Do Blocks
```kai
do {}                        // ()
do { print "start"; 42 }     // Prints then returns 42
do { print "first"; print "second"; 42; }  // Optional trailing semicolon
```

### Lambda Expressions
```text
\param -> body
\param : Type -> body
```

### Function Application
- Left-associative: `f x y` = `(f x) y`
- Higher precedence than infix operators
- Lambdas, builtins, and non-nullary data constructors are callable values. Constructors
  can be partially applied and used by higher-order operations such as `map`
  and `foldl`.

### Type Annotation Expressions
```text
(expr : Type)
```

## Top-Level Programs

Kai program files may contain top-level definitions, data declarations, imports, exports, and expressions. The whole program is type-checked before evaluation. Top-level items execute in source order, including imported module expressions. The result is the last expression if it is the final item, otherwise unit. A runtime failure stops subsequent effects. REPL loads use the same rules.

### Top-Level Definitions
```text
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

Type names and constructor names are each global across the import graph. Duplicate declarations in one scope, unknown referenced types, wrong
type arity, undeclared type parameters, and incompatible imported declarations
are rejected with `InvalidDataDeclaration`. Declaration metadata survives exports,
so hiding a constructor cannot hide a conflicting type. Imported declarations are compatible when their parameter positions and constructor
payloads match; parameter spelling does not matter. Distinct module-qualified type identities are not
implemented. Private constructors remain unavailable to pattern matching. Binding a variable
with the same name does not expose a private constructor, and its name remains
reserved against conflicting declarations.

Each constructor becomes a value in scope:
- Nullary constructors behave like values
- Constructors with arguments behave like functions and can be partially applied

### Modules
```text
import ModuleName
export value, helper, Constructor
```

For `import ModuleName`, paths are tried in this order relative to the importing
file's directory:

1. `ModuleName.kai`
2. `ModuleName/ModuleName.kai`
3. `examples/ModuleName.kai`
4. `examples/ModuleName/ModuleName.kai`

Interactive imports use the directory captured at REPL startup or the directory
of the most recently loaded file.

Without an export statement, the module exposes all of its definitions. Explicit
exports restrict imported values and constructor visibility. Imports merge values
into the importing scope; later local definitions can shadow imported values.
Circular imports fail with an error that includes the module loading stack.

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
- Recursive calls within an unannotated group share one monomorphic type. After inference, the completed definitions are generalized for later uses.

### Unification
- Occurs check prevents infinite types
- Type variables are unified across expressions
- Substitutions are composed efficiently

### Type Errors
- `UnificationError T1 T2`: Types cannot be unified, including invalid operands, conditions, and function applications
- `UnboundVariable x`: Variable not in scope
- `InfiniteType x T`: Occurs check failure
- `RecordFieldMismatch field`: Required record field is missing
- `DuplicatePatternBinding name`: A pattern binds the same name more than once
- `DuplicateRecordField field`: A record pattern or type annotation repeats a field
- `InvalidDataDeclaration message`: Invalid or conflicting data declaration
- `ConstructorPatternArity name expected actual`: Constructor pattern has the wrong number of fields
- `InvalidWildcard message`: Invalid use of `_`, such as a recursive binding
- `GeneralTypeError message`: Other program or module constraints, including duplicate names in a recursive block

## Built-in Functions

Builtins support first-class and partial use, such as `let f = length in f [1,2]`
and `let f = take 2 in f [1,2,3]`. Supplied arguments evaluate immediately; missing
arguments become lambda parameters. Builtin syntax consumes atom arguments, so
parenthesize a builtin used as an argument: `map (length) [[1],[2,3]]`.
`input`, `args`, `Nothing`, and `getCurrentDirectory` are values, not functions.


### Type Conversion Functions
```text
parseInt : String -> Maybe Int  // "42" -> Just 42; invalid or out-of-range -> Nothing
toString : Int -> String        // 42 -> "42"
show : a -> String             // Any value to string representation
discard : a -> Unit             // Evaluates and discards any value, returns ()
```

### Recursion Function
```text
fix : (a -> a) -> a             // Typed fixed-point combinator
```

### List Functions
```text
// Basic operations
head : [a] -> a            // First element (runtime error if empty)
tail : [a] -> [a]          // List without first element (runtime error if empty)
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
```text
split : String -> String -> [String]       // Split string by delimiter
join : String -> [String] -> String        // Join strings with delimiter
trim : String -> String                     // Remove leading/trailing whitespace
replace : String -> String -> String -> String  // replace pattern replacement string
strLength : String -> Int                   // Length of string
```

`split "" "ab"` yields `["", "a", "b"]`, including a leading empty element.
`show` and `print` produce readable displays, not a serialization format; strings
inside containers are unquoted and different values can have the same display.
Record displays separate fields with commas. Record inference requires exact
field sets and does not support row polymorphism.

### Tuple Functions
```text
fst : (a, b) -> a          // First element of pair
snd : (a, b) -> b          // Second element of pair
```

### I/O Functions
```text
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
- Console input/output use the host stream encoding.
- `input` reads a complete line from stdin
- Returns the line without its terminating newline, preserving other whitespace
- No prompt is displayed
- EOF returns a runtime `TypeError`; `evalPure input` returns `TypeError "input not available in pure evaluation"`

### Standard Output
- `print expr` evaluates expr, prints its value followed by a newline, flushes stdout, and returns `()`
- Output format matches value representation
- A write or flush failure returns `TypeError "print: could not write to stdout"` and stops subsequent effects
- CLI output failures return a nonzero exit status. Diagnostics fall back to stderr when stdout is unavailable; failure status is preserved even if neither stream is writable.

### File I/O
- `readFile`, `writeFile`, and `appendFile` use UTF-8 with native newline handling, independent of the host locale. Invalid UTF-8 input returns a runtime `TypeError`.
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
- `kai --version` and `kai -V` print the package-derived version and exit successfully
- File and `-e` execution do not print the final value automatically; use `print`. The REPL displays expression results, and `--debug` displays evaluation details.
- A version-looking token after a script filename remains a script argument (`kai script.kai --version`)
- `kai --check FILE.kai` verifies `// expect:` against the evaluated result and also checks an optional `// expect-type:`; failure exits nonzero. Expected values are pure Kai expressions, expected runtime errors use `error` followed by the exact error rendering, and expected types use the internal rendering such as `TInt` or `TList TString`. It reads stdin normally. `// stdin:` and `// stdout:` are test-harness directives; the CLI does not supply or compare those fixtures itself. See DEVELOPING.md for fixture conventions.
- Script, import, and REPL source reads use UTF-8 and catch decoding and IO errors. Failed REPL loads report an error and leave the prior session available.
- `kai --help` and `kai -h` print command usage; a leading `--debug` enables diagnostic output for the selected command
- The optional shell runner honors `KAI_BIN` first, then searches `PATH` while skipping runner copies, then uses the checkout's active Stack build. Without Stack, it uses the newest local build. Arguments and exit status pass through unchanged.

**Example**:
```bash
$ kai script.kai foo bar baz
```

```kai
let firstArg = head args in  // "foo"
let numArgs = length args in  // 3
do { print firstArg; print numArgs; print (show args) }
// Prints foo, then 3, then [foo, bar, baz], each on its own line
```

The REPL also accepts `:help` and `:q` (an alias for `:quit`). `:load FILE`
resets definitions on a successful load; `:reload` rereads that file. Failed loads
retain the prior session definitions, although effects already performed before
a runtime failure are not rolled back. A trailing `|` keeps a multiline data
declaration or case expression open for another alternative.

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
- A recursive initializer reads a binding before it is ready: `UninitializedRecursion "name"`
- Unbound variable references: `UnboundVariable "var_name"`
- Missing record fields: `RecordFieldNotFound "field_name"`
- Type errors in runtime contexts: `TypeError "message"`
- No case branch matches: `TypeError "No matching pattern in case expression"`
- Host I/O failures, including stdin EOF and invalid file/environment operations, are converted to `TypeError` values rather than escaping as Haskell exceptions

`exit code` is represented internally as `ExitRequested code`. The CLI and REPL
turn it into an exit status (`0` is success); it stops the program rather than
returning an ordinary value.

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

1. **Function Application and Field Access**: application and `.field` form one left-associated chain
2. **Prefix Operators**: `not`, unary `-`
3. **Multiplicative**: `*`, `/` (left-associative)
4. **Additive**: `+`, `-` (left-associative)
5. **Cons**: `::` (right-associative)
6. **Concatenation**: `++` (right-associative)
7. **Comparison**: `==`, `<`, `>` (non-associative)
8. **Logical AND**: `and` (right-associative)
9. **Logical OR**: `or` (right-associative)
10. **Sequencing**: `;` (right-associative, lowest precedence)

`f x.field` means `(f x).field`, and `record.fn x` means `(record.fn) x`.
Use `f (x.field)` to pass a field value as an argument.

```kai
let f = \r -> {a = r.a + 1} in f {a = 1}.a  // => 2
let record = {fn = \n -> n + 1} in record.fn 3  // => 4
```

Prefix operators may repeat and apply from right to left: `not not true` is `true`,
and `- - 5` is `5`. Each negation checks signed 32-bit overflow.

## Language Limitations (Current)

- **Script failures**: A parse, type, runtime, or output error stops a script; the REPL accepts subsequent input after language errors
- **Minimal REPL ergonomics**: No history, completion, or editor integration yet
- **Limited standard library depth**: Core file/process/env helpers exist, but line-oriented I/O, JSON/HTTP, and packaging are still missing
- **Polymorphic recursion requires explicit annotations**: Recursive calls in an unannotated group share one type; definitions may be generalized for later uses
- **Pattern matching**: No exhaustiveness checking, guards, or as-patterns

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
       | '\' Ident (':' TypeApplication)? '->' Expr
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
       | UnaryBuiltin Atom? | BinaryBuiltin Atom? Atom?
       | TernaryBuiltin Atom? Atom? Atom? | NullaryBuiltin

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

Pattern ::= PatternTerm ('::' Pattern)?
PatternTerm ::= ConstructorIdent PatternAtom* | PatternAtom

PatternAtom ::= Integer | Boolean | String | '()' | Ident | ConstructorIdent
              | 'Just' PatternAtom | 'Nothing'
              | 'Left' PatternAtom | 'Right' PatternAtom
              | '[' (Pattern (',' Pattern)*)? ']'
              | '{' (Ident '=' Pattern (',' Ident '=' Pattern)*)? '}'
              | '(' Pattern (',' Pattern)* ')'

Type ::= TypeApplication ('->' Type)?
TypeApplication ::= ConstructorIdent TypeAtom* | TypeAtom
TypeAtom ::= 'Int' | 'Bool' | 'String' | 'Unit' | Ident | ConstructorIdent
           | '[' Type ']' | '{' (Ident ':' Type (',' Ident ':' Type)*)? '}'
           | 'Maybe' TypeAtom | 'Either' TypeAtom TypeAtom | '(' Type ')'
           | '(' ')' | '(' Type ',' Type (',' Type)* ')'

Ident ::= [a-zA-Z][a-zA-Z0-9_]* | '_'
ConstructorIdent ::= [A-Z][a-zA-Z0-9_]*
Integer ::= [+-]?[0-9]+
Boolean ::= 'true' | 'false'
String ::= '"' StringChar* '"'
```

The grammar summarizes common forms. Identifiers also accept Unicode letters;
type-variable names begin with a lowercase letter and constructor names with an
uppercase letter. General type application is limited to custom names; `Maybe`
and `Either` have the fixed arities shown above. Parenthesize applied custom types
used as constructor fields, such as `data Holder a = Hold (Tree a)`.
Function application does not consume an unparenthesized signed argument: use
`f (-1)` or `f (+1)`.

## Implementation Notes

- **Architecture**: Modular parser, type-checker, evaluator, module, CLI, and REPL components
- **Parser**: Megaparsec with operator precedence parsing across multiple specialized modules
- **Type Checker**: Algorithm W with unification, split across specialized inference modules
- **Evaluator**: Direct AST interpretation with closure environments, dual pure/IO evaluation paths
- **Performance**: Optimized for deeply nested expressions (1000+ levels), comprehensive benchmarking suite available
- **Benchmarks**: Criterion timings and Weigh allocation measurements; CI runs one iteration, while regression comparisons require separate before/after measurements

This specification defines Kai v0.0.4.6.
