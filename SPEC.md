# Kai Language Specification (v0.0.5.0)

This document provides a comprehensive technical specification of the Kai programming language in its current state. It serves as the authoritative reference for language semantics, syntax, and behavior.

**Version**: 0.0.5.0
**Last Updated**: 2026-09-07

**Note**: Kai uses a modular architecture with focused Parser, TypeChecker, Evaluator, REPL, and module-loading components. Performance benchmarks are available via `stack bench`.

See [the 0.0.5.0 release design](RELEASE-0.0.5.0.md) for design decisions and
[the migration guide](MIGRATING-0.0.5.0.md) for changes from earlier versions.

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
`true`, `false`, `if`, `then`, `else`, `and`, `or`, `not`, `let`, `letrec`, `in`, `do`, `data`, `import`, `export`, `input`, `Int`, `Bool`, `String`, `Unit`, `Maybe`, `Either`, `Nothing`, `case`, `of`, `getCurrentDirectory`, `args`

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
- `Error`: Structured recoverable runtime failures
- `IOErrorKind`: Stable categories for operating-system and decoding failures

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
- The left operand evaluates first. `false and rhs` and `true or rhs` skip the right operand; the other cases evaluate it once. Both operands must type-check as `Bool`.

### Comparison Expressions
- `==`, `<`, `>` (non-associative)
- Equality is structural for integers, booleans, strings, unit, custom data,
  `Maybe`, `Either`, lists, records, and tuples. Different constructors of the
  same custom, `Maybe`, or `Either` type compare as `false`.
- Inference retains `Eq a` constraints and rejects functions, including nested
  callable payloads, before execution. Comparability uses every constructor
  payload of a custom type, including private constructors. Phantom parameters
  impose no requirement. Internal evaluator callers still receive a defensive
  runtime `TypeError` for callable or recursive reference values.

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

Record literals evaluate fields in source order. Duplicate field names in literals,
patterns, and annotations are static errors. Literals and `{a : Int}` annotations
are closed; an accessor such as `\r -> r.a` infers an open row and accepts extra
fields. `{a : Int | row}` explicitly names an open row. Required fields accumulate
across an expression; missing fields, conflicting payloads, infinite rows, and
repeated labels introduced through a shared row are rejected. Row variables are
scoped to one annotation and cannot also serve as value type variables.

```kai
let total = \record -> record.a + record.b in
total {a = 2, b = 3, extra = "ok"}  // => 5

let get : {a : Int | row} -> Int = \record -> record.a in
get {a = 7, other = true}  // => 7
```

Data declaration parameters have value kind. To store an open record in custom
data, use a value parameter such as `data Holder a = Hold a`; direct row-kinded
data parameters are not supported.

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
- `{field1 = pattern1 | rest}` - matches records with additional fields, binding the remaining record
- `(pattern1, pattern2, ...)` - matches tuples

A name may be bound only once within a pattern, including nested patterns.
Record patterns without `|` require exactly the listed fields. An open pattern
`{a = x | rest}` requires `a`, matches any additional fields, and binds those
additional fields as a record named `rest`; `{a = x | _}` ignores them. The rest
binding excludes every explicitly matched field and may be `{}`. `{| rest}`
matches any record and binds it whole. Rest bindings share the pattern's ordinary
duplicate-name and lexical-scope rules. Open patterns retain row polymorphism
through helpers, recursion, imports, and REPL definitions. Their nested payload
patterns must still be exhaustive, including cases with extra fields.

```kai
case {a = 1, b = true} of {a = value | rest} -> (value, rest)  // => (1, {b = true})
```

Constructor patterns must supply exactly the declared number of fields, otherwise
type checking reports `ConstructorPatternArity name expected actual`. Function-valued
constructor fields each count as one field. Repeated `_` is allowed. Duplicate
record pattern fields are rejected. These
fail with `DuplicatePatternBinding` or `DuplicateRecordField` during type checking.
Branches are tried in order. Matching a nested constructor with fields requires
parentheses, for example `Just (Box value)`. Every case must cover its scrutinee
type, including nested constructor payloads, tuple/record combinations, and list
lengths. Missing cases are static errors with a witness such as `Nothing`,
`Just false`, or `[]`. Integer and string literal alternatives need a catch-all.
When a constructor is private, the diagnostic uses `_` rather than its name;
a catch-all handles those hidden alternatives.

An alternative already covered by earlier branches produces a warning on stderr.
Warnings preserve first-match behavior and do not change the successful exit code.
The CLI and REPL report warnings from imported modules as well as local code.
The evaluator retains a defensive no-match error for internal callers that bypass
source checking.

A nested `case` consumes its own `|` alternatives. Parenthesize it before writing
another branch of the outer case:

```kai
case Just 1 of
  Just left -> (case Just 2 of Just right -> left + right | Nothing -> 0)
  | Nothing -> 0  // => 3
```

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

### Built-in Constraints

```text
Append a => a -> a -> a
Eq a => a -> a -> Bool
(Eq a, Append a) => a -> a -> Bool
```

`x ++ y` unifies both operands and retains `Append a` until the type is known.
Only strings and homogeneous lists support concatenation; list elements may be
functions. `x == y` retains `Eq a`. Primitive data are comparable; containers
require comparable stored payloads. Recursive custom types are analyzed without
expanding them indefinitely, and unused phantom parameters need no equality.

Requirements survive generalization, substitution, annotations, recursion,
module exports, and REPL definitions. Qualified annotations belong on whole
bindings or expressions, not inside constructor fields or lambda parameter types.
An unconstrained polymorphic annotation cannot hide an inferred requirement.
These two capabilities are built into Kai; users cannot define classes or instances.

At a closed execution entry point, variables used only in equality obligations
and absent from the environment and result may default to `Unit`. This permits
`[] == []` and `Nothing == Nothing`. Concrete callable types never default, and
returned/exported helpers retain their contexts. Unresolved `Append` obligations
have no default; an ambiguous entry point is rejected.

```kai
let append = \left -> \right -> left ++ right in
(append "a" "b", append [1] [2])  // => ("ab", [1, 2])

let same : Eq a => a -> a -> Bool = \left -> \right -> left == right in
same (Just 1) (Just 1)  // => true
```

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
- `DuplicateRecordField field`: A record literal, pattern, annotation, or row repeats a field
- `InvalidDataDeclaration message`: Invalid or conflicting data declaration
- `ConstructorPatternArity name expected actual`: Constructor pattern has the wrong number of fields
- `InvalidWildcard message`: Invalid use of `_`, such as a recursive binding
- `GeneralTypeError message`: Other program or module constraints, including duplicate names in a recursive block
- `UnsatisfiedConstraint predicate`: A concrete type does not support equality or concatenation
- `MissingConstraint predicate`: A polymorphic annotation omits a required context
- `AmbiguousConstraint predicate`: An execution entry point cannot resolve an obligation
- `NonExhaustivePatterns witness`: A case expression leaves a possible input uncovered
- `KindMismatch value row` / `ConflictingVariableKind name`: A row is used as a value type or vice versa

## Built-in Functions

Builtins support first-class and partial use, such as `let f = length in f [1,2]`
and `let f = take 2 in f [1,2,3]`. Supplied arguments evaluate immediately; missing
arguments become captured function parameters. Builtins use ordinary application:
`map length [[1],[2,3]]`. Local definitions may shadow callable builtin names.
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

### Recovery Functions

```text
attempt : (Unit -> a) -> Either Error a
raise : Error -> a
```

`attempt action` invokes `action ()` once and returns `Right result` or `Left error`.
The nearest active boundary catches a recoverable failure. Evaluating the action
argument happens before that boundary, and a returned function does not retain it.
`raise error` raises or rethrows an `Error` value. Handler failures propagate to
enclosing boundaries. Effects already performed are retained.

Error constructors are `DivisionByZero`, `ArithmeticOverflow`, `EmptyList String`,
`EndOfInput`, `UserError String`, and
`IOError IOErrorKind String (Maybe String) String`. I/O payloads are category,
operation, optional path, and host detail. Categories are `NotFound`,
`PermissionDenied`, `AlreadyExists`, `InvalidPath`, `InvalidEncoding`, `ResourceBusy`,
and `OtherIO`. Match categories rather than platform-specific detail text.

`exit`, cancellation, resource exhaustion, uninitialized recursive bindings,
interpreter invariant failures, parse/type errors, and invalid imported source
remain outside recovery. An unhandled failure stops a script. A handled failure
permits ordinary continuation and successful exit.

### List Functions
```text
// Basic operations
head : [a] -> a            // First element (runtime error if empty)
tail : [a] -> [a]          // List without first element (runtime error if empty)
headMaybe : [a] -> Maybe a  // Nothing for empty lists
tailMaybe : [a] -> Maybe [a] // Nothing for empty lists; Just [] for a singleton
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
Record displays separate fields with commas. Accessors infer open rows, while
literals and closed record annotations require exact field sets.

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
readLine : Unit -> Maybe String // Nothing at EOF, Just "" for a blank line

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
- `input` raises recoverable `EndOfInput` at EOF. `readLine ()` returns `Nothing` for EOF and preserves blank lines as `Just ""`.
- The host pure evaluator rejects console reads; `evalPure input` returns `TypeError "input not available in pure evaluation"`.

### Standard Output
- `print expr` evaluates expr, prints its value followed by a newline, flushes stdout, and returns `()`
- Output format matches value representation
- A write or flush failure raises an `IOError` for operation `print` and stops subsequent effects inside the action; an explicit `attempt` can handle it
- CLI output failures return a nonzero exit status. Diagnostics fall back to stderr when stdout is unavailable; failure status is preserved even if neither stream is writable.

### File I/O
- `readFile`, `writeFile`, and `appendFile` use UTF-8 with native newline handling, independent of the host locale. Invalid UTF-8 input raises an `IOError` with category `InvalidEncoding`.
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

Normal CLI and REPL type/runtime errors include file, line and column, an original
source excerpt, and a caret. Function failures retain their definition location
and call context; imported type failures include the import chain. Comments,
shebangs, blank lines and CRLF input preserve physical source positions.
`--debug` exposes structured internal errors; names listed below describe those
internal constructors, not the normal diagnostic text. Script expected errors
continue to compare the underlying constructor independently of source context.

### Parse Errors
- Invalid syntax causes immediate parse failure
- Error messages indicate location and expected tokens

### Type Errors
- Static type checking occurs before evaluation
- Type mismatches expressible by Kai's type system are rejected before evaluation
- Callable equality and unsupported concatenation operands are rejected statically

### Runtime Errors
- Division by zero: `DivByZero`
- Signed 32-bit arithmetic overflow: `IntegerOverflow`
- A recursive initializer reads a binding before it is ready: `UninitializedRecursion "name"`
- Unbound variable references: `UnboundVariable "var_name"`
- Missing record fields: `RecordFieldNotFound "field_name"`
- Type errors in runtime contexts: `TypeError "message"`
- No case branch matches: defensive `TypeError "No matching pattern in case expression"` for internal callers; public source is checked for completeness
- Empty list access: `EmptyListError operation`, exposed to recovery as `EmptyList operation`
- Stdin EOF: `EndOfInputError`, exposed as `EndOfInput`
- Host I/O failures: `IOFailure category operation path detail`, exposed as `IOError` values
- Application errors: `UserFailure message`, exposed as `UserError message`

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

1. **Field Access**: `.field`
2. **Function Application**: left-associative
3. **Prefix Operators**: `not`, unary `-`
4. **Multiplicative**: `*`, `/` (left-associative)
5. **Additive**: `+`, `-` (left-associative)
6. **Cons**: `::` (right-associative)
7. **Concatenation**: `++` (right-associative)
8. **Comparison**: `==`, `<`, `>` (non-associative)
9. **Logical AND**: `and` (right-associative)
10. **Logical OR**: `or` (right-associative)
11. **Sequencing**: `;` (right-associative, lowest precedence)

`f x.field` means `f (x.field)`, and `record.fn x` means `(record.fn) x`.
Use `(f x).field` to access a field of the function result.

```kai
let f = \r -> {a = r.a + 1} in (f {a = 1}).a  // => 2
let record = {fn = \n -> n + 1} in record.fn 3  // => 4
```

Prefix operators may repeat and apply from right to left: `not not true` is `true`,
and `- - 5` is `5`. Each negation checks signed 32-bit overflow.

## Language Limitations (Current)

- **Script failures**: Unhandled failures stop a script; `attempt` handles recoverable runtime failures and the REPL accepts subsequent input after language errors
- **Minimal REPL ergonomics**: No history, completion, or editor integration yet
- **Limited standard library depth**: Core file/process/env helpers exist, but line-oriented I/O, JSON/HTTP, and packaging are still missing
- **Polymorphic recursion requires explicit annotations**: Recursive calls in an unannotated group share one type; definitions may be generalized for later uses
- **Pattern matching**: Coverage is checked, but guards and as-patterns are not supported

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
AppExpr ::= AppExpr FieldExpr | FieldExpr
FieldExpr ::= Atom ('.' Ident)*

Atom ::= Integer | Boolean | String | ListLit | RecordLit | TupleLit
       | '(' Expr ')' | Ident | ConstructorIdent | '()' | 'input'
       | NullaryBuiltin

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
              | '{' (Ident '=' Pattern (',' Ident '=' Pattern)*)? '|' Ident '}'
              | '(' Pattern (',' Pattern)* ')'

Type ::= (ConstraintContext '=>')? FunctionType
FunctionType ::= TypeApplication ('->' FunctionType)?
ConstraintContext ::= Constraint | '(' Constraint (',' Constraint)* ')'
Constraint ::= ('Eq' | 'Append') TypeAtom
TypeApplication ::= ConstructorIdent TypeAtom* | TypeAtom
TypeAtom ::= 'Int' | 'Bool' | 'String' | 'Unit' | Ident | ConstructorIdent
           | '[' Type ']' | '{' (Ident ':' Type (',' Ident ':' Type)*)? ('|' Ident)? '}'
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

This specification defines Kai v0.0.5.0.
