# Kai Language Features

**Version**: 0.0.4
**Last Updated**: 2025-10-08

This document provides a comprehensive overview of all implemented and planned features for the Kai programming language.

---

## Implemented Features (v0.0.4)

### Core Language

#### Literals & Basic Types
- ✅ **Integer literals**: 32-bit signed integers (`-2³¹` to `2³¹-1`)
- ✅ **Boolean literals**: `true`, `false`
- ✅ **String literals**: Double-quoted with escape sequences (`\"`, `\\`, `\n`)
- ✅ **Unit literal**: `()` for side-effect operations

#### Operators
- ✅ **Arithmetic**: `+`, `-`, `*`, `/` (integer division)
- ✅ **Unary minus**: `-expr` (proper prefix operator)
- ✅ **Boolean logic**: `and`, `or` (right-associative), `not` (prefix)
- ✅ **Comparison**: `==`, `<`, `>` (non-associative)
- ✅ **String concatenation**: `++` (right-associative)
- ✅ **List concatenation**: `++` (right-associative)
- ✅ **Cons operator**: `::` (right-associative)
- ✅ **Expression sequencing**: `;` (right-associative, lowest precedence)

#### Control Flow
- ✅ **Conditionals**: `if cond then e1 else e2`
- ✅ **Pattern matching**: `case expr of pattern -> expr | pattern -> expr`

#### Functions
- ✅ **Lambda expressions**: `\x -> expr`
- ✅ **Function application**: Left-associative, higher precedence than operators
- ✅ **First-class functions**: Pass as arguments, return from functions, store in variables
- ✅ **Closures**: Lambda expressions capture lexical environment
- ✅ **Recursion**: Via `letrec` bindings

#### Variable Bindings
- ✅ **Let bindings**: `let x = value in body`
- ✅ **Recursive bindings**: `letrec f = value in body`
- ✅ **Wildcard variables**: `let _ = expr in body` to discard values
- ✅ **Type annotations**: Optional Haskell-style (`let x : Int = 42`, `\x : String -> expr`)

#### Data Structures
- ✅ **Lists**: `[1, 2, 3]`, homogeneous, with operations
- ✅ **Tuples**: `(1, "hello", true)`, heterogeneous, any number of elements
- ✅ **Records**: `{a = 1, b = true}` with field access (`record.field`)
- ✅ **Maybe type**: `Just value | Nothing` for optional values
- ✅ **Either type**: `Left error | Right value` for error propagation

#### Pattern Matching
- ✅ **Variable patterns**: `x`
- ✅ **Literal patterns**: `42`, `true`, `"hello"`, `()`
- ✅ **Maybe patterns**: `Just x`, `Nothing`
- ✅ **Either patterns**: `Left x`, `Right x`
- ✅ **List patterns**: `[]`, `x :: xs`
- ✅ **Tuple patterns**: `(x, y, z)`
- ✅ **Record patterns**: `{a = x, b = y}`

### Type System

- ✅ **Hindley-Milner type inference**: Full type inference without explicit annotations
- ✅ **Static type checking**: All types checked before evaluation
- ✅ **Unification**: With occurs check to prevent infinite types
- ✅ **Polymorphic functions**: Support for parametric polymorphism
- ✅ **Base types**: `Int`, `Bool`, `String`, `Unit`
- ✅ **Composite types**: `[T]`, `(T1, T2, ...)`, `{field: T}`
- ✅ **Function types**: `T1 -> T2` (right-associative)
- ✅ **Maybe types**: `Maybe T`
- ✅ **Either types**: `Either T U`

### Built-in Functions

#### Type Conversion (3)
- ✅ `parseInt : String -> Maybe Int` - Safe string to int conversion
- ✅ `toString : Int -> String` - Integer to string
- ✅ `show : a -> String` - Any value to string representation

#### List Operations (11)
- ✅ `head : [a] -> a` - First element (runtime error if empty)
- ✅ `tail : [a] -> [a]` - List without first element
- ✅ `null : [a] -> Bool` - Check if list is empty
- ✅ `length : [a] -> Int` - Number of elements
- ✅ `map : (a -> b) -> [a] -> [b]` - Apply function to each element
- ✅ `filter : (a -> Bool) -> [a] -> [a]` - Keep elements matching predicate
- ✅ `foldl : (b -> a -> b) -> b -> [a] -> b` - Left fold over list
- ✅ `reverse : [a] -> [a]` - Reverse list order
- ✅ `take : Int -> [a] -> [a]` - Take first n elements
- ✅ `drop : Int -> [a] -> [a]` - Drop first n elements
- ✅ `zip : [a] -> [b] -> [(a, b)]` - Combine two lists into list of tuples

#### String Operations (5)
- ✅ `split : String -> String -> [String]` - Split string by delimiter
- ✅ `join : String -> [String] -> String` - Join strings with delimiter
- ✅ `trim : String -> String` - Remove leading/trailing whitespace
- ✅ `replace : String -> String -> String -> String` - Find and replace
- ✅ `strLength : String -> Int` - Length of string

#### Tuple Operations (2)
- ✅ `fst : (a, b) -> a` - First element of pair
- ✅ `snd : (a, b) -> b` - Second element of pair

#### I/O Operations (5)
- ✅ `print : a -> Unit` - Print value and return unit
- ✅ `input : String` - Read line from stdin
- ✅ `readFile : String -> String` - Read entire file as string
- ✅ `writeFile : String -> String -> Unit` - Write string to file (overwrite)
- ✅ `args : [String]` - Command-line arguments passed to script

**Total Built-in Functions**: 26

### Parser & Syntax

- ✅ **Megaparsec-based parser**: With operator precedence and associativity
- ✅ **Line comments**: `// comment`
- ✅ **Block comments**: `/* comment */`
- ✅ **Multi-statement files**: Each line parsed as separate expression
- ✅ **Reserved keywords**: 45+ keywords properly recognized
- ✅ **Keyword boundary checking**: Prevents `trimmed` from parsing as `trim` + `med`
- ✅ **String escapes**: `\"`, `\\`, `\n` with helpful error messages for unknown escapes
- ✅ **Integer overflow detection**: Parse errors for values outside 32-bit range
- ✅ **Performance optimizations**: Handles deeply nested expressions (1000+ levels)

### Evaluator

- ✅ **Strict evaluation**: Call-by-value semantics
- ✅ **Lexical scoping**: Static binding with closure support
- ✅ **Environment-based evaluation**: Separate pure and IO evaluation modes
- ✅ **Error handling**: Graceful runtime errors with descriptive messages
- ✅ **Pure evaluation mode**: For testing without I/O side effects
- ✅ **IO evaluation mode**: For scripts with input/output/file operations

### CLI & Tooling

- ✅ **Command-line interface**: `kai` executable
- ✅ **Expression evaluation**: `kai -e "expr"`
- ✅ **File execution**: `kai script.kai [args...]`
- ✅ **Debug mode**: `kai --debug` for detailed output
- ✅ **Help system**: `kai --help`
- ✅ **Version display**: `Kai v0.0.4`
- ✅ **Script arguments**: Pass arguments to scripts
- ✅ **Clean output**: No debug noise by default
- ✅ **Install script**: `make install` to `~/.local/bin`
- ✅ **Runner script**: Lightweight wrapper for compiled binary

### Testing Infrastructure

- ✅ **435 unit tests**: Using Hspec
- ✅ **Property-based testing**: QuickCheck for algebraic laws
- ✅ **Script tests**: `.kai` files with `// expect:` directives
- ✅ **Stress tests**: Deeply nested expressions (1000+ levels)
- ✅ **Type checking tests**: All type inference scenarios
- ✅ **Parser tests**: Edge cases and error messages
- ✅ **0 HLint warnings**: Clean, idiomatic Haskell code

### Documentation

- ✅ **README.md**: Project overview, quickstart, examples, roadmap
- ✅ **SPEC.md**: Complete language specification
- ✅ **DEVELOPING.md**: Architecture, semantics, development workflow
- ✅ **AGENTS.md**: Testing guidelines for AI assistants
- ✅ **Website**: Yesod-based static site with examples
- ✅ **Working examples**: 8 practical demonstration scripts

---

## Planned Features

### Core Language (Planned)

#### Top Priority
- ⏳ **Top-level definitions**: Define functions/values at module level
- ⏳ **Module system**: Import/export across files
- ⏳ **Enhanced pattern matching**:
  - Tuple destructuring in case expressions
  - Guards in patterns (`case x of n | n > 0 -> ...`)
  - As-patterns (`case xs of all@(x:xs) -> ...`)
- ⏳ **Do-notation**: Clean syntax for I/O sequencing
- ⏳ **Match expressions**: Alternative to nested conditionals
- ⏳ **Integer patterns in case**: Currently only literals work

#### Data Structures
- ⏳ **Maps/Dictionaries**: Key-value data structure
- ⏳ **Sets**: Unique element collections
- ⏳ **Custom data types**: User-defined algebraic data types
- ⏳ **Type aliases**: `type String = [Char]`
- ⏳ **Newtype wrappers**: Zero-cost abstractions

#### Advanced Type System
- ⏳ **Type classes**: Ad-hoc polymorphism (Eq, Ord, Show, etc.)
- ⏳ **Polymorphic recursion**: Better support for complex recursive types
- ⏳ **Row polymorphism**: For extensible records
- ⏳ **GADTs**: Generalized algebraic data types
- ⏳ **Rank-N types**: Higher-rank polymorphism

#### Language Features
- ⏳ **List comprehensions**: `[x * 2 | x <- [1..10], x > 5]`
- ⏳ **Range syntax**: `[1..10]`, `[1,3..10]`
- ⏳ **Lambda case**: `\case Just x -> x; Nothing -> 0`
- ⏳ **Section syntax**: `(+1)`, `(2*)` for partial application
- ⏳ **Operator sections**: More ergonomic partial application
- ⏳ **Where clauses**: Alternative to let for local bindings
- ⏳ **Multi-way if**: `if | cond1 -> e1 | cond2 -> e2 | otherwise -> e3`

### Standard Library (Planned)

#### List Functions
- ⏳ `foldr : (a -> b -> b) -> b -> [a] -> b` - Right fold
- ⏳ `concat : [[a]] -> [a]` - Flatten list of lists
- ⏳ `concatMap : (a -> [b]) -> [a] -> [b]` - Map and flatten
- ⏳ `elem : a -> [a] -> Bool` - Check if element in list
- ⏳ `notElem : a -> [a] -> Bool` - Check if element not in list
- ⏳ `find : (a -> Bool) -> [a] -> Maybe a` - Find first matching element
- ⏳ `any : (a -> Bool) -> [a] -> Bool` - Check if any element matches
- ⏳ `all : (a -> Bool) -> [a] -> Bool` - Check if all elements match
- ⏳ `sort : [a] -> [a]` - Sort list
- ⏳ `sortBy : (a -> a -> Ordering) -> [a] -> [a]` - Sort with custom comparator
- ⏳ `group : [a] -> [[a]]` - Group consecutive equal elements
- ⏳ `nub : [a] -> [a]` - Remove duplicates
- ⏳ `intersperse : a -> [a] -> [a]` - Insert element between list elements
- ⏳ `intercalate : [a] -> [[a]] -> [a]` - Insert list between lists
- ⏳ `transpose : [[a]] -> [[a]]` - Transpose matrix
- ⏳ `partition : (a -> Bool) -> [a] -> ([a], [a])` - Split by predicate
- ⏳ `span : (a -> Bool) -> [a] -> ([a], [a])` - Split at first non-matching
- ⏳ `break : (a -> Bool) -> [a] -> ([a], [a])` - Split at first matching
- ⏳ `dropWhile : (a -> Bool) -> [a] -> [a]` - Drop while predicate holds
- ⏳ `takeWhile : (a -> Bool) -> [a] -> [a]` - Take while predicate holds
- ⏳ `zipWith : (a -> b -> c) -> [a] -> [b] -> [c]` - Zip with custom function
- ⏳ `unzip : [(a, b)] -> ([a], [b])` - Opposite of zip

#### Math Functions
- ⏳ `abs : Int -> Int` - Absolute value
- ⏳ `min : Int -> Int -> Int` - Minimum of two values
- ⏳ `max : Int -> Int -> Int` - Maximum of two values
- ⏳ `sqrt : Int -> Int` - Square root (integer)
- ⏳ `pow : Int -> Int -> Int` - Exponentiation
- ⏳ `mod : Int -> Int -> Int` - Modulo operation
- ⏳ `gcd : Int -> Int -> Int` - Greatest common divisor
- ⏳ `lcm : Int -> Int -> Int` - Least common multiple
- ⏳ `even : Int -> Bool` - Check if even
- ⏳ `odd : Int -> Bool` - Check if odd

#### String Functions
- ⏳ `lines : String -> [String]` - Split by newlines
- ⏳ `unlines : [String] -> String` - Join with newlines
- ⏳ `words : String -> [String]` - Split by whitespace
- ⏳ `unwords : [String] -> String` - Join with spaces
- ⏳ `toUpper : String -> String` - Convert to uppercase
- ⏳ `toLower : String -> String` - Convert to lowercase
- ⏳ `reverse : String -> String` - Reverse string
- ⏳ `isPrefixOf : String -> String -> Bool` - Check prefix
- ⏳ `isSuffixOf : String -> String -> Bool` - Check suffix
- ⏳ `isInfixOf : String -> String -> Bool` - Check substring
- ⏳ `stripPrefix : String -> String -> Maybe String` - Remove prefix
- ⏳ `stripSuffix : String -> String -> Maybe String` - Remove suffix

#### Function Composition
- ⏳ `(.) : (b -> c) -> (a -> b) -> (a -> c)` - Function composition
- ⏳ `($) : (a -> b) -> a -> b` - Function application (low precedence)
- ⏳ `(&) : a -> (a -> b) -> b` - Reverse application
- ⏳ `(|>) : a -> (a -> b) -> b` - Pipeline operator (F#-style)
- ⏳ `flip : (a -> b -> c) -> (b -> a -> c)` - Flip argument order
- ⏳ `const : a -> b -> a` - Constant function
- ⏳ `id : a -> a` - Identity function
- ⏳ `curry : ((a, b) -> c) -> (a -> b -> c)` - Curry function
- ⏳ `uncurry : (a -> b -> c) -> ((a, b) -> c)` - Uncurry function

#### Tuple Functions
- ⏳ `swap : (a, b) -> (b, a)` - Swap tuple elements
- ⏳ `curry3 : ((a, b, c) -> d) -> (a -> b -> c -> d)` - Curry 3-tuple
- ⏳ `uncurry3 : (a -> b -> c -> d) -> ((a, b, c) -> d)` - Uncurry to 3-tuple

### I/O and Effects (Planned)

#### File Operations
- ⏳ `appendFile : String -> String -> Unit` - Append to file
- ⏳ `deleteFile : String -> Unit` - Delete file
- ⏳ `renameFile : String -> String -> Unit` - Rename/move file
- ⏳ `fileExists : String -> Bool` - Check if file exists
- ⏳ `readLines : String -> [String]` - Read file as lines
- ⏳ `writeLines : String -> [String] -> Unit` - Write lines to file
- ⏳ `readBytes : String -> [Int]` - Read file as bytes
- ⏳ `writeBytes : String -> [Int] -> Unit` - Write bytes to file

#### Directory Operations
- ⏳ `listDirectory : String -> [String]` - List files in directory
- ⏳ `createDirectory : String -> Unit` - Create directory
- ⏳ `removeDirectory : String -> Unit` - Remove directory
- ⏳ `directoryExists : String -> Bool` - Check if directory exists
- ⏳ `getCurrentDirectory : Unit -> String` - Get current working directory
- ⏳ `setCurrentDirectory : String -> Unit` - Change working directory

#### Process & System
- ⏳ `system : String -> Int` - Run shell command, return exit code
- ⏳ `getEnv : String -> Maybe String` - Get environment variable
- ⏳ `setEnv : String -> String -> Unit` - Set environment variable
- ⏳ `getArgs : [String]` - Get command-line arguments (already `args`)
- ⏳ `exit : Int -> a` - Exit with code
- ⏳ `exitSuccess : a` - Exit successfully
- ⏳ `exitFailure : a` - Exit with failure

#### Network (Future)
- ⏳ `httpGet : String -> String` - Simple HTTP GET request
- ⏳ `httpPost : String -> String -> String` - Simple HTTP POST
- ⏳ JSON parsing/generation functions

#### Mutable References (Controlled)
- ⏳ `ref : a -> Ref a` - Create mutable reference
- ⏳ `readRef : Ref a -> a` - Read reference
- ⏳ `writeRef : Ref a -> a -> Unit` - Write to reference
- ⏳ `modifyRef : Ref a -> (a -> a) -> Unit` - Modify reference

### Scripting Conveniences (Planned)

#### REPL
- ⏳ **Interactive REPL**: Read-eval-print loop
- ⏳ **Multiline input**: Support for multi-line expressions
- ⏳ **:type command**: Query type of expression
- ⏳ **:load command**: Load files into REPL
- ⏳ **:reload command**: Reload current file
- ⏳ **:browse command**: Browse module contents
- ⏳ **History**: Command history with up/down arrows
- ⏳ **Tab completion**: For identifiers and keywords

#### Error Messages
- ⏳ **Better parse errors**: With suggestions for fixes
- ⏳ **Better type errors**: More helpful messages
- ⏳ **Error context**: Show relevant code snippets
- ⏳ **Did-you-mean suggestions**: For typos
- ⏳ **Type hole support**: `_` in expressions for type inference hints

#### Tooling
- ⏳ **Formatter**: Automatic code formatting (`kai fmt`)
- ⏳ **Linter**: Style suggestions (`kai lint`)
- ⏳ **Shebang support**: `#!/usr/bin/env kai` for executable scripts
- ⏳ **Language server**: LSP for IDE support
- ⏳ **Package manager**: Dependency management
- ⏳ **Documentation generator**: Generate docs from code
- ⏳ **Test runner**: Built-in test framework
- ⏳ **Benchmark framework**: Performance testing

### Performance & Optimization (Planned)

#### Performance Features
- ⏳ **Tail call optimization**: For recursive functions
- ⏳ **Strictness annotations**: Control evaluation strategy
- ⏳ **Lazy evaluation**: Optional lazy evaluation
- ⏳ **Mutable arrays**: For performance-critical code
- ⏳ **Imperative loops**: `for`, `while` when needed
- ⏳ **Bytecode compilation**: Faster than AST interpretation
- ⏳ **JIT compilation**: Further performance gains

#### Compiler Optimizations
- ⏳ **Constant folding**: Evaluate constants at compile time
- ⏳ **Dead code elimination**: Remove unused code
- ⏳ **Inline expansion**: Inline small functions
- ⏳ **Common subexpression elimination**: Reduce redundant computation

---

## Current Limitations

### Language Limitations
- ❌ **Single-file scripts only**: No module system or imports
- ❌ **No REPL**: Command-line execution only
- ❌ **No custom data types**: Only built-in types available
- ❌ **Limited pattern matching**: No guards, no as-patterns
- ❌ **Wildcard restrictions**: `_` only in `let` bindings, not in `letrec` or all patterns
- ❌ **No polymorphic recursion**: Type inference limitations
- ❌ **No error recovery**: One parse/type error stops execution
- ❌ **Integer-only arithmetic**: No floating-point numbers
- ❌ **Limited escape sequences**: Only `\"`, `\\`, `\n` supported
- ❌ **No regex support**: String operations are basic

### I/O Limitations
- ❌ **Basic file I/O**: No append, directory operations
- ❌ **No network operations**: No HTTP, sockets, etc.
- ❌ **No process control**: Can't spawn processes or run commands
- ❌ **No concurrent I/O**: Single-threaded only

### Standard Library Limitations
- ❌ **Limited list functions**: Missing `foldr`, `concat`, sorting, etc.
- ❌ **No math functions**: Missing `abs`, `min`, `max`, `sqrt`, etc.
- ❌ **No advanced string operations**: Missing case conversion, etc.
- ❌ **No time/date functions**: No datetime support
- ❌ **No random numbers**: No RNG support

### Tooling Limitations
- ❌ **No package manager**: Can't manage dependencies
- ❌ **No IDE support**: No language server protocol
- ❌ **No formatter**: Manual code formatting only
- ❌ **No linter**: Only HLint for Haskell implementation
- ❌ **No debugger**: Print-based debugging only
- ❌ **No profiler**: Can't measure performance

---

## Implementation Statistics

- **Lines of Haskell**: ~3,500 (estimated)
- **Test Coverage**: 435 tests, 100% passing
- **HLint Warnings**: 0
- **Core Types**: 8 (Int, Bool, String, Unit, List, Tuple, Record, Function)
- **Built-in Functions**: 26
- **Reserved Keywords**: 45+
- **Operator Precedence Levels**: 11
- **Example Scripts**: 8 working examples
- **Documentation**: 5 comprehensive markdown files

---

## Version History

### v0.0.4 (Current - 2025-10-08)
- Added tuples with pattern matching
- Added 8 list functions (map, filter, foldl, length, reverse, take, drop, zip)
- Added 5 string functions (split, join, trim, replace, strLength)
- Added file I/O (readFile, writeFile)
- Added command-line arguments (args)
- Fixed keyword parsing bug
- Increased tests from 352 to 435

### v0.0.3.3 (2025-10-05)
- Added wildcard variables (`_`)
- Added expression sequencing (`;`)
- Fixed infinite recursion bug with IO operations
- Clean CLI output by default

### v0.0.3.2
- Added type annotations
- Added Maybe/Either types
- Added pattern matching (case expressions)
- Added safe conversion functions

### v0.0.3.1
- Added let/letrec bindings
- Added recursion support

### v0.0.3.0
- Initial type system implementation
- Hindley-Milner type inference

---

## Design Philosophy

Kai is designed to be a **functional-first scripting language** with the following priorities:

1. **Functional by default**: Immutable data, pure functions, expressions over statements
2. **Imperative when needed**: Escape hatches for I/O, performance, when clearer
3. **Static first**: Strong, predictable types with inference
4. **Scriptable**: Fast edit-run cycle, no compilation step
5. **Practical**: Real-world scripting tasks with good ergonomics

---

## Contributing

Features marked with ⏳ are planned but not implemented. Contributions welcome!

See DEVELOPING.md for implementation guidelines and architecture details.
