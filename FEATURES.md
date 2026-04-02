# Kai Language Features

**Version**: 0.0.4.3
**Last Updated**: 2026-04-01

This document provides a comprehensive overview of all implemented and planned features for the Kai programming language.

---

## Implemented Features (v0.0.4.3)

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
- ✅ **Do blocks**: `do { expr1; expr2; expr3 }` for readable sequencing, with `do {}` evaluating to `()`

#### Functions
- ✅ **Lambda expressions**: `\x -> expr`
- ✅ **Function application**: Left-associative, higher precedence than operators
- ✅ **First-class functions**: Pass as arguments, return from functions, store in variables
- ✅ **Closures**: Lambda expressions capture lexical environment
- ✅ **Recursion**: Via `letrec` bindings

#### Variable Bindings
- ✅ **Let bindings**: `let x = value in body`
- ✅ **Recursive bindings**: `letrec f = value in body`
- ✅ **Wildcard variables**: `let _ = expr in body` to explicitly discard values
- ✅ **Type annotations**: Optional Haskell-style (`let x : Int = 42`, `\x : String -> expr`)
- ✅ **Top-level definitions**: `let` and `letrec` at module level (v0.0.4.2)
- ✅ **Mutual recursion**: Multiple consecutive `letrec` definitions support mutual recursion (v0.0.4.2)

#### Data Structures
- ✅ **Lists**: `[1, 2, 3]`, homogeneous, with operations
- ✅ **Tuples**: `(1, "hello", true)`, heterogeneous, any number of elements
- ✅ **Records**: `{a = 1, b = true}` with field access (`record.field`)
- ✅ **Maybe type**: `Just value | Nothing` for optional values
- ✅ **Either type**: `Left error | Right value` for error propagation

#### Pattern Matching
- ✅ **Variable patterns**: `x`
- ✅ **Wildcard patterns**: `_` (matches any value without binding)
- ✅ **Literal patterns**: `42`, `true`, `"hello"`, `()`
- ✅ **Maybe patterns**: `Just x`, `Nothing`
- ✅ **Either patterns**: `Left x`, `Right x`
- ✅ **List patterns**: `[]`, `x :: xs`
- ✅ **Tuple patterns**: `(x, y, z)`
- ✅ **Record patterns**: `{a = x, b = y}`

### Type System

- ✅ **Unification-based type inference**: Function and expression types are inferred without explicit annotations
- ✅ **Static type checking**: All types checked before evaluation
- ✅ **Unification**: With occurs check to prevent infinite types
- ✅ **Generalized let-polymorphism**: `let`, `letrec`, top-level, and imported definitions can be reused at multiple types
- ✅ **Parametric function types**: Functions such as `\x -> x` infer type variables in their signatures
- ✅ **Base types**: `Int`, `Bool`, `String`, `Unit`
- ✅ **Composite types**: `[T]`, `(T1, T2, ...)`, `{field: T}`
- ✅ **Function types**: `T1 -> T2` (right-associative)
- ✅ **Maybe types**: `Maybe T`
- ✅ **Either types**: `Either T U`

### Built-in Functions

#### Type Conversion (4)
- ✅ `parseInt : String -> Maybe Int` - Safe string to int conversion
- ✅ `toString : Int -> String` - Integer to string
- ✅ `show : a -> String` - Any value to string representation
- ✅ `discard : a -> Unit` - Evaluates and discards any value

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

**Total Built-in Functions**: 27

### Module System

- ✅ **Top-level definitions**: `let` and `letrec` at module level (v0.0.4.2)
- ✅ **Module imports**: `import ModuleName` to import modules (v0.0.4.2)
- ✅ **Module resolution**: Supports `ModuleName.kai` and `ModuleName/ModuleName.kai` patterns (v0.0.4.2)
- ✅ **Module dependencies**: Modules can import other modules (v0.0.4.2)
- ✅ **Environment merging**: Imported definitions merge into importing module's environment (v0.0.4.2)
- ✅ **Mutual recursion**: Multiple consecutive `letrec` definitions support mutual recursion (v0.0.4.2)
- ✅ **Circular import detection**: Prevents infinite loops with clear error messages showing the loading stack (v0.0.4.2)
- ✅ **Type checking**: Full cross-module type checking with import resolution (v0.0.4.2)
- ✅ **Explicit exports**: `export name1, name2` syntax for selective module exports with enforcement (v0.0.4.2)

### Parser & Syntax

- ✅ **Megaparsec-based parser**: With operator precedence and associativity
- ✅ **Line comments**: `// comment`
- ✅ **Block comments**: `/* comment */`
- ✅ **Multi-statement files**: Top-level newlines split expressions while respecting nested `()`, `[]`, `{}`, strings, and comments
- ✅ **Reserved keywords**: 45+ keywords properly recognized, including `do`
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
- ✅ **Shebang support**: `#!/usr/bin/env kai` for executable scripts
- ✅ **Debug mode**: `kai --debug` for detailed output
- ✅ **Help system**: `kai --help`
- ✅ **Version display**: `Kai v0.0.4.3`
- ✅ **Script arguments**: Pass arguments to scripts
- ✅ **Failure exit codes**: Parse, type, and runtime failures return non-zero exit codes
- ✅ **Clean output**: No debug noise by default
- ✅ **Install script**: `make install` to `~/.local/bin`
- ✅ **Runner script**: Lightweight wrapper for compiled binary

### Testing Infrastructure

- ✅ **588 test examples**: Hspec, QuickCheck, script, CLI, stress, and example smoke coverage
- ✅ **Property-based testing**: QuickCheck for algebraic laws
- ✅ **Script tests**: `.kai` files with `// expect:` directives
- ✅ **Stress tests**: Deeply nested expressions (1000+ levels)
- ✅ **Type checking tests**: All type inference scenarios
- ✅ **Parser tests**: Edge cases and error messages
- ✅ **Clean codebase**: Well-structured Haskell with comprehensive test coverage

### Documentation

- ✅ **README.md**: Project overview, quickstart, examples, roadmap
- ✅ **SPEC.md**: Complete language specification
- ✅ **DEVELOPING.md**: Architecture, semantics, development workflow
- ✅ **AGENTS.md**: Testing guidelines for AI assistants
- ✅ **Website**: Yesod-based static site with examples
- ✅ **Working examples**: 11 runnable scripts plus reusable module samples

### Performance & Optimization

#### Benchmarking Suite
- ✅ **Comprehensive benchmarks**: Speed (Criterion) and memory (Weigh) profiling
- ✅ **Parser benchmarks**: Expression size, nesting depth, lambda chains, list operations
- ✅ **Evaluator benchmarks**: Arithmetic, boolean logic, conditionals, functions, recursion, data structures
- ✅ **Type checker benchmarks**: Basic types, arithmetic, functions, polymorphism, recursion
- ✅ **Regression detection**: Automated performance monitoring and alerting
- ✅ **CI integration**: Benchmark suite integrated into automated testing

#### Performance Optimizations
- ✅ **Modular architecture**: Split monolithic components into focused submodules
- ✅ **Record access optimization**: Inlined evaluation logic (3-5% improvement)
- ✅ **Boolean operation fixes**: Corrected syntax usage (300x improvement)
- ✅ **Pure recursion optimization**: Improved LetRec evaluation efficiency
- ✅ **NFData instances**: Added for accurate benchmarking across all data types
- ✅ **Memory profiling**: Comprehensive heap usage analysis

#### Current Performance Metrics
- **Most operations**: ~20-50ns (arithmetic, conditionals, functions)
- **Record access**: ~1.93μs (optimized map lookups)
- **Recursion**: ~6μs (appropriate for function call overhead)
- **Boolean operations**: ~23ns (after syntax corrections)
- **Parser**: ~40-600ns (linear scaling with complexity)
- **Type checker**: ~20ns

---

## Roadmap

Kai is now past the point where a giant feature wishlist is useful. The next release should sharpen the language as a practical typed scripting tool, not broaden it in every possible direction.

### v0.0.4.4 Release Focus

#### 1. Interactive Workflow
- ⏳ **Interactive REPL**: Core read-eval-print loop
- ⏳ **Multiline input**: Usable for real expressions and definitions
- ⏳ **`:type`, `:load`, `:reload`**: Enough commands to make exploration practical
- ⏳ **History and completion**: Nice-to-have if the core REPL lands cleanly

#### 2. Data Modeling and Pattern Matching
- ⏳ **Custom data types**: User-defined algebraic data types
- ⏳ **Constructor patterns**: Matching on user-defined variants
- ⏳ **Tuple destructuring in `case`**: Make existing tuples less awkward
- ⏳ **Simple guards and as-patterns**: Only if they keep the implementation coherent

#### 3. Essential Scripting Stdlib
- ⏳ **File additions**: `appendFile`, `fileExists`, line-oriented helpers
- ⏳ **Directory operations**: `listDirectory`, `createDirectory`, `removeDirectory`, current-directory helpers
- ⏳ **Process and environment access**: `system`, `getEnv`, `setEnv`, explicit exit helpers
- ⏳ **Small stdlib gaps**: A few missing math/list/string helpers that matter in scripts

#### 4. Stretch Work If v0.0.4.4 Lands Early
- ⏳ **Better parse and type errors**: Better wording and code context
- ⏳ **Function composition and pipeline operators**: Worth adding once REPL and scripting flow are stronger
- ⏳ **More ergonomic pattern forms**: Only after ADTs are solid

### Deferred Until After v0.0.4.4

#### Tooling and Distribution
- ⏳ **Formatter and linter**: Useful, but not before the interactive workflow is solid
- ⏳ **Language server / IDE support**: Valuable after the surface syntax settles
- ⏳ **Package manager**: Important later, but premature before the module and stdlib story is more mature

#### Networking and Richer I/O
- ⏳ **HTTP and JSON support**: Desirable, but behind local file/process scripting basics
- ⏳ **Mutable references**: Only if the scripting story proves it needs them

#### Advanced Type System and Syntax
- ⏳ **Polymorphic recursion**: Explicitly not a near-term priority
- ⏳ **Type classes, row polymorphism, GADTs, rank-N types**: Out of scope for the next release
- ⏳ **List comprehensions, ranges, `where`, multi-way `if`**: Backlog ideas, not core release goals

#### Compiler and Runtime Work
- ⏳ **Tail call optimization and strictness controls**: Worth revisiting later
- ⏳ **Bytecode/JIT/optimization passes**: Not the next bottleneck for Kai

---

## Current Limitations

### Language Limitations
- ❌ **No REPL**: Command-line execution only
- ❌ **No custom data types**: Only built-in types available
- ❌ **Limited pattern matching**: No guards, no as-patterns
- ❌ **Wildcard restrictions**: `_` not allowed in `letrec` bindings (cannot be meaningfully recursive)
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

- **Lines of Haskell**: ~4,200 (estimated, including benchmarks)
- **Test Coverage**: 588 examples, 100% passing
- **HLint Warnings**: 0
- **Core Types**: 8 (Int, Bool, String, Unit, List, Tuple, Record, Function)
- **Built-in Functions**: 27
- **Reserved Keywords**: 45+
- **Operator Precedence Levels**: 11
- **Example Scripts**: 11 runnable scripts plus reusable module samples
- **Documentation**: 5 comprehensive markdown files
- **Benchmark Suites**: 3 (Parser, Evaluator, TypeChecker)
- **Performance Optimizations**: Record access inlining, boolean syntax fixes
- **Architecture**: Modular design with 28 focused submodules

---

## Version History

### v0.0.4.3 (Current - 2026-04-01)
- Added generalized let-polymorphism for `let`, `letrec`, top-level, and imported definitions
- Fixed CLI failure exit codes for parse, type, and runtime errors
- Added shebang parsing support for script files
- Fixed top-level `let ... in ...` expressions in program files
- Added `do { ... }` blocks as a readable sequencing form
- Fixed multiline top-level parsing so nested braces/comments survive chunk splitting
- Added CLI and polymorphism regression coverage
- Refreshed the entire example suite around modules, records, Maybe/Either, file scripting, and interactive workflows
- Added smoke tests for every runnable example and sync checks for duplicate example modules

### v0.0.4.2 (2025-11-06)
- **Performance optimizations**: Record access inlining (3-5% improvement), boolean syntax corrections (300x faster)
- **Modular architecture**: Split Evaluator, Parser, and TypeChecker into focused submodules
- **Comprehensive benchmarking**: Added Criterion speed benchmarks and Weigh memory profiling
- **Benchmark automation**: Performance regression detection and CI integration
- **NFData instances**: Added for accurate benchmarking across all data types
- Added tuples with pattern matching
- Added 8 list functions (map, filter, foldl, length, reverse, take, drop, zip)
- Added 5 string functions (split, join, trim, replace, strLength)
- Added file I/O (readFile, writeFile)
- Added command-line arguments (args)
- Fixed keyword parsing bug
- Historical note: that release increased tests from 352 to 435

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
- Unification-based type inference

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
