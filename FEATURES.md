# Kai Language Features

**Version**: 0.0.4.6
**Last Updated**: 2026-09-05

This document provides a comprehensive overview of all implemented and planned features for the Kai programming language.

---

## Implemented Features (v0.0.4.6)

### Core Language

#### Literals & Basic Types
- ✅ **Integer literals**: 32-bit signed integers (`-2³¹` to `2³¹-1`)
- ✅ **Boolean literals**: `true`, `false`
- ✅ **String literals**: Double-quoted with escape sequences (`\"`, `\\`, `\n`)
- ✅ **Unit literal**: `()` for side-effect operations

#### Operators
- ✅ **Arithmetic**: `+`, `-`, `*`, `/` (integer division with checked 32-bit overflow)
- ✅ **Unary minus**: `-expr`; repeated prefix operators apply from right to left
- ✅ **Boolean logic**: `and`, `or` (right-associative), `not` (prefix)
- ✅ **Comparison**: `==`, `<`, `>` (non-associative); equality is structural for data and rejects callable/reference values
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
- ✅ **First-class functions**: Pass as arguments, return from functions, store in variables; builtins support partial application
- ✅ **Closures**: Lambda expressions capture lexical environment
- ✅ **Recursion**: Via `letrec` bindings
- ✅ **Fixed points**: `fix : (a -> a) -> a`, with safe rejection of unproductive self-forcing values

#### Variable Bindings
- ✅ **Let bindings**: `let x = value in body`
- ✅ **Recursive bindings**: `letrec f = value in body`; constants and closures initialize in source order with guarded recursive reads
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
- ✅ **Custom data types**: `data TypeName a = Constructor ...` with partially applicable, first-class constructor functions

#### Pattern Matching
- ✅ **Variable patterns**: `x`
- ✅ **Wildcard patterns**: `_` (matches any value without binding)
- ✅ **Literal patterns**: `42`, `true`, `"hello"`, `()`
- ✅ **Maybe patterns**: `Just x`, `Nothing`
- ✅ **Either patterns**: `Left x`, `Right x`
- ✅ **List patterns**: `[]`, `x :: xs`
- ✅ **Tuple patterns**: `(x, y, z)`
- ✅ **Record patterns**: `{a = x, b = y}`
- ✅ **Constructor patterns**: `Leaf x`, `Node left right`, and other user-defined variants

### Type System

- ✅ **Unification-based type inference**: Function and expression types are inferred without explicit annotations
- ✅ **Static type checking**: All types checked before evaluation
- ✅ **Unification**: With occurs check to prevent infinite types
- ✅ **Generalized let-polymorphism**: `let`, `letrec`, top-level, and imported definitions can be reused at multiple types
- ✅ **Annotated polymorphic recursion**: Recursive bindings can recurse across different instantiations when given explicit type annotations
- ✅ **Parametric function types**: Functions such as `\x -> x` infer type variables in their signatures
- ✅ **Base types**: `Int`, `Bool`, `String`, `Unit`
- ✅ **Composite types**: `[T]`, `(T1, T2, ...)`, `{field: T}`
- ✅ **Custom algebraic types**: `Tree Int`, `Result String`, `Pair a b`
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

#### I/O Operations (16)
- ✅ `print : a -> Unit` - Print and flush value, then return unit; output failures stop later effects
- ✅ `input : String` - Read line from stdin
- ✅ `readFile : String -> String` - Read an entire UTF-8 text file
- ✅ `writeFile : String -> String -> Unit` - Write UTF-8 text to a file (overwrite)
- ✅ `appendFile : String -> String -> Unit` - Append UTF-8 text to a file
- ✅ `fileExists : String -> Bool` - Check whether a file exists
- ✅ `listDirectory : String -> [String]` - List directory entries
- ✅ `createDirectory : String -> Unit` - Create a directory
- ✅ `removeDirectory : String -> Unit` - Remove an empty directory
- ✅ `getCurrentDirectory : String` - Return the current working directory
- ✅ `setCurrentDirectory : String -> Unit` - Change the current working directory
- ✅ `system : String -> Int` - Run a shell command and return its exit code
- ✅ `getEnv : String -> Maybe String` - Read an environment variable
- ✅ `setEnv : String -> String -> Unit` - Set an environment variable
- ✅ `exit : Int -> a` - Exit the current program with an explicit code
- ✅ `args : [String]` - Command-line arguments passed to script

**Total Built-in Functions**: 39

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
- ✅ **Interactive REPL**: `kai`, `kai repl`, or `kai --repl`
- ✅ **Expression evaluation**: `kai -e "expr"`
- ✅ **File execution**: `kai script.kai [args...]`
- ✅ **Shebang support**: `#!/usr/bin/env kai` for executable scripts
- ✅ **Debug mode**: `kai --debug` for detailed output
- ✅ **Help system**: `kai --help`
- ✅ **REPL commands**: `:type`, `:load`, `:reload`, `:quit`
- ✅ **Version display**: `kai --version` and `kai -V` print the package-derived version
- ✅ **Script arguments**: Pass arguments to scripts
- ✅ **Failure exit codes**: Parse, type, and runtime failures return non-zero exit codes
- ✅ **Clean output**: No debug noise by default
- ✅ **Install script**: `make install` links the checkout's runner into `~/.local/bin`; supports a custom `PREFIX`
- ✅ **Runner script**: Explicit `KAI_BIN` selection, executable lookup on `PATH`, active Stack build selection, and invocation from other directories

### Testing Infrastructure

- ✅ **Executable test suite**: Hspec, QuickCheck properties, asserted scripts, CLI, REPL, stress, and example smoke coverage
- ✅ **Property-based testing**: QuickCheck for algebraic laws
- ✅ **Script tests**: `.kai` files with `// expect:` directives
- ✅ **Stress tests**: Deeply nested expressions (1000+ levels)
- ✅ **Type checking tests**: Unification, polymorphism, annotations, declarations, and imports
- ✅ **Parser tests**: Edge cases and error messages
- ✅ **Clean codebase**: Well-structured Haskell with comprehensive test coverage

### Documentation

- ✅ **README.md**: Project overview, quickstart, examples, roadmap
- ✅ **SPEC.md**: Complete language specification
- ✅ **DEVELOPING.md**: Architecture, semantics, development workflow
- ✅ **AGENTS.md**: Testing guidelines for AI assistants
- ✅ **Website**: Yesod-based static site with examples
- ✅ **Working examples**: 12 runnable scripts plus reusable module samples

### Performance & Optimization

#### Benchmarking Suite
- ✅ **Comprehensive benchmarks**: Speed (Criterion) and memory (Weigh) profiling
- ✅ **Parser benchmarks**: Expression size, nesting depth, lambda chains, list operations
- ✅ **Evaluator benchmarks**: Arithmetic, boolean logic, conditionals, functions, recursion, data structures
- ✅ **Type checker benchmarks**: Basic types, arithmetic, functions, polymorphism, recursion
- ✅ **Input validity gate**: Invalid benchmark programs abort instead of measuring parse/type/runtime failures
- ✅ **CI integration**: Every benchmark runs for one iteration in automated testing

#### Performance Optimizations
- ✅ **Modular architecture**: Split monolithic components into focused submodules
- ✅ **Record access optimization**: Inlined evaluation logic
- ✅ **Boolean operation fixes**: Corrected syntax usage
- ✅ **Pure recursion optimization**: Improved LetRec evaluation efficiency
- ✅ **NFData instances**: Added for accurate benchmarking across all data types
- ✅ **Memory profiling**: Comprehensive heap usage analysis

#### Performance Baselines
The suite checks its Kai inputs before measuring them. Compare benchmark timings
on the same machine and build profile when checking performance changes.

---

## Roadmap

Development focuses on practical scripting tools, useful standard-library additions, and REPL ergonomics.

### Priorities

#### 1. REPL Polish
- ⏳ **History and completion**: The core REPL works; now it needs comfort features
- ⏳ **Better interactive diagnostics**: Friendlier parse/type/runtime feedback in the session loop

#### 2. Stdlib Depth
- ⏳ **Line-oriented file helpers**: A practical follow-up to `readFile`/`writeFile`/`appendFile`
- ⏳ **JSON and HTTP**: Valuable once the local scripting story is rounded out
- ⏳ **Small utility gaps**: A few missing math/list/string helpers that matter in scripts

#### 3. Tooling and Distribution
- ⏳ **Formatter and linter**: Useful once the surface syntax is more settled
- ⏳ **Language server / IDE support**: Valuable after the interactive workflow matures
- ⏳ **Package manager**: Important later, but still premature before the stdlib and module story stabilize

#### 4. Longer-Term Type/System Work
- ⏳ **Full polymorphic recursion inference/ergonomics**: Explicitly not the next priority
- ⏳ **Type classes, row polymorphism, GADTs, rank-N types**: Out of scope for the near term
- ⏳ **List comprehensions, ranges, `where`, multi-way `if`**: Backlog ideas, not current release goals

#### Compiler and Runtime Work
- ⏳ **Tail call optimization and strictness controls**: Worth revisiting later
- ⏳ **Bytecode/JIT/optimization passes**: Not the next bottleneck for Kai

---

## Current Limitations

### Language Limitations
- ❌ **Minimal REPL ergonomics**: No history, completion, or editor integration yet
- ❌ **Limited pattern matching**: No guards, no as-patterns
- ❌ **Wildcard restrictions**: `_` not allowed in `letrec` bindings (cannot be meaningfully recursive)
- ❌ **Polymorphic recursion still needs explicit annotations**: Unannotated recursive bindings remain monomorphic
- ❌ **Script failures**: A parse/type error stops a script; the REPL accepts subsequent input
- ❌ **Integer-only arithmetic**: No floating-point numbers
- ❌ **Limited escape sequences**: Only `\"`, `\\`, `\n` supported
- ❌ **No regex support**: String operations are basic

### I/O Limitations
- ❌ **No line-oriented file helpers**: Core file and directory primitives exist, but higher-level helpers are still missing
- ❌ **No network operations**: No HTTP, sockets, etc.
- ❌ **Basic process control only**: `system` exists, but there is no richer subprocess API
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
- ❌ **No language-level profiler**: Use the Haskell benchmark suites for implementation performance

---

## Implementation Statistics

- **Tests**: Hspec, QuickCheck, script expectations, CLI and REPL integration, and stress tests
- **HLint Warnings**: 0
- **Core Types**: 8 (Int, Bool, String, Unit, List, Tuple, Record, Function)
- **Built-in Functions**: 39
- **Reserved Keywords**: 66
- **Operator Precedence Levels**: 11
- **Example Scripts**: 12 runnable scripts plus reusable module samples
- **Documentation**: 5 comprehensive markdown files
- **Benchmark Suites**: 3 (Parser, Evaluator, TypeChecker)
- **Performance Optimizations**: Record access inlining, boolean syntax fixes
- **Architecture**: Modular design with focused submodules across parser, type checker, evaluator, REPL, and module loading

---

## Version History

### v0.0.4.6

- First-class and partially applied builtins, typed lambda parameters, and tuple annotations
- Consistent recursive inference and initialization across scripts, modules, and the REPL
- Type variables scoped to each annotation, compatible renamed data parameters, and private constructor visibility
- Exact constructor-pattern arity and duplicate-pattern validation
- Arithmetic spacing, nested block comments, and multiline top-level expressions
- Repeated prefix operators with checked integer negation
- UTF-8 scripts, modules, and text-file I/O, with decoding errors reported by the CLI and REPL
- Script expectation checks with stdin fixtures and nested module coverage
- Runner installation, explicit executable selection, and active build lookup

### v0.0.4.5 (2026-07-11)
- Added `kai --version` and `kai -V`, including clean debug-prefixed behavior
- Switched release downloads to permission-preserving Linux/macOS archives and a Windows ZIP with `SHA256SUMS`
- Pinned native release runners and set the Apple Silicon deployment target to macOS 11.3
- Added native verification of the exact downloaded Linux, macOS, and Windows packages before a draft release becomes public
- Made pushed `package.yaml` version bumps on `master` start the gated release workflow automatically, with manual retry support
- Added fail-closed Apple Developer ID/notarization and Windows Authenticode workflow paths that activate only when explicitly enabled and credentialed

### v0.0.4.4 (2026-07-11)
- Added an interactive REPL with multiline input plus `:type`, `:load`, `:reload`, and `:quit`
- Added custom algebraic data types with constructor functions and constructor patterns
- Added scripting stdlib builtins for file append/existence, directory operations, current-directory management, process execution, environment access, and explicit exit codes
- Added explicitly annotated polymorphic recursion for recursive bindings, plus CLI/REPL/module regressions around it
- Added a typed `fix` combinator and removed cyclic substitutions from the occurs check
- Fixed composite unification, constructor-pattern parsing, builtin precedence, ADT equality, and left-to-right IO error propagation
- Made constructor functions work consistently in ordinary application, `map`, `foldl`, and `fix`
- Enforced signed 32-bit literals, conversions, and checked arithmetic overflow
- Made script assertions mandatory and made invalid benchmark inputs fail CI validation
- Made releases explicit and gated, and made static-site export verify that it owns the server response
- Added direct REPL coverage, custom data type coverage, and real IO-stdlib regression tests
- Added new ADT example coverage and refreshed file I/O examples around the new stdlib helpers

### v0.0.4.3 (2026-04-01)
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
- **Performance optimizations**: Record access inlining and boolean syntax corrections
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
