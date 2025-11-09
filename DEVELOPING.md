# Developing Kai

This document helps contributors work on Kai’s codebase efficiently.

## Architecture Overview

### Modular Design
The codebase follows a modular architecture with clear separation of concerns. Each major component is split into focused submodules for maintainability and performance.

### Core Components

#### Syntax (`src/Syntax.hs`)
- AST definitions (`Expr`, `Pattern`) with NFData instances for benchmarking
- Optional type annotations and comprehensive expression coverage
- Literals, operators, functions, bindings, I/O operations

#### Parser (`src/Parser/`)
- **Lexer.hs**: Lexical analysis, reserved keywords, symbol parsing
- **Literals.hs**: Basic literal parsing (integers, strings, booleans, unit)
- **Types.hs**: Type annotation parsing
- **Patterns.hs**: Pattern matching syntax parsing
- **ComplexExpr.hs**: Complex expressions (lambdas, conditionals, bindings)
- **Builtins.hs**: Built-in function parsing
- **Expressions.hs**: Main expression parser with operator precedence
- **Parser.hs**: Public interface coordinating all parsing components

#### Type Checker (`src/TypeChecker/`)
- **Types.hs**: Core type definitions and syntax-type conversions
- **Substitution.hs**: Type variable substitution with composition optimization
- **Unification.hs**: Unification algorithm with occurs check
- **Literals.hs**: Literal and variable type inference
- **Arithmetic.hs**: Arithmetic operator type checking
- **ControlFlow.hs**: Control flow type checking
- **Functions.hs**: Function and lambda type checking
- **Bindings.hs**: Let/letrec binding type checking
- **DataStructures.hs**: List, record, tuple type checking
- **Operations.hs**: Built-in operation type checking
- **Patterns.hs**: Pattern type checking
- **Inference.hs**: Main type inference dispatcher
- **TypeChecker.hs**: Public interface with typeCheck and typeCheckWithEnv

#### Evaluator (`src/Evaluator/`)
- **Types.hs**: Runtime value definitions with NFData for benchmarking
- **Helpers.hs**: Utility functions for evaluation and string operations
- **Literals.hs**: Literal value evaluation
- **Arithmetic.hs**: Arithmetic operations (pure and IO variants)
- **BooleanOps.hs**: Boolean logic evaluation (pure and IO variants)
- **ControlFlow.hs**: Control flow evaluation (pure and IO variants)
- **Functions.hs**: Function application and closures (pure and IO variants)
- **Bindings.hs**: Let/letrec binding evaluation (pure and IO variants)
- **DataStructures.hs**: List, record, tuple operations (pure and IO variants)
- **StringOps.hs**: String manipulation (pure and IO variants)
- **Conversions.hs**: Type conversion functions (pure and IO variants)
- **IOOps.hs**: I/O operations (input, print, file operations)
- **Patterns.hs**: Pattern matching evaluation (pure and IO variants)
- **Evaluator.hs**: Public interface with eval, evalWithEnv, evalPure, evalPureWithEnv

#### CLI (`src/Main.hs`)
- Command-line interface with expression evaluation and file execution
- Debug mode, clean output by default, argument passing support
- `website/`: Yesod-based static site generator used for the project website/demo.

## Language Semantics (current)

- Evaluation: strict (call-by-value).
- Unit: `()` value with type `TUnit`.
- `print : a -> Unit` prints and returns `()`.
- `input : String` reads a line from stdin.
- `args : [String]` returns command-line arguments passed to script.
- File I/O: `readFile : String -> String`, `writeFile : String -> String -> Unit`.
- Error handling: Maybe/Either types with `Just`, `Nothing`, `Left`, `Right` constructors and case expressions for pattern matching.
- Safe conversion functions: `parseInt : String -> Maybe Int`, `toString : Int -> String`, `show : a -> String`.
- List functions: `map`, `filter`, `foldl`, `length`, `reverse`, `take`, `drop`, `zip`.
- String functions: `split`, `join`, `trim`, `replace`, `strLength`.
- Tuple functions: `fst`, `snd` for pairs.
- Type annotations: Optional Haskell-style type annotations for lambdas and let bindings.
- Strings: escapes `\"`, `\\`, `\n`. Unknown escapes are errors with a helpful message.
- Precedence (highest to lowest):
  1) application (left)
  2) field access `.field` (left)
  3) prefix `not`, unary `-`
  4) `*`, `/` (left)
  5) `+`, `-` (left)
  6) `::` cons (right)
  7) `++` concatenation (right)
  8) `<`, `>`, `==` (non)
  9) `and` (right)
  10) `or` (right)
  11) `;` sequencing (right, lowest)

Notes:
- `+` is disambiguated from `++` in the lexer to ensure `++` parses correctly at its precedence.
- Application binds tighter than prefix: `-f x` parses as `Sub (IntLit 0) (f x)`.

## Build and Test

Prereqs: Stack + GHC.

- Build: `stack build`
- Tests: `stack test --fast` (all 435 tests)
- Run CLI: `stack exec kai -- --help`
- Run with debug output: `stack exec kai -- --debug -e "42 + 1"`
- Try interactive calculator: `stack exec kai examples/calculator.kai`
- Run website locally: `stack exec kai-website` (visit http://localhost:3000)

### Test Suite Structure

- Unit tests: `test/*.hs` (Hspec + QuickCheck)
- Script tests: `tests/*.kai` with `// expect:` directives
- Property tests: `PropertyBasedSpec.hs`

Run subsets:
- `stack test --test-arguments "--match Arithmetic"`
- `stack test --test-arguments "--match Script files"`

## Performance Considerations

- **Parser optimization**: Order `choice` alternatives with `parens expr` early to prevent exponential backtracking in deeply nested expressions.
- **Type checker optimization**: Avoid exponential `applySubstEnv` calls by composing substitutions efficiently and not applying them to environments unnecessarily.
- **Stack size**: Use `stack test --fast --test-arguments="--match Stress"` to verify deeply nested expressions (1000+ levels) don't cause infinite loops.
- **Memory management**: Critical for handling complex recursive structures and large expressions.

## Benchmarking

Kai includes comprehensive performance benchmarks using Criterion (speed) and Weigh (memory). See `benchmarks/README.md` for detailed documentation.

### Running Benchmarks

```bash
# Full benchmark suite
stack bench

# Specific components
stack bench --benchmark-arguments="--match pattern 'Evaluator'"
stack bench --benchmark-arguments="--match pattern 'Parser'"
stack bench --benchmark-arguments="--match pattern 'Type Checker'"

# Generate CSV output for analysis
stack bench --benchmark-arguments="--csv=results.csv"
```

### Performance Regression Detection

- **Run benchmarks before major changes** to establish baseline
- **Run benchmarks after optimizations** to verify improvements
- **Monitor for >10% regressions** which indicate potential issues
- **Use CSV output** for automated comparison in CI/CD pipelines

### Current Performance Metrics

- **Most operations**: ~20-50ns (arithmetic, conditionals, functions)
- **Record access**: ~1.93μs (optimized map lookups)
- **Recursion**: ~6μs (appropriate for function call overhead)
- **Boolean operations**: ~23ns (after syntax corrections)
- **Parser**: ~40-600ns (linear scaling with complexity)
- **Type checker**: ~20ns

## Linting & Style

- HLint: `hlint .`
  - Examples already applied: `void (symbol "()")`, avoid trivial lambdas in operator table, use `Right . VStr <$> getLine` over do-notation.
- Keep changes minimal and focused. Follow existing code style.

## Adding Features (playbook)

1) Update `Syntax` with new AST forms and NFData instances for benchmarking.
2) Extend appropriate `Parser` submodules with syntax + precedence placement.
3) Extend appropriate `TypeChecker` submodules with typing rules and unification as needed.
4) Extend appropriate `Evaluator` submodules with runtime behavior. For IO features, update both pure and IO variants.
5) Add comprehensive tests:
   - Unit tests in `test/*Spec.hs` with proper error type matching.
   - Script test in `tests/*.kai` with `// expect:`.
   - Property tests when applicable.
   - Performance stress tests for deeply nested expressions.
6) **Run benchmarks** before and after changes to measure performance impact:
   - `stack bench` for full benchmark suite
   - `stack bench --benchmark-arguments="--match pattern 'Evaluator'"` for evaluator benchmarks
   - Compare results to detect regressions (>10% slower indicates investigation needed)
7) Update all documentation to reflect changes:
   - README.md (source of truth for project goals and status)
   - SPEC.md (technical language specification - update immediately when semantics change)
   - website/Main.hs (features, examples, version)
   - DEVELOPING.md (development practices)
   - FEATURES.md (features)
   - benchmarks/README.md (benchmark documentation and guidelines)

## Versioning & Release

- Bump version in `package.yaml` (hpack regenerates `.cabal`).
- Update README header and website version display.
- Tag and build via CI to produce binaries (see README’s install section).

## Website Updates

- Edit `website/Main.hs` for content changes (features, version, examples).
- Run `stack exec kai-website` to serve locally. Static export via `scripts/export-site.sh`.

## Recent Major Improvements (v0.0.3.2+)

- **Type annotations**: Optional Haskell-style type annotations for lambdas and let bindings (`\x : Int -> expr`, `let x : Int = val`)
- **Error handling system**: Full Maybe/Either types with pattern matching for graceful error handling instead of runtime crashes
- **Safe conversion functions**: `parseInt : String -> Maybe Int` returns `Nothing` for invalid input instead of crashing
- **Case expressions**: Pattern matching for handling Maybe/Either and other data types safely
- **Wildcard variables**: Use `_` in let bindings to discard unused values (`let _ = print "hello" in 42`)
- **Expression sequencing**: Use `;` to sequence expressions for side effects (`print "first"; print "second"; 42`)
- **Interactive I/O**: `input` function reads from stdin, enabling interactive applications like the calculator example
- **Recursion fixes**: Fixed critical evaluator bug preventing infinite recursion with IO operations
- **Performance fixes**: Eliminated infinite loops in deeply nested expressions (1000+ levels) through parser and type checker optimizations
- **Clean CLI**: Debug output hidden by default, use `--debug` flag when needed for development
- **Comprehensive testing**: 318 tests including 28 new tests for wildcard variables and expression sequencing

## Notes / TODOs

- Planned next steps (from roadmap): data structures (lists, records), pattern matching, and module system.
- When changing semantics, align README.md, SPEC.md, website, and DEVELOPING.md immediately.
- Always verify that stress tests pass after performance-critical changes.