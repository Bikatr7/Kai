# Developing Kai

This document helps contributors work on Kai’s codebase efficiently.

## Architecture Overview

- `src/Syntax.hs`: AST for expressions (`Expr`) with optional type annotations. Includes literals, variables, arithmetic, booleans, comparisons, strings, `print`, `input`, conversion functions, lambdas, application, `let`, and `letrec`, plus type annotation expressions.
- `src/Parser.hs`: Megaparsec parser with performance optimizations for deeply nested expressions. Handles comments, precedence/associativity, unary minus, string escapes (`\"`, `\\`, `\n`), type annotations, and multi-statement parsing.
- `src/TypeChecker.hs`: Hindley–Milner type inference with unification, occurs check, and substitution composition optimization. Core types: `TInt`, `TBool`, `TString`, `TUnit`, and `TFun`.
- `src/Evaluator.hs`: Strict evaluator with closures and IO support. Runtime values: `VInt`, `VBool`, `VStr`, `VUnit`, `VFun`. Supports `print`, `input`, and conversion functions.
- `src/Main.hs`: CLI entry (`kai`). Parses input, type-checks, and evaluates; supports `-e`, `--debug` flag, and file execution with clean output by default.
- `website/`: Yesod-based static site generator used for the project website/demo.

## Language Semantics (current)

- Evaluation: strict (call-by-value).
- Unit: `()` value with type `TUnit`.
- `print : a -> Unit` prints and returns `()`.
- `input : String` reads a line from stdin.
- Error handling: Maybe/Either types with `Just`, `Nothing`, `Left`, `Right` constructors and case expressions for pattern matching.
- Safe conversion functions: `parseInt : String -> Maybe Int`, `toString : Int -> String`, `show : a -> String`.
- Type annotations: Optional Haskell-style type annotations for lambdas and let bindings.
- Strings: escapes `\"`, `\\`, `\n`. Unknown escapes are errors with a helpful message.
- Precedence (highest to lowest):
  1) application (left)
  2) prefix `not`, unary `-`
  3) `*`, `/` (left)
  4) `+`, `-` (left)
  5) `++` (right)
  6) `<`, `>`, `==` (non)
  7) `and` (right)
  8) `or` (right)

Notes:
- `+` is disambiguated from `++` in the lexer to ensure `++` parses correctly at its precedence.
- Application binds tighter than prefix: `-f x` parses as `Sub (IntLit 0) (f x)`.

## Build and Test

Prereqs: Stack + GHC.

- Build: `stack build`
- Tests: `stack test --fast` (all 263 tests including 24 input test files)
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

## Linting & Style

- HLint: `hlint .`
  - Examples already applied: `void (symbol "()")`, avoid trivial lambdas in operator table, use `Right . VStr <$> getLine` over do-notation.
- Keep changes minimal and focused. Follow existing code style.

## Adding Features (playbook)

1) Update `Syntax` with new AST forms.
2) Extend `Parser` with syntax + precedence placement.
3) Extend `TypeChecker` with typing rules and unification as needed.
4) Extend `Evaluator` with runtime behavior. For IO features, update both `evalWithEnv` and `evalWithEnvIO` to handle all expression types.
5) Add comprehensive tests:
   - Unit tests in `test/*Spec.hs` with proper error type matching.
   - Script test in `tests/*.kai` with `// expect:`.
   - Property tests when applicable.
   - Performance stress tests for deeply nested expressions.
6) Update all documentation to reflect changes:
   - README.md (source of truth for project goals and status)
   - SPEC.md (technical language specification - update immediately when semantics change)
   - website/Main.hs (features, examples, version)
   - DEVELOPING.md (development practices)

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
- **Interactive I/O**: `input` function reads from stdin, enabling interactive applications like the calculator example
- **Performance fixes**: Eliminated infinite loops in deeply nested expressions (1000+ levels) through parser and type checker optimizations
- **Clean CLI**: Debug output hidden by default, use `--debug` flag when needed for development
- **Comprehensive testing**: 24 new input test files covering edge cases and integration scenarios

## Notes / TODOs

- Planned next steps (from roadmap): data structures (lists, records), pattern matching, and module system.
- When changing semantics, align README.md, SPEC.md, website, and DEVELOPING.md immediately.
- Always verify that stress tests pass after performance-critical changes.