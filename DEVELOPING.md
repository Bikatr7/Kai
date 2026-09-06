# Developing Kai

This document helps contributors work on Kai’s codebase efficiently.

## Architecture Overview

### Modular Design
The codebase follows a modular architecture with clear separation of concerns. Each major component is split into focused submodules for maintainability and performance.

### Core Components

#### Syntax (`src/Syntax.hs`)
- AST definitions (`Expr`, `Pattern`) with NFData instances for benchmarking
- Optional type annotations, top-level `data` declarations, and comprehensive expression coverage
- Literals, operators, functions, bindings, I/O operations

#### Shared Declaration Logic (`src/DataDeclarations.hs`)
- Constructor scheme generation for user-defined algebraic data types
- Shared runtime/type-environment construction and validation for `data` declarations
- Import compatibility checks preserve private declaration metadata

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
- **TypeChecker.hs**: Shared program/definition inference used by files, module exports, and the REPL; one fresh-variable supply per recursive block

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
- **Program.hs**: Shared top-level execution for scripts and modules
- **Recursion.hs**: Source-order initialization with guarded recursive references
- **Evaluator.hs**: Public interface with eval, evalWithEnv, evalPure, evalPureWithEnv

#### Text Files (`src/UTF8.hs`, `src/SourceIO.hs`)
- Source files and `readFile`/`writeFile`/`appendFile` use UTF-8 independently of the host locale.
- Reads force decoding before closing the handle so decoding failures reach the caller's IO error handler.
- `UTF8Spec.hs` covers source loading, imports, REPL loads, exact file bytes, invalid sequences, and file operations under a legacy locale.

#### CLI (`src/CLI.hs`, `src/Main.hs`)
- Command-line interface with expression evaluation, file execution, and REPL entry
- Debug mode, clean output by default, argument passing support, and package-derived `--version`/`-V` output
- Non-zero exit codes for parse, type, runtime, and output failures; diagnostics fall back to stderr if stdout fails
- `src/REPL.hs`: multiline REPL with `:type`, `:load`, `:reload`, and persistent environments
- `website/`: Yesod-based static site generator used for the project website/demo.

## Language Semantics (current)

- Evaluation: strict (call-by-value).
- Integers: signed 32-bit values; literals, `parseInt`, and arithmetic results enforce the range, and arithmetic overflow returns `IntegerOverflow`.
- Unit: `()` value with type `TUnit`.
- `print : a -> Unit` prints, flushes stdout, and returns `()`. Write or flush failures return `TypeError "print: could not write to stdout"` and stop later effects.
- `input : String` reads a line from stdin.
- `args : [String]` returns command-line arguments passed to script.
- File and directory I/O: `readFile`, `writeFile`, `appendFile`, `fileExists`, `listDirectory`, `createDirectory`, `removeDirectory`, `getCurrentDirectory`, `setCurrentDirectory`.
- Process/environment access: `system`, `getEnv`, `setEnv`, `exit`.
- A leading shebang line (`#!/usr/bin/env kai`) is ignored when parsing files.
- Error handling: Maybe/Either types with `Just`, `Nothing`, `Left`, `Right` constructors and case expressions for pattern matching.
- Safe conversion functions: `parseInt : String -> Maybe Int`, `toString : Int -> String`, `show : a -> String`.
- List functions: `map`, `filter`, `foldl`, `length`, `reverse`, `take`, `drop`, `zip`.
- String functions: `split`, `join`, `trim`, `replace`, `strLength`.
- Tuple functions: `fst`, `snd` for pairs.
- Custom data types: top-level `data` declarations produce constructor functions and constructor patterns.
- Equality: primitive and composite data compare structurally; different constructors compare as false, while callable values and recursive runtime references raise a runtime `TypeError` at any nesting depth.
- Type annotations: Optional Haskell-style annotations for lambdas and let bindings. Variables are fresh for each annotation; repeated variables within one annotation remain tied.
- Constructor patterns require every field. Imported declaration compatibility compares parameter positions, and constructor visibility is tracked separately from ordinary values.
- Let, letrec, top-level, and imported definitions are generalized; lambda parameters and pattern bindings remain monomorphic within each use site.
- Recursive bindings can recurse polymorphically when they have explicit type annotations; unannotated recursive bindings remain monomorphic.
- `fix : (a -> a) -> a` provides an explicitly typed fixed-point combinator; forcing an unproductive fixed point returns a Kai runtime error.
- `do { ... }` blocks are syntactic sugar for sequencing; entries are separated by `;`, and `do {}` evaluates to `()`.
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
- Prefix chains compose from right to left; `not not true` and `- - 5` are valid. Each numeric negation checks overflow.

## Build and Test

Prereqs: Stack, GHC, Cabal, Python 3, Bash, Make, curl, tar, zip, and unzip. Cabal creates source archives during the packaging tests. macOS ZIP helpers can use Python when `ditto`, `zip`, or `unzip` is unavailable, preserving the archive's executable permissions.

- Build: `stack build`
- Tests: `stack test --fast` (unit, script, property, CLI, REPL, and stress tests)
- On Windows, put Git for Windows' `bin` and `usr/bin` directories before the Windows system directories in `PATH`, so helper subprocesses find Git Bash. Set `KAI_TEST_WEBSITE_HTML` to an exported site's `index.html` from the same source revision. Run `stack test --test-arguments='--skip "Kai runner and installation" --skip "Static site exporter"'`; these two POSIX tooling groups run on Linux and macOS. The native CLI, documentation examples, source/package helpers, and benchmark programs run on all three platforms.
- Run CLI: `stack exec kai -- --help`
- Install the checkout's runner: `make install`; `PREFIX=/path make install` selects an install prefix. The installed symlink resolves the checkout from any working directory. Add its `bin` directory to your shell's `PATH`.
- Runner selection: `KAI_BIN`, then executables on `PATH`, then the active Stack snapshot. Runner copies and symlinks are skipped during `PATH` lookup. Without Stack, the newest local executable is used.
- Run with debug output: `stack exec kai -- --debug -e "42 + 1"`
- Try module-based example: `stack exec kai -- examples/text_analysis.kai`
- Try ADT example: `stack exec kai -- examples/custom_data_types.kai`
- Try interactive calculator: `stack exec kai -- examples/calculator.kai`
- Try directory/env example: `stack exec kai -- examples/file_io.kai /tmp/kai-workspace`
- Run website locally: `stack exec kai-website` (visit http://localhost:3000)

### Test Suite Structure

- Unit tests: `test/*.hs` (Hspec + QuickCheck)
- Script tests: `tests/*.kai` with `// expect:` directives
- Property tests: `PropertyBasedSpec.hs`
- Runner integration: `RunnerSpec.hs` covers executable selection, arguments, exit status, symlinks, and installation in temporary directories.
- Documentation examples: `DocumentationSpec.hs` executes Markdown examples, checks stated results and builtin signatures, and runs the examples rendered by the website. Use `kai` fences for runnable examples and `text` fences for syntax templates and signature references.
- Source archives: `SourceDistributionSpec.hs` checks that Cabal packages all scripts, fixtures, documentation, and website assets. Generate an archive with `stack sdist`.

Every repository `.kai` file requires `// expect:`. All scripts under `tests/`
and `test/`, including nested module fixtures, execute through `kai --check`.
`// expect-type:` is an additional assertion, never a substitute for execution.
Use a pure Kai expression for the expected value, or `error DivByZero` (or another
exact runtime error rendering) for an expected error. Example:

```kai
// expect: "Hello, Ada"
// expect-type: TString
// stdin: "Ada\n"
"Hello, " ++ input
```

`// stdin:` is a JSON string supplied by the test/release harness. The CLI itself
reads actual stdin. Without a fixture, the harness supplies EOF. The release
corpus runner applies a 30-second timeout per file and rejects missing or duplicate
expectations. Runnable examples have separate smoke tests with real IO fixtures.
Output capture uses exception-safe handle restoration and temporary files, so
progress formatting, EOF, and large output cannot corrupt assertions.

Run `make test` to exercise the progress formatter, and use
`stack test --test-arguments="--qc-max-success=1000 --seed=42"` for a repeatable
expanded property run.

Run subsets:
- `stack test --test-arguments "--match Arithmetic"`
- `stack test --test-arguments='--match "Script files"'`

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

# Fast CI-equivalent validation of every benchmark program
stack bench --benchmark-arguments="--iters 1"

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

### Performance Baselines

Use `nf work input` (or `W.func label work input` for allocation measurements).
A closure such as `nf (\() -> work constant) ()` can share a cached result and
produce invalid interpreter timings.

All benchmark helpers fail immediately when their Kai input cannot be parsed,
evaluated, or type-checked. CI runs every benchmark with one iteration as a
validity gate. Performance comparisons still require before/after runs on the
same machine and build profile.

## Linting & Style

- HLint: `hlint .`
  - Examples already applied: `void (symbol "()")`, avoid trivial lambdas in operator table, use `Right . VStr <$> getLine` over unnecessary Haskell do-notation.
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

- CI runs native macOS and Windows language tests, benchmark checks, and package checks on `validation/` branches and manual dispatch, with read-only repository permissions. Documentation examples use the site exported by the Linux job from the same commit. Each native job builds, packages, extracts, and executes the CLI.
- Bump version in `package.yaml` (hpack regenerates `.cabal`).
- Update README header and website version display.
- Push the synchronized `package.yaml` version bump to `master`; it automatically starts the release workflow. Manual dispatch remains available for retries. The workflow builds permission-preserving platform packages, writes `SHA256SUMS`, creates a draft, verifies the exact downloads natively, and only then publishes the release.
- Release runners are pinned to Ubuntu 22.04, macOS 15 ARM64 with `MACOSX_DEPLOYMENT_TARGET=11.3`, and Windows Server 2022.
- Set `APPLE_SIGNING_ENABLED=true` only after configuring `APPLE_DEVELOPER_ID_P12_BASE64`, `APPLE_DEVELOPER_ID_P12_PASSWORD`, `APPLE_SIGNING_IDENTITY`, `APPLE_NOTARY_KEY_P8_BASE64`, `APPLE_NOTARY_KEY_ID`, and `APPLE_NOTARY_ISSUER_ID`.
- Set `WINDOWS_SIGNING_ENABLED=true` only after configuring `WINDOWS_CERTIFICATE_PFX_BASE64` and `WINDOWS_CERTIFICATE_PFX_PASSWORD`.
- Signing paths fail closed when enabled. Without those variables, releases have no Developer ID/notarization or Authenticode trust signature and must not be described as trusted-platform signed.

## Website Updates

- Edit `website/Main.hs` for content changes (features, version, examples).
- Run `stack exec kai-website` to serve locally. Static export via `scripts/export-site.sh`.

## Recent Major Improvements (v0.0.3.2+)

- **Type annotations**: Optional Haskell-style type annotations for lambdas and let bindings (`\x : Int -> expr`, `let x : Int = val`)
- **Error handling system**: Full Maybe/Either types with pattern matching for graceful error handling instead of runtime crashes
- **Safe conversion functions**: `parseInt : String -> Maybe Int` returns `Nothing` for invalid input instead of crashing
- **Case expressions**: Pattern matching for handling Maybe/Either and other data types safely
- **Do blocks**: `do { print "hello"; 42 }` gives Kai a readable sequencing form for effectful scripts
- **Wildcard variables**: `_` is still available in let bindings when explicit discard is the clearest fit (`let _ = expensiveCall in body`)
- **Expression sequencing**: `;` remains the underlying sequencing operator, with `do` blocks as the ergonomic surface form
- **Interactive I/O**: `input` function reads from stdin, enabling interactive applications like the calculator example
- **Recursion fixes**: Fixed critical evaluator bug preventing infinite recursion with IO operations
- **Performance fixes**: Eliminated infinite loops in deeply nested expressions (1000+ levels) through parser and type checker optimizations
- **Clean CLI**: Debug output hidden by default, use `--debug` flag when needed for development
- **Comprehensive testing**: Tests span unit, properties, asserted scripts, CLI, REPL, 1000-level stress, and example smoke coverage

## Notes / TODOs

- **For each release**: Keep package, docs, website, tests, and benchmark validation synchronized, then push the `package.yaml` version bump to `master`. The release workflow starts automatically and publishes only after every validation gate passes.
- **Development focus**: REPL polish (`history`, `completion`, better diagnostics)
- **Development focus**: Fill stdlib gaps that matter for scripts (line-oriented file helpers, JSON/HTTP, a few missing utilities)
- **Defer by default**: Package manager, formatter/linter/LSP, and full polymorphic-recursion inference or other advanced type-system work unless scripting ergonomics are already in good shape
- When changing semantics, align README.md, SPEC.md, website, and DEVELOPING.md immediately.
- Always verify that stress tests pass after performance-critical changes.
- Cross-platform support: Conditional dependencies for Windows compatibility
