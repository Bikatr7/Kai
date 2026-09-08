# Developing Kai 0.0.4.6

This document helps contributors work on Kai’s codebase efficiently.

## Next Release: 0.0.5.0

Follow [the release design](RELEASE-0.0.5.0.md) for the five required language
changes: structured error recovery, short-circuit booleans, ordinary builtin
application, record/concatenation inference, and static equality/pattern checks.
Source-aware diagnostics, safe input/list helpers, migration examples, and native
acceptance tests are included. REPL history/completion and broader stdlib work
follow these requirements.

The design specifies implementation order and acceptance coverage. Keep current
semantics documented until their implementation and tests change together. Future
syntax belongs in `text` fences in the design; executable `kai` examples in the
language docs must work with the documented interpreter version. Retain existing
tests, updating expectations only for deliberately changed semantics and adding
the corresponding migration case.

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
- **Builtins.hs**: Legacy zero-argument expression parsing; callable names use ordinary variable/application parsing
- **Expressions.hs**: Main expression parser with operator precedence
- **Source.hs**: Physical source lines, original excerpts and source-preserving top-level chunks
- **Parser.hs**: Public interface coordinating all parsing components; located entry points serve CLI, modules and REPL

#### Type Checker (`src/TypeChecker/`)
- **Types.hs**: Core type definitions and syntax-type conversions
- **Substitution.hs**: Type variable substitution with composition optimization
- **Unification.hs**: Unification and open-row reconciliation with occurs/kind checks
- **Constraints.hs**: Retained `Eq`/`Append` predicates, annotation contexts, narrow equality defaulting, and terminating custom-data comparability analysis
- **Literals.hs**: Literal and variable type inference
- **Arithmetic.hs**: Arithmetic operator type checking
- **ControlFlow.hs**: Control flow type checking
- **Functions.hs**: Function and lambda type checking
- **Bindings.hs**: Let/letrec binding type checking
- **DataStructures.hs**: List, record, tuple type checking
- **Operations.hs**: Built-in operation type checking
- **Helpers.hs**: Shared operand inference and fixed-signature constraints, with substitutions passed to later operands
- **Patterns.hs**: Pattern type checking
- **Coverage.hs**: Exhaustiveness witnesses and unreachable alternatives using constructor-pattern matrices
- **Warnings.hs**: Human-readable warning output on stderr
- **Inference.hs**: Main type inference dispatcher
- **TypeChecker.hs**: Shared program/definition inference used by files, module exports, and the REPL; one fresh-variable supply per recursive block

#### Evaluator (`src/Evaluator/`)
- **Types.hs**: Runtime value definitions with NFData for benchmarking
- **Helpers.hs**: Shared error/effect adapter and value-formatting utilities
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

Pure and I/O evaluation share the implementations of arithmetic, booleans,
conversions, strings, collections, pattern matching, and function application.
The I/O adapter uses `ExceptT` to preserve evaluation order and stop at the first
runtime error. Recursive reference allocation and external I/O remain specific
to the I/O evaluator. Module export filtering is shared by scripts, imports,
and the REPL.

#### Diagnostics (`src/Diagnostics.hs`)
- Normal errors show source file, line, column and excerpt, plus relevant function/import context.
- `Located` expressions and `TLAt` declarations carry source spans. Legacy parser entry points retain their unannotated AST contract.
- Structured type/runtime errors remain available to library callers and `--debug`. Script error expectations strip only source context before comparing the exact underlying error.
- `SourceLocationSpec.hs` checks comments, CRLF, Unicode, tabs, imported/retained definitions, type grouping and 1000-level source preservation.

#### Text Files (`src/UTF8.hs`, `src/SourceIO.hs`)
- Source files and `readFile`/`writeFile`/`appendFile` use UTF-8 independently of the host locale.
- Reads force decoding before closing the handle so decoding failures reach the caller's IO error handler.
- `UTF8Spec.hs` covers source loading, imports, REPL loads, exact file bytes, invalid sequences, and file operations under a legacy locale.

#### CLI (`src/CLI.hs`, `app/Main.hs`)
- Command-line interface with expression evaluation, file execution, and REPL entry
- Debug mode, clean output by default, argument passing support, and package-derived `--version`/`-V` output
- Non-zero exit codes for parse, type, runtime, and output failures; diagnostics fall back to stderr if stdout fails
- `src/REPL.hs`: multiline REPL with `:type`, `:load`, `:reload`, and persistent environments
- `app/Main.hs` links the interpreter library used by the tests; language modules compile once.
- `website/`: Yesod-based static site generator used for the project website/demo.

## Language Semantics (current)

- Evaluation: strict (call-by-value).
- Integers: signed 32-bit values; literals, `parseInt`, and arithmetic results enforce the range, and arithmetic overflow returns `IntegerOverflow`.
- Unit: `()` value with type `TUnit`.
- `print : a -> Unit` prints, flushes stdout, and returns `()`. Write or flush failures raise a structured `IOError` and stop later effects inside the action.
- `input : String` reads a line from stdin.
- `args : [String]` returns command-line arguments passed to the script or REPL session.
- File and directory I/O: `readFile`, `writeFile`, `appendFile`, `fileExists`, `listDirectory`, `createDirectory`, `removeDirectory`, `getCurrentDirectory`, `setCurrentDirectory`.
- Process/environment access: `system`, `getEnv`, `setEnv`, `exit`.
- A leading shebang line (`#!/usr/bin/env kai`) is ignored when parsing files.
- Error handling: `attempt : (Unit -> a) -> Either Error a` catches recoverable failures; `raise : Error -> a` raises or rethrows them. Effects already performed remain.
- `readLine ()` returns `Nothing` at EOF and `Just ""` for blank input; `headMaybe` and `tailMaybe` return optional list results.
- Functions may perform I/O regardless of their input/output type. Kai does not enforce purity.
- Safe conversion functions: `parseInt : String -> Maybe Int`, `toString : Int -> String`, `show : a -> String`.
- List functions: `map`, `filter`, `foldl`, `length`, `reverse`, `take`, `drop`, `zip`.
- String functions: `split`, `join`, `trim`, `replace`, `strLength`.
- Tuple functions: `fst`, `snd` for pairs.
- Custom data types: top-level `data` declarations produce constructor functions and constructor patterns.
- Equality: `Eq a` constraints reject callable payloads statically, including private or nested fields. Comparable data use structural equality. Direct evaluator callers retain defensive runtime checks.
- Concatenation: retained `Append a` constraints allow generic string/list helpers without defaulting unknown types. Qualified annotations must include required contexts.
- Records: accessors infer open rows; literals and closed annotations retain exact fields. Row kinds, duplicate labels, missing fields, and infinite rows are checked.
- Type annotations: Optional Haskell-style annotations for lambdas and let bindings. Variables are fresh for each annotation; repeated variables within one annotation remain tied.
- Constructor patterns require every field, and cases must be exhaustive. Coverage checks combinations of nested payloads and terminates on recursive ADTs. Private constructors require a catch-all without leaking their names.
- Unreachable alternatives warn on stderr. The warnings travel through definition inference and module loading; the REPL reports each prechecked input once.
- Imported declaration compatibility compares parameter positions, and constructor visibility is tracked separately from ordinary values.
- Let, letrec, top-level, and imported definitions are generalized; lambda parameters and pattern bindings remain monomorphic within each use site.
- Recursive calls can use different type instantiations when the binding has an explicit annotation. Without an annotation, calls within its recursive group are monomorphic; the completed definition can still be generalized for later uses.
- `fix : (a -> a) -> a` provides an explicitly typed fixed-point combinator; forcing an unproductive fixed point returns a Kai runtime error.
- `do { ... }` blocks are syntactic sugar for sequencing; entries are separated by `;`, and `do {}` evaluates to `()`.
- Strings: escapes `\"`, `\\`, `\n`. Unknown escapes are errors with a helpful message.
- Precedence (highest to lowest):
  1) field access `.field`
  2) application (left)
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
- Field access binds tighter than application: `f x.field` means `f (x.field)`, while `record.fn x` means `(record.fn) x`. Write `(f x).field` to select from the result.
- Prefix chains compose from right to left; `not not true` and `- - 5` are valid. Each numeric negation checks overflow.
- Boolean operators short-circuit: `false and rhs` and `true or rhs` skip the right operand. Both operands are statically checked. Only the selected `if` branch executes.
- File and `-e` execution print only explicit output. The REPL displays expression results; `--debug` also displays evaluation details.

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
- Property tests: `PropertyBasedSpec.hs` and feature-specific specs
- Evaluator consistency: `EvaluatorConsistencySpec.hs` compares pure and I/O results against explicit expectations, checks operand effects in source order, and injects failures at each operand to ensure later effects stop.
- Runner integration: `RunnerSpec.hs` covers executable selection, direct OS shebang launch, arguments, exit status, symlinks, and installation in temporary directories.
- Recovery example: `file_report.kai` accepts arguments or EOF-terminated path input, handles individual read failures and prints independently checked totals. `ExampleSpec.hs` asserts missing/invalid UTF-8 files, later successes, blank input, repeated paths and exact output.
- Example programs: `ExampleSpec.hs` checks exact output, interactive input, command-line arguments, and file effects. `ExampleAssertionsSpec.hs` checks every example file's value/type directives in isolated copies, rejects deliberately incorrect directives, and calls every example-library export with normal and boundary inputs, including each module copy.
- Documentation examples: `DocumentationSpec.hs` executes `kai` fences in README, SPEC, DEVELOPING, FEATURES, and MIGRATING-0.0.5.0, checks `// =>` result claims and SPEC builtin signatures, and runs rendered website examples. Unannotated examples check successful execution; they do not assert a final value or exact output. Use `text` fences for syntax templates and signature references. Examples in agent guides and benchmark commands need their own checks when edited.
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
reads actual stdin. Without a fixture, the harness supplies EOF. The harness also
checks `// stdout:` as an exact JSON string, including whitespace and newlines;
without it, the script must produce no output. A printing script needs both its
return value and its output checked:

```kai
// expect: ()
// stdin: "World\n"
// stdout: "Hello, World!\n"
print ("Hello, " ++ input ++ "!")
```

Here `()` is the return value of `print`; the greeting is asserted separately.
Both `stack test` and `scripts/check-script-corpus.py` enforce output assertions.
`kai --check` verifies the value and type while leaving output capture to the harness.
The release corpus runner applies a 30-second timeout per file. Fixture directives
must be unique and nonempty; stdin and stdout fixtures must be valid JSON strings.
The Haskell and Python runners share convention cases in
`test/fixtures/script_directives.json`. Documentation examples with asserted results
use the same output rules. Runnable example programs have separate tests that check
their full output with real IO fixtures.
Output capture uses exception-safe handle restoration and temporary files, so
progress formatting, EOF, and large output cannot corrupt assertions.

Use `TestSupport.parseExpression`, `evaluateSource`, `evaluateCheckedSource`, and
`inferSource` for valid test source. Malformed fixtures fail the test instead of
becoming a simulated language error. For negative parser tests, call `parseExpr`
directly. Assert specific type/runtime errors and use `shouldInfer` when fresh
type-variable names can vary; it checks the complete type structure.
Properties should compare against independently expected values and generate
numbers within Kai's signed 32-bit range unless testing overflow. Keep filesystem
fixtures in temporary directories and restore changed handles, directories, and
environment variables after exceptions.

Run `make test` to exercise the progress formatter, and use
`stack test --test-arguments="--qc-max-success=1000 --seed=42"` for a repeatable
expanded property run.

To save a result for every Hspec case, set `KAI_TEST_REPORT` to a writable JSONL
file. Each record includes the suite, case name, source location, duration, and
pass/fail/pending status:

```bash
KAI_TEST_REPORT=/tmp/kai-tests.jsonl stack test --test-arguments='--qc-max-success=1000 --seed=42 --strict --fail-on=empty --ignore-dot-hspec'
```

Use the default formatter for that command; an explicit `--format` selects a
different Hspec formatter. To check whether script assertions reject incorrect
results, output, types, and input, run the corpus assertion checker:

```bash
python3 scripts/audit-script-tests.py "$(stack path --local-install-root)/bin/kai" tests --json /tmp/kai-script-checks.json
```

It runs the originals and modified copies in a temporary directory. A surviving
incorrect variant or an unrelated failure makes the check fail. The script
regressions cover both useful and ineffective assertions. On Windows, use the
installed `kai.exe` path and the platform test exclusions described above.

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

# Run every benchmark once, as CI does
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
- **Use CSV output** for before/after comparisons; the current CI does not enforce timing thresholds

### Performance Baselines

Use `nf work input` (or `W.func label work input` for allocation measurements).
A closure such as `nf (\() -> work constant) ()` can share a cached result and
produce invalid interpreter timings.

Parser-suite, evaluator, and type-checker helpers fail on invalid inputs. The
small-input and allocation parser cases call `parseExpr` directly and measure its
result. CI runs every case once to catch execution failures; it does not compare
returned values or enforce performance thresholds. See `benchmarks/README.md`
for forcing depth and phase boundaries. Timing comparisons require before/after
runs on the same machine and build profile.

## Linting & Style

- HLint: `hlint .`
  - Use simple combinators where they preserve behavior, such as `void (symbol "()")`. Keep explicit IO error handling and handle cleanup when simplifying effectful code.
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
   - AGENTS.md and its CLAUDE.md/GEMINI.md entry points (contributor instructions)

## Versioning & Release

- CI runs native macOS and Windows language tests, benchmark checks, and package checks on `validation/` branches and manual dispatch, with read-only repository permissions. Documentation examples use the site exported by the Linux job from the same commit. Each native job builds, packages, extracts, and executes the CLI.
- Prepare the version in `package.yaml` and run `stack build` to regenerate `kai-lang.cabal`. The CLI derives its version from the package.
- Update README, SPEC, FEATURES, this guide, agent instructions, benchmark documentation, and the website version and feature descriptions together.
- When publishing a release, push the synchronized `package.yaml` version bump to `master`; it automatically starts the release workflow. Manual dispatch on `master` remains available for retries. The workflow builds permission-preserving platform packages, writes `SHA256SUMS`, creates a draft, verifies the exact downloads natively, and only then publishes the release. Preparing files locally does not require a tag, push, or workflow dispatch.
- Release runners are pinned to Ubuntu 22.04, macOS 15 ARM64 with `MACOSX_DEPLOYMENT_TARGET=11.3`, and Windows Server 2022.
- Set `APPLE_SIGNING_ENABLED=true` only after configuring `APPLE_DEVELOPER_ID_P12_BASE64`, `APPLE_DEVELOPER_ID_P12_PASSWORD`, `APPLE_SIGNING_IDENTITY`, `APPLE_NOTARY_KEY_P8_BASE64`, `APPLE_NOTARY_KEY_ID`, and `APPLE_NOTARY_ISSUER_ID`.
- Set `WINDOWS_SIGNING_ENABLED=true` only after configuring `WINDOWS_CERTIFICATE_PFX_BASE64` and `WINDOWS_CERTIFICATE_PFX_PASSWORD`.
- Signing paths fail closed when enabled. Without those variables, releases have no Developer ID/notarization or Authenticode trust signature and must not be described as trusted-platform signed.

## Website Updates

- Edit `website/Main.hs` for content changes (features, version, examples).
- Run `stack exec kai-website` to serve locally. Static export via `scripts/export-site.sh`.

## Changes in 0.0.4.6

- First-class and partially applied builtins, typed lambda parameters, and tuple annotations
- Shared recursive inference and source-order initialization for scripts, modules, and the REPL
- Independent annotation variables, exact constructor-pattern arity, duplicate-pattern checks, and preserved private constructors
- Arithmetic spacing, repeated prefix operators, and nested-comment parsing
- UTF-8 source and text files, contained decoding errors, and stdout failure handling
- Exact script value/type/output assertions with real stdin fixtures and tests for ineffective assertions
- Direct shebang execution, runner selection and installation, source-archive contents, and native package checks
- Separate Criterion parent and Weigh child execution, including timed CSV regression coverage

## Notes / TODOs

- **For each release**: Keep package, docs, website, tests, and benchmarks synchronized. Publishing begins with the `master` version-bump push described above.
- **Development focus**: Complete all five language changes and supporting work in [the 0.0.5.0 design](RELEASE-0.0.5.0.md).
- **Next ergonomic follow-up**: REPL history/completion and broader scripting helpers, including JSON/HTTP.
- **Defer by default**: Package manager, formatter/linter/LSP, general type classes/effects, and full polymorphic-recursion inference. Open records and the fixed equality/concatenation constraints belong to the core language.
- When changing semantics, align README.md, SPEC.md, website, and DEVELOPING.md immediately.
- Always verify that stress tests pass after performance-critical changes.
- Cross-platform support: Conditional dependencies for Windows compatibility
