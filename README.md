# Kai Language

A functional-first scripting language with static typing, implemented in Haskell.

Kai aims to be a practical scripting language that's functional by default but allows imperative programming when you really need it. Clean syntax, strong static types, and a pleasant development experience.

## Kai v0.0.4.6

Kai combines type inference, first-class functions, algebraic data types, and
practical scripting tools. Builtins support partial application, recursive
bindings initialize in source order, and scripts and modules use the same
language rules as the interactive REPL.

Features:

- **Expressions**: integers, booleans, strings, parentheses, unary minus
- **Arithmetic**: `+`, `-`, `*`, `/` (integer division, division-by-zero and 32-bit overflow errors)
- **Booleans**: `and`, `or`, `not`
- **Comparisons**: `==`, `<`, `>`
- **Strings**: string literals (`"hello"`), concatenation (`++`), escapes (`\"`, `\\`, `\n`); UTF-8 source files and text-file I/O
- **String functions**: `split`, `join`, `trim`, `replace`, `strLength` for text processing
- **Lists**: `[1, 2, 3]`, concatenation (`++`), equality (`==`), cons (`::`), operations (`head`, `tail`, `null`)
- **List functions**: `map`, `filter`, `foldl`, `length`, `reverse`, `take`, `drop`, `zip` for functional programming
- **Tuples**: `(1, "hello", true)` for grouping values, with `fst` and `snd` for pairs
- **Records**: `{a = 1, b = true}`, field access (`record.field`), equality (`==`)
- **Unit & printing**: `()` unit value; `print : a -> Unit` prints and returns `()`; `input` reads a line of stdin and returns a string
- **File & directory I/O**: `readFile`, `writeFile`, `appendFile`, `fileExists`, `listDirectory`, `createDirectory`, `removeDirectory`, `getCurrentDirectory`, and `setCurrentDirectory`
- **Process & environment access**: `system`, `getEnv`, `setEnv`, and `exit`
- **Command-line arguments**: `args : [String]` returns list of command-line arguments passed to scripts and REPL sessions
- **Conditionals**: `if cond then e1 else e2`
- **Functions**: lambdas (`\x -> expr`), application (`f x`), closures, partially applied builtins, and `fix : (a -> a) -> a`
- **Static typing & inference**: `Int`, `Bool`, `String`, `Unit`, lists, records, tuples, functions, `Maybe`, `Either`, and custom types with unification, occurs check, generalized let-polymorphism, and explicitly annotated polymorphic recursion
- **Type annotations**: Optional type annotations (`let x : Int = 42`, `\x : String -> expr`)
- **Error handling**: Maybe/Either values, structured `Error` constructors, explicit `attempt`/`raise` recovery, and case expressions
- **Safe input and list access**: `readLine ()` distinguishes EOF from a blank line; `headMaybe` and `tailMaybe` return optional values
- **Safe conversion functions**: `parseInt : String -> Maybe Int`, `toString : Int -> String`, `show : a -> String`, `discard : a -> Unit`
- **Pattern matching**: Exhaustive cases for Maybe/Either, tuples, records, lists, and user-defined constructors, with missing-pattern diagnostics and unreachable-branch warnings
- **Custom data types**: Top-level `data` declarations with first-class, partially applicable constructor functions and constructor patterns
- **Do blocks**: `do { expr1; expr2; expr3 }` for readable effect sequencing, with `do {}` evaluating to `()`
- **Wildcard variables**: `_` still works in let bindings when you truly want to discard a value (`let _ = expensiveCall in body`)
- **Expression sequencing**: `;` remains the primitive sequencing operator, with lowest precedence
- **Parser**: Megaparsec with precedence/associativity, reserved keywords, multiline top-level files, and multiline `do` blocks
- **CLI & REPL**: expression/file execution, `--version`/`-V`, plus an interactive REPL with multiline input and `:type`, `:load`, `:reload`, and `:quit`
- **Let bindings**: `let` and `letrec` for variable bindings and recursive functions
- **Top-level definitions**: `let` and `letrec` at module level for defining functions and values
- **Module system**: `import ModuleName` to import modules, module resolution supports `ModuleName.kai` and `ModuleName/ModuleName.kai`, full cross-module type checking, explicit exports with `export name1, name2`
- **Tests**: Hspec + QuickCheck, exact script results and stdout with real stdin fixtures, CLI/REPL and direct shebang coverage, and 1000-level full-pipeline stress cases
- **Working examples**: Module-based text analysis, validated CLI tools, interactive calculator and guessing game, an expression-tree ADT pipeline, list/record processing, text cleanup, directory/env-aware file workflows, wildcard matching, and discard/logging demos

Current limitations:

- REPL is functional but still minimal: no history or completion
- Standard library is broader now, but still missing line-oriented file helpers, JSON/HTTP, and a package story
- Unhandled failures stop a script; explicit `attempt` boundaries allow recovery from runtime I/O, arithmetic, list, and application-defined errors
- Record literals and closed annotations have exact field sets; inferred accessors accept additional fields through open rows
- `show` and `print` are human-readable display, not round-trip serialization
- Polymorphic recursive calls require explicit annotations; completed recursive definitions can still be generalized for later uses

Normal type and runtime diagnostics include source file, line, column and an
excerpt, with relevant function/import context. `--debug` retains structured
internal errors. See [Migrating to 0.0.5.0](MIGRATING-0.0.5.0.md) for changed
behavior and executable migration examples.

## Quickstart

Prerequisites: GHC/Stack via GHCup or your platform’s package manager.
The test suite also uses Cabal, Python 3, Bash, Make, curl, tar, zip, and unzip.
The CLI builds natively on Linux, macOS, and Windows. The website generator and POSIX runner/exporter use Linux or macOS; see `DEVELOPING.md` for the Windows test command.

Build, test, and run:

```bash
stack build
stack test

## Run interpreter
stack exec kai --                # start the REPL
stack exec kai -- --help
stack exec kai -- --version
stack exec kai -- -e 'print ("hi" ++ "!")'
stack exec kai -- -e "print (42 + 1)"
stack exec kai -- --debug -e "42 + 1"

## Run a file
stack exec kai -- path/to/script.kai

## Try practical examples
stack exec kai -- examples/text_analysis.kai
stack exec kai -- examples/calculator.kai

## Website demo (intro page)
stack exec kai-website  # visit http://localhost:3000
PORT=4000 stack exec kai-website  # optional website port override
```

File execution and `-e` print only explicit output. Use `print` to display a
value; the REPL displays expression results automatically.

Install the CLI (no explicit `stack` needed):

- `make install` links the runner to this checkout, so it works from other directories. Keep the checkout in place, or use `stack install` for a standalone binary. Add the install directory to your shell's `PATH` as shown below.
- Set `KAI_BIN` to select an executable explicitly. Otherwise the runner searches `PATH`, then the active Stack build. Without Stack, it selects the newest local build.

```bash
make install              # installs to ~/.local/bin/kai by default
export PATH="$HOME/.local/bin:$PATH"  # if not already set

# Now you can run Kai directly
kai path/to/script.kai
```

Prebuilt packages (CI Releases):

- To publish a release after synchronizing the version, documentation, and tests, push the `package.yaml` version bump to `master`. That change starts **Build and Release Binaries** automatically; manual dispatch on `master` remains available for retries. The workflow validates the release candidate, creates a draft, then downloads and tests each exact package on its native runner before publishing it.
- Release packages are named `kai-linux-amd64.tar.gz`, `kai-macos-arm64.zip`, and `kai-windows-amd64.zip`. Verify downloaded packages against `SHA256SUMS`.
- Extract the package and place `kai` (or `kai.exe`) on your `PATH`. Linux and macOS archives preserve the executable bit.
- Release CI executes the exact packages on Ubuntu 22.04 x64, Apple Silicon macOS 15, and Windows Server 2022 x64. Linux is built and tested on Ubuntu 22.04 (glibc 2.35), and the macOS binary declares an 11.3 deployment target; compatibility with other matching systems is expected but not directly exercised by this workflow.
- macOS Developer ID/notarization and Windows Authenticode signing are opt-in workflow paths that require their repository variables and certificate secrets; see `DEVELOPING.md` for configuration.
- From source, `stack install` also produces a native binary in your local Stack install path.

Export a static site bundle:

- Generate `dist-site/` with an `index.html` and static assets you can open locally or deploy to GitHub Pages/Netlify. The exporter verifies its own temporary server, so another local service cannot be captured accidentally.

```bash
bash scripts/export-site.sh
## open dist-site/index.html in a browser
```

Script samples in tests:

- `stack test` also discovers `.kai` files under `tests/` and `test/`, evaluates them, and shows each file’s result in the test output under two sections.
- Add your own `.kai` script with `// expect:` to those folders to have it run automatically. Printing tests also need an exact `// stdout:` assertion; input fixtures use `// stdin:`. See [Testing notes](#testing-notes).

## Examples

Arithmetic, booleans, conditionals:

```kai
42 * (10 - 3)
true and not false
not not true  // => true
if 10 > 5 then 84 else 0
```

Strings and printing:

```kai
"Hello, " ++ "World"
print ("The answer is " ++ "42")  // returns ()
print (if 5 > 3 then "yes" else "no")
```

Do blocks, sequencing, and wildcards:

```kai
do {
  print "Setting up...";
  print "Processing...";
  42
}  // Result: prints setup messages, returns 42

print "First"; print "Second"; print "Done"

let x = 10 in do {
  print ("x is " ++ (toString x));
  x * 2
}  // Prints message, returns 20

discard (show [1, 2, 3]); 99
// Explicitly ignore a non-Unit result when needed
```

Lambdas and application:

```kai
(\x -> x + 1) 41      // => 42
(\f -> f 10) (\n -> n * 2)  // => 20
```

Interactive input and conversions:

```kai
let name = input in
print ("Hello, " ++ name)

let numStr = input in
case parseInt numStr of
  Just num -> print ("Double: " ++ toString (num * 2))
  | Nothing -> print "Invalid number"
```

Type annotations and conversions:

```kai
let add : Int -> Int -> Int = \x : Int -> \y : Int -> x + y in
case parseInt "10" of
  Just n -> add 5 n
  | Nothing -> 0

letrec nestedLayers : Int -> [a] -> Int = \depth -> \xs ->
  if depth == 0 then length xs else 1 + nestedLayers (depth - 1) [xs]

nestedLayers 2 [1, 2, 3]  // => 3

show (42 + 3)        // => "45"
toString 100         // => "100"
parseInt "42"        // => Just 42
```

Recovering from runtime failures:

```kai
case attempt (\unit -> 1 / 0) of
  Left DivisionByZero -> 42
  | Left other -> raise other
  | Right value -> value  // => 42
```

`attempt` takes a function so the operation executes inside the recovery boundary.
It returns `Either Error a`; wrapping an operation in `Right` alone cannot catch a
failure. Earlier effects remain, while later effects inside a failing action are
skipped. `exit`, cancellation, parse/type errors, and interpreter faults do not
become recoverable values.

Lists, tuples, and records:

```kai
[1, 2] ++ [3, 4]        // => [1, 2, 3, 4]
head([1, 2, 3])         // => 1
tail([1, 2, 3])         // => [2, 3]
null([])                // => true
1 :: [2, 3]             // => [1, 2, 3]
(1, "hello", true)      // Tuple with three values
fst((42, "world"))      // => 42
snd((42, "world"))      // => "world"
{a = 1, b = true}.a    // => 1
{a = 1} == {a = 1}      // => true
```

Reusable record and constrained helpers:

```kai
let total = \record -> record.a + record.b in
total {a = 1, b = 2, extra = true}  // => 3

let append = \x -> \y -> x ++ y in
(append "a" "b", append [1] [2])  // => ("ab", [1, 2])

let same : Eq a => a -> a -> Bool = \x -> \y -> x == y in
same [1, 2] [1, 2]  // => true
```

`Append a` permits strings and lists; `Eq a` requires comparable data. These
requirements survive aliases, recursive definitions, modules, and REPL inputs.
An explicit open record annotation is `{a : Int | row}`; `{a : Int}` remains closed.

List and string functions:

```kai
map (\x -> x * 2) [1, 2, 3]           // => [2, 4, 6]
filter (\x -> x > 2) [1, 2, 3, 4]     // => [3, 4]
foldl (\acc -> \x -> acc + x) 0 [1, 2, 3]  // => 6
zip [1, 2, 3] ["a", "b", "c"]         // => [(1, "a"), (2, "b"), (3, "c")]
split " " "hello world"               // => ["hello", "world"]
join ", " ["apple", "banana"]         // => "apple, banana"
trim "  hello  "                      // => "hello"
```

Top-level definitions and modules:

```kai
// Math.kai - A simple math module
let add = \x -> \y -> x + y
let multiply = \x -> \y -> x * y

// Main.kai - Using the module
import Math
add 2 3        // => 5
multiply 4 5    // => 20

// Top-level recursive function
letrec factorial = \n -> if n == 0 then 1 else n * factorial (n - 1)
factorial 5     // => 120

// Multiple top-level definitions
let x = 10
let y = 20
x + y           // => 30
```

Runnable example scripts in `examples/`:

- `examples/text_analysis.kai`: modules, records, `Maybe`, and file-or-stdin-style scripting with `args`
- `examples/file_report.kai`: per-file recovery, exact success/failure totals, and EOF-driven path input
- `examples/file_counter.kai`: `Either`-based CLI validation plus reusable text-analysis helpers
- `examples/list_processing.kai`: lists, records, `zip`, and a let-polymorphic tagging helper
- `examples/custom_data_types.kai`: an expression-tree pipeline with constructor functions, recursive simplification, and constructor patterns
- `examples/calculator.kai` and `examples/guess_the_number.kai`: interactive input with typed parsing and recursion
- `examples/file_io.kai`, `examples/text_processing.kai`, `examples/wildcard_patterns.kai`, and `examples/discard_demo.kai`: directory/env-aware file scripting, `appendFile`/`fileExists`, text cleanup, wildcard matching, and discard-based logging

Reusable example modules live in `examples/MathUtils.kai`, `examples/StringUtils.kai`, and `examples/TextAnalysis.kai`.

Shebang support for executable scripts:

```kai
#!/usr/bin/env kai
// Make this file executable with: chmod +x script.kai
// Then run it directly: ./script.kai

print "Hello from executable Kai script!"
```

File I/O and command-line arguments:

```kai
// Write to a file
do {
  writeFile "output.txt" "Hello, world!";
  print "File written"
}

// Read from a file
let content = readFile "input.txt" in
print content

// Access command-line arguments (run with: kai script.kai arg1 arg2)
let firstArg = head args in
print ("First argument: " ++ firstArg)

// In a REPL session, start with: kai repl arg1 arg2
```

Type safety (checked before evaluation):

```kai
1 + true         // Type error: Cannot match Bool with Int.
if 5 then 1 else 2  // Type error: Cannot match Int with Bool.
```

## Performance Benchmarks

Kai includes comprehensive benchmarks for speed and memory usage:

### Running Benchmarks

```bash
# Run all benchmarks
stack bench

# Run every benchmark once (also runs in CI)
stack bench --benchmark-arguments="--iters 1"

# Run specific benchmark categories
stack bench --benchmark-arguments="--match pattern 'Evaluator'"
stack bench --benchmark-arguments="--match pattern 'Parser'"

# Generate CSV output for analysis
stack bench --benchmark-arguments="--csv=results.csv"
```

### Performance Baselines

Compare timings on the same machine and build profile. Most speed cases use
`nf work input`; the small-input latency cases use `whnf`. The one-iteration CI
run catches execution failures but does not assert exact results or a performance
threshold. Evaluator and type-checker cases include parsing. Full benchmark runs
produce timing measurements; Weigh reports allocations and garbage collections.

See `benchmarks/README.md` for detailed benchmark documentation and regression testing guidelines.

## Language Notes

- Keywords such as `if`, `let`, `letrec`, `do`, `case`, `import`, and `export` are reserved. Callable builtins and constructors use ordinary names; see `SPEC.md` for the exact keyword list.
- Wildcard variable `_` can be used in let bindings and pattern matching to discard values: `let _ = expression in body`, `case x of _ -> "any" | Just val -> "some"`.
- `do { ... }` is the idiomatic way to sequence effects. Entries are separated by semicolons, and `do {}` evaluates to `()`.
- Expression sequencing with `;` has lowest precedence and is right-associative: `a; b; c` = `a; (b; c)`.
- Unary minus is a prefix operator (e.g., `-5`, `10 - (-3)`). Use parentheses for signed function arguments: `f (-1)`; `7 -2` is subtraction.
- Prefix operators may repeat: `not not true` and `- - 5`. They apply from right to left, with integer overflow checked at each negation.
- Builtins can be stored or partially applied: `let f = take 2 in f [1,2,3]`. Builtins use ordinary application, as in `map length [[1],[2,3]]`, and may be shadowed by local definitions.
- Recursive bindings allow constants and closures. Reading a recursive binding before initialization returns `UninitializedRecursion`.
- Type variables are local to each annotation; their spelling does not connect separate annotations. Constructor patterns require every declared field.
- Cases must cover every possible input of their scrutinee type. Unreachable alternatives warn on stderr. Parenthesize a nested `case` before adding alternatives to its enclosing case.
- Imported data declarations compare parameter positions and payload types, independent of parameter spelling. Private constructors remain private.
- Integer literals, `parseInt`, and arithmetic results are constrained to signed 32-bit values. Arithmetic overflow raises `IntegerOverflow`.
- Equality is structural for comparable data. Inference retains `Eq a` requirements on reusable functions and rejects callable payloads before execution, including nested or private constructor fields. Different constructors compare as `false`.
- Concatenation (`++`) works for both strings and lists, right-associative, with lower precedence than `+`/`-`: `"a" ++ "b" ++ "c"` parses as `"a" ++ ("b" ++ "c")`, `[1, 2] ++ [3, 4]` parses as `[1, 2] ++ [3, 4]`.
- Supported string escapes: `\"`, `\\`, `\n`. Unknown escapes are errors.
- `print` evaluates its argument, prints and flushes it, and returns unit `()`. An output failure raises a structured `IOError` and stops subsequent effects inside the current action.
- Field access binds tighter than application, which binds tighter than prefix and infix operators. `f x.field` means `f (x.field)`; use `(f x).field` to access the result.
- `and` and `or` short-circuit: `false and rhs` and `true or rhs` skip `rhs`. Both operands must still type-check as booleans.
- Multi-statement files are supported: top-level newlines split expressions, while nested `()`, `[]`, `{}`, strings, and comments stay intact.

## Project Structure

```
.
├── src/                           ## Language implementation (modular architecture)
│   ├── Syntax.hs                  ## AST definitions with NFData instances for benchmarking
│   ├── Parser/                    ## Modular parser components
│   │   ├── Lexer.hs              ## Lexical analysis and reserved keywords
│   │   ├── Literals.hs           ## Basic literal parsing (numbers, strings, booleans)
│   │   ├── Types.hs              ## Type annotation parsing
│   │   ├── Patterns.hs           ## Pattern matching parsing
│   │   ├── ComplexExpr.hs        ## Complex expressions (lambdas, conditionals, bindings)
│   │   ├── Builtins.hs           ## Built-in function parsing
│   │   └── Expressions.hs        ## Main expression parsing with operator precedence
│   ├── Parser.hs                 ## Public parser interface
│   ├── TypeChecker/               ## Modular type checker
│   │   ├── Types.hs              ## Core type definitions and conversions
│   │   ├── Substitution.hs       ## Type variable substitution
│   │   ├── Unification.hs        ## Unification algorithm with occurs check
│   │   ├── Literals.hs           ## Literal and variable type inference
│   │   ├── Arithmetic.hs         ## Arithmetic operator type checking
│   │   ├── ControlFlow.hs        ## Control flow type checking
│   │   ├── Functions.hs          ## Function type checking
│   │   ├── Bindings.hs           ## Let/letrec binding type checking
│   │   ├── DataStructures.hs     ## Data structure type checking
│   │   ├── Operations.hs         ## Built-in operation type checking
│   │   ├── Helpers.hs            ## Shared operand inference and constraints
│   │   ├── Patterns.hs           ## Pattern type checking
│   │   └── Inference.hs          ## Main type inference dispatcher
│   ├── TypeChecker.hs            ## Public type checker interface
│   ├── Evaluator/                ## Shared operations for pure and I/O evaluation
│   │   ├── Types.hs              ## Runtime value definitions with NFData
│   │   ├── Helpers.hs            ## Utility functions for evaluation
│   │   ├── Literals.hs           ## Literal evaluation
│   │   ├── Arithmetic.hs         ## Arithmetic evaluation (pure & IO)
│   │   ├── BooleanOps.hs         ## Boolean logic evaluation (pure & IO)
│   │   ├── ControlFlow.hs        ## Control flow evaluation (pure & IO)
│   │   ├── Functions.hs          ## Function evaluation (pure & IO)
│   │   ├── Bindings.hs           ## Binding evaluation (pure & IO)
│   │   ├── DataStructures.hs     ## Data structure evaluation (pure & IO)
│   │   ├── StringOps.hs          ## String operation evaluation (pure & IO)
│   │   ├── Conversions.hs        ## Type conversion evaluation (pure & IO)
│   │   ├── IOOps.hs              ## I/O operation evaluation
│   │   ├── Program.hs            ## Script and module execution
│   │   ├── Recursion.hs          ## Recursive binding initialization
│   │   └── Patterns.hs           ## Pattern matching evaluation (pure & IO)
│   ├── Evaluator.hs              ## Public evaluator interface
│   ├── DataDeclarations.hs       ## User-defined data type environments
│   ├── TopLevelRecursion.hs      ## Recursive top-level binding support
│   ├── ModuleSystem.hs           ## Module loading and import resolution
│   ├── REPL.hs                   ## Interactive session implementation
│   ├── CLI.hs                    ## CLI runner and exit-code handling
│   ├── ScriptCheck.hs            ## Script result and type assertions
│   ├── SourceIO.hs               ## Source-file decoding and read errors
│   └── UTF8.hs                   ## UTF-8 text-file reads and writes
├── app/
│   └── Main.hs                   ## Thin `kai` entry point linking the library
├── benchmarks/                    ## Performance benchmarking suite
│   ├── Bench.hs                  ## Main benchmark orchestrator
│   ├── ParserBench.hs            ## Parser performance benchmarks
│   ├── EvaluatorBench.hs         ## Evaluator performance benchmarks
│   ├── TypeCheckerBench.hs       ## Type checker performance benchmarks
│   └── README.md                 ## Benchmark documentation and guidelines
├── test/                         ## Hspec/QuickCheck test suite (.hs specs)
├── tests/                        ## Sample Kai scripts (.kai) evaluated by tests
├── website/                      ## Yesod web application
│   └── static/                   ## Website assets (favicon, css)
├── scripts/                      ## Helper scripts (runner, export-site)
├── dist-site/                    ## Static site export (generated by `make site`)
├── Makefile                      ## install/build-install/uninstall/test/site targets
├── package.yaml                  ## Project config (library + exes + tests + benches)
├── kai-lang.cabal                ## Generated from package.yaml (hpack)
├── stack.yaml                    ## Stack configuration
└── README.md
```

## Vision: Kai as a Functional-First Scripting Language

Design philosophy:

- **Functional by default**: Immutable data, pure functions, expressions over statements
- **Imperative when needed**: Escape hatches for I/O, performance, or when it's genuinely clearer
- **Static first**: Strong, predictable types with great error messages and inference
- **Scriptable**: Fast edit‑run cycle, ergonomic CLI, shebang support, no compilation step
- **Practical**: Batteries-included standard library for real-world scripting tasks

Roadmap:

**Next release: 0.0.5.0 — error recovery and predictable language behavior.**
The [release design](RELEASE-0.0.5.0.md) defines the semantics, migration work,
implementation order, and acceptance requirements.

- Structured errors with explicit recovery through `attempt`/`raise`, plus safe
  line input and optional list accessors
- Short-circuit `and`/`or` and consistent builtin function application
- Open record inference and reusable constrained concatenation helpers
- Static equality constraints and exhaustive pattern checking
- Source-aware diagnostics, migration examples, and native package validation

These are targets for 0.0.5.0. The language reference and examples above describe
0.0.4.6. Function types describe inputs and outputs; they do not enforce purity.

REPL history/completion follows this language work. JSON/HTTP, formatter/linter
and editor support, package management, module-qualified types, wider numbers,
and a new execution engine remain later projects. Open records and the built-in
`Eq`/`Append` constraints are in scope; general type classes, effect types, and
other advanced type-system features remain deferred.

Example current Kai script style:

```kai
#!/usr/bin/env kai

let processLines = \text -> filter (\line -> not (trim line == "")) (map (trim) (split "\n" text))
let greet = \name -> if name == "" then "Hello, world!" else "Hello, " ++ name

let inputText = readFile "input.txt"
let processed = processLines inputText
let greeting = case processed of name :: _ -> greet name | [] -> greet ""

do {
  writeFile "output.txt" greeting;
  print greeting
}
```

## Contributing

You are more than welcome to contribute anything.

## Developing

See DEVELOPING.md for:
- Architecture overview (modules and responsibilities)
- Current language semantics (strict evaluation, Unit, print, escapes)
- Operator precedence table (Haskell-aligned)
- Build and test workflow, running subsets
- Linting with HLint and style notes
- Feature implementation playbook and testing guidance
- Versioning/release and website update steps

## Local development

Build and test quickly:

```bash
stack build --fast
stack test --fast --test-arguments "--format progress"
```

Run specific groups or examples (substring match):

```bash
stack test --test-arguments "--match Property-Based"
stack test --test-arguments "--match Stress"
```

Generate and preview the website locally:

```bash
stack exec kai-website   # http://localhost:3000
bash scripts/export-site.sh  # writes dist-site/
open dist-site/index.html
```

Use the runner without typing `stack`:

```bash
make install
export PATH="$HOME/.local/bin:$PATH"
kai tests/arithmetic.kai
```

## Testing notes

- Unit tests: parsing, evaluation, type checking.
- Property tests: AST preservation through parsing, type preservation, integer bounds, algebraic laws, and specific errors.
- Script tests: all `.kai` files under `tests/` and `test/` are parsed, type-checked, evaluated, and checked against value/error and output assertions.

For a printing/input test:

```kai
// expect: ()
// expect-type: TUnit
// stdin: "Ada\n"
// stdout: "Hello, Ada!\n"
print ("Hello, " ++ input ++ "!")
```

`()` checks the return value; `// stdout:` checks the printed greeting, including
its newline. These comments are test directives. The harness supplies stdin and
compares exact stdout; without stdout the script must be silent, and without
stdin it receives EOF. `kai --check FILE.kai` checks values and types directly;
the harness adds input/output assertions. Run `stack test` to execute both.

See [DEVELOPING.md](DEVELOPING.md#test-suite-structure) for per-test JSON reports,
script assertion checks, and tests for the harness itself.
