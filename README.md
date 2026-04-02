# Kai Language

A functional-first scripting language with static typing, implemented in Haskell.

Kai aims to be a practical scripting language that's functional by default but allows imperative programming when you really need it. Clean syntax, strong static types, and a pleasant development experience.

## Current Status (v0.0.4.3)

The language supports a comprehensive functional-first scripting language with extensive test coverage, modular architecture, and performance benchmarking. Recent additions include generalized let-polymorphism, stronger CLI/script behavior, and better top-level/module consistency.

Features available today:

- **Expressions**: integers, booleans, strings, parentheses, unary minus
- **Arithmetic**: `+`, `-`, `*`, `/` (integer division, division by zero error)
- **Booleans**: `and`, `or`, `not`
- **Comparisons**: `==`, `<`, `>`
- **Strings**: string literals (`"hello"`), concatenation (`++`), escapes (`\"`, `\\`, `\n`)
- **String functions**: `split`, `join`, `trim`, `replace`, `strLength` for text processing
- **Lists**: `[1, 2, 3]`, concatenation (`++`), equality (`==`), cons (`::`), operations (`head`, `tail`, `null`)
- **List functions**: `map`, `filter`, `foldl`, `length`, `reverse`, `take`, `drop`, `zip` for functional programming
- **Tuples**: `(1, "hello", true)` for grouping values, with `fst` and `snd` for pairs
- **Records**: `{a = 1, b = true}`, field access (`record.field`), equality (`==`)
- **Unit & printing**: `()` unit value; `print : a -> Unit` prints and returns `()`; `input` reads a line of stdin and returns a string
- **File I/O**: `readFile : String -> String` and `writeFile : String -> String -> Unit` for file operations
- **Command-line arguments**: `args : [String]` returns list of command-line arguments passed to script
- **Conditionals**: `if cond then e1 else e2`
- **Functions**: lambdas (`\x -> expr`), application (`f x`), closures
- **Static typing & inference**: `TInt`, `TBool`, `TString`, `TUnit`, `TList`, `TRecord`, `TTuple`, `TFun` with unification, occurs check, and generalized let-polymorphism
- **Type annotations**: Optional type annotations (`let x : Int = 42`, `\x : String -> expr`)
- **Error handling**: Maybe/Either types with `Just`, `Nothing`, `Left`, `Right` constructors and case expressions
- **Safe conversion functions**: `parseInt : String -> Maybe Int`, `toString : Int -> String`, `show : a -> String`, `discard : a -> Unit`
- **Pattern matching**: Case expressions for handling Maybe/Either, tuples, and other data types
- **Do blocks**: `do { expr1; expr2; expr3 }` for readable effect sequencing, with `do {}` evaluating to `()`
- **Wildcard variables**: `_` still works in let bindings when you truly want to discard a value (`let _ = expensiveCall in body`)
- **Expression sequencing**: `;` remains the primitive sequencing operator, with lowest precedence
- **Parser**: Megaparsec with precedence/associativity, reserved keywords, multiline top-level files, and multiline `do` blocks
- **CLI**: parse and evaluate expressions or files with `--help`, `-e`, and `--debug` options (clean output by default), supports passing arguments to scripts, and returns non-zero exit codes on failures
- **Let bindings**: `let` and `letrec` for variable bindings and recursive functions
- **Top-level definitions**: `let` and `letrec` at module level for defining functions and values
- **Module system**: `import ModuleName` to import modules, module resolution supports `ModuleName.kai` and `ModuleName/ModuleName.kai`, full cross-module type checking, explicit exports with `export name1, name2`
- **Tests**: Hspec + QuickCheck (588 examples) — all passing with comprehensive coverage
- **Working examples**: Module-based text analysis, validated CLI tools, interactive calculator and guessing game, list/record processing, text cleanup, file I/O, wildcard matching, and discard/logging demos

Current limitations:

- No REPL for interactive experimentation
- No standard library (beyond built-in functions)
- No error recovery (one parse error stops execution)

## Quickstart

Prerequisites: GHC/Stack via GHCup or your platform’s package manager.

Build, test, and run:

```bash
stack build
stack test

## Run interpreter
stack exec kai -- --help
stack exec kai -- -e "\"hi\" ++ \"!\""
stack exec kai -- -e "print (42 + 1)"
stack exec kai -- --debug -e "42 + 1"

## Run a file
stack exec kai -- path/to/script.kai

## Try practical examples
stack exec kai -- examples/text_analysis.kai
stack exec kai -- examples/calculator.kai

## Website demo (intro page)
stack exec kai-website  # visit http://localhost:3000
```

Install the CLI (no explicit `stack` needed):

- Lightweight runner script: installs a `kai` command that prefers a compiled binary and otherwise falls back to `stack exec kai` transparently.

```bash
make install              # installs to ~/.local/bin/kai by default
export PATH="$HOME/.local/bin:$PATH"  # if not already set

# Now you can run Kai directly
kai path/to/script.kai
```

Prebuilt binaries (CI Releases):

- Update the version in `package.yaml` and push to master. GitHub Actions will automatically create a release with binaries for Linux, macOS, and Windows.
- Download the appropriate `kai-<platform>-<arch>` binary from the Releases page, `chmod +x` (Linux/macOS), and place it on your `PATH`.
- From source, `stack install` also produces a native binary in your local Stack install path.

Export a static site bundle:

- Generate `dist-site/` with an `index.html` and static assets you can open locally or deploy to GitHub Pages/Netlify.

```bash
bash scripts/export-site.sh
## open dist-site/index.html in a browser
```

Script samples in tests:

- `stack test` also discovers `.kai` files under `tests/` and `test/`, evaluates them, and shows each file’s result in the test output under two sections.
- Add your own `.kai` script to those folders to have it run automatically.

## Examples

Arithmetic, booleans, conditionals:

```kai
42 * (10 - 3)
true and not false
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

(print "First"); (print "Second"); print "Done"
// Parenthesize print when sequencing it

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

show (42 + 3)        // => "45"
toString 100         // => "100"
parseInt "42"        // => Just 42
```

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
- `examples/file_counter.kai`: `Either`-based CLI validation plus reusable text-analysis helpers
- `examples/list_processing.kai`: lists, records, `zip`, and a let-polymorphic tagging helper
- `examples/calculator.kai` and `examples/guess_the_number.kai`: interactive input with typed parsing and recursion
- `examples/file_io.kai`, `examples/text_processing.kai`, `examples/wildcard_patterns.kai`, and `examples/discard_demo.kai`: practical file scripting, text cleanup, wildcard matching, and discard-based logging

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
```

Type safety (checked before evaluation):

```kai
1 + true         // Type error: TypeMismatch TInt TBool
if 5 then 1 else 2  // Type error: ExpectedBool TInt
```

## Performance Benchmarks

Kai includes comprehensive benchmarks for speed and memory usage:

### Running Benchmarks

```bash
# Run all benchmarks
stack bench

# Run specific benchmark categories
stack bench --benchmark-arguments="--match pattern 'Evaluator'"
stack bench --benchmark-arguments="--match pattern 'Parser'"

# Generate CSV output for analysis
stack bench --benchmark-arguments="--csv=results.csv"
```

### Current Performance

- **Most operations**: ~20-50ns (arithmetic, conditionals, functions)
- **Record access**: ~1.93μs (optimized for map lookups)
- **Recursion**: ~6μs (appropriate for function call overhead)
- **Boolean operations**: ~23ns (300x faster after syntax fixes)
- **Parser**: ~40-600ns (scales linearly with complexity)
- **Type checker**: ~20ns

See `benchmarks/README.md` for detailed benchmark documentation and regression testing guidelines.

## Language Notes

- Keywords are reserved. The current list includes control-flow, I/O, module, and data constructors such as `if`, `let`, `letrec`, `do`, `case`, `import`, `export`, `Just`, `Nothing`, `Left`, and `Right`; see `SPEC.md` for the exact list.
- Wildcard variable `_` can be used in let bindings and pattern matching to discard values: `let _ = expression in body`, `case x of _ -> "any" | Just val -> "some"`.
- `do { ... }` is the idiomatic way to sequence effects. Entries are separated by semicolons, and `do {}` evaluates to `()`.
- Expression sequencing with `;` has lowest precedence and is right-associative: `a; b; c` = `a; (b; c)`.
- Unary minus is a proper prefix operator (e.g., `-5`, `10 - (-3)`).
- Concatenation (`++`) works for both strings and lists, right-associative, with lower precedence than `+`/`-`: `"a" ++ "b" ++ "c"` parses as `"a" ++ ("b" ++ "c")`, `[1, 2] ++ [3, 4]` parses as `[1, 2] ++ [3, 4]`.
- Supported string escapes: `\"`, `\\`, `\n`. Unknown escapes are errors.
- `print` evaluates its argument, prints it, and returns unit `()`.
- Application binds tighter than infix operators (`f x + y` parses as `(f x) + y`).
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
│   │   ├── Patterns.hs           ## Pattern type checking
│   │   └── Inference.hs          ## Main type inference dispatcher
│   ├── TypeChecker.hs            ## Public type checker interface
│   ├── Evaluator/                ## Modular evaluator with pure/IO duality
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
│   │   └── Patterns.hs           ## Pattern matching evaluation (pure & IO)
│   ├── Evaluator.hs              ## Public evaluator interface
│   ├── CLI.hs                    ## CLI runner and exit-code handling
│   └── Main.hs                   ## Thin executable entry for `kai`
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
├── Makefile                      ## install/test/site/build targets
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

Kai's next release should make the language materially better for day-to-day typed scripting. The priority is workflow and practical data modeling, not advanced type theory.

**v0.0.4.4 release focus**
- Interactive REPL with multiline input plus `:type`, `:load`, and `:reload`
- Custom data types so scripts can define their own algebraic models instead of only using built-in containers
- Stronger pattern matching, starting with the forms that make ADTs and tuples ergonomic
- Essential scripting stdlib additions: `appendFile`, `fileExists`, basic directory operations, and process/environment access

**Likely stretch work if the core lands early**
- Better parse and type error messages
- More math, list, and string helpers
- Small ergonomics improvements around scripting workflows

**Explicitly after v0.0.4.4**
- Package manager and reusable dependency story
- Formatter, linter, and IDE/LSP support
- HTTP/JSON/networking work
- Mutable performance escape hatches and deeper compiler/runtime optimization
- Advanced type-system work such as polymorphic recursion, row polymorphism, GADTs, and rank-N types

Example current Kai script style:

```kai
#!/usr/bin/env kai

let processLines = \text -> filter (\line -> not (trim line == "")) (map trim (split "\n" text))
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

Runner script without stack:

```bash
make install
export PATH="$HOME/.local/bin:$PATH"
kai tests/arithmetic.kai
```

## Testing notes

- Unit tests: parsing, evaluation, type checking.
- Property tests: determinism, pretty‑print/parse stability, integer bounds, algebraic laws.
- Script tests: all `.kai` files under `tests/` and `test/` are parsed and evaluated in the suite output.

To add your own scripts, drop a `.kai` file into `tests/` and run `stack test`.
