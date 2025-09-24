# Kai Language

A functional-first scripting language with static typing, implemented in Haskell.

Kai aims to be a practical scripting language that's functional by default but allows imperative programming when you really need it. Clean syntax, strong static types, and a pleasant development experience.

## Current Status (v0.0.3.2)

The language currently supports a compact, expression‑only core with full test coverage.

Features available today:

- **Expressions**: integers, booleans, strings, parentheses, unary minus
- **Arithmetic**: `+`, `-`, `*`, `/` (integer division, division by zero error)
- **Booleans**: `and`, `or`, `not`
- **Comparisons**: `==`, `<`, `>`
- **Strings**: string literals (`"hello"`), concatenation (`++`), escapes (`\"`, `\\`, `\n`)
- **Unit & printing**: `()` unit value; `print : a -> Unit` prints and returns `()`; `input` reads a line of stdin and returns a string
- **Conditionals**: `if cond then e1 else e2`
- **Functions**: lambdas (`\x -> expr`), application (`f x`), closures
- **Static typing & inference**: `TInt`, `TBool`, `TString`, `TUnit`, `TFun` with unification and occurs check
- **Type annotations**: Optional type annotations (`let x : Int = 42`, `\x : String -> expr`)
- **Error handling**: Maybe/Either types with `Just`, `Nothing`, `Left`, `Right` constructors and case expressions
- **Safe conversion functions**: `parseInt : String -> Maybe Int`, `toString : Int -> String`, `show : a -> String`
- **Pattern matching**: Case expressions for handling Maybe/Either and other data types
- **Parser**: Megaparsec with precedence/associativity, reserved keywords, multi-statement files
- **CLI**: parse and evaluate expressions or files with `--help`, `-e`, and `--debug` options (clean output by default)
- **Let bindings**: `let` and `letrec` for variable bindings and recursive functions
- **Tests**: Hspec + QuickCheck (266 examples) — all passing with comprehensive coverage including 27 input/conversion test files
- **Working examples**: Interactive calculator demonstrating input, conversion functions, and tail recursion

Current limitations:

- Limited to single-file scripts (no modules or imports)
- No data structures for complex data manipulation (lists, maps, records)
- Limited I/O (only print statements and stdin, no file operations)
- No REPL for interactive experimentation
- No standard library (even basic functions like `length`, `head`)
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
stack exec kai path/to/script.kai

## Try the interactive calculator
stack exec kai examples/calculator.kai

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

- Update the version in `package.yaml` and push to master. GitHub Actions will automatically create a release with binaries for Linux and macOS.
- Download the appropriate `kai-<platform>` binary from the Releases page, `chmod +x`, and place it on your `PATH`.
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
let num = parseInt numStr in
print ("Double: " ++ toString (num * 2))
```

Type annotations and conversions:

```kai
let add : Int -> Int -> Int = \x : Int -> \y : Int -> x + y in
add 5 (parseInt "10")

show (42 + 3)        // => "45"
toString 100         // => "100"
parseInt "42"        // => 42
```

Type safety (checked before evaluation):

```kai
1 + true         // Type error: TypeMismatch TInt TBool
if 5 then 1 else 2  // Type error: ExpectedBool TInt
```

## Language Notes

- Keywords are reserved (`true`, `false`, `if`, `then`, `else`, `and`, `or`, `not`, `print`, `let`, `letrec`, `in`, `input`, `Int`, `Bool`, `String`, `Unit`, `parseInt`, `toString`, `show`).
- Unary minus is a proper prefix operator (e.g., `-5`, `10 - (-3)`).
- String concatenation uses `++` and is right-associative, with lower precedence than `+`/`-`: `"a" ++ "b" ++ "c"` parses as `"a" ++ ("b" ++ "c")`.
- Supported string escapes: `\"`, `\\`, `\n`. Unknown escapes are errors.
- `print` evaluates its argument, prints it, and returns unit `()`.
- Application binds tighter than infix operators (`f x + y` parses as `(f x) + y`).
- Multi-statement files are supported: each line is parsed as a separate expression.

## Project Structure

```
.
├── src/                    ## Language implementation
│   ├── Syntax.hs           ## AST definitions
│   ├── Parser.hs           ## Megaparsec parser
│   ├── TypeChecker.hs      ## Static type checker / inference (TInt, TBool, TFun)
│   ├── Evaluator.hs        ## Interpreter (closures, pure evaluation)
│   └── Main.hs             ## CLI entry for `kai`
├── test/                   ## Hspec/QuickCheck test suite (.hs specs)
├── tests/                  ## Sample Kai scripts (.kai) evaluated by tests
├── website/                ## Small Yesod site (intro)
│   └── static/             ## Website assets (favicon, css)
├── scripts/                ## Helper scripts (runner, export-site)
├── dist-site/              ## Static export (generated by `make site`)
├── Makefile                ## install/test/site targets
├── package.yaml            ## Project config (library + exes + tests)
├── kai-lang.cabal          ## Generated from package.yaml (hpack)
├── stack.yaml              ## Stack configuration
└── README.md
```

## Vision: Kai as a Functional-First Scripting Language

Design philosophy:

- **Functional by default**: Immutable data, pure functions, expressions over statements
- **Imperative when needed**: Escape hatches for I/O, performance, or when it's genuinely clearer
- **Static first**: Strong, predictable types with great error messages and inference
- **Scriptable**: Fast edit‑run cycle, ergonomic CLI, shebang support, no compilation step
- **Practical**: Batteries-included standard library for real-world scripting tasks

Planned functional-first features:

**Core Language**
- ~~Let‑bindings and recursion~~ ✅ **DONE** (`let` and `letrec`)
- ~~Hindley–Milner style type inference~~ ✅ **DONE**
- Top‑level definitions and module system
- Algebraic data types and pattern matching
- Lists, maps, records with functional operations

**Functional-First Standard Library**
- List operations: `map`, `filter`, `fold`, `zip` (immutable by default)
- String utilities: `split`, `join`, `trim`, `replace`
- Math functions: `abs`, `min`, `max`, `sqrt`
- Option/Result types for error handling
- Function composition and pipeline operators

**I/O and Effects (Controlled Imperative)**
- File I/O: `readFile`, `writeFile`, `appendFile`
- Network operations: HTTP requests, JSON parsing
- Process utilities: run external commands
- Input mechanisms: `readLine`, command-line arguments
- Mutable references when needed: `ref`, `var`

**Scripting Conveniences**
- REPL with multiline input and `:type` command
- Better error messages with suggestions
- Formatter and basic linter
- Shebang support for executable scripts
- Package/module system for reusable code

**Performance Escape Hatches**
- Mutable arrays/buffers for hot paths
- Imperative loops when performance matters
- Lazy evaluation controls

Example future script (functional-first with imperative I/O):

```kai
#!/usr/bin/env kai

import System (args, readFile, writeFile)
import List (map, filter)

// Functional by default
let processLines lines = 
  lines |> filter (not . isEmpty) |> map trim

let greet name =
  if name == "" then "Hello, world!" else "Hello, " ++ name

// Imperative when needed (I/O)
let main = do
  input <- readFile "input.txt"
  let processed = processLines (split "\n" input)
  let greeting = greet (head processed)
  writeFile "output.txt" greeting
  print greeting
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
