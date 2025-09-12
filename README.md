# Kai Language

A minimal, statically typed expression language, implemented in Haskell.

Kai aims to combine a clean, understandable syntax with a solid static type system and a pleasant scripting experience.

## Current Status (v0.0.3)

The language currently supports a compact, expression‑only core with full test coverage.

Features available today:

- **Expressions**: integers, booleans, strings, parentheses, unary minus
- **Arithmetic**: `+`, `-`, `*`, `/` (integer division, division by zero error)
- **Booleans**: `and`, `or`, `not`
- **Comparisons**: `==`, `<`, `>`
- **Strings**: string literals (`"hello"`), concatenation (`++`)
- **Print statements**: `print expr` for output
- **Conditionals**: `if cond then e1 else e2`
- **Functions**: lambdas (`\x -> expr`), application (`f x`), closures
- **Static typing & inference**: `TInt`, `TBool`, `TString`, `TFun` with unification and occurs check
- **Parser**: Megaparsec with precedence/associativity, reserved keywords, multi-statement files
- **CLI**: parse and evaluate expressions or files with `--help` and `-e` options
- **Tests**: Hspec + QuickCheck (221 examples) — all passing with clear pass/fail indicators

Limitations (by design at this stage): 

- No top‑level bindings, modules, or imports
- No lists, records, or user‑defined data types (yet)
- No user annotations yet; inference covers ints, bools, strings, and functions with unification
- Purely functional core; minimal I/O (only print statements for output)
- No REPL for interactive experimentation
- No standard library (even basic functions like `length`, `head`)
- No error recovery (one parse error stops execution)
- No variables or let-bindings (expressions only)

**Practical limitations for real-world use:**
- Limited to single-file scripts (no multi-file projects)
- No way to define reusable functions or constants
- No data structures for complex data manipulation
- No file I/O beyond print statements
- No way to handle errors gracefully

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

## Run a file
stack exec kai path/to/script.kai

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
print ("The answer is " ++ "42")
print (if 5 > 3 then "yes" else "no")
```

Lambdas and application:

```kai
(\x -> x + 1) 41      // => 42
(\f -> f 10) (\n -> n * 2)  // => 20
```

Type safety (checked before evaluation):

```kai
1 + true         // Type error: TypeMismatch TInt TBool
if 5 then 1 else 2  // Type error: ExpectedBool TInt
```

## Language Notes

- Keywords are reserved (`true`, `false`, `if`, `then`, `else`, `and`, `or`, `not`, `print`).
- Unary minus is a proper prefix operator (e.g., `-5`, `10 - (-3)`).
- String concatenation uses `++` and is left-associative: `"a" ++ "b" ++ "c"`.
- Print statements evaluate their argument and display the result.
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

## Vision: Kai as a Clean, Typed Scripting Language

Design goals:

- Static first: strong, predictable types with great errors
- Clean syntax: concise, readable, expression‑oriented
- Scriptable: fast edit‑run loop, ergonomic CLI, shebang support
- Practical: a small standard library and productive defaults

Planned language features:

- Bindings and modules
  - Let‑bindings and top‑level definitions
  - Module system and imports

- Types and inference
  - ~~Hindley–Milner style type inference with annotations when needed~~ ✅ **DONE**
  - Algebraic data types, pattern matching, type aliases
  - Parametric polymorphism; later, typeclasses/traits if justified

- Data and stdlib
  - ~~Strings~~ ✅ **DONE** (literals, concatenation)
  - Lists, maps/records, options/results
  - A focused, batteries‑included standard library for scripting
  - Interop/FFI for host system calls

- Effects and I/O
  - ~~Simple, principled I/O model that keeps the core pure~~ ✅ **DONE** (print statements)
  - File and process utilities; JSON and text handling

- Tooling and UX
  - REPL with multiline input, completion, and :type
  - Formatter and linter
  - Error messages with helpful suggestions
  - Better error recovery and diagnostics
  - Interactive debugging tools

- Standard Library
  - Basic list operations (`length`, `head`, `tail`, `map`, `filter`)
  - String utilities (`split`, `join`, `trim`)
  - Math functions (`abs`, `min`, `max`, `sqrt`)
  - Type conversion utilities

- Language Completeness
  - Variables and let-bindings for code reuse
  - Pattern matching and destructuring
  - Error handling (Maybe/Either types)
  - Recursive function definitions
  - Multi-file projects and imports

- Developer Experience
  - Interactive REPL with history and completion
  - Better error messages with suggestions
  - Syntax highlighting and IDE support
  - Package manager for libraries
  - Debugging and profiling tools

Example future script:

```kai
#!/usr/bin/env kai

import System (args, readFile, writeFile)

let greet name =
  if name == "" then "Hello, world!" else "Hello, " + name

let main =
  let input = if length args > 0 then head args else "Kai"
  in print (greet input)
```

## Contributing

You are more than welcome to contribute anything.

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
