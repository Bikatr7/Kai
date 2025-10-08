# Kai Language

A functional-first scripting language with static typing, implemented in Haskell.

Kai aims to be a practical scripting language that's functional by default but allows imperative programming when you really need it. Clean syntax, strong static types, and a pleasant development experience.

## Current Status (v0.0.4)

The language currently supports a compact, expression‑only core with full test coverage.

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
- **Static typing & inference**: `TInt`, `TBool`, `TString`, `TUnit`, `TList`, `TRecord`, `TTuple`, `TFun` with unification and occurs check
- **Type annotations**: Optional type annotations (`let x : Int = 42`, `\x : String -> expr`)
- **Error handling**: Maybe/Either types with `Just`, `Nothing`, `Left`, `Right` constructors and case expressions
- **Safe conversion functions**: `parseInt : String -> Maybe Int`, `toString : Int -> String`, `show : a -> String`
- **Pattern matching**: Case expressions for handling Maybe/Either, tuples, and other data types
- **Wildcard variables**: Use `_` in let bindings to discard unused values (`let _ = print "hello" in 42`)
- **Expression sequencing**: Use `;` to sequence expressions for side effects (`print "first"; print "second"; 42`)
- **Parser**: Megaparsec with precedence/associativity, reserved keywords, multi-statement files
- **CLI**: parse and evaluate expressions or files with `--help`, `-e`, and `--debug` options (clean output by default), supports passing arguments to scripts
- **Let bindings**: `let` and `letrec` for variable bindings and recursive functions
- **Tests**: Hspec + QuickCheck (435 examples) — all passing with comprehensive coverage
- **Working examples**: Interactive calculator, FizzBuzz, guess the number game, list processing, text processing, file I/O demonstrations

Current limitations:

- Limited to single-file scripts (no modules or imports)
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

Wildcard variables and expression sequencing:

```kai
let _ = print "Setting up..." in
let _ = print "Processing..." in
42  // Result: prints setup messages, returns 42

print "First"; print "Second"; print "Done"
// Prints all three messages in sequence

let x = 10 in
let _ = print ("x is " ++ toString x) in
x * 2  // Prints message, returns 20
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

File I/O and command-line arguments:

```kai
// Write to a file
let _ = writeFile "output.txt" "Hello, world!" in
print "File written"

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

## Language Notes

- Keywords are reserved (`true`, `false`, `if`, `then`, `else`, `and`, `or`, `not`, `print`, `let`, `letrec`, `in`, `input`, `args`, `Int`, `Bool`, `String`, `Unit`, `parseInt`, `toString`, `show`, `head`, `tail`, `null`, `fst`, `snd`, `map`, `filter`, `foldl`, `length`, `reverse`, `take`, `drop`, `zip`, `split`, `join`, `trim`, `replace`, `strLength`, `readFile`, `writeFile`).
- Wildcard variable `_` can be used in let bindings to discard values: `let _ = expression in body`.
- Expression sequencing with `;` has lowest precedence and is right-associative: `a; b; c` = `a; (b; c)`.
- Unary minus is a proper prefix operator (e.g., `-5`, `10 - (-3)`).
- Concatenation (`++`) works for both strings and lists, right-associative, with lower precedence than `+`/`-`: `"a" ++ "b" ++ "c"` parses as `"a" ++ ("b" ++ "c")`, `[1, 2] ++ [3, 4]` parses as `[1, 2] ++ [3, 4]`.
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
- ~~Basic pattern matching~~ ✅ **DONE** (`case` expressions for Maybe/Either)
- ~~Lists and records~~ ✅ **DONE** (with basic operations)
- ~~Tuples~~ ✅ **DONE** (with `fst` and `snd` for pairs)
- Top‑level definitions and module system
- Enhanced pattern matching (tuple destructuring in case, guards)
- Do-notation or block syntax for I/O sequencing
- Match expressions as alternative to nested conditionals
- Maps and advanced data structures

**Functional-First Standard Library**
- ~~List operations: `map`, `filter`, `fold`, `zip`~~ ✅ **DONE** (immutable by default)
- ~~String utilities: `split`, `join`, `trim`, `replace`~~ ✅ **DONE**
- Math functions: `abs`, `min`, `max`, `sqrt`
- ~~Option/Result types for error handling~~ ✅ **DONE** (Maybe/Either)
- Function composition and pipeline operators

**I/O and Effects (Controlled Imperative)**
- ~~Wildcard variables (`_`) for unused bindings in let expressions~~ ✅ **DONE**
- ~~Statement blocks with semicolon syntax for imperative-style code~~ ✅ **DONE** (expression sequencing)
- ~~File I/O: `readFile`, `writeFile`~~ ✅ **DONE**
- ~~Command-line arguments~~ ✅ **DONE** (`args`)
- Do-notation for clean I/O sequencing (addressing calculator verbosity)
- File I/O: `appendFile` and additional operations
- Network operations: HTTP requests, JSON parsing
- Process utilities: run external commands
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

// Imperative when needed (I/O) - proposed do-notation
let main = do {
  input <- readFile "input.txt"
  let processed = processLines (split "\n" input)
  let greeting = greet (head processed)
  writeFile "output.txt" greeting
  print greeting
}

// Alternative: expression sequencing for imperative style
let main =
  let input = readFile "input.txt" in
  let processed = processLines (split "\n" input) in
  let greeting = greet (head processed) in
  let _ = writeFile "output.txt" greeting in
  let _ = print greeting in
  ()
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
