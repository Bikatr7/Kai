# Kai Language

A minimal, statically typed expression language, implemented in Haskell.

Kai aims to combine a clean, understandable syntax with a solid static type system and a pleasant scripting experience.

## Current Status (MVP)

The language currently supports a compact, expression‑only core with full test coverage.

Features available today:

- Expressions: integers, booleans, parentheses, unary minus
- Arithmetic: `+`, `-`, `*`, `/` (integer division, division by zero error)
- Booleans: `and`, `or`, `not`
- Comparisons: `==`, `<`, `>`
- Conditionals: `if cond then e1 else e2`
- Functions: lambdas (`\x -> expr`), application (`f x`), closures
- Static checking: simple type checker with `TInt`, `TBool`, `TFun`
- Parser: Megaparsec with precedence/associativity, reserved keywords
- CLI: parse and evaluate expressions or files
- Tests: Hspec suite (103 examples) — all passing

Limitations (by design at this stage):

- No top‑level bindings, modules, or imports
- No strings, lists, records, or user‑defined data types (yet)
- Type checking is simple; lambdas default an `Int` parameter type (placeholder for real inference)
- Purely functional core; no side effects or I/O in the language itself

## Quickstart

Prerequisites: GHC/Stack via GHCup or your platform’s package manager.

Build, test, and run:

```bash
stack build
stack test

## Run interpreter (no args = REPL-like single input mode)
stack exec kai

## Run a file
stack exec kai path/to/script.kai

## Website demo (intro page)
stack exec kai-website  # visit http://localhost:3000
```

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

- Keywords are reserved (`true`, `false`, `if`, `then`, `else`, `and`, `or`, `not`).
- Unary minus is a proper prefix operator (e.g., `-5`, `10 - (-3)`).
- Application binds tighter than infix operators (`f x + y` parses as `(f x) + y`).

## Project Structure

```
.
├── src/                   ## Language implementation
│   ├── Syntax.hs          ## AST definitions
│   ├── Parser.hs          ## Megaparsec parser
│   ├── TypeChecker.hs     ## Static type checker (TInt, TBool, TFun)
│   ├── Evaluator.hs       ## Interpreter (closures, pure evaluation)
│   └── Main.hs            ## CLI entry for `kai`
├── website/               ## Small Yesod site (intro)
├── test/                  ## Hspec test suite
├── package.yaml           ## Project config (library + exes + tests)
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
  - Script runner (`kai script.kai`) and `#!/usr/bin/env kai` support

- Types and inference
  - Hindley–Milner style type inference with annotations when needed
  - Algebraic data types, pattern matching, type aliases
  - Parametric polymorphism; later, typeclasses/traits if justified

- Data and stdlib
  - Strings, lists, maps/records, options/results
  - A focused, batteries‑included standard library for scripting
  - Interop/FFI for host system calls

- Effects and I/O
  - Simple, principled I/O model that keeps the core pure
  - File and process utilities; JSON and text handling

- Tooling and UX
  - Formatter and linter
  - REPL with multiline input, completion, and :type
  - Error messages with helpful suggestions

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
