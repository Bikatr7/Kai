# Developing Kai

This document helps contributors work on Kai’s codebase efficiently.

## Architecture Overview

- `src/Syntax.hs`: AST for expressions (`Expr`). Includes literals, variables, arithmetic, booleans, comparisons, strings, `print`, lambdas, application, `let`, and `letrec`, plus a unit literal `()`.
- `src/Parser.hs`: Megaparsec parser. Handles comments, precedence/associativity, unary minus, string escapes (`\"`, `\\`, `\n`), and multi-statement parsing.
- `src/TypeChecker.hs`: Hindley–Milner type inference with unification and occurs check. Core types: `TInt`, `TBool`, `TString`, `TUnit`, and `TFun`.
- `src/Evaluator.hs`: Strict evaluator with closures. Runtime values: `VInt`, `VBool`, `VStr`, `VUnit`, `VFun`. `print` returns `VUnit`.
- `src/Main.hs`: CLI entry (`kai`). Parses input, type-checks, and evaluates; supports `-e` and file execution.
- `website/`: Yesod-based static site generator used for the project website/demo.

## Language Semantics (current)

- Evaluation: strict (call-by-value).
- Unit: `()` value with type `TUnit`.
- `print : a -> Unit` prints and returns `()`.
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
- Tests: `stack test --fast`
- Run CLI: `stack exec kai -- --help`
- Run website locally: `stack exec kai-website` (visit http://localhost:3000)

### Test Suite Structure

- Unit tests: `test/*.hs` (Hspec + QuickCheck)
- Script tests: `tests/*.kai` with `// expect:` directives
- Property tests: `PropertyBasedSpec.hs`

Run subsets:
- `stack test --test-arguments "--match Arithmetic"`
- `stack test --test-arguments "--match Script files"`

## Linting & Style

- HLint: `hlint .`
  - Examples already applied: `void (symbol "()")`, avoid trivial lambdas in operator table.
- Keep changes minimal and focused. Follow existing code style.

## Adding Features (playbook)

1) Update `Syntax` with new AST forms.
2) Extend `Parser` with syntax + precedence placement.
3) Extend `TypeChecker` with typing rules and unification as needed.
4) Extend `Evaluator` with runtime behavior.
5) Add tests:
   - Unit tests in `test/*Spec.hs`.
   - Script test in `tests/*.kai` with `// expect:`.
   - Property tests when applicable.
6) Update README (source of truth) and website to reflect user-visible changes.

## Versioning & Release

- Bump version in `package.yaml` (hpack regenerates `.cabal`).
- Update README header and website version display.
- Tag and build via CI to produce binaries (see README’s install section).

## Website Updates

- Edit `website/Main.hs` for content changes (features, version, examples).
- Run `stack exec kai-website` to serve locally. Static export via `scripts/export-site.sh`.

## Notes / TODOs

- Planned next steps (from roadmap): minimal prelude (`echo : a -> a`), shadowing warnings + `--no-warn-shadow`, ADTs + `match`, and a REPL.
- When changing semantics, align README and website immediately.