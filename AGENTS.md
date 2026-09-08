# AGENTS.md

Contributor instructions for Kai **0.0.5.0**.

## Read README.md in its entirety.
## Read DEVELOPING.md in its entirety.
## Read FEATURES.md in its entirety.
## Read SPEC.md in its entirety.

# Instructions for Agents

You will write tests for all functionality you write. These must be comprehensive and cover all edge cases.

You are not to hardcode tests, you are not to delete tests. If a test fails and there is no flaw in the test, you must fix the language itself.

# Kai Testing for Agents

## Test Structure
- **Unit tests**: `test/*.hs` files using Hspec
- **Script tests**: `.kai` files discovered recursively under `tests/` and `test/`
- **Property tests**: QuickCheck in `PropertyBasedSpec.hs` and the feature specs
- **Integration tests**: CLI, REPL, modules, examples, UTF-8 I/O, runner installation, source archives, and release helpers

## Adding Tests

### Unit Test Pattern
```haskell
module NewFeatureSpec where
import Test.Hspec
import Evaluator (Value(..))
import TestSupport (evaluateCheckedSource)

spec :: Spec
spec = describe "Feature" $ do
  it "description" $ do
    evaluateCheckedSource "40 + 2" `shouldBe` Right (VInt 42)
```

Use `TestSupport` for valid source fixtures: parse/type setup failures must fail
the test, never become simulated language errors. Use `inferSource` for type-error
tests and `parseExpr` directly for parser-error tests. Use `shouldInfer` to compare
complete polymorphic types without depending on fresh-variable names.

### Script Test Pattern
```kai
// expect: 42
40 + 2
```

Printing and input require observable assertions as well as the return value:

```kai
// expect: ()
// expect-type: TUnit
// stdin: "Ada\n"
// stdout: "Hello, Ada!\n"
print ("Hello, " ++ input ++ "!")
```

`()` is the result of `print`. The exact greeting is checked by `// stdout:`.
`stack test` and `scripts/check-script-corpus.py` supply the JSON `// stdin:`
fixture and compare exact stdout, including whitespace and newlines. Missing
stdin means EOF; missing stdout means silence. `kai --check FILE.kai` checks
value/error and optional type directives; it reads real stdin and does not check
stdout itself. Keep directives unique, nonempty, and at the start of their lines.

### Error Testing
```haskell
evaluateCheckedSource "5 / 0" `shouldBe` Left DivByZero
```

Import `RuntimeError(DivByZero)` from `Evaluator` for this assertion. Test exact
error constructors, and assert that later effects do not occur after a failure.

## Rules
- ALL `.kai` files MUST have `// expect:` directive
- Test specific error types, not generic failures
- Add new unit tests to `test/Spec.hs`
- Run tests: `stack test`
- Run expanded properties: `stack test --test-arguments='--qc-max-success=1000 --seed=42'`
- Assert every behavior named by the test: returned values, printed output,
  filesystem changes, and exit status as applicable. A definition-only module
  loading successfully does not establish that its exported functions work.
- Keep filesystem fixtures in temporary directories and restore stdin, stdout,
  environment variables, and the current directory after exceptions.
- See [DEVELOPING.md](DEVELOPING.md#test-suite-structure) for fixture helpers,
  per-test reports, script assertion checks, and platform-specific commands.

## File Organization
- `ArithmeticSpec.hs` + `tests/arithmetic.kai`
- `BooleanSpec.hs` + `tests/boolean_logic.kai` 
- `LambdaSpec.hs` + `tests/lambda_*.kai`
- `LetBindingSpec.hs` + `tests/let_*.kai`

## Performance Requirements
- **Language must handle deeply nested expressions** (1000+ levels) without infinite loops
- **Parser optimization**: Reorder `choice` alternatives for performance
- **Type checker optimization**: Avoid exponential `applySubstEnv` calls
- **Test ALL stress cases**: Memory, parsing, and type inference stress tests must pass

## Project Guidance
- **README.md is the source of truth** for project goals, design decisions, and current status
- **SPEC.md documents the current language state** - update immediately when language semantics change
- **Website must stay in sync with README.md** - update both when making changes
- **When in doubt**: consult README.md first, then SPEC.md for technical details
- **Design decisions**: Follow the functional-first philosophy described in README.md
- **Feature priorities**: Match the roadmap and current limitations listed in README.md
- **Documentation updates**: When adding language features, update README.md, SPEC.md, website, FEATURES.md and DEVELOPING.md

## Kai 0.0.5.0 Implementation Notes
- Builtins are first-class and support partial application; supplied arguments evaluate immediately.
- Recursive constants and closures initialize in source order. Reading an uninitialized recursive binding returns `UninitializedRecursion`.
- Annotation variables are scoped independently; constructor patterns require the declared arity and cannot repeat bound names.
- Imported declaration compatibility preserves parameter positions and constructor privacy.
- Repeated prefix operators are supported; literals, conversions, and arithmetic enforce signed 32-bit bounds.
- Scripts, imported modules, and text-file operations use UTF-8. Console streams use their host encoding.
- `print` flushes stdout; an output failure stops later effects and makes the CLI fail.
- Stress tests exercise 1000-level parsing, inference, and evaluation. Benchmark execution and timing comparisons are separate checks.

## Recovery and diagnostic testing

- Callable builtins use ordinary application and may be shadowed; field access binds tighter than application.
- `attempt` catches structured recoverable errors inside a `Unit -> a` action. Assert retained earlier effects, skipped later effects and process-control exclusions.
- `and`/`or` short-circuit while both operands remain statically checked.
- Open records and retained `Eq`/`Append` contexts must work through annotations, recursion, imports and REPL definitions.
- Open record patterns `{a = x | rest}` must preserve and type the unmatched record fields. Test `| _`, empty remainders, duplicate bindings, nested payload coverage and row-polymorphic exports; exact patterns without `|` retain their field-set requirement.
- Cases must be exhaustive; unreachable alternatives warn without changing execution.
- CLI/module/REPL parsing preserves source spans. Assert exact readable diagnostics with excerpts and relevant call/import context. Keep exact structured error checks for library APIs and script expectations.
- Execute migration examples in `MIGRATING-0.0.5.0.md` through the documentation checker. The file-report example must retain its exact failure, continuation and EOF tests.
