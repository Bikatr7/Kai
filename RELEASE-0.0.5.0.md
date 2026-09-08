# Kai 0.0.5.0 release design

## Purpose

Make Kai dependable for scripts that must recover from failures and for reusable
functional code. The release keeps strict evaluation, immutable values, lexical
scope, type inference, and direct scripting. It adds explicit recovery and removes
the five language-design obstacles listed below.

This document defines the next release's target behavior. SPEC.md describes
0.0.4.6 until the corresponding language changes are implemented and tested.

## Required scope

| Finding | 0.0.5.0 requirement | Observable acceptance condition |
| --- | --- | --- |
| 1. Scripts cannot recover from runtime failures | Structured errors and an explicit recovery boundary | A script catches a failed file read, supplies a fallback, and continues successfully |
| 2. Boolean guards evaluate unsafe operands | Short-circuit `and` and `or` | `false and (1 / 0 == 0)` returns `false` without evaluating the division |
| 3. Builtins use special application rules | Ordinary function application for builtins | `map length [[1], [2, 3]]` returns `[1, 2]`, just like a user-defined helper |
| 4. Common record and concatenation helpers fail inference | Open record inference and retained concatenation constraints | Unannotated helpers read multiple fields, accept extra fields, and concatenate strings or lists |
| 5. Predictable failures pass static checking | Equality constraints and exhaustive pattern checking | Callable equality and incomplete matches are rejected before effects run |

All five are release requirements. Documentation, examples, tests, CLI, REPL,
imports, and the website must agree on their final behavior.

## 1. Structured errors and recovery

### Public operations

Use the existing `Either` type to expose recovery as a value:

```text
attempt : (Unit -> a) -> Either Error a
raise   : Error -> a
readLine : Unit -> Maybe String
headMaybe : [a] -> Maybe a
tailMaybe : [a] -> Maybe [a]
```

`attempt action` calls `action ()` exactly once. Success returns `Right value`;
a recoverable runtime failure returns `Left error`. The thunk is necessary:
strict evaluation would execute `attempt (readFile path)` before entering the
recovery boundary, and that expression must be a type error.

Target example:

```text
case attempt (\unit -> readFile "settings.txt") of
  Right text -> print text
  | Left error -> print "Using default settings"
```

Existing `readFile`, write/directory/environment operations, arithmetic, and
partial list operations retain their successful return types. Their failures
become structured, recoverable errors. `Maybe`/`Either` constructors themselves
do not intercept evaluation failures. `raise` supports deliberate application
failures and rethrowing an error to an enclosing recovery boundary.

`readLine ()` returns `Nothing` for normal EOF, `Just ""` for a blank line, and
raises an I/O error for an actual stream failure. It preserves other whitespace.
Existing `input` remains available and raises `EndOfInput` at EOF. `headMaybe`
and `tailMaybe` return `Nothing` for an empty list; existing `head`/`tail` remain
available with explicit empty-list errors.

### Error values

Provide a standard algebraic `Error` type with public constructors:

```text
DivisionByZero
ArithmeticOverflow
EmptyList String
EndOfInput
IOError IOErrorKind String (Maybe String) String
UserError String

IOErrorKind = NotFound | PermissionDenied | AlreadyExists
           | InvalidPath | InvalidEncoding | ResourceBusy | OtherIO
```

`EmptyList` carries the operation name. `IOError` carries the category,
operation, optional path, and human-readable detail, in that order. Match on
constructors/categories, never host-specific message text. Preserve the original
failure detail when available; map unknown host errors to `OtherIO`. The public
`Error` and `IOErrorKind` types follow ordinary constructor privacy, equality,
pattern checking, and import rules, with one shared standard declaration identity.

Parse/type errors, missing or invalid imported source, uninitialized recursive
bindings, interpreter invariant failures, resource exhaustion, cancellation, and
`exit` are outside this recovery mechanism. `exit 0` and `exit 7` retain their
process-control meaning inside `attempt`. Never catch all Haskell exceptions.

### Effect and boundary rules

- Recovery is lexical and nested: the nearest active `attempt` handles a failure.
- Operands and statements still run left to right; effects after a failing
  operation inside that action do not run.
- Effects already performed are retained. Recovery does not undo a file write,
  printed output, a changed directory, or a changed environment variable.
- A handler runs outside the completed boundary. A handler failure propagates
  to an enclosing boundary; it does not repeatedly invoke the same handler.
- Errors may be returned, stored, pattern-matched, compared, and rethrown.
- A handled failure permits normal continuation and exit status 0. An unhandled
  failure prints a diagnostic and exits nonzero. CLI diagnostic-output failures
  must preserve nonzero status even when stdout and stderr cannot be written.
- The host's pure evaluator supports arithmetic/list/application recovery, while
  retaining its explicit rejection of external I/O. Pure and I/O evaluators must
  share error classification and boundary semantics where their operations overlap.

This release does not add an effect type system. Function types continue to
describe inputs and outputs; they do not promise purity. Document this directly.

## 2. Short-circuit boolean operators

Evaluate the left operand first. `false and rhs` returns `false` without running
`rhs`; `true or rhs` returns `true` without running `rhs`. The other two cases
evaluate `rhs` once. Both operands must still type-check as `Bool`, including a
branch skipped at runtime. Precedence and associativity remain unchanged.

This intentionally changes scripts that rely on right-operand effects. Migrate
those scripts to an explicit `let` or `do` sequence before combining their values.
Keep coverage for left-operand failure, necessary right-operand failure, skipped
input/file/process operations, and nested combinations in both evaluators.

## 3. One function application model

Represent callable builtins as values in a shared standard environment, with
ordinary `App` parsing and application. A builtin's name must not greedily consume
arguments in the parser. Keep control forms and operator syntax special; function
names such as `map`, `length`, and `readFile` become ordinary identifiers.

Users may alias, import, shadow, pass, and partially apply builtin functions just
as they do other functions. Local definitions take precedence over the standard
environment. Constructors use the same application rules while retaining their
identity for patterns. `input`, `args`, and `getCurrentDirectory` keep their
existing zero-argument behavior; changing their API is outside this release.

Preserve strict partial application: every supplied argument evaluates once when
supplied, and the resulting function captures that value. Do not re-evaluate it
on each later call. Extra arguments apply to a returned function when its type
allows that; otherwise type checking rejects them.

Keep `head([1, 2])`, `map (length) xs`, ordinary constructor application, and signed
arguments such as `f (-1)` working. Correct application/field precedence at the
same time: `f x.field` will mean `f (x.field)`; `(f x).field` explicitly accesses
the result. `record.fn x` continues to call the stored function.

## 4. Useful inference for records and concatenation

### Open records

Infer an open row for a field accessor instead of immediately fixing the entire
record to that single field. Accumulate all required fields across the body:

```text
let getA = \r -> r.a
let sumFields = \r -> r.a + r.b
getA {a = 1, extra = true}                  // 1
sumFields {a = 1, b = 2, extra = "ok"}     // 3
```

Record literals and `{a : Int}` annotations remain closed. Add explicit open
annotations using `{a : Int | r}`. Row variables are distinct from value type
variables, scoped to their annotation, and generalized/instantiated through let,
letrec, module exports, and REPL definitions. Missing fields, conflicting field
types, invalid row kinds, duplicate labels, and infinite rows remain errors.
Reject duplicate fields in record literals as well as patterns and annotations;
silently replacing a field hides mistakes and discarded effects.

### Constrained concatenation

Retain an `Append a` constraint while inferring `x ++ y`; do not demand a concrete
string/list type before the helper is used. Support `String` and homogeneous
lists only. A reusable helper can be instantiated separately for strings and
lists; mixed operands and numeric/function concatenation are static errors.

```text
let append = \x -> \y -> x ++ y
(append "a" "b", append [1] [2])          // ("ab", [1, 2])
append : Append a => a -> a -> a
```

Constraints must survive generalization, substitution, annotations, recursive
groups, exports, and interactive definitions. Do not silently default an unresolved
`Append` constraint to strings. Report ambiguity when an entry point cannot
resolve a constraint. This is a fixed built-in constraint system, not user-defined
type classes or arbitrary instances.

## 5. Reject predictable failures before execution

### Equality

Infer `Eq a` for `\x -> \y -> x == y` and retain it through the same mechanisms
as `Append`. Allow primitive values and composite data whose stored payloads are
comparable. Reject functions, including those nested in lists, records, tuples,
Maybe/Either, or custom data. A constructor's phantom type parameter does not
require equality if no payload uses it. Recursive data analysis must terminate.
Private declaration metadata still determines comparability without exposing
private constructors.

Keep ordinary empty-data comparisons such as `[] == []` and `Nothing == Nothing`
usable. At a closed execution entry point, an otherwise unconstrained type variable
used only in `Eq` obligations and absent from the environment/result may default
to `Unit`. Never default a variable constrained to a function or another concrete
type; retain constraints on exported or returned functions. Test this narrow rule
separately from ordinary generalization and ambiguous `Append` constraints.

Qualified annotations use `Eq a => a -> a -> Bool`, or
`(Eq a, Append a) => a -> a -> Bool` when both obligations apply. Explicitly
unconstrained polymorphic annotations cannot hide required constraints.

The evaluator retains defensive equality validation for internal callers that
bypass source type checking. Public scripts must fail at the static stage.

### Exhaustive patterns

Require every case expression to cover its scrutinee type. Diagnose a missing
pattern with a concrete witness, for example `Nothing`, `false`, or `[]`. Analyze
nested constructors, tuple combinations, closed/open record patterns, list
length/cons patterns, and recursive ADTs. Integer/string literal matches require
a remaining catch-all. Hidden constructors require a catch-all outside their
defining module; diagnostics must not disclose private names.

Report unreachable alternatives as warnings without changing first-match order.
Make coverage checking terminate on recursive types and include 1000-level
stress cases. Retain a defensive runtime failure for malformed internal ASTs.

## Supporting work required for 0.0.5.0

- Human-readable type and runtime diagnostics with file, line, column, and source
  excerpt, plus the relevant function/import context. Normal users should see
  `Int` and missing field names rather than internal `TVar`/`UnificationError`
  dumps. Keep structured errors available to tests and debug tooling.
- Source locations preserved through builtin application, desugaring, inference,
  and module loading, so the new errors identify the actual failing expression.
- One practical example that processes several files, reports individual read
  failures, continues with the others, and produces an independently checked
  summary. Also exercise EOF-driven input without turning normal EOF into failure.
- A migration guide covering recovery boundaries, structured error rendering,
  short-circuit effects, builtin shadowing/application, field precedence, duplicate
  record fields, qualified types, and previously accepted incomplete matches.
- Update README, SPEC, FEATURES, DEVELOPING, examples, agent guidance, and website
  together as each implemented behavior becomes available.

REPL history/completion remains the next ergonomic follow-up. JSON/HTTP, package
management, module-qualified type identities, wider numeric types/floats, general
effect types, user-defined type classes, GADTs, rank-N types, and a bytecode/JIT
runtime are separate projects. Preserve checked signed 32-bit arithmetic here.
Tail-call/runtime work needs measurements; do not infer a guaranteed depth or
performance ceiling from the separate compiler-lab application's guest workloads.

## Implementation order

1. Capture the current examples and diagnostics; add regression cases and source
   locations. Establish native baseline results and benchmark measurements.
2. Replace builtin-specific application parsing with a shared standard environment;
   migrate syntax/examples and prove strict argument behavior.
3. Add structured runtime errors, `attempt`/`raise`, and safe line/list helpers;
   verify nested boundaries, host-error mapping, and process-control exclusions.
4. Implement short-circuit booleans and migrate effect-dependent examples/tests.
5. Extend schemes with row variables and built-in constraints; update every
   inference, substitution, annotation, recursion, module, and REPL path.
6. Enforce equality constraints and exhaustive patterns; add useful diagnostics
   and migrate existing programs while preserving their intended behavior.
7. Complete the practical examples, migration guide, documentation, and native
   package acceptance runs before scheduling publication.

Existing tests remain. Where a test encodes a deliberately changed rule, update
its expectation and add the migration case; do not delete it or weaken its
assertions. Parser/type failures must never substitute for an intended runtime
failure, or vice versa.

## Release acceptance

| Area | Required evidence |
| --- | --- |
| Recovery | Success/failure values; every error category; nesting/rethrow; handler failure; effects before/after failure; exit/cancellation exclusions |
| Real I/O | Missing paths, permission failures where supported, invalid UTF-8, EOF vs blank input, failed writes/flushes, retained file effects, Unicode/spaced paths |
| Functions | Bare/aliased/shadowed/partially applied builtins; constructor functions; strict argument ordering; higher-order use; application/field precedence |
| Inference | Multiple and extra record fields; closed annotations; row occurs/kind checks; polymorphic append/equality; constrained annotations and recursive/imported/REPL uses |
| Static rejection | Nested callable equality, invalid append types, missing fields, incomplete nested patterns, unreachable warnings, private constructor behavior |
| Test quality | Exact values, stdout, files, and exit codes; generated properties against independent expectations; deliberate wrong implementations rejected |
| Full project | `stack test`; expanded properties; every script, example, and module export; rendered documentation examples; CLI/REPL and shebang behavior |
| Performance | Full native parser/inference/evaluator stress tests, full Criterion/Weigh measurements on a matching baseline, investigation of regressions over 10% |
| Platforms/packages | Ubuntu, macOS, Windows CI; source archive contents; exact extracted native binaries; installed invocation outside the checkout |

Use `stack test --test-arguments='--qc-max-success=1000 --seed=42'` for expanded
properties and `stack bench` for measurements. Run the existing corpus assertion
checker, documentation checker, release-corpus runner, and native packaging checks
as documented in DEVELOPING.md. Record platform-specific exclusions explicitly.
The one-iteration benchmark CI job alone does not establish performance.

Preparation does not publish a release. Synchronize package/CLI versions and
release-facing documentation after implementation passes these requirements;
publication remains a separate action through the existing release workflow.
