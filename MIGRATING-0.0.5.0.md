# Migrating to Kai 0.0.5.0

Kai keeps strict evaluation, immutable values, lexical scope and inferred types.
The changes below make recovery explicit and reject more mistakes before a script
performs effects. Function types still describe inputs and outputs, not purity.

## Recover from failures inside an action

`Right (readFile path)` does not catch a failed read: its argument executes first.
Pass a function to `attempt` so the operation executes inside the boundary:

```kai
// expect: ()
// stdout: "Using defaults\nContinuing\n"
case attempt (\unit -> readFile "missing-settings.txt") of
  Right text -> print text
  | Left (IOError NotFound _ _ _) -> print "Using defaults"
  | Left other -> raise other
print "Continuing"
```

`attempt : (Unit -> a) -> Either Error a` invokes the action once. A nested
boundary handles its action's recoverable errors; `raise` rethrows an error or
raises `UserError "message"`. Writes, output and environment changes completed
before a failure remain. The handler executes after its boundary has completed.
Parse/type/import-source errors, `exit`, cancellation and interpreter faults do
not become `Left` values.

## Match structured errors, not diagnostic text

Use `DivisionByZero`, `ArithmeticOverflow`, `EmptyList operation`, `EndOfInput`,
`UserError message`, or `IOError category operation path detail`. I/O categories
are `NotFound`, `PermissionDenied`, `AlreadyExists`, `InvalidPath`,
`InvalidEncoding`, `ResourceBusy` and `OtherIO`. Host detail varies by platform.

Normal CLI errors now show readable types and file/line/column excerpts, with
function and import context. Update tools that depended on text such as
`UnificationError TBool TInt` or `Runtime error: DivByZero`. Debug output retains
structured internal errors. Script directives such as `// expect: error DivByZero`
continue to assert the internal error independently of source positions.

## Treat EOF as normal input completion

`input` raises `EndOfInput` at EOF. `readLine ()` returns `Nothing` instead, while
a blank line is `Just ""`. `headMaybe` and `tailMaybe` provide optional list access;
`head` and `tail` remain partial and raise `EmptyList` on an empty list.

```kai
// expect: Nothing
// stdin: ""
readLine ()
```

The [file report example](examples/file_report.kai) takes file paths as arguments,
or reads one path per stdin line until EOF. It reports each read failure, continues
with later files, and prints successful/failed counts and total characters. Blank
input lines are ignored; spaces within paths remain part of the path. A completed
report exits successfully even if some or all file reads failed.

```bash
kai examples/file_report.kai first.txt missing.txt "other file.txt"
printf '%s\n' first.txt second.txt | kai examples/file_report.kai
```

## Make required effects explicit before boolean guards

`false and rhs` and `true or rhs` skip `rhs`. Both operands still type-check as
booleans. If both computations must execute, bind their results first:

```kai
// expect: false
// stdout: "checked\n"
let left = false
let right = do { print "checked"; true }
left and right
```

For guarding unsafe computations, use short-circuiting directly:

```kai
false and (1 / 0 == 0)  // => false
```

## Builtins follow ordinary function application and shadowing

Callable builtin names are ordinary identifiers. Aliases, partial application and
higher-order calls use the same rules as user-defined functions:

```kai
map length [[1], [2, 3]]  // => [1, 2]
let firstTwo = take 2 in firstTwo [1, 2, 3]  // => [1, 2]
```

A local `length`, `print` or `readFile` binding takes precedence over the standard
function. Rename a colliding local binding, or capture the builtin before shadowing:

```kai
let listLength = length in
let length = \text -> strLength text in
(listLength [1, 2], length "Kai")  // => (2, 3)
```

Supplied arguments still execute once when supplied, including partial
application. `head([1, 2])` remains valid. Parenthesize signed arguments, as in
`take (-1) xs`. `input`, `args` and `getCurrentDirectory` retain their zero-argument
behavior.

## Parenthesize field access on a function result

`f x.field` now means `f (x.field)`. To select a field of the result, write
`(f x).field`. `record.fn x` continues to call a function stored in a record.

```kai
let wrap = \x -> {value = x} in (wrap 7).value  // => 7
let record = {fn = \x -> x + 1} in record.fn 7  // => 8
```

## Use open records for reusable accessors; remove duplicate fields

Accessors infer the fields they require and accept additional fields. Explicit
`{a : Int}` annotations remain closed; `{a : Int | row}` permits extra fields:

```kai
let get : {a : Int | row} -> Int = \record -> record.a in
get {a = 7, extra = true}  // => 7
```

Duplicate fields are static errors, including duplicates introduced by shared
rows. Replace a literal such as `{a = old, a = new}` with `{a = new}`. If evaluating
`old` is intentional, sequence `discard old` explicitly before the literal.
Rows have a distinct kind; a row variable cannot also name a value type variable.

## Preserve equality and concatenation constraints in annotations

Generic helpers retain their requirements rather than prematurely choosing a
concrete type. Annotated helpers must state those requirements:

```kai
let append : Append a => a -> a -> a = \left -> \right -> left ++ right in
(append "a" "b", append [1] [2])  // => ("ab", [1, 2])

let same : Eq a => a -> a -> Bool = \left -> \right -> left == right in
same (Just 1) (Just 1)  // => true
```

Use `(Eq a, Append a) => ...` when both are required. `Append` supports strings
and lists; it has no default for unresolved ambiguity. `Eq` rejects functions,
including functions stored inside containers or custom types. Compare a suitable
data projection or explicit identifier when your value contains a function.
Empty-data comparisons such as `[] == []` remain valid. Users cannot define new
classes or instances.

## Cover every case and group nested matches

Previously accepted incomplete matches are now static errors. Add the missing
alternative and decide its behavior explicitly; this example supplies a default:

```kai
let optionalNumber = Nothing in
case optionalNumber of Just value -> value | Nothing -> 0  // => 0
```

Integer/string literal matches and matches involving hidden constructors need a
catch-all. Nested patterns must cover all combinations. An alternative covered
by earlier branches warns on stderr while preserving first-match behavior.
Reorder or remove redundant alternatives in your scripts as appropriate.

A nested `case` consumes its own `|` alternatives. Parenthesize it to resume an
outer match:

```kai
case Just true of
  Just flag -> (case flag of true -> 1 | false -> 2)
  | Nothing -> 0  // => 1
```

Run `stack test` when migrating the repository. Script result/error directives,
exact stdout fixtures, real stdin, example behavior and documentation claims are
checked separately so a parse failure cannot masquerade as successful recovery.
