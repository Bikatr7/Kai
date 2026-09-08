{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Yesod
import Yesod.Static

data Site = Site
  { siteStatic :: Static
  , siteExportToken :: Text.Text
  }

mkYesod "Site" [parseRoutes|
/         HomeR    GET
/static   StaticR  Static siteStatic
/favicon.ico FaviconR GET
|]

instance Yesod Site

-- Handlers (define each exactly once)
getFaviconR :: Handler TypedContent
getFaviconR = redirect (StaticR (StaticRoute ["favicon.svg"] []))

getHomeR :: Handler Html
getHomeR = do
  site <- getYesod
  addHeader "X-Kai-Site" "kai-language"
  if Text.null (siteExportToken site)
    then pure ()
    else addHeader "X-Kai-Export-Token" (siteExportToken site)
  defaultLayout $ do
    setTitle "Kai Language"
    addStylesheet (StaticR (StaticRoute ["style.css"] []))
    toWidgetHead [hamlet|
      <link rel="icon" type="image/svg+xml" href=@{StaticR (StaticRoute ["favicon.svg"] [])}>
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <meta name="kai-site" content="kai-language">
    |]
    [whamlet|
    <div .container>
      <header>
        <h1>Kai
        <p .tagline>A functional-first scripting language with static typing
        <div .stats-container>
          <div .stat-item>
            <div .stat-number>Hspec
            <div .stat-label>Tests & Properties
          <div .stat-item>
            <div .stat-number>4
            <div .stat-label>Base Types
          <div .stat-item>
            <div .stat-number>44
            <div .stat-label>Built-in Operations
          <div .stat-item>
            <div .stat-number>v0.0.5.0
            <div .stat-label>Current Version

      <nav>
        <ul>
          <li><a href="#features">Features
          <li><a href="#quickstart">Quick Start
          <li><a href="#elements">Elements
          <li><a href="#examples">Examples
          <li><a href="#limitations">Limitations
          <li><a href="#roadmap">Roadmap

      <section #features>
        <h2>Core Features
        <div .features-grid>
          <div .feature>
            <h3>Static Typing & Inference
            <p>Static type inference with unification, occurs check, generalized let-polymorphism, and explicitly annotated polymorphic recursion for ints, bools, strings, functions, and data structures.
          <div .feature>
            <h3>Clean Syntax
            <p>Typed lambdas, tuple annotations, partially applied builtins, `do { ... }` blocks, and multi-statement UTF-8 source files.
          <div .feature>
            <h3>Interactive I/O & Conversions
            <p>User input with `input`, UTF-8 text-file reads and writes, readable effect sequencing via `do` blocks, type conversions (`parseInt`, `toString`, `show`), and practical examples including text analysis, CLI tools, expression trees, and workspace-style file flows.
          <div .feature>
            <h3>Comprehensive Testing
            <p>Unit, property, script, and integration tests check values, exact printed output, real input, modules, executable shebang scripts, and deeply nested expressions.
          <div .feature>
            <h3>Developer Experience
            <p>CLI plus a multiline REPL with `:type`, `:load`, and `:reload`, alongside file execution, --debug, --version/-V, and source diagnostics with file, line, column and call/import context.
          <div .feature>
            <h3>Module System
            <p>Import modules with `import ModuleName`, define values with `let` and `letrec`, and control constructor visibility with explicit exports. Recursive initializers run in source order and stop on errors.

      <section #quickstart>
        <h2>Quick Start
        <p>The CLI builds on Linux, macOS, and Windows. Build the website and use the POSIX runner/exporter on Linux or macOS; see DEVELOPING.md for the Windows test setup.
          <div .element-block>
          <h3>Install & Run
          <div .code-example>
            <code>stack build && stack test
            <br>
            <code>stack exec kai --
            <br>
            <code>stack exec kai -- --help
            <br>
            <code>stack exec kai -- --version
            <br>
            <code>stack exec kai -- -e "print (42 + 1)"
            <br>
            <code>stack exec kai -- --debug -e "42 + 1"
            <br>
            <code>stack exec kai -- path/to/script.kai
            <br>
            <code>stack exec kai -- examples/text_analysis.kai
            <br>
            <code>stack exec kai -- examples/calculator.kai

        <div .element-block>
          <h3>Install CLI
          <p>Files and -e expressions display explicit print output. The REPL displays expression results automatically.
          <p>Link the runner to this checkout and add it to PATH. It works from other directories; keep the checkout in place or use stack install for a standalone binary. Set KAI_BIN to select an executable explicitly.
          <div .code-example>
            <code>make install
            <br>
            <code>export PATH="$HOME/.local/bin:$PATH"
            <br>
            <code>kai tests/arithmetic.kai

      <section #elements>
        <h2>Language Elements

        <div .element-block>
          <h3>Basic Types
          <div .code-example .kai-example>
            <code>42
            <code>-3
            <code>true
            <code>false
            <code>not not true
            <code>"hi"
            <code>()

        <div .element-block>
          <h3>Operators
          <p>Application and field access form one left-associated chain: f x.field means (f x).field. Write f (x.field) to pass a field value. Both operands of and/or execute; use if to evaluate only one branch.
          <div .code-example>
            <code>+
            <code>-
            <code>*
            <code>/
            <code>++
            <code>==
            <code><
            <code>>
            <code>and
            <code>or
            <code>not

        <div .element-block>
          <h3>Control Flow
          <div .code-example .kai-example>
            <code>if 5 > 3 then 42 else 0

        <div .element-block>
          <h3>Lambda Functions
          <div .code-example .kai-example>
            <code>\x -> x + 1
            <code>\f -> f 42

        <div .element-block>
          <h3>Let Bindings, Blocks & Wildcards
          <div .code-example .kai-example>
            <code>let x = 42 in x + 1
            <code>do { print "hello"; 42 }      <!-- Preferred sequencing form -->
            <code>let _ = print "hello" in 42   <!-- Explicit discard when needed -->
            <code>letrec factorial = \n -> if n == 0 then 1 else n * (factorial (n - 1)) in factorial 5

        <div .element-block>
          <h3>Type Annotations & Conversions
          <div .code-example .kai-example>
            <code>let add : Int -> Int -> Int = \x : Int -> \y : Int -> x + y
            <code>parseInt "42"     <!-- String to Maybe Int -->
            <code>toString 100      <!-- Int to String -->
            <code>discard 42        <!-- Evaluates and discards any value -->
            <code>show (42 + 3)     <!-- Any type to String -->

        <div .element-block>
          <h3>Data Structures
          <div .code-example .kai-example>
            <code>[1, 2, 3]         <!-- Lists -->
            <code>(1, "hi", true)   <!-- Tuples -->
            <code>{a = 1, b = true} <!-- Records -->
            <code>Just 42           <!-- Maybe values -->
            <code>Left "error"      <!-- Either values -->

        <div .element-block>
          <h3>List & String Functions
          <div .code-example .kai-example>
            <code>map (\n -> n * 2) [1, 2, 3]
            <code>filter (\n -> n > 1) [1, 2, 3]
            <code>foldl (\total -> \n -> total + n) 0 [1, 2, 3]
            <code>join ", " (split " " "hello world")
            <code>fst (42, "kai")

        <div .element-block>
          <h3>Interactive I/O & File Operations
          <p>Print flushes each value to stdout. Output failures stop subsequent effects and return a runtime error.
          <div .code-example .kai-example>
            <code>input             <!-- Read line from stdin -->
            <code>print "Hello"     <!-- Print and return () -->
            <code>readFile "path"   <!-- Read file contents -->
            <code>writeFile "path" "content"  <!-- Write to file -->
            <code>args              <!-- Command-line arguments -->
            <code>do { print "A"; print "B"; 42 }  <!-- Sequence expressions -->

      <section #examples>
        <h2>Example Scripts & Patterns
        <div .element-block>
          <h3>Module-Based Text Analysis
          <div .code-example .kai-example>
            <code>import TextAnalysis
            <code>let text = case args of [] -> "Kai examples should stay practical, typed, and honest." | path :: _ -> readFile path
            <code>let summary = summarize text
            <code>print ("Preview: " ++ summary.preview)
            <code>case summary.firstLongWord of Just word -> print ("First long word: " ++ word) | Nothing -> print "First long word: none"

        <div .element-block>
          <h3>Validated CLI Scripts
          <div .code-example .kai-example>
            <code>let validateNames : [String] -> Either String [String] = \cliArgs -> if null cliArgs then Left "Usage..." else Right cliArgs
            <code>letrec greetAll : [String] -> Unit = \names -> case names of [] -> print "All greetings sent." | name :: rest -> do { print ("Hello, " ++ name ++ "!"); greetAll rest }
            <code>case validateNames args of Left message -> print message | Right names -> greetAll names

        <div .element-block>
          <h3>List Processing & Let Polymorphism
          <div .code-example .kai-example>
            <code>let numbers = [10, 15, 20, 25]
            <code>let even = \n -> (n / 2) * 2 == n
            <code>let report = {count = length numbers, evenCount = length (filter even numbers), total = foldl (\acc -> \n -> acc + n) 0 numbers, labels = zip numbers (map (\n -> if n > 20 then "high" else "steady") numbers)}
            <code>let tag = \label -> \value -> {label = label, value = value}
            <code>show (tag "total" (report.total))
            <code>show (tag "status" "ready")

        <div .element-block>
          <h3>Interactive Input & Parsing
          <div .code-example .kai-example>
            <code>let parseSecret : [String] -> Int = \cliArgs -> case cliArgs of value :: _ -> (case parseInt value of Just n -> n | Nothing -> 42) | [] -> 42
            <code>let promptGuess : Int -> String = \attempt -> do { print ("Attempt " ++ toString attempt ++ ": enter a guess"); input }
            <code>let secret = parseSecret args
            <code>let guessText = promptGuess 1
            <code>case parseInt guessText of Just guess -> print (if guess == secret then "Correct!" else "Try again.") | Nothing -> print "Please enter an integer."

        <div .element-block>
          <h3>Workspace-Style File I/O
          <div .code-example .kai-example>
            <code>let workspace = case args of path :: _ -> path | [] -> "."
            <code>let reportPath = if workspace == "." then "kai_output.txt" else workspace ++ "/report.txt"
            <code>do { if workspace == "." then () else createDirectory workspace; writeFile reportPath "Kai report"; appendFile reportPath "\nComplete" }
            <code>print ("Read back: " ++ replace "\n" " | " (readFile reportPath))

        <div .element-block>
          <h3>Custom Data Types
          <div .code-example .kai-example>
            <code>data Expr = Lit Int | Add (Expr) (Expr) | Mul (Expr) (Expr) | Neg (Expr)
            <code>let liftByFive = Add (Lit 5)
            <code>let program = Mul (liftByFive (Lit 3)) (Neg (Lit 2))
            <code>letrec eval = \expr -> case expr of Lit n -> n | Add l r -> eval l + eval r | Mul l r -> eval l * eval r | Neg inner -> 0 - eval inner
            <code>print (eval program)

        <div .element-block>
          <h3>Wildcard Patterns
          <div .code-example .kai-example>
            <code>case Right {ok = true, message = "loaded"} of Right {ok = true, message = _} -> "status: success" | Left _ -> "status: failure" | Right _ -> "status: unexpected"
            <code>case (42, "kai", true) of (_, name, true) -> "tuple for " ++ name | _ -> "tuple mismatch"
            <code>case [1, 2, 3, 4] of _ :: _ -> "list has values" | [] -> "list is empty"

        <div .element-block>
          <h3>Reusable Helpers and Recovery
          <div .code-example .kai-example>
            <code>let append = \left -> \right -> left ++ right
            <code>let total = \record -> record.a + record.b
            <code>print (append "a" "b", append [1] [2], total {a = 2, b = 3, extra = true})
            <code>case attempt (\unit -> 1 / 0) of Left DivisionByZero -> print "Recovered" | Left other -> raise other | Right value -> print value
          <p>Reusable functions retain Eq and Append requirements. Callable equality and incomplete matches are rejected before effects run. Unreachable alternatives produce warnings. Function types describe inputs and outputs without enforcing purity.

        <div .element-block>
          <h3>File Reports That Continue After Read Failures
          <p>Run examples/file_report.kai with file paths as arguments, or supply one path per stdin line until EOF. It reports each read failure, processes later files, and summarizes successful reads, failures and total characters.
          <p>See MIGRATING-0.0.5.0.md for recovery boundaries, changed boolean effects, builtin shadowing, field precedence, record rows, constraints and exhaustive matches.

        <div .element-block>
          <h3>Type Safety Examples
          <div .code-example>
            <code>1 + true
            <span .error-comment>// Type error: Cannot match Bool with Int.
            <br>
            <code>if 5 then 1 else 2
            <span .error-comment>// Type error: Cannot match Int with Bool.

      <section #limitations>
        <h2>Current Limitations
        <div .element-block>
          <h3>Current Limitations
          <div .code-example>
            <span .limitation>×
            <span>REPL is still minimal: no history or completion yet
            <br>
            <span .limitation>×
            <span>Unhandled errors stop scripts; attempt/raise provide explicit runtime recovery
            <br>
            <span .limitation>×
            <span>Integer-only arithmetic (no floating-point)
            <br>
            <span .limitation>×
            <span>Polymorphic recursive calls need explicit annotations; completed recursive definitions can still be generalized
            <br>
            <span .limitation>×
            <span>No JSON/HTTP/package manager yet; show/print are display rather than serialization

      <section #roadmap>
        <h2>Kai v0.0.5.0 & Roadmap
        <div .timeline>
          <div .timeline-item>
            <div .timeline-marker data-step="1">
            <div .timeline-content>
              <h3>Expressions (Done)
              <p>Ints, bools, strings (with ++), arithmetic (with unary minus), comparisons, conditionals
          <div .timeline-item>
            <div .timeline-marker data-step="2">
            <div .timeline-content>
              <h3>Lambda Functions (Done)
              <p>First-class functions, closures, application
          <div .timeline-item>
            <div .timeline-marker data-step="3">
            <div .timeline-content>
              <h3>Strings & Print (Done)
              <p>String literals, concatenation, print statements, multi-statement files
          <div .timeline-item>
            <div .timeline-marker data-step="4">
            <div .timeline-content>
              <h3>Type System (Done)
              <p>Static inference with open record rows, Eq/Append constraints, and occurs/kind checks
          <div .timeline-item>
            <div .timeline-marker data-step="5">
            <div .timeline-content>
              <h3>Let Bindings (Done)
              <p>Variable bindings and recursive definitions with letrec, including constants and guarded initialization
          <div .timeline-item>
            <div .timeline-marker data-step="6">
            <div .timeline-content>
              <h3>Type Annotations & Conversions (Done)
              <p>Optional type annotations, parseInt/toString/show functions, interactive I/O, wildcards, sequencing
          <div .timeline-item>
            <div .timeline-marker data-step="7">
            <div .timeline-content>
              <h3>Data Structures (Done)
              <p>Lists, tuples, records, custom data types with first-class constructor functions, exhaustive pattern matching, Maybe/Either error handling
          <div .timeline-item>
            <div .timeline-marker data-step="8">
            <div .timeline-content>
              <h3>Standard Library (Done)
              <p>List functions, string functions, file/directory/process helpers, a typed fixpoint combinator, and 44 built-ins
          <div .timeline-item>
            <div .timeline-marker data-step="9">
            <div .timeline-content>
              <h3>Top-Level Definitions & Modules (Done)
              <p>Module system with imports, top-level let/letrec definitions, mutual recursion support
          <div .timeline-item>
            <div .timeline-marker data-step="10">
            <div .timeline-content>
              <h3>File I/O & Scripting (Done)
              <p>readFile, writeFile, command-line arguments, practical scripting capabilities
          <div .timeline-item>
            <div .timeline-marker data-step="11">
            <div .timeline-content>
              <h3>v0.0.4.4 (Released 2026-07-11)
              <p>REPL, custom data types, constructor patterns, checked integers, sound composite inference, and essential scripting stdlib work
          <div .timeline-item>
            <div .timeline-marker data-step="12">
            <div .timeline-content>
              <h3>v0.0.4.5 (Released 2026-07-11)
              <p>Version flags, permission-preserving archives, checksums, automatic version-driven releases, pinned deployment baselines, and native verification of exact release downloads
          <div .timeline-item>
            <div .timeline-marker data-step="13">
            <div .timeline-content>
              <h3>v0.0.4.6
              <p>Partially applied builtins, typed lambdas and tuples, consistent recursion, private constructors, UTF-8 source and text files, checked output failures, exact script assertions, and reliable executable-script installation
          <div .timeline-item .current>
            <div .timeline-marker .current data-step="14">
            <div .timeline-content>
              <h3>v0.0.5.0
              <p>Structured errors with attempt/raise recovery; safe line input and optional list accessors; short-circuit booleans; ordinary builtin application; open record inference and reusable concatenation; static equality constraints and exhaustive patterns; source-aware diagnostics and migration examples
          <div .timeline-item>
            <div .timeline-marker data-step="15">
            <div .timeline-content>
              <h3>Later Releases
              <p>REPL history/completion, richer stdlib helpers, HTTP/JSON, formatter/linter and editor support, package management, module-qualified types, wider numbers, and later runtime/type-system work

      <footer>
        <p .copyright>Kai Language · Functional-first scripting · Implemented in Haskell
    |]

websitePort :: IO Int
websitePort = do
  configuredPort <- lookupEnv "PORT"
  case configuredPort of
    Nothing -> pure 3000
    Just rawPort ->
      case readMaybe rawPort of
        Just port | port >= 1 && port <= 65535 -> pure port
        _ -> ioError (userError "PORT must be an integer between 1 and 65535")

main :: IO ()
main = do
  port <- websitePort
  exportToken <- Text.pack . fromMaybe "" <$> lookupEnv "KAI_SITE_EXPORT_TOKEN"
  putStrLn ("Starting server on http://localhost:" ++ show port)
  staticSite <- static "website/static"
  warp port (Site staticSite exportToken)
