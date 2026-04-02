{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Yesod.Static

newtype HelloWorld = HelloWorld
  { getStatic :: Static
  }

mkYesod "HelloWorld" [parseRoutes|
/         HomeR    GET
/static   StaticR  Static getStatic
/favicon.ico FaviconR GET
|]

instance Yesod HelloWorld

-- Handlers (define each exactly once)
getFaviconR :: Handler TypedContent
getFaviconR = redirect (StaticR (StaticRoute ["favicon.ico"] []))

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Kai Language"
  addStylesheet (StaticR (StaticRoute ["style.css"] []))
  toWidgetHead [hamlet|
    <link rel="icon" href=@{StaticR (StaticRoute ["favicon.ico"] [])}>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
  |]
  [whamlet|
    <div .container>
      <header>
        <h1>Kai
        <p .tagline>A functional-first scripting language with static typing
        <div .stats-container>
          <div .stat-item>
            <div .stat-number>583
            <div .stat-label>Tests Passing
          <div .stat-item>
            <div .stat-number>8
            <div .stat-label>Core Types
          <div .stat-item>
            <div .stat-number>27
            <div .stat-label>Built-in Functions
          <div .stat-item>
            <div .stat-number>v0.0.4.3
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
            <p>Static type inference with unification, occurs check, and generalized let-polymorphism for ints, bools, strings, functions, and data structures.
          <div .feature>
            <h3>Clean Syntax
            <p>Haskell-like lambdas, `do { ... }` blocks, precedence, keywords, and multi-statement files with expression-only core.
          <div .feature>
            <h3>Interactive I/O & Conversions
            <p>User input with `input`, readable effect sequencing via `do` blocks, type conversions (`parseInt`, `toString`, `show`), and practical examples including text analysis, CLI tools, and interactive workflows.
          <div .feature>
            <h3>Comprehensive Testing
            <p>583 passing examples with property-based testing, script evaluation, CLI coverage, stress checks, and example smoke coverage.
          <div .feature>
            <h3>Developer Experience
            <p>CLI with help, inline evaluation, file execution, --debug flag for development, and comprehensive documentation.
          <div .feature>
            <h3>Module System
            <p>Import modules with `import ModuleName`, top-level definitions with `let` and `letrec`, mutual recursion support, circular import detection, explicit exports, and module resolution.

      <section #quickstart>
        <h2>Quick Start
        <div .element-block>
          <h3>Install & Run
          <div .code-example>
            <code>stack build && stack test
            <br>
            <code>stack exec kai -- --help
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
          <div .code-example>
            <code>42
            <code>-3
            <code>true
            <code>false
            <code>"hi"
            <code>()

        <div .element-block>
          <h3>Operators
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
          <div .code-example>
            <code>if condition then expr1 else expr2

        <div .element-block>
          <h3>Lambda Functions
          <div .code-example>
            <code>\\x -> x + 1
            <code>\\f -> f 42

        <div .element-block>
          <h3>Let Bindings, Blocks & Wildcards
          <div .code-example>
            <code>let x = 42 in x + 1
            <code>do { print "hello"; 42 }      <!-- Preferred sequencing form -->
            <code>let _ = expensiveCall in 42   <!-- Explicit discard when needed -->
            <code>letrec factorial = \\n -> if n == 0 then 1 else n * (factorial (n - 1)) in factorial 5

        <div .element-block>
          <h3>Type Annotations & Conversions
          <div .code-example>
            <code>let add : Int -> Int -> Int = \\x : Int -> \\y : Int -> x + y
            <code>parseInt "42"     <!-- String to Maybe Int -->
            <code>toString 100      <!-- Int to String -->
            <code>discard 42        <!-- Evaluates and discards any value -->
            <code>show (42 + 3)     <!-- Any type to String -->

        <div .element-block>
          <h3>Data Structures
          <div .code-example>
            <code>[1, 2, 3]         <!-- Lists -->
            <code>(1, "hi", true)   <!-- Tuples -->
            <code>{a = 1, b = true} <!-- Records -->
            <code>Just 42           <!-- Maybe values -->
            <code>Left "error"      <!-- Either values -->

        <div .element-block>
          <h3>List & String Functions
          <div .code-example>
            <code>map filter foldl
            <code>length reverse take drop zip
            <code>split join trim replace strLength
            <code>head tail null fst snd

        <div .element-block>
          <h3>Interactive I/O & File Operations
          <div .code-example>
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
          <div .code-example>
            <code>import TextAnalysis
            <code>let text = case args of [] -> "Kai examples should stay practical, typed, and honest." | path :: _ -> readFile path
            <code>let summary = summarize text
            <code>print ("Preview: " ++ summary.preview)
            <code>case summary.firstLongWord of Just word -> print ("First long word: " ++ word) | Nothing -> print "First long word: none"

        <div .element-block>
          <h3>Validated CLI Scripts
          <div .code-example>
            <code>let validateNames : [String] -> Either String [String] = \\cliArgs -> if null cliArgs then Left "Usage..." else Right cliArgs
            <code>letrec greetAll : [String] -> Unit = \\names -> case names of [] -> print "All greetings sent." | name :: rest -> do { print ("Hello, " ++ name ++ "!"); greetAll rest }
            <code>case validateNames args of Left message -> print message | Right names -> greetAll names

        <div .element-block>
          <h3>List Processing & Let Polymorphism
          <div .code-example>
            <code>let report = {count = length numbers, evenCount = length (filter even numbers), total = foldl (\\acc -> \\n -> acc + n) 0 numbers, labels = zip numbers (map (\\n -> if n > 20 then "high" else "steady") numbers)}
            <code>let tag = \\label -> \\value -> {label = label, value = value}
            <code>show (tag "total" (report.total))
            <code>show (tag "status" "ready")

        <div .element-block>
          <h3>Interactive Input & Parsing
          <div .code-example>
            <code>let parseSecret : [String] -> Int = \\cliArgs -> case cliArgs of value :: _ -> (case parseInt value of Just n -> n | Nothing -> 42) | [] -> 42
            <code>let promptGuess : Int -> String = \\attempt -> do { print ("Attempt " ++ toString attempt ++ ": enter a guess"); input }
            <code>case parseInt guessText of Just guess -> ... | Nothing -> do { print "Please enter an integer."; loop secret attempt }

        <div .element-block>
          <h3>File I/O & Arguments
          <div .code-example>
            <code>let outputPath = case args of path :: _ -> path | [] -> "kai_output.txt"
            <code>let content = join "\n" ["Kai writes files", "Kai reads them back", "Kai keeps scripts typed"]
            <code>do { writeFile outputPath content; print ("Wrote " ++ outputPath) }
            <code>print ("Read back: " ++ replace "\n" " | " (readFile outputPath))

        <div .element-block>
          <h3>Wildcard Patterns
          <div .code-example>
            <code>case Right {ok = true, message = "loaded"} of Right {ok = true, message = _} -> "status: success" | Left _ -> "status: failure" | Right _ -> "status: unexpected"
            <code>case (42, "kai", true) of (_, name, true) -> "tuple for " ++ name | _ -> "tuple mismatch"
            <code>case [1, 2, 3, 4] of _ :: _ -> "list has values" | [] -> "list is empty"

        <div .element-block>
          <h3>Type Safety Examples
          <div .code-example>
            <code>1 + true
            <span .error-comment>// Type error: TypeMismatch TInt TBool
            <br>
            <code>if 5 then 1 else 2
            <span .error-comment>// Type error: ExpectedBool TInt

      <section #limitations>
        <h2>Current Limitations
        <div .element-block>
          <h3>Current Limitations
          <div .code-example>
            <span .limitation>×
            <span>No REPL for interactive experimentation
            <br>
            <span .limitation>×
            <span>No error recovery (one parse error stops execution)
            <br>
            <span .limitation>×
            <span>Integer-only arithmetic (no floating-point)
            <br>
            <span .limitation>×
            <span>No custom data types (only built-in types)
            <br>
            <span .limitation>×
            <span>No polymorphic recursion for recursively-defined functions

      <section #roadmap>
        <h2>Current Status (v0.0.4.3) & v0.0.4.4 Focus
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
              <p>Static type inference with unification and occurs check
          <div .timeline-item>
            <div .timeline-marker data-step="5">
            <div .timeline-content>
              <h3>Let Bindings (Done)
              <p>Variable bindings and recursive function definitions with letrec
          <div .timeline-item>
            <div .timeline-marker data-step="6">
            <div .timeline-content>
              <h3>Type Annotations & Conversions (Done)
              <p>Optional type annotations, parseInt/toString/show functions, interactive I/O, wildcards, sequencing
          <div .timeline-item>
            <div .timeline-marker data-step="7">
            <div .timeline-content>
              <h3>Data Structures (Done)
              <p>Lists, tuples, records, pattern matching, Maybe/Either error handling
          <div .timeline-item>
            <div .timeline-marker data-step="8">
            <div .timeline-content>
              <h3>Standard Library (Done)
              <p>List functions (map, filter, fold, zip), string functions (split, join, trim), 27 built-ins
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
          <div .timeline-item .current>
            <div .timeline-marker .current data-step="11">
            <div .timeline-content>
              <h3>v0.0.4.4 Focus
              <p>REPL, custom data types, stronger pattern matching, and essential scripting stdlib work
          <div .timeline-item>
            <div .timeline-marker data-step="12">
            <div .timeline-content>
              <h3>Later Releases
              <p>Formatter, linter, IDE support, package manager, HTTP/JSON, and deeper optimization work

      <footer>
        <p .copyright>Kai Language · Functional-first scripting · Implemented in Haskell
  |]

main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:3000"
  staticSite <- static "website/static"
  warp 3000 (HelloWorld staticSite)
