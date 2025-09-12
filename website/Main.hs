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
            <div .stat-number>238
            <div .stat-label>Tests Passing
          <div .stat-item>
            <div .stat-number>5
            <div .stat-label>Core Types
          <div .stat-item>
            <div .stat-number>46
            <div .stat-label>Script Tests
          <div .stat-item>
            <div .stat-number>v0.0.3.1
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
            <p>Full Hindley-Milner type inference with unification and occurs check for ints, bools, strings, and functions.
          <div .feature>
            <h3>Clean Syntax
            <p>Haskell-like lambdas, precedence, keywords, and multi-statement files with expression-only core.
          <div .feature>
            <h3>String Support
            <p>String literals, escapes (\", \\, \n), concatenation (++), and print statements (returning unit) with proper type checking.
          <div .feature>
            <h3>Comprehensive Testing
            <p>238 tests with clear pass/fail indicators, property-based testing, and script evaluation.
          <div .feature>
            <h3>Developer Experience
            <p>CLI with help, inline evaluation, file execution, and comprehensive documentation.
          <div .feature>
            <h3>Type Safety
            <p>All expressions type-checked before evaluation with descriptive error messages.

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
            <code>stack exec kai path/to/script.kai

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
          <h3>Let Bindings
          <div .code-example>
            <code>let x = 42 in x + 1
            <code>letrec factorial = \\n -> if n == 0 then 1 else n * (factorial (n - 1)) in factorial 5

        <div .element-block>
          <h3>Print Statements
          <div .code-example>
            <code>print "Hello"    <!-- returns () -->
            <code>print (42 + 1)  <!-- returns () -->

      <section #examples>
        <h2>Example Expressions
        <div .element-block>
          <h3>Arithmetic & Logic
          <div .code-example>
            <code>42 * (10 - 3)
            <code>5 > 3 and true
            <code>not false
            <br>
            <code>-5 + 3
            <code>10 / 2
            <code>7 == 7

        <div .element-block>
          <h3>Conditionals
          <div .code-example>
            <code>if 5 > 3 then 42 * 2 else 0
            <br>
            <code>if true and false then 1 else 2

        <div .element-block>
          <h3>Lambda Functions & Application
          <div .code-example>
            <code>\\x -> x + 1
            <br>
            <code>(\\x -> x + 1) 5
            <code>(\\x -> x * x) 4
            <br>
            <code>(\\f -> f 10) (\\n -> n * 2)

        <div .element-block>
          <h3>Let Bindings & Recursion
          <div .code-example>
            <code>let add = \\x -> \\y -> x + y in add 5 3
            <br>
            <code>letrec factorial = \\n -> if n == 0 then 1 else n * (factorial (n - 1)) in factorial 5
            <br>
            <code>let compose = \\f -> \\g -> \\x -> f (g x) in compose (\\x -> x * 2) (\\x -> x + 1) 10

        <div .element-block>
          <h3>Strings & Print
          <div .code-example>
            <code>"Hello, " ++ "World"
            <br>
            <code>print ("The answer is " ++ "42")
            <br>
            <code>print (if 5 > 3 then "yes" else "no")
            <br>
            <code>(\\x -> print (x ++ "!")) "Hi"

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
            <span>Limited to single-file scripts (no modules or imports)
            <br>
            <span .limitation>×
            <span>No data structures for complex data manipulation (lists, maps, records)
            <br>
            <span .limitation>×
            <span>Limited I/O (only print statements, no input or file operations)
            <br>
            <span .limitation>×
            <span>No REPL for interactive experimentation
            <br>
            <span .limitation>×
            <span>No standard library (even basic functions like length, head)
            <br>
            <span .limitation>×
            <span>No error recovery (one parse error stops execution)
            <br>
            <span .limitation>×
            <span>No way to handle errors gracefully

      <section #roadmap>
        <h2>Current Status (v0.0.3.1) & Roadmap
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
              <p>Hindley-Milner inference with unification and occurs check
          <div .timeline-item>
            <div .timeline-marker data-step="5">
            <div .timeline-content>
              <h3>Let Bindings (Done)
              <p>Variable bindings and recursive function definitions with letrec
          <div .timeline-item .current>
            <div .timeline-marker .current data-step="6">
            <div .timeline-content>
              <h3>Data Structures (Planned)
              <p>Lists, records, pattern matching, error handling
          <div .timeline-item>
            <div .timeline-marker data-step="7">
            <div .timeline-content>
              <h3>I/O & Effects (Planned)
              <p>File I/O, input mechanisms, controlled imperative features
          <div .timeline-item>
            <div .timeline-marker data-step="8">
            <div .timeline-content>
              <h3>Modules & Stdlib (Planned)
              <p>Module system, standard library, functional-first operations
          <div .timeline-item>
            <div .timeline-marker data-step="9">
            <div .timeline-content>
              <h3>Developer Tools (Planned)
              <p>REPL, formatter, linter, IDE support, package manager

      <footer>
        <p .copyright>Kai Language · Functional-first scripting · Implemented in Haskell
  |]

main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:3000"
  staticSite <- static "website/static"
  warp 3000 (HelloWorld staticSite)
