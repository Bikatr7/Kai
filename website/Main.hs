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
        <p .tagline>A minimal, statically typed expression language
        <div .stats-container>
          <div .stat-item>
            <div .stat-number>221
            <div .stat-label>Tests Passing
          <div .stat-item>
            <div .stat-number>4
            <div .stat-label>Core Types
          <div .stat-item>
            <div .stat-number>29
            <div .stat-label>Script Tests
          <div .stat-item>
            <div .stat-number>v0.0.3
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
            <p>String literals, concatenation (++), and print statements for output with proper type checking.
          <div .feature>
            <h3>Comprehensive Testing
            <p>221 tests with clear pass/fail indicators, property-based testing, and script evaluation.
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
            <code>lambda x -> x + 1
            <code>lambda f -> f 42

        <div .element-block>
          <h3>Print Statements
          <div .code-example>
            <code>print "Hello"
            <code>print (42 + 1)

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
            <code>lambda x -> x + 1
            <br>
            <code>(lambda x -> x + 1) 5
            <code>(lambda x -> x * x) 4
            <br>
            <code>(lambda f -> f 10) (lambda n -> n * 2)

        <div .element-block>
          <h3>Strings & Print
          <div .code-example>
            <code>"Hello, " ++ "World"
            <br>
            <code>print ("The answer is " ++ "42")
            <br>
            <code>print (if 5 > 3 then "yes" else "no")
            <br>
            <code>(lambda x -> print (x ++ "!")) "Hi"

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
          <h3>Language Completeness
          <div .code-example>
            <span .limitation>×
            <span>No variables or let-bindings (expressions only)
            <br>
            <span .limitation>×
            <span>No lists, records, or user-defined data types
            <br>
            <span .limitation>×
            <span>No top-level bindings, modules, or imports
            <br>
            <span .limitation>×
            <span>No REPL for interactive experimentation

        <div .element-block>
          <h3>Practical Limitations
          <div .code-example>
            <span .limitation>×
            <span>Limited to single-file scripts (no multi-file projects)
            <br>
            <span .limitation>×
            <span>No way to define reusable functions or constants
            <br>
            <span .limitation>×
            <span>No data structures for complex data manipulation
            <br>
            <span .limitation>×
            <span>No file I/O beyond print statements
            <br>
            <span .limitation>×
            <span>No error recovery (one parse error stops execution)

        <div .element-block>
          <h3>Developer Experience
          <div .code-example>
            <span .limitation>×
            <span>No standard library (even basic functions like length, head)
            <br>
            <span .limitation>×
            <span>No way to handle errors gracefully
            <br>
            <span .limitation>×
            <span>No syntax highlighting or IDE support
            <br>
            <span .limitation>×
            <span>No package manager for libraries

      <section #roadmap>
        <h2>Current Status (v0.0.3) & Roadmap
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
          <div .timeline-item .current>
            <div .timeline-marker .current data-step="5">
            <div .timeline-content>
              <h3>Variables & Data (Planned)
              <p>Let-bindings, lists, records, pattern matching, error handling
          <div .timeline-item>
            <div .timeline-marker data-step="6">
            <div .timeline-content>
              <h3>Modules & Stdlib (Planned)
              <p>Module system, standard library, file I/O, JSON handling
          <div .timeline-item>
            <div .timeline-marker data-step="7">
            <div .timeline-content>
              <h3>Developer Tools (Planned)
              <p>REPL, formatter, linter, IDE support, package manager

      <footer>
        <p .copyright>Kai Language · Implemented in Haskell
  |]

main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:3000"
  staticSite <- static "website/static"
  warp 3000 (HelloWorld staticSite)
