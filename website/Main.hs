{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Yesod.Static

data HelloWorld = HelloWorld 
    { getStatic :: Static
    }

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/static StaticR Static getStatic
/favicon.ico FaviconR GET
|]

instance Yesod HelloWorld

getFaviconR :: Handler TypedContent
getFaviconR = redirect $ StaticR $ StaticRoute ["favicon.ico"] []

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Kai Language"
    addStylesheet $ StaticR $ StaticRoute ["style.css"] []
    toWidgetHead [hamlet|<link rel="icon" href=@{StaticR $ StaticRoute ["favicon.ico"] []}>|]
    toWidgetHead [hamlet|<meta name="viewport" content="width=device-width, initial-scale=1.0">|]
    [whamlet|
        <div .container>
            <header>
                <h1>Kai
                <p .tagline>A minimal, statically typed expression language

            <nav>
                <ul>
                    <li><a href="#features">Features</a>
                    <li><a href="#elements">Elements</a>
                    <li><a href="#examples">Examples</a>
                    <li><a href="#roadmap">Roadmap</a>

            <section #features>
                <h2>Core Features
                <div .features-grid>
                    <div .feature>
                        <h3>Static Typing (MVP)
                        <p>Simple type checker for ints, bools, and functions.
                    <div .feature>
                        <h3>Clean Syntax
                        <p>Haskell-like lambdas, precedence, and keywords.
                    <div .feature>
                        <h3>Pure Expressions
                        <p>Predictable evaluation without side effects (core is pure).

            <section #elements>
                <h2>Language Elements
                
                <div .element-block>
                    <h3>Basic Types
                    <div .code-example>
                        <code>42</code> · <code>-3</code> · <code>true</code> · <code>false</code>
                
                <div .element-block>
                    <h3>Operators
                    <div .code-example>
                        <code>+</code> · <code>-</code> · <code>*</code> · <code>/</code> · <code>==</code> · <code><</code> · <code>></code> · <code>and</code> · <code>or</code> · <code>not</code>
                
                <div .element-block>
                    <h3>Control Flow
                    <div .code-example>
                        <code>if condition then expr1 else expr2</code>
                        
                <div .element-block>
                    <h3>Lambda Functions
                    <div .code-example>
                        <code>\x -> x + 1</code> · <code>\f -> f 42</code>

            <section #examples>
                <h2>Example Expressions
                <div .element-block>
                    <h3>Arithmetic & Logic
                    <div .code-example>
                        <code>42 * (10 - 3)</code> · <code>5 > 3 and true</code> · <code>not false</code>
                
                <div .element-block>
                    <h3>Conditionals
                    <div .code-example>
                        <code>if 5 > 3 then 42 * 2 else 0</code>
                        
                <div .element-block>
                    <h3>Lambda Functions & Application
                    <div .code-example>
                        <code>\x -> x + 1</code>
                        <br>
                        <code>(\x -> x + 1) 5</code> · <code>(\x -> x * x) 4</code>
                        
                <div .element-block>
                    <h3>Higher-Order Functions
                    <div .code-example>
                        <code>(\f -> f 42) (\x -> x + 1)</code>

            <section #roadmap>
                <h2>Current Status & Roadmap
                <div .timeline>
                    <div .timeline-item>
                        <div .timeline-marker data-step="1">
                        <div .timeline-content>
                            <h3>Expressions (Done)
                            <p>Ints, bools, arithmetic, comparisons, conditionals
                    <div .timeline-item .current>
                        <div .timeline-marker .current data-step="2">
                        <div .timeline-content>
                            <h3>Lambda Functions (Done)
                            <p>First-class functions, closures, application
                    <div .timeline-item>
                        <div .timeline-marker data-step="3">
                        <div .timeline-content>
                            <h3>Type System (In Progress)
                            <p>Improve inference and error messages
                    <div .timeline-item>
                        <div .timeline-marker data-step="4">
                        <div .timeline-content>
                            <h3>Variables & Modules (Planned)
                            <p>Let-bindings, top-level defs, modules/imports
                    <div .timeline-item>
                        <div .timeline-marker data-step="5">
                        <div .timeline-content>
                            <h3>Stdlib & I/O (Planned)
                            <p>Strings, lists, file/process/JSON utilities

            <footer>
                <p .copyright>Kai Language · Implemented in Haskell
    |]

main :: IO ()
main = do
    putStrLn "Starting server on http://localhost:3000"
    static <- static "website/static"
    warp 3000 $ HelloWorld static 
