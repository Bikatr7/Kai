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
                <p .tagline>A minimal, typed expression language

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
                        <h3>Static Typing
                        <p>Catch errors before they happen with compile-time type checking.
                    <div .feature>
                        <h3>Simple Syntax
                        <p>Clean, intuitive syntax focused on what matters.
                    <div .feature>
                        <h3>Pure Expressions
                        <p>Predictable evaluation without side effects.

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
                    <h3>Complex Expressions  
                    <div .code-example>
                        <code>if (10 / 2) == 5 then "success" else "fail"</code>

            <section #roadmap>
                <h2>Development Roadmap
                <div .timeline>
                    <div .timeline-item .current>
                        <div .timeline-marker .current data-step="1">
                        <div .timeline-content>
                            <h3>Expressions
                            <p>Core types, operators, and control flow
                    <div .timeline-item>
                        <div .timeline-marker data-step="2">
                        <div .timeline-content>
                            <h3>Lambda Functions
                            <p>First-class functions and application
                    <div .timeline-item>
                        <div .timeline-marker data-step="3">
                        <div .timeline-content>
                            <h3>Variables
                            <p>Binding and environments
                    <div .timeline-item>
                        <div .timeline-marker data-step="4">
                        <div .timeline-content>
                            <h3>Error Handling
                            <p>Robust error recovery
                    <div .timeline-item>
                        <div .timeline-marker data-step="5">
                        <div .timeline-content>
                            <h3>State Management
                            <p>Controlled mutability

            <footer>
                <p .copyright>Kai Language · Implemented in Haskell
    |]

main :: IO ()
main = do
    putStrLn "Starting server on http://localhost:3000"
    static <- static "website/static"
    warp 3000 $ HelloWorld static 