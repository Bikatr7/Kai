# Kai Language

A minimal, typed expression language implemented in Haskell.

## Note

Work in progress, i have little time, nor know anything about how to write a language properly, or haskell really lol.

## Overview

Kai is designed to be a simple and intuitive language that combines the ease of use of traditional scripting languages with the safety and performance of Haskell. It features:

- Static typing with compile-time error checking
- Pure expressions without side effects
- Simple, clean syntax
- Built-in type inference
- First-class functions

## Current Status

The project is in early development. Current features include:

- Basic types (numbers, booleans)
- Core operators (arithmetic, comparison, logical)
- Control flow (if-then-else expressions)
- Type system foundation

## Planned Features

1. Lambda Functions
   - First-class functions
   - Function application
   - Closures

2. Variables and Environments
   - Variable binding
   - Scope management
   - Environment handling

3. Error Handling
   - Type-safe error recovery
   - Clear error messages
   - Debugging support

4. State Management
   - Controlled mutability
   - Safe state updates
   - Resource management

## Getting Started

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell Tool Stack)

### Setup

1. Clone this repository
2. Navigate to the project directory
3. Run `stack setup` to set up the GHC compiler
4. Run `stack build` to build the project

### Running the Website

```bash
stack run
```

The website will be available at http://localhost:3000

## Project Structure

- `app/Main.hs` - Main application code
- `static/` - Static files (CSS, JavaScript, images)
- `src/` - Kai language implementation (coming soon)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
