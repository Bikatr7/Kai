# Kai Language

A minimal, typed expression language implemented in Haskell.

## Note

Work in progress, i have little time, nor know anything about how to write a language properly, or haskell really lol.

## Overview

Kai is a simple yet powerful programming language that combines static typing with clean syntax. It's designed to be easy to learn while providing the safety and expressiveness of modern functional programming.

### Features

- ✅ **Static Typing** - Catch errors at compile time
- ✅ **Pure Expressions** - Predictable evaluation without side effects  
- ✅ **Simple Syntax** - Clean, intuitive language design
- ✅ **Script Files** - Parse and run `.kai` files
- ✅ **Web Interface** - Modern landing page with language documentation

### Current Language Support

- **Basic Types**: Integers (`42`, `-5`) and Booleans (`true`, `false`)
- **Operators**: `+`, `-`, `*`, `/`, `==`, `<`, `>`, `and`, `or`, `not`
- **Control Flow**: `if condition then expr1 else expr2`
- **Parentheses**: For grouping expressions

## Getting Started

### Prerequisites

Install the Haskell toolchain:

```bash
# Option 1: GHCup (Recommended)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc cabal stack

# Option 2: Homebrew (macOS)
brew install ghc cabal-install stack
```

### Build & Run

```bash
# Build the project
stack build

# Run the Kai interpreter with examples
stack exec kai

# Run a Kai script file
stack exec kai tests/test.kai

# Start the web interface
stack exec kai-website
```

Visit http://localhost:3000 for the web interface. This is currently just an introduction site.

### Example Usage

Create a file `example.kai`:
```kai
if 10 > 5 then 42 * 2 else 0
```

Run it:
```bash
stack exec kai example.kai
```

Output:
```
Running file: example.kai
AST: If (Gt (IntLit 10) (IntLit 5)) (Mul (IntLit 42) (IntLit 2)) (IntLit 0)
Type: TInt
Evaluation: VInt 84
```

## Project Structure

```
.
├── src/                    # Kai language implementation
│   ├── Main.hs            # CLI interpreter
│   ├── Syntax.hs          # AST definitions
│   ├── Parser.hs          # Parser (Megaparsec)
│   ├── TypeChecker.hs     # Static type checker
│   └── Evaluator.hs       # Expression evaluator
├── website/               # Web interface
│   ├── Main.hs           # Yesod web server
│   └── static/           # CSS, favicon, etc.
├── tests/                # Test files
│   └── test.kai         # Example Kai scripts
├── package.yaml         # Project configuration
└── README.md           # This file
```

## Development Roadmap

### ✅ Phase 1: Core Expressions (Current)
- [x] Basic types and operators
- [x] Control flow (if-then-else)
- [x] File parsing and CLI
- [x] Type checking and evaluation

### 🚧 Phase 2: Lambda Functions (Next)
- [ ] Function definitions
- [ ] Function application
- [ ] First-class functions

### 📋 Phase 3: Variables & Environments
- [ ] Variable binding (`let x = 5 in x + 1`)
- [ ] Scope management
- [ ] Environment handling

### 📋 Phase 4: Error Handling
- [ ] Better error messages
- [ ] Error recovery
- [ ] Debugging support

### 📋 Phase 5: Advanced Features
- [ ] Pattern matching
- [ ] Data types
- [ ] Module system

## Language Examples

### Arithmetic & Logic
```kai
42 * (10 - 3)
5 > 3 and true
not false
```

### Conditionals
```kai
if 5 > 3 then 42 * 2 else 0
if (10 / 2) == 5 then 100 else 0
```

### Type Safety
```kai
1 + true          // Type error: TypeMismatch TInt TBool
10 / 0            // Runtime error: DivByZero  
```

## Contributing

Contributions welcome! This is a learning project, so feel free to:

- Add new language features
- Improve error messages
- Enhance the web interface
- Write documentation
- Add tests