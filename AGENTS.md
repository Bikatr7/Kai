# AGENTS.md

## Read README.md in it's entirety.
## Read DEVELOPING.md in it's entirety.

# Instructions for Agents

You will write tests for all functionality you write. These must be comprehensive and cover all edge cases.

You are not to hardcode tests, you are not to delete tests. If a test fails and there is no flaw in the test, you must fix the language itself.

# Kai Testing for Agents

## Test Structure
- **Unit tests**: `test/*.hs` files using Hspec
- **Script tests**: `tests/*.kai` files with `// expect:` directives
- **Property tests**: QuickCheck in PropertyBasedSpec.hs

## Adding Tests

### Unit Test Pattern
```haskell
module NewFeatureSpec where
import Test.Hspec
import Syntax
import Parser
import Evaluator

spec :: Spec
spec = describe "Feature" $ do
  it "description" $ do
    parseEvaluate "expression" `shouldBe` Right (VInt 42)

parseEvaluate :: String -> Either RuntimeError Value
parseEvaluate input = case parseExpr input of
  Left _ -> Left (TypeError "Parse error")
  Right expr -> eval expr
```

### Script Test Pattern
```kai
// expect: 42
expression_to_test
```

### Error Testing
```haskell
case parseEvaluate "5 / 0" of
  Left DivByZero -> True `shouldBe` True
  _ -> expectationFailure "Should be division by zero"
```

## Rules
- ALL `.kai` files MUST have `// expect:` directive
- Test specific error types, not generic failures
- Add new unit tests to `test/Spec.hs`
- Run tests: `stack test`

## File Organization
- `ArithmeticSpec.hs` + `tests/arithmetic.kai`
- `BooleanSpec.hs` + `tests/boolean_logic.kai` 
- `LambdaSpec.hs` + `tests/lambda_*.kai`
- `LetBindingSpec.hs` + `tests/let_*.kai`

## Performance Requirements
- **Language must handle deeply nested expressions** (1000+ levels) without infinite loops
- **Parser optimization**: Reorder `choice` alternatives for performance
- **Type checker optimization**: Avoid exponential `applySubstEnv` calls
- **Test ALL stress cases**: Memory, parsing, and type inference stress tests must pass

## Project Guidance
- **README.md is the source of truth** for project goals, design decisions, and current status
- **SPEC.md documents the current language state** - update immediately when language semantics change
- **Website must stay in sync with README.md** - update both when making changes
- **When in doubt**: consult README.md first, then SPEC.md for technical details
- **Design decisions**: Follow the functional-first philosophy described in README.md
- **Feature priorities**: Match the roadmap and current limitations listed in README.md
- **Documentation updates**: When adding language features, update README.md, SPEC.md, website, and DEVELOPING.md

## Recent Major Improvements (v0.0.3.2+)
- **Type annotations**: Optional Haskell-style type annotations for lambdas and let bindings
- **Error handling system**: Full Maybe/Either types with pattern matching for graceful error handling
- **Safe conversion functions**: `parseInt : String -> Maybe Int` returns `Nothing` for invalid input
- **Case expressions**: Pattern matching for handling Maybe/Either and other data types safely
- **Wildcard variables**: Use `_` in let bindings to discard unused values (`let _ = print "hello" in 42`)
- **Expression sequencing**: Use `;` to sequence expressions for side effects (`print "first"; print "second"; 42`)
- **Recursion fixes**: Fixed critical evaluator bug preventing infinite recursion with IO operations
- **Performance fixes**: Eliminated infinite loops in deeply nested expressions (1000+ levels)
- **Clean CLI**: Debug output hidden by default, use `--debug` flag when needed
- **Interactive examples**: Working calculator demonstrating language features including new wildcard and sequencing features