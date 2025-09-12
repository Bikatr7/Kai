# AGENTS.md

## Read README.md in it's entirety.
## Read DEVELOPING.md in it's entirety.

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

## Project Guidance
- **README.md is the source of truth** for project goals, design decisions, and current status
- **Website must stay in sync with README.md** - update both when making changes
- **When in doubt**: consult README.md first, then ask the user
- **Design decisions**: Follow the functional-first philosophy described in README.md
- **Feature priorities**: Match the roadmap and current limitations listed in README.md