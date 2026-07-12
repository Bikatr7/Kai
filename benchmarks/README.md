# Kai Benchmarks

Kai uses Criterion for runtime measurements and Weigh for allocation measurements.
Benchmark inputs are parsed and evaluated or type-checked before a sample is
accepted; an invalid Kai program aborts the benchmark instead of measuring an
error value.

## Run

```bash
stack bench
stack bench --benchmark-arguments="--match pattern 'Parser'"
stack bench --benchmark-arguments="--match pattern 'Evaluator'"
stack bench --benchmark-arguments="--match pattern 'Type Checker'"
stack bench --benchmark-arguments="--csv=results.csv"
```

Use a short validation run before committing benchmark changes:

```bash
stack bench --benchmark-arguments="--iters 1"
```

## Suites

- `ParserBench.hs` measures expression size, nesting, lambda chains, lists, and
  multi-statement parsing.
- `EvaluatorBench.hs` measures pure evaluation across core language features.
- `TypeCheckerBench.hs` measures inference, unification, recursion, annotations,
  and composite data.
- `Bench.hs` combines those suites with end-to-end and allocation measurements.

## Regression Process

1. Run the short validation command and the full test suite.
2. Record a full benchmark baseline on the same machine and build profile.
3. Make the implementation change.
4. Repeat the same command under comparable system load.
5. Investigate repeatable regressions greater than 10 percent rather than relying
   on a single sample.

Benchmark results are machine- and build-specific. Do not copy old timings into
release documentation without rerunning them on the current revision.
