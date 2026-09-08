# Kai 0.0.5.0 Benchmarks

Kai uses Criterion for runtime measurements and Weigh for allocation measurements.
The suite contains 83 speed cases and 8 allocation cases. Parser-suite,
evaluator, and type-checker helpers reject parse/type/runtime failures. Small-input
and allocation parser cases call `parseExpr` directly and measure its result.

## Run

```bash
stack bench
stack bench --benchmark-arguments="--match pattern 'Parser'"
stack bench --benchmark-arguments="--match pattern 'Evaluator'"
stack bench --benchmark-arguments="--match pattern 'Type Checker'"
stack bench --benchmark-arguments="--csv=results.csv"
```

Use a short execution check before committing benchmark changes:

```bash
stack bench --benchmark-arguments="--iters 1"
```

The combined runner executes allocation cases in Weigh child processes and
runs Criterion once in the parent. Check that process boundary and timed CSV
output after changing the runner:

```bash
python3 test/benchmark_runner_test.py "$(stack path --dist-dir)/build/kai-bench/kai-bench"
```

## Suites

- `ParserBench.hs` measures expression size, nesting, lambda chains, lists, and
  multi-statement parsing.
- `EvaluatorBench.hs` measures pure evaluation across core language features.
- `TypeCheckerBench.hs` measures inference, unification, recursion, annotations,
  and composite data.
- `Bench.hs` combines those suites with end-to-end and allocation measurements.

The `Small-input latency` group uses `whnf`; the other speed groups use `nf`.
`nf` forces the available `NFData` representation, but function bodies are not
evaluated merely by forcing a closure. `EvaluatorBench.hs` and `Bench.hs` parse
and evaluate without a static type-checking pass. Type-checker workloads parse
and infer a type without evaluating the program.

## Regression Process

1. Run the short execution check and the full test suite.
2. Record a full benchmark baseline on the same machine and build profile.
3. Make the implementation change.
4. Repeat the same command under comparable system load.
5. Investigate repeatable regressions greater than 10 percent rather than relying
   on a single sample.

Benchmark results are machine- and build-specific. Do not copy old timings into
release documentation without rerunning them on the current revision.

## Measurement integrity

Pass the changing input to the measured function: `nf parseEval source`, not
`nf (\() -> parseEval source) ()`. The latter can be optimized into repeated
forcing of a cached result. Speed cases use explicit inputs, and Weigh uses
`W.func` for pure work. The one-iteration CI gate catches execution failures; it
does not compare returned values, assert successful parsing for direct
`parseExpr` cases, or impose a timing threshold. Functional correctness is checked
by the Hspec, property, and script suites. Evaluator/type-checker timings include
parsing; their category names describe the workload, not an isolated phase.

Weigh's table reports allocated bytes and garbage collections for each case,
not peak resident memory or a heap profile. The Criterion CSV contains timing
statistics only; retain the Weigh output separately for allocation comparisons.
