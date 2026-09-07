"""Check the built benchmark's parent and memory-child processes independently."""
import csv
import json
import os
import pathlib
import subprocess
import sys
import tempfile
import unittest

BENCHMARK = pathlib.Path(sys.argv.pop(1)).resolve()
if os.name == 'nt' and BENCHMARK.suffix.lower() != '.exe':
    BENCHMARK = BENCHMARK.with_suffix('.exe')


class BenchmarkRunnerTests(unittest.TestCase):
    def run_benchmark(self, args, child=None):
        env = os.environ.copy()
        env.pop('WEIGH_CASE', None)
        env['GHCRTS'] = '-N1 -T'
        if child is not None:
            env['WEIGH_CASE'] = child
        return subprocess.run([str(BENCHMARK), *args], env=env, text=True,
                              capture_output=True, timeout=60)

    def test_allocation_child_does_not_run_criterion(self):
        with tempfile.TemporaryDirectory(prefix='kai-memory-child-') as directory:
            result_file = pathlib.Path(directory) / 'allocation.txt'
            child = json.dumps(['/Memory: Parse Small Expr', str(result_file)])
            result = self.run_benchmark(['--list'], child)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertTrue(result_file.exists())
            self.assertIn('weightAllocatedBytes', result_file.read_text())
            self.assertEqual(result.stdout, '')

    def test_invalid_allocation_child_fails(self):
        with tempfile.TemporaryDirectory(prefix='kai-bad-memory-child-') as directory:
            result_file = pathlib.Path(directory) / 'allocation.txt'
            child = json.dumps(['No such benchmark', str(result_file)])
            result = self.run_benchmark(['--list'], child)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn('No such case', result.stderr)
            self.assertFalse(result_file.exists())
            self.assertNotIn('Speed Benchmarks', result.stdout)

    def test_parent_runs_memory_and_lists_speed_cases(self):
        result = self.run_benchmark(['--list'])
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('Memory: Parse Small Expr', result.stdout)
        self.assertIn('Memory: TypeCheck Function', result.stdout)
        self.assertEqual(result.stdout.count('Speed Benchmarks (Criterion):'), 1)
        self.assertIn('Speed Benchmarks/End-to-End/Fibonacci (n=10)', result.stdout)

    def test_parent_writes_a_timed_report(self):
        with tempfile.TemporaryDirectory(prefix='kai-timed-report-') as directory:
            report = pathlib.Path(directory) / 'timings.csv'
            name = 'Speed Benchmarks/End-to-End/Fibonacci (n=10)'
            result = self.run_benchmark(['--time-limit', '0.1', '--resamples', '100',
                                         '--csv', str(report), '--match', 'prefix', name])
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            with report.open() as handle:
                rows = list(csv.DictReader(handle))
            self.assertEqual([row['Name'] for row in rows], [name])
            self.assertGreater(float(rows[0]['Mean']), 0)


if __name__ == '__main__':
    unittest.main()
