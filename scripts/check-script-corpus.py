#!/usr/bin/env python3
"""Run real expectation checks with explicit stdin fixtures and a per-file timeout."""
import json
import pathlib
import subprocess
import sys


def check_corpus(binary, root):
    files = sorted(pathlib.Path(root).rglob('*.kai'))
    if not files:
        print('release binary test corpus is empty', file=sys.stderr)
        return 1
    failures = []
    for path in files:
        try:
            inputs = [line[len('// stdin:'):].strip()
                      for line in path.read_text(encoding='utf-8').splitlines()
                      if line.startswith('// stdin:')]
            if len(inputs) > 1:
                raise ValueError('Duplicate stdin directive')
            stdin = json.loads(inputs[0]) if inputs else ''
            if not isinstance(stdin, str):
                raise ValueError('stdin fixture must be a JSON string')
            result = subprocess.run([str(binary), '--check', str(path)], input=stdin,
                                    text=True, capture_output=True, timeout=30)
            if result.returncode or not result.stdout.endswith('Script checks passed\n'):
                failures.append(f'{path}: {result.stdout}{result.stderr}')
        except (OSError, ValueError, subprocess.TimeoutExpired) as error:
            failures.append(f'{path}: {error}')
    for failure in failures:
        print('release binary failure: ' + failure, file=sys.stderr)
    print(f'release binary tests passed: {len(files) - len(failures)}/{len(files)}')
    return int(bool(failures))


if __name__ == '__main__':
    sys.exit(check_corpus(pathlib.Path(sys.argv[1]).resolve(), sys.argv[2]))
