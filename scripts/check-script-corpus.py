#!/usr/bin/env python3
"""Run real expectation checks with explicit stdin fixtures and a per-file timeout."""
import pathlib
import subprocess
import sys
from script_fixtures import string_fixture, validate_fixture


def check_script(binary, path):
    """Return a failure diagnostic, or None after value/type/output checks pass."""
    path = pathlib.Path(path)
    try:
        source = path.read_text(encoding='utf-8')
        validate_fixture(source)
        stdin = string_fixture(source, 'stdin')
        expected_stdout = string_fixture(source, 'stdout') + 'Script checks passed\n'
        result = subprocess.run([str(binary), '--check', str(path)], input=stdin,
                                text=True, encoding='utf-8', capture_output=True, timeout=30)
        if result.returncode or result.stderr or result.stdout != expected_stdout:
            detail = f'{path}: exit {result.returncode}: {result.stdout}{result.stderr}'
            if result.stdout != expected_stdout:
                detail += f'\nexpected stdout {expected_stdout!r}, got {result.stdout!r}'
            return detail
    except (OSError, ValueError, subprocess.TimeoutExpired) as error:
        return f'{path}: {error}'
    return None


def check_corpus(binary, root):
    files = sorted(pathlib.Path(root).rglob('*.kai'))
    if not files:
        print('release binary test corpus is empty', file=sys.stderr)
        return 1
    failures = [failure for path in files if (failure := check_script(binary, path)) is not None]
    for failure in failures:
        print('release binary failure: ' + failure, file=sys.stderr)
    print(f'release binary tests passed: {len(files) - len(failures)}/{len(files)}')
    return int(bool(failures))


if __name__ == '__main__':
    sys.exit(check_corpus(pathlib.Path(sys.argv[1]).resolve(), sys.argv[2]))
