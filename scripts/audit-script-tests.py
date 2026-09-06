#!/usr/bin/env python3
"""Exercise each script's original assertions and deliberately incorrect variants."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import shutil
import tempfile

from script_fixtures import directive, string_fixture

_spec = importlib.util.spec_from_file_location('corpus', Path(__file__).with_name('check-script-corpus.py'))
corpus = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(corpus)


def variants(source):
    expected = directive(source, 'expect', required=True)
    if expected.startswith('error '):
        headers = [line for line in source.splitlines() if line.startswith(('// ', '#!'))]
        yield 'missing-runtime-error', '\n'.join(headers) + '\n()\n'
    else:
        if expected.startswith('"'):
            wrong = f'({expected}) ++ "incorrect"'
        elif expected in ('true', 'false'):
            wrong = f'not ({expected})'
        elif expected.lstrip('-+').isdigit():
            wrong = '1' if int(expected) == 0 else '0'
        else:
            wrong = '0'
        yield 'wrong-final-value', source.rstrip() + '\n' + wrong + '\n'

    # Preserve the original result and prepend an extra effect. This tests output
    # independently of the value assertion, including scripts that should be silent.
    lines = source.splitlines(keepends=True)
    index = 1 if lines and lines[0].startswith('#!') else 0
    yield 'extra-output', ''.join(lines[:index]) + 'print "unexpected output"\n' + ''.join(lines[index:])

    expected_type = directive(source, 'expect-type')
    if expected_type is not None:
        wrong_type = 'TBool' if expected_type == 'TInt' else 'TInt'
        yield 'wrong-type-expectation', '\n'.join(
            '// expect-type: ' + wrong_type if line.startswith('// expect-type:') else line
            for line in source.splitlines()) + '\n'

    if directive(source, 'stdin') is not None:
        # Supply enough distinct lines for either branch to complete without EOF.
        wrong_input = 'incorrect input\n' * (string_fixture(source, 'stdin').count('\n') + 2)
        yield 'wrong-input', '\n'.join(
            '// stdin: ' + json.dumps(wrong_input) if line.startswith('// stdin:') else line
            for line in source.splitlines()) + '\n'


def audit_corpus(binary, root):
    root = Path(root).resolve()
    files = sorted(root.rglob('*.kai'))
    if not files:
        raise ValueError('No Kai test files found')
    rows = []
    with tempfile.TemporaryDirectory(prefix='kai-script-audit-') as directory:
        copied = Path(directory) / 'corpus'
        shutil.copytree(root, copied)
        for original in files:
            relative = original.relative_to(root)
            target = copied / relative
            source = original.read_text(encoding='utf-8')
            baseline = corpus.check_script(binary, target)
            controls = []
            for name, changed in variants(source):
                try:
                    target.write_text(changed, encoding='utf-8')
                    failure = corpus.check_script(binary, target)
                    controls.append({'name': name, 'result': control_result(name, failure),
                                     'diagnostic': (failure or '').replace(str(copied), root.name)})
                finally:
                    target.write_text(source, encoding='utf-8')
            rows.append({'file': str(Path(root.name) / relative),
                         'sha256': hashlib.sha256(original.read_bytes()).hexdigest(),
                         'expect': directive(source, 'expect', required=True),
                         'expect_type': directive(source, 'expect-type'),
                         'stdin': string_fixture(source, 'stdin'), 'stdout': string_fixture(source, 'stdout'),
                         'baseline': 'PASS' if baseline is None else 'FAIL',
                         'diagnostic': (baseline or '').replace(str(copied), root.name),
                         'controls': controls})
    return rows


def control_result(name, failure):
    if failure is None:
        return 'SURVIVED'
    output_rejected = ': exit 0:' in failure and 'expected stdout' in failure
    value_rejected = ': exit 1:' in failure and 'Script check failed: Expected ' in failure
    if name == 'extra-output':
        rejected = output_rejected
    elif name == 'wrong-input':
        rejected = output_rejected or value_rejected
    else:
        rejected = value_rejected
    # Parse errors, missing executables, timeouts and invalid fixtures do not prove
    # that the intended value/type/output assertion detected the changed behavior.
    return 'REJECTED' if rejected else 'INVALID_CONTROL'


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('binary', type=Path)
    parser.add_argument('root', type=Path)
    parser.add_argument('--json', type=Path, required=True)
    args = parser.parse_args(argv)
    rows = audit_corpus(args.binary.resolve(), args.root)
    args.json.write_text(json.dumps(rows, indent=2, ensure_ascii=False) + '\n', encoding='utf-8')
    failed = [row for row in rows if row['baseline'] != 'PASS' or
              any(control['result'] != 'REJECTED' for control in row['controls'])]
    print(f'Script assertion checks: {len(rows) - len(failed)}/{len(rows)} passed')
    return int(bool(failed))


if __name__ == '__main__':
    raise SystemExit(main())
