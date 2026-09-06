"""Exercise script sensitivity checks using actual Kai programs and failure cases."""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
BINARY = Path(sys.argv.pop(1)).resolve()
sys.path.insert(0, str(ROOT / 'scripts'))
SPEC = importlib.util.spec_from_file_location('audit_scripts', ROOT / 'scripts/audit-script-tests.py')
AUDIT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(AUDIT)


class ScriptAuditTests(unittest.TestCase):
    def test_all_value_kinds_and_runtime_errors_reject_incorrect_results(self):
        cases = [('// expect: 0\n0', 2), ('// expect: -2\n-2', 2),
                 ('// expect: false\nfalse', 2), ('// expect: "é雪"\n"é雪"', 2),
                 ('// expect: ()\n()', 2), ('// expect: [1,2]\n[1,2]', 2),
                 ('// expect: (1,true)\n(1,true)', 2),
                 ('// expect: error DivByZero\n1/0', 2),
                 ('#!/usr/bin/env kai\n// expect: ()\n// stdout: "hello\\n"\nprint "hello"', 2),
                 ('// expect: "Ada"\n// expect-type: TString\n// stdin: "Ada\\n"\ninput', 4),
                 ('// expect: ()\n// expect-type: TUnit\n// stdin: "Ada\\n"\n// stdout: "Ada\\n"\nprint input', 4)]
        with tempfile.TemporaryDirectory(prefix='kai-audit-values-') as directory:
            root = Path(directory)
            for index, (source, _) in enumerate(cases):
                (root / f'{index:02}.kai').write_text(source, encoding='utf-8')
            rows = AUDIT.audit_corpus(BINARY, root)
            self.assertEqual(len(rows), len(cases))
            for row, (_, count) in zip(rows, cases):
                with self.subTest(file=row['file']):
                    self.assertEqual(row['baseline'], 'PASS', row)
                    self.assertEqual(len(row['controls']), count)
                    self.assertEqual({control['result'] for control in row['controls']}, {'REJECTED'}, row)

    def test_keeps_imports_available_and_preserves_original_files(self):
        with tempfile.TemporaryDirectory(prefix='kai-audit-import-') as directory:
            root = Path(directory)
            sources = {'Helper.kai': '// expect: 2\nlet double = \\x -> x * 2\ndouble 1',
                       'main.kai': '// expect: 42\nimport Helper\ndouble 21'}
            for name, source in sources.items():
                (root / name).write_text(source, encoding='utf-8')
            rows = AUDIT.audit_corpus(BINARY, root)
            self.assertEqual([row['baseline'] for row in rows], ['PASS', 'PASS'])
            self.assertTrue(all(control['result'] == 'REJECTED' for row in rows for control in row['controls']), rows)
            self.assertEqual({p.name: p.read_text(encoding='utf-8') for p in root.iterdir()}, sources)

    def test_reports_an_unused_input_fixture_as_a_surviving_control(self):
        with tempfile.TemporaryDirectory(prefix='kai-audit-unused-') as directory:
            root = Path(directory)
            (root / 'unused.kai').write_text('// expect: 42\n// stdin: "ignored\\n"\n42', encoding='utf-8')
            report = root / 'results.json'
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(AUDIT.main([str(BINARY), str(root), '--json', str(report)]), 1)
            rows = json.loads(report.read_text(encoding='utf-8'))
            self.assertEqual(rows[0]['baseline'], 'PASS')
            self.assertIn({'name': 'wrong-input', 'result': 'SURVIVED', 'diagnostic': ''}, rows[0]['controls'])

    def test_reports_a_failing_original_without_claiming_success(self):
        with tempfile.TemporaryDirectory(prefix='kai-audit-failure-') as directory:
            root = Path(directory)
            (root / 'wrong.kai').write_text('// expect: 42\n0', encoding='utf-8')
            report = root / 'results.json'
            with contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(AUDIT.main([str(BINARY), str(root), '--json', str(report)]), 1)
            rows = json.loads(report.read_text(encoding='utf-8'))
            self.assertEqual(rows[0]['baseline'], 'FAIL')
            self.assertIn('Expected 42', rows[0]['diagnostic'])

    def test_rejects_empty_corpora(self):
        with tempfile.TemporaryDirectory(prefix='kai-audit-empty-') as directory:
            with self.assertRaisesRegex(ValueError, 'No Kai test files'):
                AUDIT.audit_corpus(BINARY, directory)

    def test_does_not_count_unrelated_errors_as_a_successful_negative_control(self):
        for failure in ['missing executable', 'Timed out', 'file: exit 1: Parse error: bad',
                        'file: exit 1: Script check failed: Missing // expect: directive',
                        'file: exit 1: Type error: UnboundVariable "missing"']:
            for name in ['wrong-final-value', 'extra-output']:
                with self.subTest(name=name, failure=failure):
                    self.assertEqual(AUDIT.control_result(name, failure), 'INVALID_CONTROL')


if __name__ == '__main__':
    unittest.main()
