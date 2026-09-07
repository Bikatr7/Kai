"""Exercise script value, stdin, and stdout assertions using the built Kai CLI."""
import contextlib
import importlib.util
import io
import json
import os
import pathlib
import subprocess
import sys
import tempfile
import unittest

KAI = pathlib.Path(sys.argv.pop(1)).resolve()
ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
SPEC = importlib.util.spec_from_file_location('script_corpus', ROOT / 'scripts/check-script-corpus.py')
CORPUS = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CORPUS)


class ScriptCorpusTests(unittest.TestCase):
    def test_shared_directive_conventions(self):
        cases = json.loads((ROOT / 'test/fixtures/script_directives.json').read_text(encoding='utf-8'))
        self.assertTrue(cases, 'Shared fixture convention cases must not be empty')
        for name, source, success in cases:
            with self.subTest(name=name):
                if success:
                    CORPUS.validate_fixture(source)
                else:
                    with self.assertRaises(ValueError):
                        CORPUS.validate_fixture(source)

    def check_source(self, source, success):
        with tempfile.TemporaryDirectory(prefix='kai-script-corpus-') as directory:
            path = pathlib.Path(directory) / 'nested' / 'fixture.kai'
            path.parent.mkdir()
            path.write_text(source, encoding='utf-8')
            output = io.StringIO()
            with contextlib.redirect_stdout(output), contextlib.redirect_stderr(output):
                result = CORPUS.check_corpus(KAI, directory)
            self.assertEqual(result, 0 if success else 1, output.getvalue())
            self.assertIn(f'release binary tests passed: {1 if success else 0}/1\n', output.getvalue())

    def test_real_greeting_fixture(self):
        self.check_source((ROOT / 'tests/input.kai').read_text(encoding='utf-8'), True)

    def test_wrong_greeting_still_returns_unit_but_fails(self):
        source = (ROOT / 'tests/input.kai').read_text(encoding='utf-8')
        self.check_source(source.replace('print ("Hello, "', 'print ("Wrong, "'), False)

    def test_wrong_input_fails_output_assertion(self):
        source = (ROOT / 'tests/input.kai').read_text(encoding='utf-8')
        self.check_source(source.replace('// stdin: "World\\n"', '// stdin: "Ada\\n"'), False)

    def test_silent_value(self):
        self.check_source('// expect: 42\n42', True)

    def test_unexpected_output(self):
        self.check_source('// expect: ()\nprint "unexpected"', False)

    def test_missing_output(self):
        self.check_source('// expect: ()\n// stdout: "hello\\n"\n()', False)

    def test_extra_output(self):
        self.check_source('// expect: ()\n// stdout: "hello\\n"\nprint "hello"; print "extra"', False)

    def test_output_order(self):
        self.check_source('// expect: ()\n// stdout: "first\\nsecond\\n"\nprint "second"; print "first"', False)

    def test_missing_expected_newline(self):
        self.check_source('// expect: ()\n// stdout: "hello"\nprint "hello"', False)

    def test_blank_lines_spaces_and_quotes(self):
        self.check_source('// expect: ()\n// stdout: "  \\n\\n\\\"hello\\\"\\n"\nprint "  "; print ""; print "\\\"hello\\\""', True)

    def test_output_does_not_replace_value_assertion(self):
        self.check_source('// expect: 42\n// stdout: "hello\\n"\nprint "hello"; 0', False)

    def test_expected_runtime_error(self):
        self.check_source('// expect: error DivByZero\n// stdout: "before\\n"\nprint "before"; 1 / 0', True)

    def test_unexpected_runtime_error(self):
        self.check_source('// expect: ()\n// stdout: "before\\n"\nprint "before"; 1 / 0', False)

    def test_eof_input(self):
        self.check_source('// expect: error TypeError "input: could not read from stdin"\ninput', True)

    def test_empty_input_line(self):
        self.check_source('// expect: ()\n// stdin: "\\n"\n// stdout: "Hello, !\\n"\nprint ("Hello, " ++ input ++ "!")', True)

    def test_unicode_json_fixture(self):
        self.assertEqual(CORPUS.string_fixture('// stdin: "é雪\\n"', 'stdin'), 'é雪\n')
        self.assertEqual(CORPUS.string_fixture('// stdout: "é雪\\n"', 'stdout'), 'é雪\n')

    def test_unicode_input_and_output_under_a_legacy_locale(self):
        with tempfile.TemporaryDirectory(prefix='kai-legacy-corpus-') as directory:
            fixture = pathlib.Path(directory) / 'unicode.kai'
            fixture.write_text('// expect: ()\n// stdin: "é雪\\n"\n'
                               '// stdout: "é雪\\n"\nprint input', encoding='utf-8')
            # Model a non-UTF-8 host even when the installed OS only has UTF-8 locales.
            bootstrap = ('import locale, runpy, sys; '
                         'locale.getencoding = lambda: "ascii"; '
                         'locale.getpreferredencoding = lambda *args: "ascii"; '
                         'sys.argv = sys.argv[1:]; runpy.run_path(sys.argv[0], run_name="__main__")')
            env = dict(os.environ, PYTHONUTF8='0', PYTHONPATH=str(ROOT / 'scripts'))
            result = subprocess.run([sys.executable, '-c', bootstrap,
                                     str(ROOT / 'scripts/check-script-corpus.py'), str(KAI), directory],
                                    env=env, capture_output=True, text=True, encoding='utf-8', timeout=40)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(result.stdout, 'release binary tests passed: 1/1\n')
            self.assertEqual(result.stderr, '')

    def test_invalid_fixtures(self):
        for name in ['stdin', 'stdout']:
            for value in ['42', 'null', 'true', '[]', '{}', 'invalid', '']:
                with self.subTest(name=name, value=value):
                    self.check_source(f'// expect: ()\n// {name}: {value}\n()', False)

    def test_duplicate_fixtures(self):
        for name in ['stdin', 'stdout']:
            with self.subTest(name=name):
                self.check_source(f'// expect: ()\n// {name}: ""\n// {name}: ""\n()', False)

    def test_empty_corpus(self):
        with tempfile.TemporaryDirectory(prefix='kai-empty-corpus-') as directory:
            with contextlib.redirect_stderr(io.StringIO()):
                self.assertEqual(CORPUS.check_corpus(KAI, directory), 1)


if __name__ == '__main__':
    unittest.main()
