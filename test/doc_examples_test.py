"""Regression tests for the documentation checker, using the real interpreter."""

import importlib.util
from contextlib import redirect_stderr
import io
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parent.parent
BINARY = sys.argv.pop(1)
spec = importlib.util.spec_from_file_location("doc_examples", ROOT / "scripts/check-doc-examples.py")
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)

MATH = "```kai\n// Math.kai\nlet add = \\x -> \\y -> x+y\n// Main.kai\nadd 1 2\n```\n"
HTML = '<h3>Example</h3><div class="code-example kai-example"><code>40 + 2</code></div>'


class ParsingTests(unittest.TestCase):
    def test_accepts_a_website_executable(self):
        args = checker.parse_args(['kai', 'kai-website'])
        self.assertEqual(args.website, 'kai-website')
        self.assertIsNone(args.html)

    def test_accepts_rendered_html(self):
        args = checker.parse_args(['kai', '--html', 'site/index.html'])
        self.assertEqual(args.html, Path('site/index.html'))
        self.assertIsNone(args.website)

    def test_rejects_missing_or_ambiguous_website_inputs(self):
        for arguments in [['kai'], ['kai', 'kai-website', '--html', 'index.html']]:
            with self.subTest(arguments=arguments), redirect_stderr(io.StringIO()):
                with self.assertRaises(SystemExit) as error:
                    checker.parse_args(arguments)
                self.assertEqual(error.exception.code, 2)

    def test_fences_preserve_code_and_distinguish_reference_syntax(self):
        self.assertEqual(checker.fences('inline `x`\n```kai\n\\x -> x\n```\n```text\nf : a\n```'),
                         [('kai', '\\x -> x\n'), ('text', 'f : a\n')])

    def test_comment_markers_inside_strings_are_not_expectations(self):
        self.assertEqual(checker.line_comment('show "a // => b"'), ('show "a // => b"', ''))
        self.assertEqual(checker.line_comment('"\\\" // => literal" // => "ok"'),
                         ('"\\\" // => literal" ', '=> "ok"'))

    def test_rendered_code_decodes_entities_and_ignores_html_comments(self):
        parser = checker.WebsiteExamples()
        parser.feed('<h3>First</h3><div class="kai-example"><code>1 &lt; 2<!-- note --></code>'
                    '<code>"&amp;"</code></div><h3>Second</h3><div class="kai-example"><code>42</code></div>')
        self.assertEqual(parser.examples, [('First', '1 < 2\n"&"'), ('Second', '42')])

    def test_reference_lists_are_not_runnable_blocks(self):
        parser = checker.WebsiteExamples()
        parser.feed('<div class="code-example"><code>+ - *</code></div>')
        self.assertEqual(parser.examples, [])


class ExecutionTests(unittest.TestCase):
    def test_executes_supplied_html_without_launching_a_server(self):
        with tempfile.TemporaryDirectory() as directory:
            html = Path(directory) / 'index.html'
            output = Path(directory) / 'results.json'
            html.write_text(HTML, encoding='utf-8')
            with patch.object(checker, 'website_html', side_effect=AssertionError('must use supplied HTML')):
                self.assertEqual(checker.main([BINARY, '--html', str(html), '--json', str(output)]), 0)
            self.assertTrue(output.is_file())

    def test_rejects_false_claims_in_supplied_html(self):
        with tempfile.TemporaryDirectory() as directory:
            html = Path(directory) / 'index.html'
            html.write_text(HTML.replace('40 + 2', '1 + true'), encoding='utf-8')
            self.assertEqual(checker.main([BINARY, '--html', str(html)]), 1)

    def test_missing_supplied_html_is_an_error(self):
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaises(FileNotFoundError):
                checker.main([BINARY, '--html', str(Path(directory) / 'missing.html')])

    def test_supplied_html_with_no_examples_is_an_error(self):
        with tempfile.TemporaryDirectory() as directory:
            html = Path(directory) / 'index.html'
            html.write_text('<html></html>', encoding='utf-8')
            with self.assertRaisesRegex(RuntimeError, 'no runnable examples'):
                checker.main([BINARY, '--html', str(html)])

    def run_examples(self, extra='', signature='', html=HTML):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'examples').mkdir()
            for module in ['MathUtils', 'StringUtils', 'TextAnalysis']:
                (root / 'examples' / (module + '.kai')).write_text('// expect: ()\n')
            (root / 'README.md').write_text(MATH + extra)
            (root / 'SPEC.md').write_text(signature)
            for file in ['DEVELOPING.md', 'FEATURES.md']:
                (root / file).write_text('')
            return checker.check_examples(BINARY, root, html)

    def test_accepts_values_signatures_and_exact_errors(self):
        results = self.run_examples('```kai\n40+2 // => 42\n```\n'
                                    '```kai\n1+true // Type error: UnificationError TBool TInt\n```',
                                    '```text\nlength : [a] -> Int\n```')
        self.assertTrue(all(row['passed'] for row in results), results)

    def test_rejects_a_false_result_claim(self):
        results = self.run_examples('```kai\n40+2 // => 0\n```')
        self.assertTrue(any(not row['passed'] and 'Expected 0' in row['detail'] for row in results))

    def test_rejects_an_invalid_runnable_example(self):
        results = self.run_examples('```kai\n1+true\n```')
        self.assertTrue(any(not row['passed'] and 'UnificationError' in row['detail'] for row in results))

    def test_rejects_an_incorrect_error_claim(self):
        results = self.run_examples('```kai\n1+true // Type error: UnboundVariable "x"\n```')
        self.assertTrue(any(not row['passed'] for row in results))

    def test_rejects_an_incorrect_builtin_signature(self):
        results = self.run_examples(signature='```text\nlength : String -> Int\n```')
        self.assertTrue(any(not row['passed'] and 'signature length' in row['name'] for row in results))

    def test_rejects_extra_backslashes_in_rendered_lambdas(self):
        results = self.run_examples(html=HTML.replace('40 + 2', '\\\\x -> x'))
        self.assertTrue(any(not row['passed'] and 'Parse error:' in row['detail'] for row in results))

    def test_requires_rendered_examples(self):
        with self.assertRaisesRegex(RuntimeError, 'no runnable examples'):
            self.run_examples(html='<html></html>')

    def test_rejects_empty_rendered_examples(self):
        with self.assertRaisesRegex(RuntimeError, 'Empty website example'):
            self.run_examples(html=HTML.replace('40 + 2', ''))

    def test_accepts_comment_like_string_values(self):
        results = self.run_examples('```kai\n"a // => b"\n```')
        self.assertTrue(all(row['passed'] for row in results), results)


if __name__ == '__main__':
    unittest.main()
