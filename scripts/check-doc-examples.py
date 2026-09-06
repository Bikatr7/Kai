#!/usr/bin/env python3
"""Execute documentation examples with the built Kai interpreter."""

import argparse
from contextlib import contextmanager
from html.parser import HTMLParser
import json
import os
from pathlib import Path
import re
import shutil
import socket
import subprocess
import tempfile
import time
from urllib.request import urlopen
from script_fixtures import string_fixture, validate_fixture


def fences(text):
    return [(match.group(1), match.group(2)) for match in
            re.finditer(r"^```(\w+)\n(.*?)^```", text, re.M | re.S)]


def line_comment(line):
    quoted, escaped = False, False
    for index, char in enumerate(line):
        if quoted:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                quoted = False
        elif char == '"':
            quoted = True
        elif line[index:index + 2] == "//":
            return line[:index], line[index + 2:].strip()
    return line, ""


class WebsiteExamples(HTMLParser):
    def __init__(self):
        super().__init__()
        self.examples = []
        self.depth = 0
        self.current = None
        self.in_code = False
        self.in_title = False
        self.title = ""

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "h3":
            self.in_title = True
            self.title = ""
        if tag == "div":
            self.depth += 1
            if "kai-example" in attrs.get("class", "").split():
                self.current = {"title": self.title, "depth": self.depth, "code": []}
        if tag == "code" and self.current is not None:
            self.in_code = True
            self.current["code"].append("")

    def handle_endtag(self, tag):
        if tag == "code":
            self.in_code = False
        if tag == "h3":
            self.in_title = False
        if tag == "div":
            if self.current is not None and self.current["depth"] == self.depth:
                self.examples.append((self.current["title"], "\n".join(self.current["code"])))
                self.current = None
            self.depth -= 1

    def handle_data(self, data):
        if self.in_title:
            self.title += data
        if self.in_code and self.current is not None:
            self.current["code"][-1] += data


@contextmanager
def website_html(binary):
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        port = sock.getsockname()[1]
    with tempfile.TemporaryFile() as log:
        server = subprocess.Popen([binary], env=dict(os.environ, PORT=str(port)),
                                  stdout=log, stderr=log)
        try:
            for _ in range(100):
                if server.poll() is not None:
                    log.seek(0)
                    raise RuntimeError("Website exited: " + log.read().decode(errors="replace"))
                try:
                    with urlopen(f"http://127.0.0.1:{port}/", timeout=1) as response:
                        html = response.read().decode()
                    break
                except OSError:
                    time.sleep(0.05)
            else:
                raise RuntimeError("Website did not become ready")
            yield html
        finally:
            server.terminate()
            try:
                server.wait(timeout=5)
            except subprocess.TimeoutExpired:
                server.kill()
                server.wait()


def check_examples(binary, root, html):
    results = []
    readme = (root / "README.md").read_text(encoding="utf-8")
    math = next(body.split("// Main.kai")[0] for _, body in fences(readme)
                if "// Math.kai" in body)

    def check(name, source, expected=None, error=None, arguments=("foo", "bar", "baz")):
        with tempfile.TemporaryDirectory(prefix="kai-doc-example-") as directory:
            work = Path(directory)
            for module in ["MathUtils", "StringUtils", "TextAnalysis"]:
                shutil.copyfile(root / "examples" / (module + ".kai"), work / (module + ".kai"))
            (work / "Math.kai").write_text(math, encoding="utf-8")
            for name_in in ["input.txt", "path"]:
                (work / name_in).write_text("Ada\n", encoding="utf-8")
            source_file = work / "example.kai"
            if expected is not None:
                source += "\n// expect: " + expected + "\n"
            source_file.write_text(source, encoding="utf-8")
            checked = expected is not None or re.search(r"^// expect:", source, re.M)
            command = [binary, "--check", str(source_file)] if checked else [binary, str(source_file), *arguments]
            expected_stdout = None
            try:
                validate_fixture(source, require_expect=bool(checked))
                stdin = string_fixture(source, "stdin", default="Ada\n42\n")
                expected_stdout = string_fixture(source, "stdout", default="" if checked else None)
                run = subprocess.run(command, cwd=work, input=stdin, capture_output=True,
                                     text=True, encoding="utf-8", timeout=10)
                passed = (run.returncode == 1 and run.stdout == error + "\n") if error else run.returncode == 0
                detail = run.stdout + run.stderr
                passed = passed and not run.stderr
                if expected_stdout is not None:
                    if checked and not error:
                        expected_stdout += "Script checks passed\n"
                    if run.stdout != expected_stdout:
                        passed = False
                        detail += f"\nExpected stdout {expected_stdout!r}, got {run.stdout!r}"
            except (ValueError, OSError) as failure:
                passed, detail = False, str(failure)
            except subprocess.TimeoutExpired:
                passed, detail = False, "Timed out after 10 seconds"
            results.append({"name": name, "passed": passed, "detail": detail,
                            "source": source, "checks_result": bool(checked),
                            "expected_error": error,
                            "checks_stdout": expected_stdout is not None})

    for filename in ["README.md", "SPEC.md", "DEVELOPING.md", "FEATURES.md"]:
        for index, (language, source) in enumerate(fences((root / filename).read_text(encoding="utf-8")), 1):
            label = f"{filename} block {index}"
            if language == "kai":
                errors = [(code, comment) for code, comment in map(line_comment, source.splitlines())
                          if comment.startswith("Type error:")]
                if errors:
                    for expression, error in errors:
                        check(label + " expected error", expression, error=error)
                    continue
                check(label, source)
                lines = source.splitlines()
                for line_index, line in enumerate(lines):
                    _, comment = line_comment(line)
                    if comment.startswith("=>"):
                        expected = comment[2:].strip()
                        check(label + f" result {line_index + 1}", "\n".join(lines[:line_index + 1]), expected)
            elif filename == "SPEC.md" and language == "text":
                for line in source.splitlines():
                    code, _ = line_comment(line)
                    match = re.match(r"^([a-z][A-Za-z0-9_]*)\s*:\s*(.+?)$", code)
                    if match:
                        check(label + " signature " + match.group(1), "(" + code + ")")

    page = WebsiteExamples()
    page.feed(html)
    if not page.examples:
        raise RuntimeError("Rendered website contains no runnable examples")
    for title, source in page.examples:
        if not source.strip():
            raise RuntimeError("Empty website example: " + title)
        check("Website: " + title, source, arguments=())
    return results


def parse_args(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", help="Built kai executable")
    parser.add_argument("website", nargs="?", help="Built kai-website executable")
    parser.add_argument("--html", type=Path, help="Rendered website HTML from the same source revision")
    parser.add_argument("--json", type=Path, help="Optional result file")
    args = parser.parse_args(argv)
    if bool(args.website) == bool(args.html):
        parser.error("provide exactly one website executable or --html file")
    return args


def main(argv=None):
    args = parse_args(argv)
    if args.json:
        args.json = args.json.resolve()
    binary = str(Path(shutil.which(args.binary) or args.binary).resolve())
    root = Path(__file__).resolve().parent.parent
    if args.html:
        results = check_examples(binary, root, args.html.read_text(encoding="utf-8"))
    else:
        website = str(Path(shutil.which(args.website) or args.website).resolve())
        os.chdir(root)
        with website_html(website) as html:
            results = check_examples(binary, root, html)
    failures = [result for result in results if not result["passed"]]
    for result in failures:
        print(result["name"] + ": " + result["detail"])
    if args.json:
        args.json.write_text(json.dumps(results, indent=2) + "\n", encoding="utf-8")
    print(f"Documentation examples: {len(results) - len(failures)}/{len(results)} passed")
    return bool(failures)


if __name__ == "__main__":
    raise SystemExit(main())
