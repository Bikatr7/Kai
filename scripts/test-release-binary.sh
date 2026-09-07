#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: $0 <binary> <expected-version>" >&2
  exit 1
fi

binary=$1
expected_version=$2
root_dir=${KAI_TEST_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)}

if [ ! -f "$binary" ]; then
  echo "release binary not found: $binary" >&2
  exit 1
fi

actual_version=$("$binary" --version | tr -d '\r')
if [ "$actual_version" != "Kai v${expected_version}" ]; then
  echo "unexpected version output: $actual_version" >&2
  exit 1
fi

help_output=$("$binary" --help | tr -d '\r')
if ! grep -Fq "kai --version" <<< "$help_output"; then
  echo "release binary help does not document --version" >&2
  exit 1
fi

expression_output=$("$binary" --debug -e '6 * 7' | tr -d '\r')
if ! grep -Fq "Evaluation: 42" <<< "$expression_output"; then
  echo "release binary did not evaluate the smoke expression" >&2
  exit 1
fi

set +e
error_output=$("$binary" -e '1 / 0' 2>&1)
error_status=$?
set -e
if [ "$error_status" -eq 0 ] || ! grep -Fq "DivByZero" <<< "$error_output"; then
  echo "release binary did not report division by zero correctly" >&2
  exit 1
fi

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
python3 "$script_dir/check-script-corpus.py" "$binary" "$root_dir/tests"
