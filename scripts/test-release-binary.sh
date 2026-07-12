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

total=0
failed=0
failures=()
while IFS= read -r -d '' test_file; do
  total=$((total + 1))
  status=0
  case "$test_file" in
    */input*)
      case "$(basename "$test_file")" in
        input_boolean_logic.kai)
          test_input='yes\nyes'
          ;;
        input_comparison_chain.kai)
          test_input='a\na'
          ;;
        input_empty_handling.kai)
          test_input=''
          ;;
        input_nested_conditionals.kai)
          test_input='admin\nadmin'
          ;;
        *)
          test_input='test_input'
          ;;
      esac
      if [ "$(basename "$test_file")" = "input_empty_handling.kai" ]; then
        printf '\n' | "$binary" "$test_file" >/dev/null 2>&1 || status=$?
      else
        printf '%b' "$test_input" | "$binary" "$test_file" >/dev/null 2>&1 || status=$?
      fi
      ;;
    *)
      "$binary" "$test_file" >/dev/null 2>&1 || status=$?
      ;;
  esac

  if [ "$status" -ne 0 ]; then
    failed=$((failed + 1))
    failures+=("$test_file:$status")
  fi
done < <(find "$root_dir/tests" -type f -name '*.kai' -print0)

if [ "$total" -eq 0 ]; then
  echo "release binary test corpus is empty" >&2
  exit 1
fi

if [ "$failed" -ne 0 ]; then
  printf 'release binary failure: %s\n' "${failures[@]}" >&2
  exit 1
fi

printf 'release binary tests passed: %d/%d\n' "$total" "$total"
