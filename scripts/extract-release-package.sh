#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "usage: $0 <os> <package> <output-dir>" >&2
  exit 1
fi

os=$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')
package=$2
output_dir=$3

if [ ! -f "$package" ]; then
  echo "release package not found: $package" >&2
  exit 1
fi

mkdir -p "$output_dir"
reported_output_dir=${output_dir%/}
package=$(cd "$(dirname "$package")" && pwd)/$(basename "$package")
output_dir=$(cd "$output_dir" && pwd)
case "$os" in
  windows)
    zip_header=$(od -An -tx1 -N4 "$package" | tr -d '[:space:]')
    if [ "$zip_header" != "504b0304" ]; then
      echo "Windows release package is not a valid non-empty ZIP archive" >&2
      exit 1
    fi
    if command -v powershell.exe >/dev/null 2>&1; then
      package_windows=$(cygpath -w "$package")
      output_windows=$(cygpath -w "$output_dir")
      # PowerShell, not Bash, expands the variables in the command string.
      # shellcheck disable=SC2016
      KAI_PACKAGE_WINDOWS="$package_windows" \
      KAI_OUTPUT_WINDOWS="$output_windows" \
      powershell.exe -NoLogo -NoProfile -NonInteractive -Command \
        '$ErrorActionPreference = "Stop"; Expand-Archive -LiteralPath $env:KAI_PACKAGE_WINDOWS -DestinationPath $env:KAI_OUTPUT_WINDOWS -Force' \
        >/dev/null
    elif command -v unzip >/dev/null 2>&1; then
      unzip -q "$package" -d "$output_dir"
    else
      echo "PowerShell or unzip is required to extract the Windows release" >&2
      exit 1
    fi
    binary="$output_dir/kai.exe"
    ;;
  macos|darwin)
    zip_header=$(od -An -tx1 -N4 "$package" | tr -d '[:space:]')
    if [ "$zip_header" != "504b0304" ]; then
      echo "macOS release package is not a valid non-empty ZIP archive" >&2
      exit 1
    fi
    if command -v ditto >/dev/null 2>&1; then
      ditto -x -k "$package" "$output_dir"
    elif command -v unzip >/dev/null 2>&1; then
      unzip -q "$package" -d "$output_dir"
    elif command -v python3 >/dev/null 2>&1; then
      python3 - "$package" "$output_dir" <<'PY'
from pathlib import Path
import os
import sys
import zipfile

with zipfile.ZipFile(sys.argv[1]) as archive:
    archive.extractall(sys.argv[2])
    mode = archive.getinfo("kai").external_attr >> 16
    if mode:
        os.chmod(Path(sys.argv[2]) / "kai", mode & 0o777)
PY
    else
      echo "ditto, unzip, or python3 is required to extract the macOS release" >&2
      exit 1
    fi
    binary="$output_dir/kai"
    ;;
  linux)
    tar -xzf "$package" -C "$output_dir"
    binary="$output_dir/kai"
    ;;
  *)
    echo "Unsupported OS: $1" >&2
    exit 1
    ;;
esac

if [ ! -f "$binary" ]; then
  echo "release package did not contain the expected binary: $binary" >&2
  exit 1
fi

printf '%s/%s\n' "$reported_output_dir" "$(basename "$binary")"
