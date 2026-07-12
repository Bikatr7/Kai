#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 4 ]; then
  echo "usage: $0 <os> <arch> <binary> <output-dir>" >&2
  exit 1
fi

os=$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')
arch=$2
binary=$3
output_dir=$4

if [ ! -f "$binary" ]; then
  echo "release binary not found: $binary" >&2
  exit 1
fi

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
package_name=$("${script_dir}/release-package-name.sh" "$os" "$arch")
work_dir=$(mktemp -d "${TMPDIR:-/tmp}/kai-release-package.XXXXXX")

cleanup() {
  rm -rf "$work_dir"
}
trap cleanup EXIT

mkdir -p "$output_dir"
reported_package_path="${output_dir%/}/$package_name"
output_dir=$(cd "$output_dir" && pwd)
package_path="$output_dir/$package_name"
case "$os" in
  windows)
    cp "$binary" "$work_dir/kai.exe"
    if command -v powershell.exe >/dev/null 2>&1; then
      binary_windows=$(cygpath -w "$work_dir/kai.exe")
      package_windows=$(cygpath -w "$package_path")
      # PowerShell, not Bash, expands the variables in the command string.
      # shellcheck disable=SC2016
      KAI_BINARY_WINDOWS="$binary_windows" \
      KAI_PACKAGE_WINDOWS="$package_windows" \
      powershell.exe -NoLogo -NoProfile -NonInteractive -Command \
        '$ErrorActionPreference = "Stop"; Compress-Archive -LiteralPath $env:KAI_BINARY_WINDOWS -DestinationPath $env:KAI_PACKAGE_WINDOWS -CompressionLevel Optimal -Force' \
        >/dev/null
    elif command -v zip >/dev/null 2>&1; then
      (cd "$work_dir" && zip -q "$package_path" kai.exe)
    else
      echo "PowerShell or zip is required to package the Windows release" >&2
      exit 1
    fi
    ;;
  macos|darwin)
    cp "$binary" "$work_dir/kai"
    chmod 755 "$work_dir/kai"
    if command -v ditto >/dev/null 2>&1; then
      ditto -c -k --norsrc "$work_dir/kai" "$package_path"
    elif command -v zip >/dev/null 2>&1; then
      (cd "$work_dir" && zip -q "$package_path" kai)
    else
      echo "ditto or zip is required to package the macOS release" >&2
      exit 1
    fi
    ;;
  linux)
    cp "$binary" "$work_dir/kai"
    chmod 755 "$work_dir/kai"
    tar -czf "$package_path" -C "$work_dir" kai
    ;;
  *)
    echo "Unsupported OS: $1" >&2
    exit 1
    ;;
esac

case "$os" in
  windows|macos|darwin)
    zip_header=$(od -An -tx1 -N4 "$package_path" | tr -d '[:space:]')
    if [ "$zip_header" != "504b0304" ]; then
      echo "release package is not a valid non-empty ZIP archive: $package_path" >&2
      exit 1
    fi
    ;;
esac

printf '%s\n' "$reported_package_path"
