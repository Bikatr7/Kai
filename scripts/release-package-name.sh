#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: $0 <os> <arch>" >&2
  exit 1
fi

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
binary_name=$(bash "${script_dir}/release-asset-name.sh" "$1" "$2")
os=$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')

case "$os" in
  linux)
    printf '%s.tar.gz\n' "${binary_name}"
    ;;
  macos|darwin)
    printf '%s.zip\n' "${binary_name}"
    ;;
  windows)
    printf '%s.zip\n' "${binary_name%.exe}"
    ;;
  *)
    echo "Unsupported OS: $1" >&2
    exit 1
    ;;
esac
