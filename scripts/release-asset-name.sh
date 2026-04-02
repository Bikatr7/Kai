#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: $0 <os> <arch>" >&2
  exit 1
fi

os=$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')
arch=$(printf '%s' "$2" | tr '[:upper:]' '[:lower:]')

case "$os" in
  linux)
    platform="linux"
    ;;
  macos|darwin)
    platform="macos"
    ;;
  windows)
    platform="windows"
    ;;
  *)
    echo "Unsupported OS: $1" >&2
    exit 1
    ;;
esac

case "$arch" in
  x64|x86_64|amd64)
    target_arch="amd64"
    ;;
  arm64|aarch64)
    target_arch="arm64"
    ;;
  *)
    echo "Unsupported arch: $2" >&2
    exit 1
    ;;
esac

if [ "$platform" = "windows" ]; then
  printf 'kai-%s-%s.exe\n' "$platform" "$target_arch"
else
  printf 'kai-%s-%s\n' "$platform" "$target_arch"
fi
