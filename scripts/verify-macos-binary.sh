#!/usr/bin/env bash
set -euo pipefail

if [ $# -ne 1 ]; then
  echo "usage: $0 <path-to-macos-binary>" >&2
  exit 64
fi

bin="$1"

if ! command -v otool >/dev/null 2>&1; then
  echo "error: otool is required to inspect macOS binaries" >&2
  exit 69
fi

if [ ! -f "$bin" ]; then
  echo "error: binary not found: $bin" >&2
  exit 66
fi

echo "=== otool -L $bin ==="
otool -L "$bin"

unexpected_deps="$(
  otool -L "$bin" \
    | tail -n +2 \
    | awk '{print $1}' \
    | grep -Ev '^(/usr/lib/|/System/Library/)' || true
)"

if [ -n "$unexpected_deps" ]; then
  echo "error: macOS binary has non-system dynamic dependencies:" >&2
  echo "$unexpected_deps" >&2
  exit 1
fi

echo "Clean — only system dylibs."
