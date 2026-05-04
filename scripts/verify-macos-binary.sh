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

if ! command -v xcrun >/dev/null 2>&1; then
  echo "error: xcrun is required to inspect macOS weak binds" >&2
  exit 69
fi

echo "=== xcrun objdump --weak-bind --macho $bin ==="
if ! weak_bind_output="$(xcrun objdump --weak-bind --macho "$bin")"; then
  echo "error: failed to inspect macOS weak binds" >&2
  exit 1
fi
weak_bind_count="$(printf '%s\n' "$weak_bind_output" | grep -Eic 'llvm|mlir' || true)"

if ! command -v nm >/dev/null 2>&1; then
  echo "error: nm is required to classify macOS weak binds" >&2
  exit 69
fi

# Mach-O weak-bind output includes C++ weak/coalesced definitions that are
# present in the executable as well as true undefined weak imports.  The
# release-package failure this script guards against is the latter: an
# LLVM/MLIR symbol dyld must resolve from a missing Homebrew dylib at runtime.
# Classify with nm's undefined-symbol view before failing.
undefined_llvm_mlir="$(
  nm -m -u "$bin" \
    | grep -Ei 'llvm|mlir' || true
)"

if [ -n "$undefined_llvm_mlir" ]; then
  echo "error: macOS binary has undefined LLVM/MLIR imports:" >&2
  echo "$undefined_llvm_mlir" >&2
  exit 1
fi

if [ "$weak_bind_count" -gt 0 ]; then
  echo "Info — LLVM/MLIR weak-bind entries are defined in the executable." >&2
fi

echo "Clean — no undefined LLVM/MLIR imports."
