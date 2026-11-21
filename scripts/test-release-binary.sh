#!/usr/bin/env bash
# Build and execute a native program with the release compiler.
#
# This guard exists to catch ABI-mismatch aborts (see issue #1606): bugs
# where the release binary silently crashes at process exit, which debug
# builds and unit tests do not exercise because they use different C++ runtime
# linkage.
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "==> Release binary smoke test"

# Cargo's artifact root is configurable through CARGO_TARGET_DIR and Cargo
# configuration.  Resolve it the same way the Makefile does so the binary we
# provisioned is the binary this smoke test executes, and fail closed when that
# resolved artifact is absent.
CARGO_NATIVE_OUT="$("$REPO_ROOT/scripts/cargo-output-dir.py" --native)"
if [[ "$CARGO_NATIVE_OUT" != /* ]]; then
    CARGO_NATIVE_OUT="$REPO_ROOT/$CARGO_NATIVE_OUT"
fi
HEW="$CARGO_NATIVE_OUT/release/hew"
if [[ ! -x "$HEW" ]]; then
    echo "error: $HEW not found — run 'make release-host' first" >&2
    exit 1
fi
FIXTURE="$REPO_ROOT/examples/v05/hello_int.hew"
EXPECTED_EXIT=42
WORK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hew-release-smoke-XXXXXX")"
trap 'rm -rf "$WORK_DIR"' EXIT

NATIVE_BIN="$WORK_DIR/hello_int"
case "$(uname -s)" in
MINGW* | MSYS* | CYGWIN*) NATIVE_BIN+=".exe" ;;
esac
echo "==> Building the release smoke program"
if ! "$HEW" build "$FIXTURE" -o "$NATIVE_BIN"; then
    echo "FAIL: hew build exited nonzero (expected 0)" >&2
    exit 1
fi

if [[ ! -x "$NATIVE_BIN" ]]; then
    echo "FAIL: expected native binary at $NATIVE_BIN, not found" >&2
    exit 1
fi

set +e
"$NATIVE_BIN"
actual_exit=$?
set -e

if ((actual_exit != EXPECTED_EXIT)); then
    echo "FAIL: native binary exited $actual_exit (expected $EXPECTED_EXIT)" >&2
    exit 1
fi

echo "PASS: release binary compiled fixture and produced binary that exited $EXPECTED_EXIT"
