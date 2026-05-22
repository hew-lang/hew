#!/usr/bin/env bash
# test-release-binary.sh — Smoke-test the release hew binary via compile.
#
# Builds hew-cli in release mode and drives a trivial Hew program through
# the IR ladder (`hew compile`) to confirm the release binary
# produces a working native executable.
#
# This guard exists to catch ABI-mismatch aborts (see issue #1606): bugs
# where the release binary silently crashes at process exit, which debug
# builds and unit tests do not exercise because they use different C++ runtime
# linkage.
#
# Usage:
#   scripts/test-release-binary.sh             # build + run smoke test
#   scripts/test-release-binary.sh --no-build  # skip cargo build (reuse existing)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

NO_BUILD=0
for arg in "$@"; do
    case "$arg" in
        --no-build) NO_BUILD=1 ;;
        *) echo "Unknown argument: $arg" >&2; exit 1 ;;
    esac
done

echo "==> Release binary smoke test"

if (( NO_BUILD == 0 )); then
    echo "==> Building hew-cli (release)"
    cargo build --release -p hew-cli 2>&1
fi

HEW="$REPO_ROOT/target/release/hew"
if [[ ! -x "$HEW" ]]; then
    echo "error: $HEW not found — run without --no-build or run 'cargo build --release -p hew-cli' first" >&2
    exit 1
fi
# Drive the spine fixture through the IR ladder. The compile
# subcommand emits artefacts under `.tmp/compile-out/` relative to the
# current working directory, so run it from a scratch directory and then
# execute the native binary it produced. The fixture's `main` returns 42.
FIXTURE="$REPO_ROOT/examples/v05/hello_int.hew"
EXPECTED_EXIT=42
WORK_DIR="$(mktemp -d /tmp/hew-release-smoke-XXXXXX)"
trap 'rm -rf "$WORK_DIR"' EXIT

cp "$FIXTURE" "$WORK_DIR/hello_int.hew"

echo "==> Running: $HEW compile hello_int.hew (cwd=$WORK_DIR)"
if ! ( cd "$WORK_DIR" && "$HEW" compile hello_int.hew ); then
    echo "FAIL: hew compile exited nonzero (expected 0)" >&2
    echo "This may be a process-exit SIGABRT — see issue #1606." >&2
    exit 1
fi

NATIVE_BIN="$WORK_DIR/.tmp/compile-out/hello_int"
if [[ ! -x "$NATIVE_BIN" ]]; then
    echo "FAIL: expected native binary at $NATIVE_BIN, not found" >&2
    exit 1
fi

set +e
"$NATIVE_BIN"
actual_exit=$?
set -e

if (( actual_exit != EXPECTED_EXIT )); then
    echo "FAIL: native binary exited $actual_exit (expected $EXPECTED_EXIT)" >&2
    exit 1
fi

echo "PASS: release binary compiled fixture and produced binary that exited $EXPECTED_EXIT"
