#!/usr/bin/env bash
# test-release-binary.sh — Smoke-test the release hew binary with `hew run`.
#
# Builds hew-cli in release mode and runs a trivial Hew program to confirm
# process-exit is clean (no SIGABRT, no malloc errors).
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

# Write a minimal Hew program to a temp file.
SMOKE_FILE="$(mktemp /tmp/hew-release-smoke-XXXXXX).hew"
trap 'rm -f "$SMOKE_FILE"' EXIT

cat > "$SMOKE_FILE" <<'HEWEOF'
fn main() {
    print("hello-release-smoke")
}
HEWEOF

echo "==> Running: $HEW run $SMOKE_FILE"
# Under set -euo pipefail, a non-zero exit inside $(...) propagates before
# status=$? executes, making the FAIL diagnostic unreachable.  Using the
# if-negation form keeps set -e active and lets us emit the operator message.
if ! output=$("$HEW" run "$SMOKE_FILE" 2>/dev/null); then
    echo "FAIL: hew run exited nonzero (expected 0)" >&2
    echo "This may be a process-exit SIGABRT — see issue #1606." >&2
    exit 1
fi

if [[ "$output" != *"hello-release-smoke"* ]]; then
    echo "FAIL: expected 'hello-release-smoke' in output, got: $output" >&2
    exit 1
fi

echo "PASS: release binary ran cleanly (exit 0, output: $output)"
