#!/usr/bin/env bash
# o2-differential-selftest.sh - Self-proof for scripts/o2-differential.sh.
#
# Three independently-failable cases prove the differential gate distinguishes
# O0/O2 outcome sets, accepts identical outcome sets, and fails closed when a
# test runner does not produce a summary.
#
# Exit codes:
#   0  all cases pass
#   1  one or more cases fail (details on stderr)

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GATE="$SCRIPT_DIR/o2-differential.sh"

TMPDIR_BASE="$(mktemp -d /tmp/hew-o2-differential-selftest.XXXXXX)"
trap 'rm -rf "$TMPDIR_BASE"' EXIT

TESTS_DIR="$TMPDIR_BASE/tests"
STUB="$TMPDIR_BASE/hew-stub"
mkdir -p "$TESTS_DIR"

pass() { echo "PASS $1"; }
fail() { echo "FAIL $1: $2" >&2; exit 1; }

cat > "$STUB" <<'EOF'
#!/usr/bin/env bash
set -u

case "${O2_DIFF_SELFTEST_CASE:-}" in
  divergence-caught)
    echo "test alpha ... PASSED"
    if [[ "${HEW_OPT_LEVEL:-0}" == "2" ]]; then
      echo "test beta ... FAILED"
      echo "test result: FAILED. 1 passed; 1 failed; 0 ignored"
    else
      echo "test beta ... PASSED"
      echo "test result: ok. 2 passed; 0 failed; 0 ignored"
    fi
    ;;
  baseline-identical)
    echo "test alpha ... PASSED"
    echo "test beta ... ignored"
    echo "test result: ok. 1 passed; 0 failed; 1 ignored"
    ;;
  no-summary-fail-closed)
    echo "test alpha ... PASSED"
    echo "runner terminated before summary"
    ;;
  *)
    echo "unknown O2_DIFF_SELFTEST_CASE: ${O2_DIFF_SELFTEST_CASE:-}" >&2
    exit 2
    ;;
esac
EOF
chmod +x "$STUB"

run_case() {
  local name="$1"
  local expected_rc="$2"
  local log="$TMPDIR_BASE/$name.log"
  local rc=0

  echo "--- Case: $name ---"
  O2_DIFF_SELFTEST_CASE="$name" HEW_BIN="$STUB" \
    bash "$GATE" --tests-dir "$TESTS_DIR" > "$log" 2>&1 || rc=$?

  if [[ "$rc" -eq "$expected_rc" ]]; then
    pass "$name"
  else
    sed 's/^/  /' "$log" >&2
    fail "$name" "gate exited $rc (expected $expected_rc)"
  fi
}

run_case "divergence-caught" 1
run_case "baseline-identical" 0
run_case "no-summary-fail-closed" 1

echo ""
echo "o2-differential-selftest: all 3 cases PASS"
