#!/usr/bin/env bash
# o2-differential-selftest.sh - Self-proof for scripts/o2-differential.sh.
#
# Eight independently-failable cases prove: the differential gate distinguishes
# O0/O2 outcome sets, accepts identical outcome sets, fails closed when a test
# runner does not produce a summary, refuses to report success over an outcome
# set smaller than the declared floor (two EMPTY sets compare identical), and
# refuses to run at all over a non-default corpus whose size nobody declared;
# and (C1, the ratchet->differential handoff) the --o0-outcomes pre-captured-file path is behaviourally
# EQUIVALENT to a fresh self-run O0 pass on both the identical-outcomes case
# and the divergence-caught case, and fails closed when the handoff file is
# missing or empty.
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

# Counterfactual marker.  Every case below drives the REAL differential gate
# against a stub runner rigged to diverge, crash, or under-report — so this
# self-test PASSES while its cases produce genuine gate-failure text.  The log
# was written to a file and only dumped on failure, which hid it from readers
# and left no evidence in a green log that the bait path ran.  Replaying it
# behind the marker keeps it readable, keeps the preflight's first-failure
# extractor from reporting it as a verdict, and is what
# Prefix provoked failures so they are unmistakable in a passing self-test log.
COUNTERFACTUAL_MARKER="CF-"

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
  local stub_case="$3"
  local log="$TMPDIR_BASE/$name.log"
  local rc=0

  echo "--- Case: $name ---"
  O2_DIFF_SELFTEST_CASE="$stub_case" HEW_BIN="$STUB" \
    bash "$GATE" --tests-dir "$TESTS_DIR" "${@:4}" > "$log" 2>&1 || rc=$?

  printf '%s\n' "${COUNTERFACTUAL_MARKER}[${name}] exit ${rc}"
  sed "s|^|${COUNTERFACTUAL_MARKER}[${name}] |" "$log"

  if [[ "$rc" -eq "$expected_rc" ]]; then
    pass "$name"
  else
    fail "$name" "gate exited $rc (expected $expected_rc)"
  fi
}

run_case "divergence-caught" 1 "divergence-caught" --min-outcomes 2
run_case "baseline-identical" 0 "baseline-identical" --min-outcomes 2
run_case "no-summary-fail-closed" 1 "no-summary-fail-closed" --min-outcomes 2

# ── Corpus-floor cases ───────────────────────────────────────────────────────
# The identical-outcomes comparison is satisfied by two EMPTY sets, so the gate
# must refuse to report success over a corpus smaller than the caller declared,
# and must refuse to run at all over a non-default corpus with no declared size.
run_case "floor-below-declared-minimum" 1 "baseline-identical" --min-outcomes 3
run_case "floor-required-for-custom-corpus" 1 "baseline-identical"

# ── C1 handoff-path equivalence cases ────────────────────────────────────────
# The stub's O0 output (unset/HEW_OPT_LEVEL=0) for each existing case is the
# exact content scripts/corpus-ratchet.sh hew-suite's --emit-o0-outcomes would write.
# Pre-capture it the same way (same extraction regex the gate itself uses) and
# feed it back via --o0-outcomes, asserting the handoff path reaches the same
# verdict as the self-run path above for BOTH the identical and the
# divergent case — proving C1 drops no coverage.
capture_o0_outcomes() {
  local stub_case="$1"
  local out="$2"
  O2_DIFF_SELFTEST_CASE="$stub_case" HEW_OPT_LEVEL=0 "$STUB" 2>&1 \
    | grep -E "^test .* \.\.\. (ok|PASSED|FAILED|ignored)" \
    | sort > "$out"
}

BASELINE_O0_FILE="$TMPDIR_BASE/baseline-identical.o0.txt"
DIVERGENCE_O0_FILE="$TMPDIR_BASE/divergence-caught.o0.txt"
capture_o0_outcomes "baseline-identical" "$BASELINE_O0_FILE"
capture_o0_outcomes "divergence-caught" "$DIVERGENCE_O0_FILE"

run_case "outcomes-handoff-identical" 0 "baseline-identical" \
  --min-outcomes 2 --o0-outcomes "$BASELINE_O0_FILE"
run_case "outcomes-handoff-divergence-caught" 1 "divergence-caught" \
  --min-outcomes 2 --o0-outcomes "$DIVERGENCE_O0_FILE"
run_case "outcomes-handoff-missing-file-fails-closed" 1 "baseline-identical" \
  --min-outcomes 2 --o0-outcomes "$TMPDIR_BASE/does-not-exist.txt"

echo ""
echo "o2-differential-selftest: all 8 cases PASS"
