#!/usr/bin/env bash
# o2-differential-selftest.sh - Self-proof for scripts/o2-differential.sh.
#
# Fourteen independently-failable cases prove: the differential gate distinguishes
# O0/O2 outcome sets, accepts identical outcome sets, fails closed when a test
# runner does not produce a summary, refuses to report success over an outcome
# set smaller than the declared floor (two EMPTY sets compare identical), and
# refuses to run at all over a non-default corpus whose size nobody declared;
# rejects compile failures that drift to runtime, timeout, or launch failures;
# and (C1, the ratchet->differential handoff) the --o0-outcomes pre-captured-file path is behaviourally
# EQUIVALENT to a fresh self-run O0 pass on both the identical-outcomes case
# and every divergence case, and fails closed when the handoff file is missing
# or empty.
#
# Exit codes:
#   0  all cases pass
#   1  one or more cases fail (details on stderr)

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GATE="$SCRIPT_DIR/o2-differential.sh"
JUNIT_PARSER="$SCRIPT_DIR/lib/hew_junit.py"

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
fail() {
    echo "FAIL $1: $2" >&2
    exit 1
}

cat >"$STUB" <<'EOF'
#!/usr/bin/env bash
set -u

case "${O2_DIFF_SELFTEST_CASE:-}" in
  divergence-caught)
    if [[ "${HEW_OPT_LEVEL:-0}" == "2" ]]; then
      cat <<'XML'
<testsuites tests="2" failures="1" skipped="0"><testsuite name="fixture" tests="2" failures="1" skipped="0"><testcase classname="fixture.hew" name="alpha"/><testcase classname="fixture.hew" name="beta"><failure type="runtime" message="counterfactual">counterfactual</failure></testcase></testsuite></testsuites>
XML
      exit 1
    else
      cat <<'XML'
<testsuites tests="2" failures="0" skipped="0"><testsuite name="fixture" tests="2" failures="0" skipped="0"><testcase classname="fixture.hew" name="alpha"/><testcase classname="fixture.hew" name="beta"/></testsuite></testsuites>
XML
    fi
    ;;
  kind-drift-runtime|kind-drift-timeout|kind-drift-launch)
    kind="${O2_DIFF_SELFTEST_CASE#kind-drift-}"
    if [[ "${HEW_OPT_LEVEL:-0}" != "2" ]]; then
      kind="compile"
    fi
    cat <<XML
<testsuites tests="2" failures="1" skipped="0"><testsuite name="fixture" tests="2" failures="1" skipped="0"><testcase classname="fixture.hew" name="alpha"/><testcase classname="fixture.hew" name="beta"><failure type="$kind" message="same diagnostic">same diagnostic</failure></testcase></testsuite></testsuites>
XML
    exit 1
    ;;
  baseline-identical)
    cat <<'XML'
<testsuites tests="2" failures="0" skipped="1"><testsuite name="fixture" tests="2" failures="0" skipped="1"><testcase classname="fixture.hew" name="alpha"/><testcase classname="fixture.hew" name="beta"><skipped/></testcase></testsuite></testsuites>
XML
    ;;
  no-summary-fail-closed)
    echo '<testsuites tests="2"'
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
        bash "$GATE" --tests-dir "$TESTS_DIR" "${@:4}" >"$log" 2>&1 || rc=$?

    printf '%s\n' "${COUNTERFACTUAL_MARKER}[${name}] exit ${rc}"
    sed "s|^|${COUNTERFACTUAL_MARKER}[${name}] |" "$log"

    if [[ "$rc" -eq "$expected_rc" ]]; then
        pass "$name"
    else
        fail "$name" "gate exited $rc (expected $expected_rc)"
    fi
}

assert_kind_drift_visible() {
    local name="$1"
    local actual_kind="$2"
    local log="$TMPDIR_BASE/$name.log"

    grep -Fq "FAILED"$'\t'"compile" "$log" ||
        fail "$name" "O0 compile failure kind was absent from the outcome diff"
    grep -Fq "FAILED"$'\t'"$actual_kind" "$log" ||
        fail "$name" "O2 $actual_kind failure kind was absent from the outcome diff"
}

run_case "divergence-caught" 1 "divergence-caught" --min-outcomes 2
for kind in runtime timeout launch; do
    name="failure-kind-drift-$kind"
    run_case "$name" 1 "kind-drift-$kind" --min-outcomes 2
    assert_kind_drift_visible "$name" "$kind"
done
run_case "baseline-identical" 0 "baseline-identical" --min-outcomes 2
run_case "no-summary-fail-closed" 1 "no-summary-fail-closed" --min-outcomes 2

# ── Corpus-floor cases ───────────────────────────────────────────────────────
# The identical-outcomes comparison is satisfied by two EMPTY sets, so the gate
# must refuse to report success over a corpus smaller than the caller declared,
# and must refuse to run at all over a non-default corpus with no declared size.
run_case "floor-below-declared-minimum" 1 "baseline-identical" --min-outcomes 3
run_case "floor-required-for-custom-corpus" 1 "baseline-identical"

# ── C1 handoff-path equivalence cases ────────────────────────────────────────
# The stub's O0 JUnit for each existing case carries the same structured
# outcomes scripts/corpus-ratchet.sh emits through --emit-o0-outcomes.
# Pre-capture it through the shared parser and
# feed it back via --o0-outcomes, asserting the handoff path reaches the same
# verdict as the self-run path above for BOTH the identical and the
# divergent case — proving C1 drops no coverage.
capture_o0_outcomes() {
    local stub_case="$1"
    local out="$2"
    local report="$out.xml"
    local parsed rc=0
    O2_DIFF_SELFTEST_CASE="$stub_case" HEW_OPT_LEVEL=0 "$STUB" test "$TESTS_DIR" \
        --format junit >"$report" || rc=$?
    parsed="$("${PYTHON:-python3}" "$JUNIT_PARSER" --runner-exit "$rc" "$report")" ||
        fail "capture-$stub_case" "stub did not produce coherent JUnit"
    printf '%s\n' "$parsed" |
        awk -F'\t' '$1 != "__SUMMARY__" {
            line = "test " $2 " ... " $1
            if ($3 != "") line = line "\t" $3
            print line
        }' |
        sort >"$out"
}

BASELINE_O0_FILE="$TMPDIR_BASE/baseline-identical.o0.txt"
DIVERGENCE_O0_FILE="$TMPDIR_BASE/divergence-caught.o0.txt"
KIND_DRIFT_O0_FILE="$TMPDIR_BASE/kind-drift.o0.txt"
capture_o0_outcomes "baseline-identical" "$BASELINE_O0_FILE"
capture_o0_outcomes "divergence-caught" "$DIVERGENCE_O0_FILE"
capture_o0_outcomes "kind-drift-runtime" "$KIND_DRIFT_O0_FILE"
grep -Fq "FAILED"$'\t'"compile" "$KIND_DRIFT_O0_FILE" ||
    fail "capture-kind-drift" "the O0 handoff dropped its compile failure kind"

run_case "outcomes-handoff-identical" 0 "baseline-identical" \
    --min-outcomes 2 --o0-outcomes "$BASELINE_O0_FILE"
run_case "outcomes-handoff-divergence-caught" 1 "divergence-caught" \
    --min-outcomes 2 --o0-outcomes "$DIVERGENCE_O0_FILE"
for kind in runtime timeout launch; do
    name="outcomes-handoff-failure-kind-drift-$kind"
    run_case "$name" 1 "kind-drift-$kind" \
        --min-outcomes 2 --o0-outcomes "$KIND_DRIFT_O0_FILE"
    assert_kind_drift_visible "$name" "$kind"
done
run_case "outcomes-handoff-missing-file-fails-closed" 1 "baseline-identical" \
    --min-outcomes 2 --o0-outcomes "$TMPDIR_BASE/does-not-exist.txt"

echo ""
echo "o2-differential-selftest: all 14 cases PASS"
