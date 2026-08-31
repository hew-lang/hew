#!/usr/bin/env bash
# oracle-selftest.sh — Five independently-failable validation tests for
# scripts/fuzz/run-oracle.py.
#
# Test 1 (oracle-flags-a-known-crash):
#   A checker-valid program that crashes at runtime (runtime-abort) must make
#   the oracle exit 1 and report a FAIL verdict.  If the harness wrongly
#   classifies the crash as clean, this test goes red.
#
# Test 2 (oracle-honours-expected-failure):
#   The same crashing program, listed with its exact runtime-crash class, must
#   be tolerated. A mismatched class and a missing file must fail closed. A
#   listed fixture that becomes clean reports recovery in PR mode and fails
#   under strict accounting.
#
# Test 3 (oracle-passes-clean-program):
#   A program with // EXPECT: 7 must exit 0 with verdict clean.  Mutating
#   EXPECT to 8 must exit 1 with verdict wrong-output.
#
# Test 4 (oracle-refuses-an-empty-candidate-set):
#   A run that collected no candidates must exit 1 rather than reporting PASS
#   over nothing, and a corpus override with no declared minimum must be
#   refused outright rather than defaulting to a floor of zero.
#
# Test 5 (oracle-ratchets-positive-rejections):
#   An unlisted positive rejection must fail, the exact listed rejection must
#   pass, and replacing that fixture with a clean program while it remains
#   listed reports recovery in normal mode and fails strict accounting.
#
# All five must pass for the self-test to succeed.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "${ROOT}/scripts/lib/cargo-output-dir.sh"
HEW="${HEW_BIN:-$(cargo_debug_dir "${ROOT}")/hew}"
ORACLE="${ROOT}/scripts/fuzz/run-oracle.py"
TMPDIR_BASE="$(mktemp -d "${TMPDIR:-/tmp}/hew-oracle-selftest.XXXXXX")"
trap 'rm -rf "${TMPDIR_BASE}"' EXIT

# Empty directory passed as --vertical-slice-dir so the self-tests only run
# the temp regressions corpus, not all 245+ vertical-slice/accept fixtures.
# The real oracle run (make fuzz-oracle) covers those; self-tests prove the
# harness classification logic only.
EMPTY_DIR="${TMPDIR_BASE}/empty"
mkdir -p "${EMPTY_DIR}"

# Counterfactual marker.  Every invocation below drives the REAL oracle against
# deliberately broken input, so this self-test PASSES while its output carries
# "ORACLE gate: FAIL", "UNEXPECTED FAILURES" and "error: ...".  Anything that
# reads a log for the first concrete failure line could report this bait instead
# of a real defect. run_counterfactual replays the
# provoked output behind the marker so it stays readable and unmistakable, and
# preserves the exit code the assertions below depend on.
# Keep the prefix on every provoked line.
COUNTERFACTUAL_MARKER="CF-"

run_counterfactual() {
    local label="$1"
    shift
    local output=""
    local status=0

    output="$("$@" 2>&1)" || status=$?
    if [[ -n "$output" ]]; then
        printf '%s\n' "$output" | sed "s/^/${COUNTERFACTUAL_MARKER}[${label}] /"
    fi
    return "$status"
}

pass() { echo "PASS $1"; }
fail() {
    echo "FAIL $1: $2" >&2
    exit 1
}

# ---------------------------------------------------------------------------
# Shared crash fixture: a checker-valid program that aborts at runtime.
#
# This deliberately uses a trap the language GUARANTEES (integer division by
# zero) rather than a reproducer for some specific open bug.  The previous
# fixture was the Vec<record> slice repro; when that bug was fixed the program
# stopped crashing, the oracle correctly reported it clean, and this self-test
# went red -- a test that only passes while a bug remains unfixed.  A guaranteed
# trap cannot rot that way.
# ---------------------------------------------------------------------------
CRASH_SOURCE='fn main() {
  let a = 10;
  let b = 0;
  println(a / b);
}
'

# ---------------------------------------------------------------------------
# Test 1: oracle-flags-a-known-crash
# ---------------------------------------------------------------------------
T1_DIR="${TMPDIR_BASE}/t1_regressions"
mkdir -p "${T1_DIR}"
printf '%s' "${CRASH_SOURCE}" >"${T1_DIR}/crash_fixture.hew"

# Create a matching expected-failures file that does NOT list crash_fixture.hew.
T1_EF="${TMPDIR_BASE}/t1_expected_failures.txt"
printf '# empty\n' >"${T1_EF}"

echo "--- Test 1: oracle-flags-a-known-crash ---"
# Oracle must exit 1 (unexpected failure — crash not listed in expected-failures).
rc=0
run_counterfactual "t1-known-crash" python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T1_DIR}" \
    --expected-failures "${T1_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 ||
    rc=$?
if [[ "${rc}" -ne 1 ]]; then
    fail "oracle-flags-a-known-crash" "expected oracle exit 1, got ${rc}"
fi

# Capture report to verify the verdict token.
T1_REPORT="${TMPDIR_BASE}/t1_report.json"
rc=0
run_counterfactual "t1-report" python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T1_DIR}" \
    --expected-failures "${T1_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 \
    --report "${T1_REPORT}" ||
    rc=$?

# The JSON report must contain runtime-abort or runtime-crash for the fixture.
if ! python3 -c "
import json, sys
r = json.load(open('${T1_REPORT}'))
fails = [v for v in r['verdicts']
         if v['classification'] in ('runtime-abort','runtime-crash','nyi-codegen','build-ice')]
if not fails:
    print('No FAIL verdict found in report', file=sys.stderr)
    sys.exit(1)
print('Found FAIL verdict:', fails[0]['classification'])
"; then
    fail "oracle-flags-a-known-crash" "report did not contain runtime-abort/runtime-crash verdict"
fi

pass "oracle-flags-a-known-crash"

# ---------------------------------------------------------------------------
# Test 2: oracle-honours-expected-failure (ratchet correctness)
# ---------------------------------------------------------------------------
echo "--- Test 2: oracle-honours-expected-failure ---"

T2_DIR="${TMPDIR_BASE}/t2_regressions"
mkdir -p "${T2_DIR}"
printf '%s' "${CRASH_SOURCE}" >"${T2_DIR}/crash_fixture.hew"

# 2a: crash_fixture.hew is listed with its observed class — tolerate it.
T2_EF="${TMPDIR_BASE}/t2_expected_failures.txt"
printf 'crash_fixture.hew runtime-crash  # known crash; issue: #test\n' >"${T2_EF}"

rc=0
run_counterfactual "t2a-listed-crash" python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T2_DIR}" \
    --expected-failures "${T2_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 ||
    rc=$?
if [[ "${rc}" -ne 0 ]]; then
    fail "oracle-honours-expected-failure (2a: listed crash tolerated)" \
        "expected oracle exit 0, got ${rc}"
fi

# 2b: Pinning a different failure class is drift, not a tolerated failure.
printf 'crash_fixture.hew timeout  # deliberately wrong class\n' >"${T2_EF}"
rc=0
output=""
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T2_DIR}" \
    --expected-failures "${T2_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 \
    --report "${TMPDIR_BASE}/t2_drift_report.json" \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 || "${output}" != *"EXPECTED FAILURE CLASS DRIFT"* ]]; then
    fail "oracle-honours-expected-failure (2b: class drift fails gate)" \
        "expected class-drift failure; output: ${output}"
fi
if ! python3 -c "
import json, sys
r = json.load(open('${TMPDIR_BASE}/t2_drift_report.json'))
d = r['expected_failure_class_drifts']
sys.exit(0 if len(d) == 1 and d[0]['expected_classification'] == 'timeout' and d[0]['observed_classification'] == 'runtime-crash' else 1)
"; then
    fail "oracle-honours-expected-failure (2b: class drift fails gate)" \
        "JSON report did not preserve expected and observed classes"
fi

# 2c: A listed fixture absent from selection is an inventory error, not recovery.
printf 'crash_fixture.hew runtime-crash  # known crash; issue: #test\n' >"${T2_EF}"
rm "${T2_DIR}/crash_fixture.hew"
rc=0
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T2_DIR}" \
    --expected-failures "${T2_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 || "${output}" != *"expected failure absent from corpus"* ]]; then
    fail "oracle-honours-expected-failure (2c: missing-but-listed fails gate)" \
        "expected missing-entry failure; output: ${output}"
fi

# 2d/2e: Only a genuinely clean fixture recovers. PR mode reports it; strict
# accounting rejects the stale ledger entry.
printf '// EXPECT: 7\nfn main() { println(7); }\n' >"${T2_DIR}/crash_fixture.hew"
rc=0
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T2_DIR}" \
    --expected-failures "${T2_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 0 || "${output}" != *"NOW-PASSES: crash_fixture.hew"* ]]; then
    fail "oracle-honours-expected-failure (2d: clean recovery reports)" \
        "expected nonblocking clean recovery; output: ${output}"
fi
rc=0
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T2_DIR}" \
    --expected-failures "${T2_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 \
    --strict-recoveries \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 || "${output}" != *"NOW-PASSES: crash_fixture.hew"* ]]; then
    fail "oracle-honours-expected-failure (2e: strict clean recovery fails)" \
        "expected strict clean recovery failure; output: ${output}"
fi

pass "oracle-honours-expected-failure"

# ---------------------------------------------------------------------------
# Test 3: oracle-passes-clean-program (no false positive + exact stdout check)
# ---------------------------------------------------------------------------
echo "--- Test 3: oracle-passes-clean-program ---"

T3_DIR="${TMPDIR_BASE}/t3_regressions"
mkdir -p "${T3_DIR}"
T3_EF="${TMPDIR_BASE}/t3_expected_failures.txt"
printf '# empty\n' >"${T3_EF}"

# 3a: clean program with matching EXPECT — must exit 0, verdict clean.
printf '// EXPECT: 7\nfn main() { println(7); }\n' >"${T3_DIR}/clean_fixture.hew"

rc=0
run_counterfactual "t3a-clean-program" python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T3_DIR}" \
    --expected-failures "${T3_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 ||
    rc=$?
if [[ "${rc}" -ne 0 ]]; then
    fail "oracle-passes-clean-program (3a: clean program passes)" \
        "expected oracle exit 0, got ${rc}"
fi

# 3b: mutate EXPECT to wrong value — must exit 1, verdict wrong-output.
printf '// EXPECT: 8\nfn main() { println(7); }\n' >"${T3_DIR}/clean_fixture.hew"

rc=0
output=""
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T3_DIR}" \
    --expected-failures "${T3_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 ]]; then
    fail "oracle-passes-clean-program (3b: wrong EXPECT fails gate)" \
        "expected oracle exit 1, got ${rc}; output: ${output}"
fi
if ! echo "${output}" | grep -q "wrong-output"; then
    fail "oracle-passes-clean-program (3b: wrong EXPECT fails gate)" \
        "expected 'wrong-output' in output; got: ${output}"
fi

pass "oracle-passes-clean-program"

# ---------------------------------------------------------------------------
echo "--- Test 4: oracle-refuses-an-empty-candidate-set ---"
# Both gate conditions are searches over the collected candidates, so a run
# that collected none finds no unexpected failures and no unexpected passes
# and prints PASS. 4a: an empty corpus below the declared minimum must fail.
T4_DIR="${TMPDIR_BASE}/t4"
T4_EF="${TMPDIR_BASE}/t4_expected_failures.txt"
mkdir -p "${T4_DIR}"
printf '# empty\n' >"${T4_EF}"

rc=0
output=""
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T4_DIR}" \
    --expected-failures "${T4_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --min-candidates 1 \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 ]]; then
    fail "oracle-refuses-an-empty-candidate-set (4a: empty corpus fails gate)" \
        "expected oracle exit 1, got ${rc}; output: ${output}"
fi
if ! echo "${output}" | grep -q "CORPUS MINIMUM"; then
    fail "oracle-refuses-an-empty-candidate-set (4a: empty corpus fails gate)" \
        "expected 'CORPUS MINIMUM' in output; got: ${output}"
fi

# 4b: a corpus override with no declared minimum must be refused outright,
# rather than defaulting to a floor of zero.
rc=0
output=""
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T4_DIR}" \
    --expected-failures "${T4_EF}" \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 ]]; then
    fail "oracle-refuses-an-empty-candidate-set (4b: undeclared corpus refused)" \
        "expected oracle exit 1, got ${rc}; output: ${output}"
fi
if ! echo "${output}" | grep -q -- "--min-candidates"; then
    fail "oracle-refuses-an-empty-candidate-set (4b: undeclared corpus refused)" \
        "expected '--min-candidates' in output; got: ${output}"
fi

pass "oracle-refuses-an-empty-candidate-set"

# ---------------------------------------------------------------------------
echo "--- Test 5: oracle-ratchets-positive-rejections ---"
T5_VERTICAL="${TMPDIR_BASE}/t5_vertical"
T5_REGRESSIONS="${TMPDIR_BASE}/t5_regressions"
T5_EF="${TMPDIR_BASE}/t5_expected_failures.txt"
T5_ER="${TMPDIR_BASE}/t5_expected_rejects.txt"
T5_REPORT="${TMPDIR_BASE}/t5_report.json"
mkdir -p "${T5_VERTICAL}" "${T5_REGRESSIONS}"
printf '# empty\n' >"${T5_EF}"
printf '# empty\n' >"${T5_ER}"
printf 'fn main() { let value: i64 = "not-an-integer"; }\n' \
    >"${T5_VERTICAL}/reject_fixture.hew"

# 5a: A positive fixture rejected by the frontend and absent from the exact
# rejection ratchet must fail closed.
rc=0
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T5_REGRESSIONS}" \
    --expected-failures "${T5_EF}" \
    --expected-rejects "${T5_ER}" \
    --vertical-slice-dir "${T5_VERTICAL}" \
    --min-candidates 1 \
    --report "${T5_REPORT}" \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 ]]; then
    fail "oracle-ratchets-positive-rejections (5a: unexpected reject fails)" \
        "expected oracle exit 1, got ${rc}; output: ${output}"
fi
if [[ "${output}" != *"UNEXPECTED REJECTIONS (not in expected-reject.txt)"* ]]; then
    fail "oracle-ratchets-positive-rejections (5a: unexpected reject fails)" \
        "unexpected-rejection list was not printed; output: ${output}"
fi
if ! python3 -c "
import json, sys
r = json.load(open('${T5_REPORT}'))
paths = [entry['path'] for entry in r['unexpected_rejects']]
sys.exit(0 if paths == ['tests/vertical-slice/accept/reject_fixture.hew'] else 1)
"; then
    fail "oracle-ratchets-positive-rejections (5a: unexpected reject fails)" \
        "report did not identify the exact unexpected rejection; output: ${output}"
fi

# 5b: Listing that exact fixture identity tolerates the known rejection.
printf 'tests/vertical-slice/accept/reject_fixture.hew\n' >"${T5_ER}"
rc=0
run_counterfactual "t5b-listed-reject" python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T5_REGRESSIONS}" \
    --expected-failures "${T5_EF}" \
    --expected-rejects "${T5_ER}" \
    --vertical-slice-dir "${T5_VERTICAL}" \
    --min-candidates 1 ||
    rc=$?
if [[ "${rc}" -ne 0 ]]; then
    fail "oracle-ratchets-positive-rejections (5b: listed reject passes)" \
        "expected oracle exit 0, got ${rc}"
fi

# 5c: make the listed fixture clean. Normal PR mode reports the stale entry
# without blocking; scheduled strict accounting rejects it.
printf '// EXPECT: 7\nfn main() { println(7); }\n' \
    >"${T5_VERTICAL}/reject_fixture.hew"
rc=0
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T5_REGRESSIONS}" \
    --expected-failures "${T5_EF}" \
    --expected-rejects "${T5_ER}" \
    --vertical-slice-dir "${T5_VERTICAL}" \
    --min-candidates 1 \
    --report "${T5_REPORT}" \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 0 ]]; then
    fail "oracle-ratchets-positive-rejections (5c: recovery reports in PR mode)" \
        "expected oracle exit 0, got ${rc}; output: ${output}"
fi
if [[ "${output}" != *"UNEXPECTED PASSES (listed in expected-reject.txt but passed)"* ]]; then
    fail "oracle-ratchets-positive-rejections (5c: recovery reports in PR mode)" \
        "unexpected-pass list was not printed; output: ${output}"
fi
if [[ "${output}" != *"NOW-PASSES: tests/vertical-slice/accept/reject_fixture.hew"* ]]; then
    fail "oracle-ratchets-positive-rejections (5c: recovery reports in PR mode)" \
        "exact recovery identity was not printed; output: ${output}"
fi
if ! python3 -c "
import json, sys
r = json.load(open('${T5_REPORT}'))
paths = [entry['path'] for entry in r['unexpected_reject_passes']]
sys.exit(0 if paths == ['tests/vertical-slice/accept/reject_fixture.hew'] else 1)
"; then
    fail "oracle-ratchets-positive-rejections (5c: recovery reports in PR mode)" \
        "report did not identify the exact unexpected pass; output: ${output}"
fi

rc=0
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T5_REGRESSIONS}" \
    --expected-failures "${T5_EF}" \
    --expected-rejects "${T5_ER}" \
    --vertical-slice-dir "${T5_VERTICAL}" \
    --min-candidates 1 \
    --strict-recoveries \
    --report "${T5_REPORT}" \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 ]]; then
    fail "oracle-ratchets-positive-rejections (5c: strict recovery fails)" \
        "expected oracle exit 1, got ${rc}; output: ${output}"
fi
if [[ "${output}" != *"NOW-PASSES: tests/vertical-slice/accept/reject_fixture.hew"* ]]; then
    fail "oracle-ratchets-positive-rejections (5c: strict recovery fails)" \
        "strict output did not preserve the exact recovery identity; output: ${output}"
fi

pass "oracle-ratchets-positive-rejections"

# ---------------------------------------------------------------------------
echo ""
echo "oracle-selftest: all 5 tests PASS"
