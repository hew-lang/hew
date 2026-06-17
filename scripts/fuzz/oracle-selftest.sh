#!/usr/bin/env bash
# oracle-selftest.sh — Three independently-failable validation tests for
# scripts/fuzz/run-oracle.py.
#
# Test 1 (oracle-flags-a-known-crash):
#   A checker-valid program that crashes at runtime (runtime-abort) must make
#   the oracle exit 1 and report a FAIL verdict.  If the harness wrongly
#   classifies the crash as clean, this test goes red.
#
# Test 2 (oracle-honours-expected-failure):
#   The same crashing program, listed in expected-failures.txt, must be
#   tolerated (exit 0).  Then, with the file removed while still listed, the
#   oracle must exit 1 with "expected-failing entry not found".
#
# Test 3 (oracle-passes-clean-program):
#   A program with // EXPECT: 7 must exit 0 with verdict clean.  Mutating
#   EXPECT to 8 must exit 1 with verdict wrong-output.
#
# All three must pass for the self-test to succeed.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW="${ROOT}/target/debug/hew"
ORACLE="${ROOT}/scripts/fuzz/run-oracle.py"
TMPDIR_BASE="$(mktemp -d /tmp/hew-oracle-selftest.XXXXXX)"
trap 'rm -rf "${TMPDIR_BASE}"' EXIT

# Empty directory passed as --vertical-slice-dir so the self-tests only run
# the temp regressions corpus, not all 245+ vertical-slice/accept fixtures.
# The real oracle run (make fuzz-oracle) covers those; self-tests prove the
# harness classification logic only.
EMPTY_DIR="${TMPDIR_BASE}/empty"
mkdir -p "${EMPTY_DIR}"

pass() { echo "PASS $1"; }
fail() { echo "FAIL $1: $2" >&2; exit 1; }

# ---------------------------------------------------------------------------
# Shared crash fixture (the E4 Vec<record> slice repro from the plan).
# This is a checker-valid program that aborts at runtime with a PANIC.
# ---------------------------------------------------------------------------
CRASH_SOURCE='record Person { name: string }

fn main() {
  let v: Vec<Person> = [Person { name: "a" }];
  let s = v[0..1];
  println(s.len());
}
'

# ---------------------------------------------------------------------------
# Test 1: oracle-flags-a-known-crash
# ---------------------------------------------------------------------------
T1_DIR="${TMPDIR_BASE}/t1_regressions"
mkdir -p "${T1_DIR}"
printf '%s' "${CRASH_SOURCE}" > "${T1_DIR}/crash_fixture.hew"

# Create a matching expected-failures file that does NOT list crash_fixture.hew.
T1_EF="${TMPDIR_BASE}/t1_expected_failures.txt"
printf '# empty\n' > "${T1_EF}"

echo "--- Test 1: oracle-flags-a-known-crash ---"
# Oracle must exit 1 (unexpected failure — crash not listed in expected-failures).
rc=0
python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T1_DIR}" \
    --expected-failures "${T1_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    2>&1 || rc=$?
if [[ "${rc}" -ne 1 ]]; then
    fail "oracle-flags-a-known-crash" "expected oracle exit 1, got ${rc}"
fi

# Capture report to verify the verdict token.
T1_REPORT="${TMPDIR_BASE}/t1_report.json"
rc=0
python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T1_DIR}" \
    --expected-failures "${T1_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    --report "${T1_REPORT}" \
    2>&1 || rc=$?

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
printf '%s' "${CRASH_SOURCE}" > "${T2_DIR}/crash_fixture.hew"

# 2a: crash_fixture.hew IS listed — oracle must exit 0 (known gap tolerated).
T2_EF="${TMPDIR_BASE}/t2_expected_failures.txt"
printf 'crash_fixture.hew  # known crash; issue: #test\n' > "${T2_EF}"

rc=0
python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T2_DIR}" \
    --expected-failures "${T2_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    2>&1 || rc=$?
if [[ "${rc}" -ne 0 ]]; then
    fail "oracle-honours-expected-failure (2a: listed crash tolerated)" \
         "expected oracle exit 0, got ${rc}"
fi

# 2b: Remove crash_fixture.hew from disk while leaving it in expected-failures.
# Oracle must exit 1 with "expected-failing entry not found".
rm "${T2_DIR}/crash_fixture.hew"

rc=0
output=""
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T2_DIR}" \
    --expected-failures "${T2_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    2>&1)" || rc=$?
if [[ "${rc}" -ne 1 ]]; then
    fail "oracle-honours-expected-failure (2b: missing-but-listed fails gate)" \
         "expected oracle exit 1, got ${rc}; output: ${output}"
fi
if ! echo "${output}" | grep -q "expected-failing entry not found"; then
    fail "oracle-honours-expected-failure (2b: missing-but-listed fails gate)" \
         "expected 'expected-failing entry not found' in output; got: ${output}"
fi

pass "oracle-honours-expected-failure"

# ---------------------------------------------------------------------------
# Test 3: oracle-passes-clean-program (no false positive + exact stdout check)
# ---------------------------------------------------------------------------
echo "--- Test 3: oracle-passes-clean-program ---"

T3_DIR="${TMPDIR_BASE}/t3_regressions"
mkdir -p "${T3_DIR}"
T3_EF="${TMPDIR_BASE}/t3_expected_failures.txt"
printf '# empty\n' > "${T3_EF}"

# 3a: clean program with matching EXPECT — must exit 0, verdict clean.
printf '// EXPECT: 7\nfn main() { println(7); }\n' > "${T3_DIR}/clean_fixture.hew"

rc=0
python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T3_DIR}" \
    --expected-failures "${T3_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
    2>&1 || rc=$?
if [[ "${rc}" -ne 0 ]]; then
    fail "oracle-passes-clean-program (3a: clean program passes)" \
         "expected oracle exit 0, got ${rc}"
fi

# 3b: mutate EXPECT to wrong value — must exit 1, verdict wrong-output.
printf '// EXPECT: 8\nfn main() { println(7); }\n' > "${T3_DIR}/clean_fixture.hew"

rc=0
output=""
output="$(python3 "${ORACLE}" \
    --hew "${HEW}" \
    --repo-root "${ROOT}" \
    --regressions-dir "${T3_DIR}" \
    --expected-failures "${T3_EF}" \
    --vertical-slice-dir "${EMPTY_DIR}" \
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
echo ""
echo "oracle-selftest: all 3 tests PASS"
