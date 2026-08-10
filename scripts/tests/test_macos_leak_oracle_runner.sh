#!/usr/bin/env bash
# Teeth for scripts/macos-leak-oracle.sh's derived inventory authority.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RUNNER="${ROOT}/scripts/macos-leak-oracle.sh"
DISCOVERY="${ROOT}/scripts/macos-leak-source-inventory.py"
work_dir="$(mktemp -d "${TMPDIR:-/tmp}/hew-leak-runner-selftest.XXXXXX")"
cleanup_work_dir() { rm -rf "${work_dir}"; }
trap cleanup_work_dir EXIT

expect_red() {
    local label="$1" expected="$2" inventory="$3"
    local output rc=0
    output="$("${RUNNER}" --check-inventory-file "${inventory}" 2>&1)" || rc=$?
    if [[ "${rc}" -eq 0 ]]; then
        echo "FAIL ${label}: counterfactual unexpectedly passed" >&2
        exit 1
    fi
    if [[ "${output}" != *"${expected}"* ]]; then
        echo "FAIL ${label}: expected diagnostic containing '${expected}', got:" >&2
        printf '%s\n' "${output}" >&2
        exit 1
    fi
    echo "PASS counterfactual RED ${label}: ${expected}"
}

live="${work_dir}/live.txt"
"${RUNNER}" --list-inventory > "${live}"
"${RUNNER}" --check-inventory-file "${live}"
EXPECTED_TESTS="$(wc -l < "${live}" | tr -d '[:space:]')"
echo "PASS live nextest inventory is nonempty and source-derived"

good="${work_dir}/good.txt"
cp "${live}" "${good}"
"${RUNNER}" --check-inventory-file "${good}"

duplicate="${work_dir}/duplicate.txt"
head -n "$(( EXPECTED_TESTS - 1 ))" "${good}" > "${duplicate}"
head -n 1 "${good}" >> "${duplicate}"
expect_red "duplicate verdict" "duplicate nextest inventory verdict" "${duplicate}"

empty="${work_dir}/empty.txt"
: > "${empty}"
expect_red "empty corpus" "inventory is empty" "${empty}"

malformed="${work_dir}/malformed.txt"
cp "${good}" "${malformed}"
printf '%s\n' "hew-cli::synthetic_001_oracle unexpected third-field" >> "${malformed}"
expect_red "malformed inventory" "malformed nextest inventory" "${malformed}"

unexpected_binary="${work_dir}/unexpected-binary.txt"
cp "${good}" "${unexpected_binary}"
printf '%s\n' 'hew-cli::unexpected invented_verdict' >> "${unexpected_binary}"
expect_red "unexpected binary" "no source-derived allocator authority" "${unexpected_binary}"

missing_ffi="${work_dir}/missing-ffi.txt"
grep -Fv 'hew-cli::ffi_link_e2e ' "${good}" > "${missing_ffi}"
expect_red "missing ffi authority" "required ffi_link_e2e leak verdict is absent" "${missing_ffi}"

synthetic_sources="${work_dir}/synthetic-sources"
mkdir -p "${synthetic_sources}"
cat > "${synthetic_sources}/unlisted_probe.rs" <<'EOF'
#[test]
fn a_real_allocator_probe() {
    let _leaks = measure_leaks_exact(&binary);
}
EOF
cat > "${synthetic_sources}/comment_and_import_only.rs" <<'EOF'
use support::leak_slope::{assert_frame_slope_below_tolerance, run_under_malloc_scribble};
// run_under_malloc_scribble(&not_an_execution);
const DOCUMENTATION: &str = "measure_leaks_with_args is not a call";
fn measure_leaks_exact(_binary: &str) {}
EOF
synthetic_discovery="$(python3 "${DISCOVERY}" --tests-dir "${synthetic_sources}")"
if [[ "${synthetic_discovery}" != "unlisted_probe" ]]; then
    echo "FAIL source discovery admitted comments/imports or missed a real call: ${synthetic_discovery}" >&2
    exit 1
fi
echo "PASS source discovery ignores definitions/comments/imports/literals and finds actual calls"
source_missing_output="${work_dir}/source-missing.out"
source_missing_rc=0
HEW_LEAK_SOURCE_TESTS_DIR="${synthetic_sources}" \
    "${RUNNER}" --check-inventory-file "${good}" >"${source_missing_output}" 2>&1 \
    || source_missing_rc=$?
if [[ "${source_missing_rc}" -eq 0 ]]; then
    echo "FAIL source-discovered non-oracle binary: counterfactual unexpectedly passed" >&2
    exit 1
fi
if ! grep -Fq "source-discovered allocator probe binary is absent" "${source_missing_output}"; then
    echo "FAIL source-discovered non-oracle binary: missing diagnostic" >&2
    cat "${source_missing_output}" >&2
    exit 1
fi
echo "PASS counterfactual RED source-discovered non-oracle binary missing from inventory"

echo "macOS leak-oracle runner selftest: PASS"
