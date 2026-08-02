#!/usr/bin/env bash
# Teeth for scripts/macos-leak-oracle.sh's exact inventory authority.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RUNNER="${ROOT}/scripts/macos-leak-oracle.sh"
DISCOVERY="${ROOT}/scripts/macos-leak-source-inventory.py"
# shellcheck source=scripts/lib/corpus-floor.sh
# shellcheck disable=SC1091
source "${ROOT}/scripts/lib/corpus-floor.sh"

exact_floor() {
    local key="$1" row mode floor
    row="$(corpus_floor_row "${key}")" || {
        echo "FAIL no corpus floor registered for ${key}" >&2
        return 1
    }
    IFS=$'\t' read -r _ mode floor _ _ <<< "${row}"
    if [[ "${mode}" != "exact" ]]; then
        echo "FAIL ${key} must use an exact corpus floor, found ${mode}" >&2
        return 1
    fi
    printf '%s\n' "${floor}"
}

EXPECTED_BINARIES="$(exact_floor macos-leak-oracle-binaries)"
EXPECTED_TESTS="$(exact_floor macos-leak-oracle-tests)"
SOURCE_BINARIES="$(
    python3 "${DISCOVERY}" --tests-dir "${ROOT}/hew-cli/tests" \
        | grep -Fvx 'ffi_link_e2e'
)"

work_dir="$(mktemp -d "${TMPDIR:-/tmp}/hew-leak-runner-selftest.XXXXXX")"
cleanup_work_dir() { rm -rf "${work_dir}"; }
trap cleanup_work_dir EXIT

write_inventory() {
    local output="$1" binary_count="$2" test_count="$3" include_ffi="$4"
    : > "${output}"
    local synthetic_count="${binary_count}"
    local written_tests=0
    if [[ "${include_ffi}" == "yes" ]]; then
        printf '%s\n' \
            "hew-cli::ffi_link_e2e ffi_borrow_boundary_has_no_drop_or_leak_slope" \
            >> "${output}"
        synthetic_count=$(( synthetic_count - 1 ))
        written_tests=1
    fi
    local source_binary
    while IFS= read -r source_binary; do
        [[ -n "${source_binary}" ]] || continue
        printf 'hew-cli::%s source_discovered_allocator_verdict\n' "${source_binary}" >> "${output}"
        synthetic_count=$(( synthetic_count - 1 ))
        written_tests=$(( written_tests + 1 ))
    done <<< "${SOURCE_BINARIES}"
    if (( synthetic_count < 0 )); then
        echo "FAIL source-derived allocator corpus exceeds binary floor" >&2
        exit 1
    fi
    local i
    for (( i = 1; i <= synthetic_count; i++ )); do
        printf 'hew-cli::synthetic_%03d_oracle verdict_%03d\n' "${i}" "${i}" >> "${output}"
        written_tests=$(( written_tests + 1 ))
    done
    for (( i = written_tests + 1; i <= test_count; i++ )); do
        printf 'hew-cli::synthetic_001_oracle extra_verdict_%03d\n' "${i}" >> "${output}"
    done
}

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

good="${work_dir}/good.txt"
write_inventory "${good}" "${EXPECTED_BINARIES}" "${EXPECTED_TESTS}" yes
"${RUNNER}" --check-inventory-file "${good}" >/dev/null
echo "PASS accepted exact ${EXPECTED_BINARIES}-binary/${EXPECTED_TESTS}-test inventory with ffi_link_e2e"

live="${work_dir}/live.txt"
"${RUNNER}" --list-inventory > "${live}"
"${RUNNER}" --check-inventory-file "${live}" >/dev/null
echo "PASS live nextest inventory matches the exact binary/test authorities"

shrunken_binaries="${work_dir}/shrunken-binaries.txt"
write_inventory \
    "${shrunken_binaries}" \
    "$(( EXPECTED_BINARIES - 1 ))" \
    "${EXPECTED_TESTS}" \
    yes
expect_red "binary corpus shrink" "macos-leak-oracle-binaries" "${shrunken_binaries}"

shrunken_tests="${work_dir}/shrunken-tests.txt"
write_inventory \
    "${shrunken_tests}" \
    "${EXPECTED_BINARIES}" \
    "$(( EXPECTED_TESTS - 1 ))" \
    yes
expect_red "test corpus shrink" "macos-leak-oracle-tests" "${shrunken_tests}"

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
sed 's/hew-cli::synthetic_001_oracle/hew-cli::unexpected/' "${good}" > "${unexpected_binary}"
expect_red "unexpected binary" "no source-derived allocator authority" "${unexpected_binary}"

missing_ffi="${work_dir}/missing-ffi.txt"
write_inventory "${missing_ffi}" "${EXPECTED_BINARIES}" "${EXPECTED_TESTS}" no
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
