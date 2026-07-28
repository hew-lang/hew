#!/usr/bin/env bash
# Teeth for scripts/macos-leak-oracle.sh's exact inventory authority.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RUNNER="${ROOT}/scripts/macos-leak-oracle.sh"
EXPECTED_BINARIES=90

work_dir="$(mktemp -d "${TMPDIR:-/tmp}/hew-leak-runner-selftest.XXXXXX")"
cleanup_work_dir() { rm -rf "${work_dir}"; }
trap cleanup_work_dir EXIT

write_inventory() {
    local output="$1" binary_count="$2" include_ffi="$3"
    : > "${output}"
    local synthetic_count="${binary_count}"
    if [[ "${include_ffi}" == "yes" ]]; then
        printf '%s\n' \
            "hew-cli::ffi_link_e2e ffi_borrow_boundary_has_no_drop_or_leak_slope" \
            >> "${output}"
        synthetic_count=$(( synthetic_count - 1 ))
    fi
    local i
    for (( i = 1; i <= synthetic_count; i++ )); do
        printf 'hew-cli::synthetic_%03d_oracle verdict_%03d\n' "${i}" "${i}" >> "${output}"
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
write_inventory "${good}" "${EXPECTED_BINARIES}" yes
"${RUNNER}" --check-inventory-file "${good}" >/dev/null
echo "PASS accepted exact ${EXPECTED_BINARIES}-binary inventory with ffi_link_e2e"

shrunken="${work_dir}/shrunken.txt"
write_inventory "${shrunken}" "$(( EXPECTED_BINARIES - 1 ))" yes
expect_red "corpus shrink" "CORPUS FLOOR" "${shrunken}"

empty="${work_dir}/empty.txt"
: > "${empty}"
expect_red "empty corpus" "inventory is empty" "${empty}"

missing_ffi="${work_dir}/missing-ffi.txt"
write_inventory "${missing_ffi}" "${EXPECTED_BINARIES}" no
expect_red "missing ffi authority" "required ffi_link_e2e leak verdict is absent" "${missing_ffi}"

echo "macOS leak-oracle runner selftest: PASS"
