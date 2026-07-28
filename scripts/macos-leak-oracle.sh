#!/usr/bin/env bash
# macos-leak-oracle.sh — canonical, ratcheted Darwin memory-oracle corpus.
#
# Runs every hew-cli integration-test binary whose name contains `oracle` plus
# the real `ffi_link_e2e::ffi_borrow_boundary_has_no_drop_or_leak_slope`
# allocator probe. The pre-run inventory is exact-floored so a renamed or
# deleted oracle cannot silently turn a smaller corpus green.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/corpus-floor.sh
# shellcheck disable=SC1091
source "${ROOT}/scripts/lib/corpus-floor.sh"

FILTER='binary(~oracle) + test(ffi_borrow_boundary_has_no_drop_or_leak_slope)'
FFI_VERDICT='hew-cli::ffi_link_e2e ffi_borrow_boundary_has_no_drop_or_leak_slope'

verify_inventory() {
    local inventory="$1"

    if [[ ! -s "${inventory}" ]]; then
        echo "macos-leak-oracle: inventory is empty; refusing a vacuous memory verdict" >&2
        return 1
    fi

    local malformed_line
    malformed_line="$(awk 'NF != 2 { print NR; exit }' "${inventory}")"
    if [[ -n "${malformed_line}" ]]; then
        echo "macos-leak-oracle: malformed nextest inventory at line ${malformed_line}" >&2
        return 1
    fi

    if ! grep -Fqx -- "${FFI_VERDICT}" "${inventory}"; then
        echo "macos-leak-oracle: required ffi_link_e2e leak verdict is absent:" >&2
        echo "  ${FFI_VERDICT}" >&2
        return 1
    fi

    local unexpected_binaries
    unexpected_binaries="$(
        awk '$1 != "hew-cli::ffi_link_e2e" && $1 !~ /oracle/ { print $1 }' "${inventory}" \
            | LC_ALL=C sort -u
    )"
    if [[ -n "${unexpected_binaries}" ]]; then
        echo "macos-leak-oracle: filter admitted non-oracle binaries besides ffi_link_e2e:" >&2
        printf '  %s\n' "${unexpected_binaries}" >&2
        return 1
    fi

    local binary_count test_count
    binary_count="$(
        awk '{ print $1 }' "${inventory}" | LC_ALL=C sort -u | wc -l | tr -d '[:space:]'
    )"
    test_count="$(wc -l < "${inventory}" | tr -d '[:space:]')"
    corpus_floor_assert \
        "macos-leak-oracle-binaries" \
        "${binary_count}" \
        "${test_count} selected test verdicts, including ffi_link_e2e"
    echo "macos-leak-oracle: inventory accepted (${binary_count} binaries, ${test_count} tests)"
}

if [[ "${1:-}" == "--check-inventory-file" ]]; then
    if [[ "$#" -ne 2 ]]; then
        echo "usage: scripts/macos-leak-oracle.sh --check-inventory-file <nextest-oneline-file>" >&2
        exit 2
    fi
    verify_inventory "$2"
    exit
fi
if [[ "$#" -ne 0 ]]; then
    echo "usage: scripts/macos-leak-oracle.sh" >&2
    exit 2
fi

if [[ "$(uname -s)" != "Darwin" ]]; then
    echo "macos-leak-oracle: Darwin is required; this gate may not report a non-macOS skip as green" >&2
    exit 1
fi
if ! command -v leaks >/dev/null 2>&1; then
    echo "macos-leak-oracle: leaks(1) is missing from PATH; the memory corpus cannot measure" >&2
    exit 1
fi

inventory_dir="$(mktemp -d "${TMPDIR:-/tmp}/hew-macos-leak-oracle.XXXXXX")"
cleanup_inventory() { rm -rf "${inventory_dir}"; }
trap cleanup_inventory EXIT
inventory="${inventory_dir}/nextest-oneline.txt"

echo "==> Enumerating the macOS leak-oracle corpus"
(
    cd "${ROOT}"
    cargo nextest list \
        -p hew-cli \
        --profile ci \
        --run-ignored all \
        --no-pager \
        --color never \
        --cargo-quiet \
        -E "${FILTER}" \
        --message-format oneline \
        --list-type full
) > "${inventory}"
verify_inventory "${inventory}"

echo "==> Running the complete macOS leak-oracle corpus"
cd "${ROOT}"
cargo nextest run \
    -p hew-cli \
    --profile ci \
    --run-ignored all \
    --no-fail-fast \
    --color never \
    -E "${FILTER}"
