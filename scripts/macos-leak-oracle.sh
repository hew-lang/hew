#!/usr/bin/env bash
# macos-leak-oracle.sh — canonical, ratcheted Darwin memory-oracle corpus.
#
# Runs every hew-cli integration-test binary whose name contains `oracle`, every
# binary structurally discovered to call the allocator/leak execution helpers,
# and the exact `ffi_link_e2e` allocator probe. The source-derived edge prevents
# a real probe in a non-`oracle` file from silently falling outside the runner.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/corpus-floor.sh
# shellcheck disable=SC1091
source "${ROOT}/scripts/lib/corpus-floor.sh"

SOURCE_DISCOVERY="${ROOT}/scripts/macos-leak-source-inventory.py"
SOURCE_TESTS_DIR="${HEW_LEAK_SOURCE_TESTS_DIR:-${ROOT}/hew-cli/tests}"
# ffi_link_e2e remains an exact single-test authority below; selecting its
# whole binary would dilute that verdict with unrelated FFI tests.
SOURCE_BINARIES="$(
    python3 "${SOURCE_DISCOVERY}" --tests-dir "${SOURCE_TESTS_DIR}" \
        | grep -Fvx 'ffi_link_e2e'
)"
FILTER='binary(~oracle) + test(ffi_borrow_boundary_has_no_drop_or_leak_slope)'
while IFS= read -r binary; do
    [[ -n "${binary}" ]] || continue
    FILTER="${FILTER} + binary(${binary})"
done <<< "${SOURCE_BINARIES}"
FFI_VERDICT='hew-cli::ffi_link_e2e ffi_borrow_boundary_has_no_drop_or_leak_slope'

enumerate_inventory() {
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
    )
}

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

    local duplicate_line
    duplicate_line="$(awk 'seen[$0]++ { print NR; exit }' "${inventory}")"
    if [[ -n "${duplicate_line}" ]]; then
        echo "macos-leak-oracle: duplicate nextest inventory verdict at line ${duplicate_line}" >&2
        return 1
    fi

    if ! grep -Fqx -- "${FFI_VERDICT}" "${inventory}"; then
        echo "macos-leak-oracle: required ffi_link_e2e leak verdict is absent:" >&2
        echo "  ${FFI_VERDICT}" >&2
        return 1
    fi

    local source_binary
    while IFS= read -r source_binary; do
        [[ -n "${source_binary}" ]] || continue
        if ! grep -Fq -- "hew-cli::${source_binary} " "${inventory}"; then
            echo "macos-leak-oracle: source-discovered allocator probe binary is absent:" >&2
            echo "  hew-cli::${source_binary}" >&2
            return 1
        fi
    done <<< "${SOURCE_BINARIES}"

    local unexpected_binaries="" inventory_binary short_binary
    while IFS= read -r inventory_binary; do
        [[ "${inventory_binary}" == "hew-cli::ffi_link_e2e" ]] && continue
        [[ "${inventory_binary}" == *oracle* ]] && continue
        short_binary="${inventory_binary#hew-cli::}"
        if ! grep -Fqx -- "${short_binary}" <<< "${SOURCE_BINARIES}"; then
            unexpected_binaries="${unexpected_binaries}${inventory_binary}"$'\n'
        fi
    done < <(awk '{ print $1 }' "${inventory}" | LC_ALL=C sort -u)
    if [[ -n "${unexpected_binaries}" ]]; then
        echo "macos-leak-oracle: filter admitted binaries with no source-derived allocator authority:" >&2
        while IFS= read -r inventory_binary; do
            [[ -n "${inventory_binary}" ]] && printf '  %s\n' "${inventory_binary}" >&2
        done <<< "${unexpected_binaries}"
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
    corpus_floor_assert \
        "macos-leak-oracle-tests" \
        "${test_count}" \
        "${binary_count} selected binaries, including ffi_link_e2e"
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
if [[ "${1:-}" == "--list-inventory" ]]; then
    if [[ "$#" -ne 1 ]]; then
        echo "usage: scripts/macos-leak-oracle.sh --list-inventory" >&2
        exit 2
    fi
    enumerate_inventory
    exit
fi
if [[ "$#" -ne 0 ]]; then
    echo "usage: scripts/macos-leak-oracle.sh [--list-inventory | --check-inventory-file <nextest-oneline-file>]" >&2
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
enumerate_inventory > "${inventory}"
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
