#!/usr/bin/env bash
# Execute every strict expected-failure ledger family, retaining evidence from
# later families even if an earlier ledger reports stale accounting.

set -uo pipefail

if [[ "${RATCHET_STRICT_RECOVERIES:-}" != "1" ]]; then
    echo "error: ratchet accounting requires RATCHET_STRICT_RECOVERIES=1" >&2
    exit 2
fi

make_command="${RATCHET_ACCOUNTING_MAKE:-${MAKE:-make}}"
families=(
    test
    test-hew-ratchet
    test-core-matrix
    test-stdlib-ratchet
    test-doc-examples
    fuzz-oracle
    hew-check-all
)

failed=0
for family in "${families[@]}"; do
    echo "==> Ratchet accounting: ${family}"
    if "${make_command}" "${family}"; then
        echo "==> Ratchet accounting: ${family}: PASSED"
    else
        status=$?
        echo "==> Ratchet accounting: ${family}: FAILED (exit ${status})" >&2
        failed=1
    fi
done

if ((failed)); then
    echo "==> Ratchet accounting: FAILED (one or more families failed)" >&2
    exit 1
fi
echo "==> Ratchet accounting: PASSED"
