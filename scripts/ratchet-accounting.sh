#!/usr/bin/env bash
# Execute every expected-failure ledger family on its schedule, retaining
# evidence from later families even if an earlier one fails. A recovered row
# is reported, never a blocking failure (D555 amendment), so this script's
# only job is to run every ratcheted suite and surface real regressions —
# `xtask ratchet check` already refuses an unlisted failure on every call.

set -uo pipefail

make_command="${RATCHET_ACCOUNTING_MAKE:-${MAKE:-make}}"
families=(
    test
    test-hew-ratchet
    core-acceptance
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
