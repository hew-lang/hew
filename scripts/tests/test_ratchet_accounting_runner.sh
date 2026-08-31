#!/usr/bin/env bash
# Counterfactuals for scripts/ratchet-accounting.sh's keep-going contract.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RUNNER="${ROOT}/scripts/ratchet-accounting.sh"
TMP_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-ratchet-accounting-test.XXXXXX")"
trap 'rm -rf "${TMP_ROOT}"' EXIT

FAKE_MAKE="${TMP_ROOT}/fake-make"
LOG="${TMP_ROOT}/families.log"
cat >"${FAKE_MAKE}" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$1" >>"${RATCHET_ACCOUNTING_TEST_LOG}"
if [[ "$1" == "test" || "$1" == "test-doc-examples" ]]; then
    exit 1
fi
EOF
chmod +x "${FAKE_MAKE}"

status=0
output="$(RATCHET_STRICT_RECOVERIES=1 \
    RATCHET_ACCOUNTING_MAKE="${FAKE_MAKE}" \
    RATCHET_ACCOUNTING_TEST_LOG="${LOG}" \
    "${RUNNER}" 2>&1)" || status=$?
if [[ "${status}" -ne 1 ]]; then
    echo "FAIL: runner must fail only after executing all families (got ${status})" >&2
    exit 1
fi

expected=$'test\ntest-hew-ratchet\ntest-core-matrix\ntest-stdlib-ratchet\ntest-doc-examples\nfuzz-oracle\nhew-check-all'
actual="$(cat "${LOG}")"
if [[ "${actual}" != "${expected}" ]]; then
    echo "FAIL: runner skipped or reordered a family" >&2
    printf 'expected:\n%s\nactual:\n%s\n' "${expected}" "${actual}" >&2
    exit 1
fi
if [[ "${output}" != *"test-doc-examples: FAILED"* || "${output}" != *"hew-check-all: PASSED"* ]]; then
    echo "FAIL: runner did not retain later-family evidence" >&2
    exit 1
fi

echo "PASS: accounting runner executes and reports every family after failures"
