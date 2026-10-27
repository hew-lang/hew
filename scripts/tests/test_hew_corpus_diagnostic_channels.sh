#!/usr/bin/env bash
# Counterfactual coverage for structured compiler diagnostic channel exits.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HARNESS="${REPO_ROOT}/scripts/corpus-ratchet.sh"
TMP_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-corpus-diagnostic-channel-test.XXXXXX")"
trap 'rm -rf "${TMP_ROOT}"' EXIT

BIN_DIR="${TMP_ROOT}/bin"
FAKE_HEW="${BIN_DIR}/hew"
EXPECTED="${TMP_ROOT}/expected-failures.txt"
TEST_PATH="tests/ratchet-diagnostic-channel.hew"
mkdir -p "${BIN_DIR}"
printf '%s\n' "${TEST_PATH}" >"${EXPECTED}"

cat >"${BIN_DIR}/git" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

[[ "$1" == "ls-files" ]] || exit 64
printf '%s\n' "${HEW_CORPUS_TEST_PATH:?}"
EOF
chmod +x "${BIN_DIR}/git"

cat >"${FAKE_HEW}" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

[[ "$1" == "check" ]] || exit 64
case "${HEW_CORPUS_TEST_MODE:?}" in
mixed)
    case "${*: -1}" in
    */first.hew) exit 0 ;;
    */second.hew) echo "error[E_TEST_USER]: user diagnostic" >&2; exit 1 ;;
    */third.hew) echo "raw process failure" >&2; exit 3 ;;
    *) exit 64 ;;
    esac
    ;;
user)
    echo "error[E_TEST_USER]: user diagnostic" >&2
    exit 1
    ;;
limitation)
    echo "error[E_TEST_LIMITATION]: limitation diagnostic" >&2
    exit 3
    ;;
internal)
    echo "error[E_TEST_INTERNAL]: internal diagnostic" >&2
    exit 4
    ;;
nondiagnostic-3)
    echo "raw process failure" >&2
    exit 3
    ;;
nondiagnostic-4)
    echo "raw process failure" >&2
    exit 4
    ;;
signal)
    kill -TERM "$$"
    ;;
*)
    exit 64
    ;;
esac
EOF
chmod +x "${FAKE_HEW}"

OUTPUT=""
STATUS=0
run_case() {
    local mode="$1"

    STATUS=0
    OUTPUT="$(PATH="${BIN_DIR}:${PATH}" \
        HEW_CORPUS_TEST_MODE="${mode}" \
        HEW_CORPUS_TEST_PATH="${TEST_PATH}" \
        "${HARNESS}" hew-corpus \
        --expected-failures "${EXPECTED}" \
        --hew-bin "${FAKE_HEW}" 2>&1)" || STATUS=$?
}

assert_status() {
    local expected="$1" label="$2"
    if [[ "${STATUS}" -ne "${expected}" ]]; then
        echo "FAIL: ${label} (expected exit ${expected}, got ${STATUS})" >&2
        printf '%s\n' "${OUTPUT}" >&2
        exit 1
    fi
}

run_case user
assert_status 0 "User diagnostic exit is accepted"

run_case limitation
assert_status 0 "Limitation diagnostic exit is accepted"

run_case internal
assert_status 0 "Internal diagnostic exit is accepted"

run_case nondiagnostic-3
assert_status 1 "non-diagnostic exit 3 is rejected"

run_case nondiagnostic-4
assert_status 1 "non-diagnostic exit 4 is rejected"

run_case signal
assert_status 1 "signal is rejected"

TEST_PATH=$'tests/first.hew\ntests/second.hew\ntests/third.hew'
printf '%s\n' 'tests/second.hew' 'tests/third.hew' >"${EXPECTED}"
HEW_CORPUS_JOBS=1 run_case mixed
assert_status 1 "serial sweep rejects the non-diagnostic failure"
SERIAL_OUTPUT="${OUTPUT}"
HEW_CORPUS_JOBS=4 run_case mixed
assert_status 1 "parallel sweep rejects the non-diagnostic failure"
if [[ "${OUTPUT}" != "${SERIAL_OUTPUT}" || "${OUTPUT}" != *"Files checked:          3"* ]]; then
    echo "FAIL: parallel sweep must preserve all serial verdicts and diagnostics" >&2
    printf '%s\n' "${OUTPUT}" >&2
    exit 1
fi
HEW_CORPUS_JOBS=0 run_case user
assert_status 1 "invalid worker count is rejected"
[[ "${OUTPUT}" == *"HEW_CORPUS_JOBS must be a positive integer"* ]]

TEST_PATH=$'std/first.hew\ntests/first.hew'
: >"${EXPECTED}"
HEW_CORPUS_SKIP_STDLIB=0 run_case mixed
assert_status 0 "standalone sweep checks stdlib sources"
[[ "${OUTPUT}" == *"Files checked:          2"* ]]
HEW_CORPUS_SKIP_STDLIB=1 run_case mixed
assert_status 0 "composed sweep can leave stdlib to its owning gate"
[[ "${OUTPUT}" == *"Files checked:          1"* ]]

echo "PASS: structured diagnostic channels are accepted without accepting process failures"
