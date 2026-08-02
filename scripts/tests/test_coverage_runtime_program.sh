#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/lib/coverage-runtime-program.sh
# shellcheck disable=SC1091
source "${ROOT}/scripts/lib/coverage-runtime-program.sh"

TEST_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hew-coverage-runtime-program.XXXXXX")"
trap 'rm -rf "${TEST_DIR}"' EXIT

cat > "${TEST_DIR}/timeout" <<'EOF_TIMEOUT'
#!/usr/bin/env bash
set -euo pipefail
test "$1" = "9"
shift
exec "$@"
EOF_TIMEOUT
chmod +x "${TEST_DIR}/timeout"

cat > "${TEST_DIR}/probe" <<'EOF_PROBE'
#!/usr/bin/env bash
set -euo pipefail
printf '%s' "${LLVM_PROFILE_FILE:?}|$#"
for arg in "$@"; do
    printf '|%s' "${arg}"
done
printf '\n'
EOF_PROBE
chmod +x "${TEST_DIR}/probe"

grep_input="${TEST_DIR}/grep-input.txt"
printf 'needle\n' > "${grep_input}"

zero_output="$(coverage_runtime_run_program \
    hello \
    "${TEST_DIR}/zero.profraw" \
    "${TEST_DIR}/timeout" \
    9 \
    "${TEST_DIR}/probe" \
    "${grep_input}")"
test "${zero_output}" = "${TEST_DIR}/zero.profraw|0"

args_output="$(coverage_runtime_run_program \
    hew_grep \
    "${TEST_DIR}/args.profraw" \
    "${TEST_DIR}/timeout" \
    9 \
    "${TEST_DIR}/probe" \
    "${grep_input}")"
test "${args_output}" = "${TEST_DIR}/args.profraw|2|needle|${grep_input}"

echo "PASS: runtime coverage program invocation supports zero and command arguments"
