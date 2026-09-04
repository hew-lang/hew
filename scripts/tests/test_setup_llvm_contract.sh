#!/usr/bin/env bash
# Execute the setup-llvm metadata resolver for supported and unsupported hosts.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ACTION="$ROOT/.github/actions/setup-llvm/action.yml"
TMP_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-setup-llvm-contract.XXXXXX")"
trap 'rm -rf "$TMP_ROOT"' EXIT

RESOLVER="$TMP_ROOT/resolver.sh"
awk '
  /^    - name: Resolve LLVM asset metadata$/ { in_layout=1 }
  in_layout && /^        set -euo pipefail$/ { in_run=1 }
  in_run {
    line = $0
    sub(/^        /, "", line)
    print line
    if (line == "echo \"prefix=${prefix}\" >> \"$GITHUB_OUTPUT\"") exit
  }
' "$ACTION" |
    sed -e 's|\${{ runner\.os }}|$TEST_RUNNER_OS|g' \
        -e 's|\${{ runner\.arch }}|$TEST_RUNNER_ARCH|g' \
        -e 's|\${{ inputs\.version }}|22.1.5|g' >"$RESOLVER"

RUNNER_TEMP="$TMP_ROOT/runner-temp"
GITHUB_OUTPUT="$TMP_ROOT/linux.output"
TEST_RUNNER_OS=Linux TEST_RUNNER_ARCH=X64 \
    RUNNER_TEMP="$RUNNER_TEMP" GITHUB_OUTPUT="$GITHUB_OUTPUT" \
    bash "$RESOLVER"

grep -Fx 'asset=LLVM-22.1.5-Linux-X64.tar.xz' "$GITHUB_OUTPUT"
grep -Fx 'sha256=04dfa3ab6f1c332dd73a057daeb8f48cdaacdef24178f8eccddf2cbfa8944aa4' "$GITHUB_OUTPUT"

if TEST_RUNNER_OS=FreeBSD TEST_RUNNER_ARCH=X64 \
    RUNNER_TEMP="$RUNNER_TEMP" GITHUB_OUTPUT="$TMP_ROOT/unknown.output" \
    bash "$RESOLVER" >"$TMP_ROOT/unknown.log" 2>&1; then
    echo "unsupported LLVM target unexpectedly resolved" >&2
    exit 1
fi
grep -Fqx '::error::No upstream LLVM binary asset known for FreeBSD-X64' "$TMP_ROOT/unknown.log"

echo "setup-llvm metadata contract: PASS"
