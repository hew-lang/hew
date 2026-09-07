#!/usr/bin/env bash
# Run the GitHub Actions "Build & test (Linux)" job locally — equivalent to CI —
# by executing its exact step sequence on a NATIVE x86_64 Linux host over ssh.
#
# WHY a remote native host (not Docker): on an Apple-Silicon dev machine Docker
# emulates x86_64 with qemu, which segfaults rustc, and arm64 containers diverge
# from CI (ppv-lite86 SIMD path, ARM64 LLVM tarball quirks). A native x86_64
# Linux box is the only faithful, fast local parity. See the parity principle
# in docs/internal/engineering-invariants.md.
#
# Usage:
#   make ci-local-linux CI_LINUX_HOST=<user@host>             # full Linux job
#   make ci-local-linux CI_LINUX_HOST=<host> STEP=test-vertical-slice
#   CI_LINUX_HOST=<host> scripts/ci-local-linux.sh [step]
#
# Config (env):
#   CI_LINUX_HOST       ssh target of a native x86_64 Linux box (required)
#   HEW_CI_REMOTE_REL   home-relative path of the hew clone on the host
#                       (default: projects/hew-lang/hew)
#   HEW_CI_LLVM_PREFIX  LLVM_SYS_221_PREFIX on the host (default: /usr/lib/llvm-22).
#                       Point at an unpacked copy of CI's upstream LLVM tarball
#                       for byte-faithful parity (a host's system lld/LLVM can
#                       diverge from CI on fixture linking).
#   HEW_CI_REMOTE_WORKTREE_REL
#                       Home-relative parent for unique retained worktrees (default:
#                       .cache/hew-ci). Each run prints its worktree path; remove
#                       it with git worktree remove after inspecting its results.
set -euo pipefail

STEP="${1:-${STEP:-preflight}}"
HOST="${CI_LINUX_HOST:-}"
REMOTE_REL="${HEW_CI_REMOTE_REL:-projects/hew-lang/hew}"
LLVM_PREFIX="${HEW_CI_LLVM_PREFIX:-/usr/lib/llvm-22}"

if [[ -z "${HOST}" ]]; then
    echo "error: set CI_LINUX_HOST=<user@host> (a native x86_64 Linux box)" >&2
    exit 2
fi

case "$STEP" in
all) TARGET=preflight ;;
preflight | lint | ci-shard-1 | ci-shard-2 | ci-shard-3 | test-vertical-slice | test-pkg-import | test-hew-ratchet | test-stdlib-ratchet | sandbox-parity)
    TARGET="$STEP"
    ;;
*)
    echo "unknown STEP=$STEP (use preflight, lint, ci-shard-1/2/3 or a listed Make target)" >&2
    exit 2
    ;;
esac

WT_REL="${HEW_CI_REMOTE_WORKTREE_REL:-.cache/hew-ci}"
for relative in "$REMOTE_REL" "$WT_REL"; do
    case "/$relative/" in
    //* | */../* | */./*)
        echo "error: remote paths must be home-relative without . or .. components" >&2
        exit 2
        ;;
    esac
done

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BRANCH="$(git -C "${ROOT}" rev-parse --abbrev-ref HEAD)"
SHA="$(git -C "${ROOT}" rev-parse HEAD)"
REF="refs/hew-ci/${SHA}"

echo "==> Syncing ${BRANCH} (${SHA}) → ${HOST}:${REMOTE_REL}"
# Content-addressed refs need no force update and cannot race with another
# commit's run. Use the captured SHA even if HEAD changes during the push.
git -C "${ROOT}" push "${HOST}:${REMOTE_REL}" "${SHA}:${REF}"

echo "==> Running '${TARGET}' on ${HOST} (LLVM_SYS_221_PREFIX=${LLVM_PREFIX})"
# SSH joins command arguments through the remote shell: preserve spaces and
# metacharacters before passing the quoted heredoc's arguments to Bash.
printf -v REMOTE_COMMAND 'bash -s -- %q %q %q %q %q' \
    "$TARGET" "$SHA" "$WT_REL" "$REMOTE_REL" "$LLVM_PREFIX"
# Arguments were shell-escaped above; this expansion is the SSH command.
# shellcheck disable=SC2029
ssh "${HOST}" "$REMOTE_COMMAND" <<'REMOTE'
set -euo pipefail
TARGET="$1"; SHA="$2"; WT_PARENT="$HOME/$3"; REMOTE_REL="$4"; LLVM_PREFIX="$5"
cd "$HOME/$REMOTE_REL"
mkdir -p "$WT_PARENT"
WT="$(mktemp -d "$WT_PARENT/run.XXXXXXXX")"
echo "==> Retained CI worktree: $WT (commit $SHA)"
git worktree add --detach "$WT" "$SHA" >/dev/null
cd "$WT"
export LLVM_SYS_221_PREFIX="$LLVM_PREFIX"
export CARGO_TERM_COLOR=always
# Non-login SSH shells do not inherit the installer-managed Wasmtime path.
# Prefer an already-provisioned per-user install before declaring the WASI
# runner unavailable; this keeps the reusable Linux parity harness faithful to
# CI without requiring host-specific shell startup files.
if [[ -x "$HOME/.wasmtime/bin/wasmtime" ]]; then
  export PATH="$HOME/.wasmtime/bin:$PATH"
fi
export CARGO_TARGET_WASM32_WASIP1_RUNNER="wasmtime run"

make "$TARGET"
echo "CI_LOCAL_LINUX_OK host=$(hostname) target=$TARGET"
REMOTE
