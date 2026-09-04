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
#   make ci-local-linux CI_LINUX_HOST=<host> STEP=vertical-slice
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
#                       Home-relative scratch worktree path (default:
#                       .cache/hew-ci/<branch>). Keeping build output on the
#                       user's filesystem avoids small or quota-limited /tmp.
set -euo pipefail

STEP="${1:-${STEP:-preflight}}"
HOST="${CI_LINUX_HOST:-}"
REMOTE_REL="${HEW_CI_REMOTE_REL:-projects/hew-lang/hew}"
LLVM_PREFIX="${HEW_CI_LLVM_PREFIX:-/usr/lib/llvm-22}"

if [[ -z "${HOST}" ]]; then
    echo "error: set CI_LINUX_HOST=<user@host> (a native x86_64 Linux box)" >&2
    exit 2
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BRANCH="$(git -C "${ROOT}" rev-parse --abbrev-ref HEAD)"
SHA="$(git -C "${ROOT}" rev-parse --short HEAD)"
SLUG="ci-local-$(printf '%s' "${BRANCH}" | tr '/:' '--')" # slash-safe ref/worktree name
REF="refs/hew-ci/${SLUG}"
WT_REL="${HEW_CI_REMOTE_WORKTREE_REL:-.cache/hew-ci/${SLUG}}"

echo "==> Syncing ${BRANCH} (${SHA}) → ${HOST}:${REMOTE_REL} as ${SLUG}"
# This private non-branch ref is scratch space owned by the harness. Keeping it
# outside refs/heads means a prior detached validation worktree can never make
# an ordinary rerun fail because the receive side considers the ref checked
# out. No project branch is addressed by this operation.
git -C "${ROOT}" push --force "${HOST}:${REMOTE_REL}" "HEAD:${REF}" 2>&1 | tail -2

echo "==> Running 'Build & test (Linux)' step '${STEP}' on ${HOST} (LLVM_SYS_221_PREFIX=${LLVM_PREFIX})"
# Quoted heredoc: the body runs verbatim server-side; locals are passed as args.
ssh "${HOST}" bash -s -- "${STEP}" "${SLUG}" "${REF}" "${WT_REL}" "${REMOTE_REL}" "${LLVM_PREFIX}" <<'REMOTE'
set -euo pipefail
STEP="$1"; SLUG="$2"; REF="$3"; WT="$HOME/$4"; REMOTE_REL="$5"; LLVM_PREFIX="$6"
cd "$HOME/$REMOTE_REL"
git fetch . "$REF" >/dev/null
rm -rf "$WT"; git worktree prune
git worktree add --detach "$WT" FETCH_HEAD >/dev/null
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

case "$STEP" in
  all) TARGET=preflight ;;
  preflight|lint|ci-shard-1|ci-shard-2|ci-shard-3|test-vertical-slice|test-pkg-import|test-hew-ratchet|test-stdlib-ratchet|sandbox-parity)
    TARGET="$STEP" ;;
  *) echo "unknown STEP=$STEP (preflight|lint|ci-shard-1|ci-shard-2|ci-shard-3 or a focused listed gate)" >&2; exit 2 ;;
esac
make "$TARGET"
echo "CI_LOCAL_LINUX_OK host=$(hostname) step=$STEP"
REMOTE
