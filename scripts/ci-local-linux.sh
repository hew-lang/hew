#!/usr/bin/env bash
# Run the GitHub Actions "Build & test (Linux)" job locally — equivalent to CI —
# by executing its exact step sequence on a NATIVE x86_64 Linux host over ssh.
#
# WHY a remote native host (not Docker): on an Apple-Silicon dev machine Docker
# emulates x86_64 with qemu, which segfaults rustc, and arm64 containers diverge
# from CI (ppv-lite86 SIMD path, ARM64 LLVM tarball quirks). A native x86_64
# Linux box is the only faithful, fast local parity. See LESSONS.md
# `ci-local-parity-needs-native-x86_64`.
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
set -euo pipefail

STEP="${1:-${STEP:-all}}"
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
SLUG="ci-local-$(printf '%s' "${BRANCH}" | tr '/:' '--')"   # slash-safe ref/worktree name
WT="/tmp/${SLUG}"

echo "==> Syncing ${BRANCH} (${SHA}) → ${HOST}:${REMOTE_REL} as ${SLUG}"
git -C "${ROOT}" push --force-with-lease "${HOST}:${REMOTE_REL}" "HEAD:refs/heads/${SLUG}" 2>&1 | tail -2

echo "==> Running 'Build & test (Linux)' step '${STEP}' on ${HOST} (LLVM_SYS_221_PREFIX=${LLVM_PREFIX})"
# Quoted heredoc: the body runs verbatim server-side; locals are passed as args.
ssh "${HOST}" bash -s -- "${STEP}" "${SLUG}" "${WT}" "${REMOTE_REL}" "${LLVM_PREFIX}" <<'REMOTE'
set -euo pipefail
STEP="$1"; SLUG="$2"; WT="$3"; REMOTE_REL="$4"; LLVM_PREFIX="$5"
cd "$HOME/$REMOTE_REL"
git fetch . "$SLUG" >/dev/null 2>&1 || true
rm -rf "$WT"; git worktree prune
git worktree add "$WT" "$SLUG" >/dev/null
cd "$WT"
export LLVM_SYS_221_PREFIX="$LLVM_PREFIX"
export CARGO_TERM_COLOR=always
export CARGO_TARGET_WASM32_WASIP1_RUNNER="wasmtime run"

case "$STEP" in
  wasm)            cargo test -p hew-runtime --target wasm32-wasip1 --no-default-features --lib ;;
  workspace)       cargo nextest run --workspace --exclude hew-wasm --exclude hew-cabi --profile ci ;;
  vertical-slice)  make test-vertical-slice ;;
  pkg-import)      make test-pkg-import ;;
  hew-ratchet)     make test-hew-ratchet ;;
  stdlib-ratchet)  make test-stdlib-ratchet ;;
  sandbox)         make sandbox-parity ;;
  all)
    cargo test -p hew-runtime --target wasm32-wasip1 --no-default-features --lib
    cargo nextest run --workspace --exclude hew-wasm --exclude hew-cabi --profile ci
    make test-vertical-slice
    make test-pkg-import
    make test-hew-ratchet
    make test-stdlib-ratchet
    make sandbox-parity ;;
  *) echo "unknown STEP=$STEP (wasm|workspace|vertical-slice|pkg-import|hew-ratchet|stdlib-ratchet|sandbox|all)" >&2; exit 2 ;;
esac
echo "CI_LOCAL_LINUX_OK host=$(hostname) step=$STEP"
REMOTE
