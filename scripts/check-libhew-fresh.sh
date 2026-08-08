#!/usr/bin/env bash
# check-libhew-fresh.sh — assert that the combined runtime + stdlib archive is
# not stale.
#
# Verifies the content-addressed certificate that `libhew-debug` writes after a
# successful Cargo build.  It binds libhew.a (hew.lib on Windows) to the
# semantic input closure, rather than treating raw mtimes as build authority.
#
# This runs before a build step consumes the archive, so a stale archive is
# reported where it can still be fixed rather than at the linker.
#
# Usage: scripts/check-libhew-fresh.sh [--debug-dir <dir>]
#   --debug-dir <dir>   Override the Cargo output dir. The default is whatever
#                       scripts/cargo-output-dir.py resolves, which is what
#                       Cargo itself would use: `target/debug` only when
#                       CARGO_TARGET_DIR, build.target-dir, CARGO_BUILD_TARGET
#                       and build.target are all unset. Checking a hard-coded
#                       `target/debug` while Cargo writes elsewhere is how a
#                       stale default archive gets reported as current.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
resolved_out="$("${REPO_ROOT}/scripts/cargo-output-dir.py" --profile debug)"
case "$resolved_out" in
    /*) DEBUG_DIR="$resolved_out" ;;
    *)  DEBUG_DIR="${REPO_ROOT}/${resolved_out}" ;;
esac

while [[ $# -gt 0 ]]; do
    case "$1" in
        --debug-dir)
            shift
            [[ $# -gt 0 ]] || { echo "error: --debug-dir requires a path" >&2; exit 1; }
            DEBUG_DIR="$1"
            shift
            ;;
        *)
            echo "error: unknown argument: $1" >&2
            exit 1
            ;;
    esac
done

exec "${REPO_ROOT}/scripts/libhew-freshness.py" verify --debug-dir "${DEBUG_DIR}"
