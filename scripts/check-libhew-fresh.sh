#!/usr/bin/env bash
# check-libhew-fresh.sh — assert that the combined runtime + stdlib archive is
# not stale.
#
# Compares the mtime of libhew.a (hew.lib on Windows) in the directory Cargo
# actually writes to against the newest input file that feeds the archive.
# Exits 0 if the archive is current; exits 1 if it predates any input.
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

LIBHEW="${DEBUG_DIR}/libhew.a"

# Cargo emits `hew.lib` for hew-lib's staticlib on MSVC targets and `libhew.a`
# everywhere else. Probe both so a Windows host runs the same check rather than
# a bespoke one.
if [[ ! -f "$LIBHEW" && -f "${DEBUG_DIR}/hew.lib" ]]; then
    LIBHEW="${DEBUG_DIR}/hew.lib"
fi

if [[ ! -f "$LIBHEW" ]]; then
    echo "error: ${LIBHEW} not found — run 'make hew-native' first" >&2
    exit 1
fi

# Portable mtime. Order matters: try GNU `stat -c %Y` (Linux) FIRST, then fall
# back to BSD `stat -f %m` (macOS). A BSD-first probe is NOT safe on Linux —
# GNU `stat -f` is --file-system mode and SUCCEEDS on a regular file, emitting a
# multi-line filesystem description that begins `  File: "..."`. That non-zero-
# exit-free success means the `||` fallback never runs, so `mtime` captured the
# description and later blew up `(( mtime > ... ))` under `set -u` on the bare
# token `File` ("File: unbound variable"). GNU's `-c` fails cleanly on BSD, so
# GNU-first works on both platforms.
get_mtime() {
    stat -c %Y "$1" 2>/dev/null || stat -f %m "$1" 2>/dev/null
}

lib_mtime=$(get_mtime "$LIBHEW")

# Scan the archive's input set. The list comes from scripts/libhew-inputs.py,
# which derives it rather than hand-listing it: hew-lib's non-dev
# path-dependency closure (hew-runtime, hew-std, hew-cabi), those crates' Rust
# sources and manifests, the assets their code embeds with
# include_str!/include_bytes!, and the workspace manifest and lockfile.
# Scanning a hand-written list instead is how an input that changes the archive
# ends up not counting toward freshness.
if ! src_list="$("${REPO_ROOT}/scripts/libhew-inputs.py" files)"; then
    echo "error: could not resolve the archive's input set" >&2
    exit 1
fi

latest_src_mtime=0
latest_src_file=""

while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    mtime=$(get_mtime "${REPO_ROOT}/${f}")
    if (( mtime > latest_src_mtime )); then
        latest_src_mtime=$mtime
        latest_src_file="${REPO_ROOT}/${f}"
    fi
done <<< "$src_list"

if (( latest_src_mtime == 0 )); then
    echo "error: the archive's input set is empty; refusing to certify ${LIBHEW} as fresh" >&2
    exit 1
fi

if (( lib_mtime >= latest_src_mtime )); then
    echo "ok: ${LIBHEW} is current (lib mtime=${lib_mtime} >= newest src mtime=${latest_src_mtime})"
    exit 0
else
    echo "error: ${LIBHEW} is stale" >&2
    echo "  library mtime : ${lib_mtime}  ($(date -r "$lib_mtime" 2>/dev/null || date -d "@${lib_mtime}" 2>/dev/null || echo 'unknown'))" >&2
    echo "  newest source : ${latest_src_mtime}  ${latest_src_file}  ($(date -r "$latest_src_mtime" 2>/dev/null || date -d "@${latest_src_mtime}" 2>/dev/null || echo 'unknown'))" >&2
    echo "  Run 'make hew-native' to rebuild the driver and the archive together." >&2
    exit 1
fi
