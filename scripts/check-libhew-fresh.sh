#!/usr/bin/env bash
# check-libhew-fresh.sh — assert that the combined runtime + stdlib archive is
# not stale.
#
# Compares the mtime of target/debug/libhew.a (target/debug/hew.lib on Windows)
# against the newest input file found under hew-lib/, hew-runtime/ and hew-std/
# (*.rs, Cargo.toml, build.rs). Exits 0 if the archive is current; exits 1 if it
# predates any source input.
#
# This is the build-graph half of the staleness defence. The other half lives in
# the compiler driver, which embeds a digest of the same input set and refuses
# to link an archive whose stamp disagrees (see hew-build-identity). Neither
# subsumes the other: this check runs before a build step consumes the archive,
# the driver's runs at the moment of link and therefore also covers direct
# `cargo build`, IDE builds and copied-in artefacts.
#
# Usage: scripts/check-libhew-fresh.sh [--debug-dir <dir>]
#   --debug-dir <dir>   Override the Cargo output dir (default: target/debug)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DEBUG_DIR="${REPO_ROOT}/target/debug"

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

# Scan source inputs: all .rs files, Cargo.toml, and build.rs under
# hew-lib/, hew-runtime/, and hew-std/ — the crates that feed libhew.a. The
# hew-lib umbrella links hew-runtime plus the consolidated hew-std staticlib,
# so a stdlib edit changes libhew.a and must count toward freshness.
latest_src_mtime=0
latest_src_file=""

while IFS= read -r -d '' f; do
    mtime=$(get_mtime "$f")
    if (( mtime > latest_src_mtime )); then
        latest_src_mtime=$mtime
        latest_src_file="$f"
    fi
done < <(find \
    "${REPO_ROOT}/hew-lib" \
    "${REPO_ROOT}/hew-runtime" \
    "${REPO_ROOT}/hew-std" \
    \( -name "*.rs" -o -name "Cargo.toml" -o -name "build.rs" \) \
    -not -path "*/target/*" \
    -print0)

if (( latest_src_mtime == 0 )); then
    echo "error: no source files found under hew-lib/, hew-runtime/ or hew-std/" >&2
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
