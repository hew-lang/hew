#!/usr/bin/env bash
# check-licenses-fresh.sh — assert that THIRD-PARTY-LICENSES is not stale
# relative to Cargo.lock and the about.toml/about.hbs templates.
#
# Regenerates the file in a temp location and diffs it against the committed
# copy. Exits 0 if they match; exits 1 and shows the diff if they differ.
#
# Prerequisites: cargo-about must be installed.
#   cargo install cargo-about --locked
#
# Usage: scripts/check-licenses-fresh.sh [--quiet]
#   --quiet   Suppress progress messages; only print failures.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
QUIET=0

while [[ $# -gt 0 ]]; do
    case "$1" in
        --quiet) QUIET=1; shift ;;
        *) echo "error: unknown argument: $1" >&2; exit 1 ;;
    esac
done

COMMITTED="${REPO_ROOT}/THIRD-PARTY-LICENSES"

if [[ ! -f "$COMMITTED" ]]; then
    echo "error: THIRD-PARTY-LICENSES not found at ${COMMITTED}" >&2
    echo "  Run 'make licenses' to generate it." >&2
    exit 1
fi

if ! command -v cargo-about &>/dev/null && ! cargo about --version &>/dev/null 2>&1; then
    echo "error: cargo-about not found — install with:" >&2
    echo "  cargo install cargo-about --locked" >&2
    exit 1
fi

TMPFILE="$(mktemp "/tmp/third-party-licenses.XXXXXX")"
trap 'rm -f "$TMPFILE"' EXIT

(( QUIET == 0 )) && echo "Regenerating THIRD-PARTY-LICENSES for freshness check..."

# Suppress cargo-about's own progress chatter; capture only the generated output.
cargo about generate "${REPO_ROOT}/about.hbs" --workspace \
    --manifest-path "${REPO_ROOT}/Cargo.toml" \
    2>/dev/null > "$TMPFILE"

if diff -u "$COMMITTED" "$TMPFILE"; then
    (( QUIET == 0 )) && echo "ok: THIRD-PARTY-LICENSES is current"
    exit 0
else
    echo "" >&2
    echo "error: THIRD-PARTY-LICENSES is stale (Cargo.lock or about.hbs changed)." >&2
    echo "  Run 'make licenses' and commit the updated file." >&2
    exit 1
fi
