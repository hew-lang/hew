#!/usr/bin/env bash
# scripts/lint-stdlib-int-surface.sh
#
# Verifies that std/**/*.hew files do not use the removed `int`/`uint` aliases.
# See docs/stdlib-style-contract.md for the full contract.
#
# Exit 0 — no violations found.
# Exit 1 — one or more removed-alias uses detected.
#
# Usage:
#   bash scripts/lint-stdlib-int-surface.sh

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

FILES=$(git ls-files 'std/**/*.hew' 'std/*.hew' || true)

if [ -z "$FILES" ]; then
    echo "[stdlib-int-surface] No stdlib .hew files found — nothing to check." >&2
    exit 0
fi

FAIL=0

for f in $FILES; do
    # Match `int` or `uint` as whole words using -w (word-boundary) flag.
    # -w is POSIX-compatible on macOS and Linux grep.
    result=$(grep -nw 'int\|uint' "$f" || true)

    if [ -n "$result" ]; then
        printf '%s\n' "$result" | sed "s|^|$f:|"
        FAIL=1
    fi
done

if [ "$FAIL" -ne 0 ]; then
    cat >&2 <<'EOF'

[stdlib-int-surface] Removed aliases `int`/`uint` found in stdlib.
Use explicit-width types: `i64`/`u64` for fixed 64-bit, `isize`/`usize` for
pointer-sized. See docs/stdlib-style-contract.md.
EOF
    exit 1
fi
