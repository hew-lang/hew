#!/usr/bin/env bash
# scripts/lint-stdlib-int-surface.sh
#
# Verifies that std/**/*.hew public surfaces use `int`, not `i32`/`i64`.
# See docs/stdlib-style-contract.md for the full contract.
#
# Exit 0 — no violations found.
# Exit 1 — one or more public-surface violations detected.
#
# Usage:
#   bash scripts/lint-stdlib-int-surface.sh
#
# To add an exemption for a mandatory ABI seam, end the offending line with:
#   // INTERNAL-ABI: <one-sentence reason>

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# std/builtins.hew is excluded: its declarations must literally match
# hew-types/src/check/registration.rs (see docs/stdlib-style-contract.md).
FILES=$(git ls-files 'std/**/*.hew' 'std/*.hew' | grep -v '^std/builtins\.hew$' || true)

if [ -z "$FILES" ]; then
    echo "[stdlib-int-surface] No stdlib .hew files found — nothing to check." >&2
    exit 0
fi

FAIL=0

for f in $FILES; do
    # Strategy: extract only the signature portion (before the opening `{` of the body)
    # from lines that contain a fn declaration, then check that portion for i32/i64.
    # This avoids flagging `as i64` or `as i32` narrowing casts in the function body.
    #
    # AWK state machine (POSIX-compatible — no \s, no \b):
    #   - Tracks extern "C" { ... } blocks and skips them entirely.
    #   - Skips lines with // INTERNAL-ABI: annotation.
    #   - For fn declaration lines: strips the body (from first `{`) before checking.
    #   - For standalone `->` lines (multi-line signatures): checks the return type.
    result=$(awk -v file="$f" '
        BEGIN { in_extern = 0; found = 0 }

        # Track extern "C" { ... } blocks; closing } must be on its own line
        /^[ \t]*extern[ \t]+"C"[ \t]*\{/ { in_extern = 1; next }
        in_extern && /^[ \t]*\}[ \t]*$/ { in_extern = 0; next }
        in_extern { next }

        # Skip lines with the INTERNAL-ABI annotation
        /\/\/ *INTERNAL-ABI:/ { next }

        # Process fn declaration lines
        /^[ \t]*(pub[ \t]+)?fn[ \t]+[A-Za-z_][A-Za-z0-9_]*[ \t]*[<(]/ {
            # Extract only the signature: everything before the first `{`
            sig = $0
            brace = index(sig, "{")
            if (brace > 0) {
                sig = substr(sig, 1, brace - 1)
            }
            if (sig ~ /[^a-zA-Z0-9_](i32|i64)/) {
                printf("%s:%d: %s\n", file, NR, $0)
                found = 1
            }
            next
        }

        # Flag standalone return-arrow lines with i32/i64 in return type
        # (multi-line signatures where -> is on its own line)
        /^[ \t]*->[ \t]*/ {
            if ($0 ~ /[^a-zA-Z0-9_](i32|i64)/) {
                printf("%s:%d: %s\n", file, NR, $0)
                found = 1
            }
            next
        }

        END { exit (found ? 1 : 0) }
    ' "$f") || FAIL=1

    if [ -n "$result" ]; then
        printf '%s\n' "$result"
    fi
done

if [ "$FAIL" -ne 0 ]; then
    cat >&2 <<'EOF'

[stdlib-int-surface] Public stdlib surfaces must use `int`, not `i32`/`i64`.
See docs/stdlib-style-contract.md.

If the match is an internal ABI seam, end the offending line with:
    // INTERNAL-ABI: <one-sentence reason>

Lines inside `extern "C" { ... }` blocks are automatically exempt.
EOF
    exit 1
fi
