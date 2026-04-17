#!/usr/bin/env bash
# Verify that formatter dispatch paths in hew-parser/src/fmt.rs stay exhaustive.
#
# Rationale: `format_item`, `format_expr`, `format_stmt`, and `format_else_block`
# route the AST to per-variant printer arms. A `_ =>` fallback in any of those
# sites hides a missing variant — adding `Expr::NewThing` silently compiles and
# the formatter emits nothing for it, producing a parse-format-parse AST drift.
#
# This script grep-gates the file. It counts `_ =>` arms in fmt.rs and asserts
# the total matches the approved allowlist of peripheral helpers:
#   - escape_byte_string (byte escaper)
#   - escape_char (char escape lookup)
#   - extract_comments (two arms: byte-scanner inside /* */ and outer state)
#   - find_block_close (two arms: inside /* */ scanner and outer brace walker)
# Total: 6 wildcards, all in utility code, none in dispatch paths.
#
# If this count changes, the diff must either:
#   1) live in a new peripheral helper (bump the count and update this comment), OR
#   2) have removed a `_ =>` from a dispatch path (bump the count down).
# Both are legitimate; the gate forces a review.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FMT_FILE="${REPO_ROOT}/hew-parser/src/fmt.rs"
EXPECTED=6

if [ ! -f "$FMT_FILE" ]; then
    echo "check-fmt-dispatch-exhaustive: $FMT_FILE not found" >&2
    exit 2
fi

ACTUAL="$(grep -c '_ =>' "$FMT_FILE" || true)"

if [ "$ACTUAL" != "$EXPECTED" ]; then
    echo "check-fmt-dispatch-exhaustive: unexpected \`_ =>\` count in hew-parser/src/fmt.rs" >&2
    echo "  expected: $EXPECTED (peripheral helpers only)" >&2
    echo "  actual:   $ACTUAL" >&2
    echo "" >&2
    echo "Either a dispatch site regressed to a wildcard fallback (forbidden)," >&2
    echo "or a new peripheral helper was added. Review the diff; if the change" >&2
    echo "is legitimate, bump EXPECTED in this script and update the comment." >&2
    echo "" >&2
    echo "Current matches:" >&2
    grep -n '_ =>' "$FMT_FILE" >&2 || true
    exit 1
fi

echo "check-fmt-dispatch-exhaustive: ok ($ACTUAL wildcards, all in peripheral helpers)"
