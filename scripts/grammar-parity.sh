#!/usr/bin/env bash
# grammar-parity.sh — parse every accepted .hew program with the pinned
# tree-sitter-hew grammar and fail on any ERROR node.
#
# The tree-sitter grammar (tree-sitter-hew, a sibling repo) is a syntax
# highlighting/editor-tooling mirror of the real authority, hew-parser
# (D19). This gate is the leash that keeps the mirror honest: it never
# type-checks or runs anything, it only asks whether the pinned grammar
# commit can produce a parse tree with zero ERROR nodes for every file the
# compiler itself accepts. A red run here means the mirror has drifted,
# either because a compiler-side syntax change landed without a matching
# tree-sitter-hew update (fix tree-sitter-hew, push it, bump the lock), or
# because a parsed file is not actually acceptable Hew (fix the file).
#
# Usage:
#   scripts/grammar-parity.sh
#
# Environment variables:
#   HEW_SYNC_TREE_SITTER   Path to an existing tree-sitter-hew checkout to
#                           parse with instead of cloning into .tmp/. Used
#                           as-is: never fetched or checked out by this
#                           script (same env scheme as sync-downstream.sh).
#                           A HEAD that does not match the pinned commit is
#                           a warning, not an error — local iteration on an
#                           unpushed tree-sitter-hew change is the point.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
LOCK_FILE="$REPO_ROOT/tools/downstream/tree-sitter.lock"
CLONE_DIR="$REPO_ROOT/.tmp/tree-sitter-hew"
GIT_URL="https://github.com/hew-lang/tree-sitter-hew.git"

# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$SCRIPT_DIR/lib/corpus-nonempty.sh"

if [[ ! -f "$LOCK_FILE" ]]; then
    echo "grammar-parity: $LOCK_FILE not found" >&2
    exit 1
fi

LOCK_COMMIT="$(sed -n 's/^commit *= *"\([^"]*\)".*/\1/p' "$LOCK_FILE" | head -1)"
if [[ ! "$LOCK_COMMIT" =~ ^[0-9a-f]{40}$ ]]; then
    echo "grammar-parity: tools/downstream/tree-sitter.lock's commit is not a 40-character hex sha (got '$LOCK_COMMIT')" >&2
    exit 1
fi

# ── Resolve the tree-sitter-hew checkout ────────────────────────────────
if [[ -n "${HEW_SYNC_TREE_SITTER:-}" ]]; then
    TS_DIR="$HEW_SYNC_TREE_SITTER"
    if [[ ! -d "$TS_DIR/.git" ]]; then
        echo "grammar-parity: HEW_SYNC_TREE_SITTER=$TS_DIR is not a git checkout" >&2
        exit 1
    fi
    TS_HEAD="$(cd "$TS_DIR" && git rev-parse HEAD)"
    if [[ "$TS_HEAD" != "$LOCK_COMMIT" ]]; then
        echo "grammar-parity: warning: HEW_SYNC_TREE_SITTER's HEAD ($TS_HEAD) does not match the pinned commit ($LOCK_COMMIT); parsing with HEAD as-is" >&2
    fi
else
    TS_DIR="$CLONE_DIR"
    if [[ ! -d "$TS_DIR/.git" ]]; then
        mkdir -p "$(dirname "$TS_DIR")"
        git clone --quiet "$GIT_URL" "$TS_DIR"
    fi
    (cd "$TS_DIR" && git fetch --quiet origin)
    if ! (cd "$TS_DIR" && git checkout --quiet "$LOCK_COMMIT"); then
        echo "grammar-parity: commit $LOCK_COMMIT (from tools/downstream/tree-sitter.lock) was not found in $GIT_URL after fetching" >&2
        exit 1
    fi
fi

# ── Ensure the tree-sitter CLI is available in that checkout ───────────
if [[ ! -x "$TS_DIR/node_modules/.bin/tree-sitter" ]]; then
    (cd "$TS_DIR" && npm install --no-audit --no-fund >/dev/null)
fi

# ── Enumerate the accepted corpus: vertical-slice accept fixtures, all of
# std/, all of examples/. `find` (not `git ls-files`) so an untracked
# scratch file placed under one of these roots is still parsed — the
# negative control this gate names relies on that.
FILE_LIST="$(mktemp)"
trap 'rm -f "$FILE_LIST"' EXIT
{
    find "$REPO_ROOT/tests/vertical-slice/accept" -name '*.hew' -type f
    find "$REPO_ROOT/std" -name '*.hew' -type f
    find "$REPO_ROOT/examples" -name '*.hew' -type f
} | LC_ALL=C sort >"$FILE_LIST"

TOTAL=$(wc -l <"$FILE_LIST")
corpus_nonempty_assert "grammar-parity-files" "$TOTAL" || exit 1

# ── Parse the whole set in one CLI invocation and read the JSON summary.
# The CLI also prints one plain-text diagnostic line per failing file to
# stdout ahead of the JSON blob (undocumented, version-specific); the
# Python reader below skips to the first line that is exactly "{" rather
# than relying on that text, so it does not depend on CLI-version
# formatting quirks.
PARSE_OUT="$(mktemp)"
PARSE_ERR="$(mktemp)"
trap 'rm -f "$FILE_LIST" "$PARSE_OUT" "$PARSE_ERR"' EXIT
(cd "$TS_DIR" && node_modules/.bin/tree-sitter parse --json-summary --paths "$FILE_LIST") \
    >"$PARSE_OUT" 2>"$PARSE_ERR" || true

FAILURES="$(
    python3 - "$PARSE_OUT" "$PARSE_ERR" "$REPO_ROOT" <<'PYEOF'
import json
import sys

out_path, err_path, repo_root = sys.argv[1], sys.argv[2], sys.argv[3].rstrip("/") + "/"
lines = open(out_path, encoding="utf-8").read().splitlines()
start = next((i for i, line in enumerate(lines) if line == "{"), None)
if start is None:
    print("grammar-parity: tree-sitter parse produced no JSON summary; stderr was:", file=sys.stderr)
    print(open(err_path, encoding="utf-8").read(), file=sys.stderr)
    sys.exit(1)
data = json.loads("\n".join(lines[start:]))
for summary in data["parse_summaries"]:
    if not summary["successful"]:
        path = summary["file"]
        if path.startswith(repo_root):
            path = path[len(repo_root):]
        print(path)
PYEOF
)"

if [[ -n "$FAILURES" ]]; then
    FAIL_COUNT=$(printf '%s\n' "$FAILURES" | wc -l)
    echo "grammar-parity: $FAIL_COUNT of $TOTAL file(s) parsed with an ERROR node (tree-sitter-hew @ $LOCK_COMMIT):" >&2
    printf '%s\n' "$FAILURES" | sed 's/^/  /' >&2
    exit 1
fi

echo "grammar-parity: $TOTAL files parsed cleanly with tree-sitter-hew @ $LOCK_COMMIT"
