#!/usr/bin/env bash
# Run the pinned ast-grep binary plus the authority inventory ratchet.
set -euo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOCK="$REPO_ROOT/tools/ast-grep.lock"
# shellcheck disable=SC1090
source "$LOCK"
TOOL_ROOT="$REPO_ROOT/.ast-grep/tool"
AST_GREP="$TOOL_ROOT/bin/ast-grep"
BOOTSTRAP=0
if [[ "${1:-}" == "--bootstrap" ]]; then BOOTSTRAP=1; shift; fi

if [[ "$BOOTSTRAP" == 1 ]]; then
    "$REPO_ROOT/scripts/build-ast-grep-lang.sh" --bootstrap
    if [[ ! -x "$AST_GREP" ]] || [[ "$("$AST_GREP" --version)" != "ast-grep $AST_GREP_VERSION" ]]; then
        command -v cargo >/dev/null || { echo "error: cargo is required to install pinned ast-grep" >&2; exit 1; }
        cargo install "$AST_GREP_CARGO_PACKAGE" --version "$AST_GREP_VERSION" --locked --root "$TOOL_ROOT"
    fi
else
    "$REPO_ROOT/scripts/build-ast-grep-lang.sh"
fi
[[ -x "$AST_GREP" ]] || {
    echo "error: pinned ast-grep $AST_GREP_VERSION is absent; run '$0 --bootstrap' once (network required)." >&2; exit 1;
}
[[ "$("$AST_GREP" --version)" == "ast-grep $AST_GREP_VERSION" ]] || {
    echo "error: pinned ast-grep version mismatch; remove .ast-grep/tool and bootstrap again" >&2; exit 1;
}
cd "$REPO_ROOT"
python3 scripts/structural-authority-audit.py --ast-grep "$AST_GREP"
exec "$AST_GREP" scan "$@"
