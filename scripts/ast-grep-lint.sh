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
# --install-only provisions and verifies the pinned toolchain, then stops
# before the authority audit and the scan. CI jobs that merely need the
# binary present (the lifecycle evidence gates read
# .ast-grep/tool/bin/ast-grep directly) provision through this; the scan
# itself stays owned by `make structural-lint` in the lint job, so it runs
# exactly once per commit instead of once per consumer.
INSTALL_ONLY=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        --bootstrap) BOOTSTRAP=1; shift ;;
        --install-only) INSTALL_ONLY=1; shift ;;
        *) break ;;
    esac
done

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
if [[ "$INSTALL_ONLY" == 1 ]]; then
    echo "pinned ast-grep $AST_GREP_VERSION ready: $AST_GREP"
    exit 0
fi
cd "$REPO_ROOT"
python3 scripts/structural-authority-audit.py --ast-grep "$AST_GREP"
python3 scripts/tests/test_canonical_keyspace_lint.py "$AST_GREP"
python3 scripts/canonical-keyspace-lint.py --ast-grep "$AST_GREP"
exec "$AST_GREP" scan "$@"
