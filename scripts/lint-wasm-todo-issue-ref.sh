#!/usr/bin/env bash
# lint-wasm-todo-issue-ref.sh
#
# Reject WASM-TODO markers that do not carry an issue reference.
# Every actionable WASM-TODO in source code must use the form:
#   WASM-TODO(#NNN): <description>
#
# Rationale: bare WASM-TODO comments with no issue reference cannot be
# tracked or prioritised. Using WASM-TODO(#1451) (or another specific issue)
# lets the WASM parity backlog be managed in one place.
#
# Excluded paths (category labels, not actionable TODO markers):
#   - docs/                         — uses WASM-TODO as a table column label
#   - CONTRIBUTING.md               — documents the marker convention itself
#   - .github/                      — PR template references the form by example
#   - Makefile                      — contains DROP-TODO|WASM-TODO grep pattern literal
#   - wasm-capability-manifest.toml — uses WASM-TODO as tracking_label data values
#   - hew-capability-gen/src/       — struct/field docs reference "WASM-TODO backlog" by name
#   - hew-capability-gen/tests/     — test string literals match markdown headings
#   - this script itself
set -Eeuo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# Match any WASM-TODO that is NOT immediately followed by an open paren
# containing a hash-prefixed issue number.
# Positive reference form: WASM-TODO(#NNN):
# Rejected forms: WASM-TODO:, WASM-TODO backlog, WASM-TODO (anything without #NNN)
bad=$(git grep -nE 'WASM-TODO([^(]|\([^#]|\(#[^0-9])' -- \
    ':!scripts/lint-wasm-todo-issue-ref.sh' \
    ':!docs/' \
    ':!CONTRIBUTING.md' \
    ':!.github/' \
    ':!Makefile' \
    ':!wasm-capability-manifest.toml' \
    ':!hew-capability-gen/src/' \
    ':!hew-capability-gen/tests/' \
    2>/dev/null || true)

if [[ -n "$bad" ]]; then
    echo "lint-wasm-todo: found WASM-TODO comments without issue reference:" >&2
    echo "$bad" >&2
    echo "" >&2
    echo "Use the form: WASM-TODO(#NNN): <description>" >&2
    echo "Umbrella issue for WASM parity gaps: https://github.com/hew-lang/hew/issues/1451" >&2
    exit 1
fi

echo "lint-wasm-todo: ok" >&2
