#!/usr/bin/env bash
# Pre-push: run the local CI preflight gate from the repository root.
# Called by git-multi-hook via .git/hooks/pre-push.d/ci-preflight symlink.
#
# Compatible with Bash 3.2 (macOS default) and Bash 5+ (Linux).

set -Eeuo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

echo "pre-push: running make ci-preflight" >&2

if ! make ci-preflight; then
    echo "pre-push: preflight failed — run \`make ci-preflight\` to see details, fix, then re-push. Never bypass with \`--no-verify\`." >&2
    exit 1
fi
