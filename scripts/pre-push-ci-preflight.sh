#!/usr/bin/env bash
# Pre-push fast gate. Local-only quick check. CI is the comprehensive gate.
# Invoked via .git/hooks/pre-push.d/ci-preflight symlink.
#
# Compatible with Bash 3.2 (macOS default) and Bash 5+ (Linux).

set -Eeuo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

echo "pre-push: cargo fmt --all -- --check (fast gate; CI runs the comprehensive lane)" >&2

if ! cargo fmt --all -- --check; then
    echo "pre-push: cargo fmt failed — run \`cargo fmt --all\` and re-push." >&2
    exit 1
fi
