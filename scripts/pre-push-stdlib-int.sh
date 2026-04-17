#!/usr/bin/env bash
# scripts/pre-push-stdlib-int.sh
#
# Pre-push hook entry point for the stdlib int-surface lint.
# Invoked by .git/hooks/pre-push.d/stdlib-int-surface (symlink).
#
# Install:
#   mkdir -p .git/hooks/pre-push.d
#   ln -sf ../../../scripts/pre-push-stdlib-int.sh \
#          .git/hooks/pre-push.d/stdlib-int-surface
#
# See docs/stdlib-style-contract.md and scripts/lint-stdlib-int-surface.sh.

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
exec bash "$REPO_ROOT/scripts/lint-stdlib-int-surface.sh"
