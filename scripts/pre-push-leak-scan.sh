#!/usr/bin/env bash
# Pre-push orchestration-token leak gate — tracked source + commit messages.
# Invoked via .git/hooks/pre-push.d/leak-scan symlink.
#
# Install:
#   mkdir -p .git/hooks/pre-push.d
#   ln -sf ../../../scripts/pre-push-leak-scan.sh \
#          .git/hooks/pre-push.d/leak-scan
#
# Scans both tracked source files and commit-message bodies for orchestration
# tokens (lane IDs, Q-tags, PCA references, .tmp/ paths).
# See scripts/lint-orchestration-leak.sh for the full token catalogue.

set -Eeuo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"

echo "pre-push: lint-orchestration-leak (source files)" >&2
bash "$REPO_ROOT/scripts/lint-orchestration-leak.sh"

echo "pre-push: lint-orchestration-leak (commit messages)" >&2
bash "$REPO_ROOT/scripts/lint-orchestration-leak.sh" --scan-commits
