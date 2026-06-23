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

# Read push info from git's stdin: <local-ref> <local-sha1> <remote-ref> <remote-sha1>
# Derive commit range for each ref and scan commit messages with an explicit range.
# The explicit range avoids the fail-closed error that fires when origin/main is
# absent (e.g. first push to a new remote).
echo "pre-push: lint-orchestration-leak (commit messages)" >&2
_null_sha="0000000000000000000000000000000000000000"
while IFS=' ' read -r _local_ref _local_sha _remote_ref _remote_sha; do
    [[ -z "$_local_ref" ]] && continue
    [[ "$_local_sha" == "$_null_sha" ]] && continue  # branch deletion — nothing to scan
    if [[ "$_remote_sha" == "$_null_sha" ]]; then
        # New branch: scan all commits reachable from the local tip
        _range="$_local_sha"
    else
        _range="${_remote_sha}..${_local_sha}"
    fi
    bash "$REPO_ROOT/scripts/lint-orchestration-leak.sh" --scan-commits "$_range"
done
