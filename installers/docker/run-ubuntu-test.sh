#!/usr/bin/env bash
# End-to-end Ubuntu 24.04 installation test.
# Builds from local release artifacts — run `make release` first.
#
# Usage (from repo root):
#   ./installers/docker/run-ubuntu-test.sh

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
cd "${REPO_DIR}"

echo "==> Step 1: Assembling test tarball from release artifacts"
bash installers/docker/assemble-test-tarball.sh

echo ""
echo "==> Step 2: Building Ubuntu 24.04 test image"
docker build \
    -f installers/docker/Dockerfile.ubuntu-test \
    --build-context tarball=dist/test-tarball \
    -t hew-ubuntu-test \
    .

echo ""
echo "==> Step 3: Running stdlib smoke test in Ubuntu 24.04 container"
docker run --rm hew-ubuntu-test

echo ""
echo "==> All checks passed on Ubuntu 24.04."
