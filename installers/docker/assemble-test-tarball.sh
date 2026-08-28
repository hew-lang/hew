#!/usr/bin/env bash
# Assembles a local release tarball from build/ artifacts for Docker testing.
# Must be run from the repo root after `make release`.
#
# Output: dist/test-tarball/ — a directory with the tarball layout:
#   bin/hew
#   lib/libhew.a and target-specific libraries
#   std/**/*.hew

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
STAGING="${REPO_DIR}/dist/test-tarball"

echo "==> Assembling test tarball at ${STAGING}"

if [ ! -x "${REPO_DIR}/build/bin/hew" ] || [ ! -f "${REPO_DIR}/build/lib/libhew.a" ]; then
    echo "Error: staged release artifacts not found. Run 'make release' first." >&2
    exit 1
fi

rm -rf "${STAGING}"
mkdir -p "${STAGING}/bin" "${STAGING}/lib" "${STAGING}/std"

# Binaries (codegen is embedded in the hew binary)
install -m755 "${REPO_DIR}/build/bin/hew" "${STAGING}/bin/hew"

# Complete release library layout, including WASI and native target subtrees.
cp -LR "${REPO_DIR}/build/lib/." "${STAGING}/lib/"
echo "    libhew.a: $(du -h "${STAGING}/lib/libhew.a" | cut -f1)"

# Standard library sources (all .hew files, including subdirectories)
cp -r "${REPO_DIR}/std/." "${STAGING}/std/"
hew_count=$(find "${STAGING}/std" -name "*.hew" | wc -l)
echo "    stdlib sources:    ${hew_count} .hew files"

echo "==> Test tarball ready at ${STAGING}"
