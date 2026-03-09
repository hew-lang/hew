#!/usr/bin/env bash
# Assembles a local release tarball from build/ artifacts for Docker testing.
# Must be run from the repo root after `make release`.
#
# Output: dist/test-tarball/ — a directory with the tarball layout:
#   bin/hew, bin/hew-codegen
#   lib/libhew_runtime.a, lib/libhew_std_*.a
#   std/**/*.hew

set -euo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
STAGING="${REPO_DIR}/dist/test-tarball"

echo "==> Assembling test tarball at ${STAGING}"

rm -rf "${STAGING}"
mkdir -p "${STAGING}/bin" "${STAGING}/lib" "${STAGING}/std"

# Binaries
install -m755 "${REPO_DIR}/target/release/hew" "${STAGING}/bin/hew"
install -m755 "${REPO_DIR}/hew-codegen/build/src/hew-codegen" "${STAGING}/bin/hew-codegen"

# Runtime library
install -m644 "${REPO_DIR}/target/release/libhew_runtime.a" "${STAGING}/lib/libhew_runtime.a"

# Stdlib package static libraries
found=0
for f in "${REPO_DIR}/target/release/libhew_std_"*.a; do
    [ -f "$f" ] || continue
    install -m644 "$f" "${STAGING}/lib/"
    found=$((found + 1))
done
echo "    stdlib staticlibs: ${found} .a files"

# Standard library sources (all .hew files, including subdirectories)
cp -r "${REPO_DIR}/std/." "${STAGING}/std/"
hew_count=$(find "${STAGING}/std" -name "*.hew" | wc -l)
echo "    stdlib sources:    ${hew_count} .hew files"

echo "==> Test tarball ready at ${STAGING}"
