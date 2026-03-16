#!/usr/bin/env bash
# Combine libhew_runtime.a and all libhew_std_*.a into a single libhew.a.
# Usage: scripts/combine-hew-lib.sh [target_dir]
#   target_dir defaults to target/debug

set -euo pipefail

TARGET_DIR="$(cd "${1:-target/debug}" && pwd)"
OUTPUT="${TARGET_DIR}/libhew.a"
RUNTIME="${TARGET_DIR}/libhew_runtime.a"

if [ ! -f "${RUNTIME}" ]; then
    echo "error: ${RUNTIME} not found" >&2
    exit 1
fi

WORKDIR=$(mktemp -d)
trap 'rm -rf "${WORKDIR}"' EXIT

idx=0
for archive in "${TARGET_DIR}"/libhew_runtime.a "${TARGET_DIR}"/libhew_std_*.a; do
    [ -f "${archive}" ] || continue
    mkdir -p "${WORKDIR}/${idx}"
    (cd "${WORKDIR}/${idx}" && ar x "${archive}")
    idx=$((idx + 1))
done

# shellcheck disable=SC2046
find "${WORKDIR}" -name '*.o' -print0 | xargs -0 ar rcs "${OUTPUT}"

echo "  libhew.a: ${idx} archives combined -> $(du -h "${OUTPUT}" | cut -f1)"
