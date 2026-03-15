#!/usr/bin/env bash
# Combine libhew_runtime.a and all libhew_std_*.a into a single libhew.a.
set -euo pipefail

RELEASE_DIR="${1:-target/release}"
OUTPUT="${2:-${RELEASE_DIR}/libhew.a}"

WORKDIR=$(mktemp -d)
trap 'rm -rf "${WORKDIR}"' EXIT

echo "==> Combining runtime + stdlib into ${OUTPUT}"
idx=0
for archive in "${RELEASE_DIR}"/libhew_runtime.a "${RELEASE_DIR}"/libhew_std_*.a; do
    [ -f "${archive}" ] || continue
    name=$(basename "${archive}" .a)
    mkdir -p "${WORKDIR}/${name}"
    (cd "${WORKDIR}/${name}" && ar x "${archive}")
    idx=$((idx + 1))
done

# shellcheck disable=SC2046
find "${WORKDIR}" -name '*.o' -print0 | xargs -0 ar rcs "${OUTPUT}"

echo "    Archives: ${idx}, Output: $(du -h "${OUTPUT}" | cut -f1)"
