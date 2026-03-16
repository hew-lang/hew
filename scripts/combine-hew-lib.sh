#!/usr/bin/env bash
# Combine libhew_runtime.a (staticlib) with all stdlib rlib .o files into
# a single libhew.a.  Stdlib rlibs contain only the crate's own objects
# (no bundled dependencies), so there are no duplicate symbols.
#
# Usage: scripts/combine-hew-lib.sh [target_dir]
#   target_dir defaults to target/debug

set -euo pipefail

TARGET_DIR="$(cd "${1:-target/debug}" && pwd)"
OUTPUT="${TARGET_DIR}/libhew.a"
RUNTIME="${TARGET_DIR}/libhew_runtime.a"
DEPS_DIR="${TARGET_DIR}/deps"

if [ ! -f "${RUNTIME}" ]; then
    echo "error: ${RUNTIME} not found" >&2
    exit 1
fi

WORKDIR=$(mktemp -d)
trap 'rm -rf "${WORKDIR}"' EXIT

# Extract the runtime staticlib (includes all shared deps: tokio, mio, etc.)
mkdir -p "${WORKDIR}/runtime"
(cd "${WORKDIR}/runtime" && ar x "${RUNTIME}")

# Extract .o files from each stdlib rlib (crate's own objects only, no deps).
# Rlibs live in target/*/deps/ and also contain lib.rmeta which we skip.
stdlib_count=0
obj_count=0
for rlib in "${DEPS_DIR}"/libhew_std_*.rlib; do
    [ -f "${rlib}" ] || continue
    subdir="${WORKDIR}/std_${stdlib_count}"
    mkdir -p "${subdir}"
    (cd "${subdir}" && ar x "${rlib}")
    # Remove metadata — only .o files go into the archive
    rm -f "${subdir}"/lib.rmeta "${subdir}"/*.rmeta
    obj_count=$((obj_count + $(find "${subdir}" -name '*.o' | wc -l)))
    stdlib_count=$((stdlib_count + 1))
done

if [ "${stdlib_count}" -eq 0 ]; then
    echo "error: no stdlib rlibs found in ${DEPS_DIR}" >&2
    exit 1
fi

# Combine everything into libhew.a
find "${WORKDIR}" -name '*.o' -print0 | xargs -0 ar rcs "${OUTPUT}"

echo "  libhew.a: runtime + ${stdlib_count} stdlib crates (${obj_count} objects) -> $(du -h "${OUTPUT}" | cut -f1)"
