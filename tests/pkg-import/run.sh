#!/usr/bin/env bash
# Cross-module package-import oracle: real .hew fixtures importing the
# in-tree `hew::testffi` package (tests/pkg-import/pkgs/testffi) through
# `hew run --pkg-path`. Exercises, end-to-end:
#   - imported-actor value asks (i32 / i64 / string / record replies)
#   - imported-type trait methods (rows/get/total/free on the handle)
#   - the prelude-shadowing record name (`type Result`)
#   - [native] auto-link: the package's Rust staticlib builds on demand
#   - local-actor asks coexisting with imported asks (regression guard)
# On macOS the trait-method demo re-runs under MallocScribble/GuardEdges to
# hold the handle's single-release contract.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW="${ROOT}/target/debug/hew"
DIR="${ROOT}/tests/pkg-import"
PKGS="${DIR}/pkgs"

cargo build -q -p hew-lib
cargo build -q -p hew-cli

mkdir -p "${ROOT}/.tmp"
actual_output="${ROOT}/.tmp/pkg-import-actual.txt"
trap 'rm -f "${actual_output}"' EXIT

fixtures=(
  imported_actor_ask_i32
  imported_actor_ask_i64
  imported_actor_ask_string
  imported_actor_ask_record
  imported_trait_method
  local_actor_ask_guard
)

for fixture in "${fixtures[@]}"; do
  if ! "${HEW}" run --pkg-path "${PKGS}" "${DIR}/${fixture}.hew" >"${actual_output}" 2>/dev/null; then
    echo "FAIL ${fixture}: hew run exited non-zero" >&2
    "${HEW}" run --pkg-path "${PKGS}" "${DIR}/${fixture}.hew" >&2 || true
    exit 1
  fi
  if ! diff -u "${DIR}/${fixture}.expected" "${actual_output}"; then
    echo "FAIL ${fixture}: output diverged from ${fixture}.expected" >&2
    exit 1
  fi
  echo "PASS ${fixture}"
done

# Memory-safety pass on the ask-reply + explicit-release path (macOS only:
# MallocScribble/MallocGuardEdges are libmalloc features).
if [[ "$(uname -s)" == "Darwin" ]]; then
  if ! MallocScribble=1 MallocGuardEdges=1 \
    "${HEW}" run --pkg-path "${PKGS}" "${DIR}/imported_trait_method.hew" \
    >"${actual_output}" 2>/dev/null; then
    echo "FAIL imported_trait_method under MallocScribble" >&2
    exit 1
  fi
  if ! diff -u "${DIR}/imported_trait_method.expected" "${actual_output}"; then
    echo "FAIL imported_trait_method: MallocScribble output diverged" >&2
    exit 1
  fi
  echo "PASS imported_trait_method (MallocScribble)"
fi

echo "pkg-import oracle: all fixtures pass"
