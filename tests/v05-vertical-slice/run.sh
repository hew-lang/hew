#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW="${ROOT}/target/debug/hew"

cargo build -q -p hew-cli

"${HEW}" compile-v05 "${ROOT}/tests/v05-vertical-slice/accept/string_return.hew" \
  | grep -q 'hew.return : String'

"${HEW}" compile-v05 "${ROOT}/tests/v05-vertical-slice/accept/01-arith.hew" \
  | grep -q 'hew.return : int'

reject_output="$(mktemp)"
trap 'rm -f "${reject_output}"' EXIT

if "${HEW}" compile-v05 "${ROOT}/tests/v05-vertical-slice/reject/unresolved_symbol.hew" >"${reject_output}" 2>&1; then
  echo "expected unresolved symbol fixture to fail" >&2
  exit 1
fi
grep -q 'UnresolvedSymbol' "${reject_output}"

if "${HEW}" compile-v05 "${ROOT}/tests/v05-vertical-slice/reject/use_after_consume.hew" >"${reject_output}" 2>&1; then
  echo "expected use-after-consume fixture to fail" >&2
  exit 1
fi
grep -q 'UseAfterConsume' "${reject_output}"

if "${HEW}" compile-v05 "${ROOT}/tests/v05-vertical-slice/reject/unresolved_inference.hew" >"${reject_output}" 2>&1; then
  echo "expected unresolved-inference fixture to fail" >&2
  exit 1
fi
grep -q 'UnresolvedInferenceVar' "${reject_output}"

if "${HEW}" compile-v05 "${ROOT}/tests/v05-vertical-slice/reject/unknown_named_type.hew" >"${reject_output}" 2>&1; then
  echo "expected unknown-named-type fixture to fail" >&2
  exit 1
fi
grep -q 'UnknownType' "${reject_output}"
