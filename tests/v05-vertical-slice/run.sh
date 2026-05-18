#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW="${ROOT}/target/debug/hew"

cargo build -q -p hew-cli

mkdir -p "${ROOT}/.tmp"
accept_output="${ROOT}/.tmp/v05-vertical-slice-accept-output.txt"
reject_output="${ROOT}/.tmp/v05-vertical-slice-reject-output.txt"
trap 'rm -f "${accept_output}" "${reject_output}"' EXIT

"${HEW}" compile --dump-mir raw "${ROOT}/tests/v05-vertical-slice/accept/string_return.hew" >"${accept_output}"
grep -q 'return_ty: String' "${accept_output}"

"${HEW}" compile --dump-mir raw "${ROOT}/tests/v05-vertical-slice/accept/01-arith.hew" >"${accept_output}"
grep -q 'return_ty: I64' "${accept_output}"

if "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/reject/unresolved_symbol.hew" >"${reject_output}" 2>&1; then
  echo "expected unresolved symbol fixture to fail" >&2
  exit 1
fi
grep -q 'UndefinedVariable' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/reject/use_after_consume.hew" >"${reject_output}" 2>&1; then
  echo "expected use-after-consume fixture to fail" >&2
  exit 1
fi
grep -q 'UseAfterConsume' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/reject/unresolved_inference.hew" >"${reject_output}" 2>&1; then
  echo "expected unresolved-inference fixture to fail" >&2
  exit 1
fi
grep -q 'UnresolvedInferenceVar' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/reject/unknown_named_type.hew" >"${reject_output}" 2>&1; then
  echo "expected unknown-named-type fixture to fail" >&2
  exit 1
fi
grep -q 'UnknownType' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/reject/unknown_named_tuple_type.hew" >"${reject_output}" 2>&1; then
  echo "expected unknown-named-tuple-type fixture to fail" >&2
  exit 1
fi
grep -q 'UnknownType' "${reject_output}"
if grep -q 'panicked at' "${reject_output}"; then
  echo "unknown-named-tuple-type fixture panicked instead of reporting a diagnostic" >&2
  exit 1
fi

if "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/reject/unknown_named_array_type.hew" >"${reject_output}" 2>&1; then
  echo "expected unknown-named-array-type fixture to fail" >&2
  exit 1
fi
grep -q 'UnknownType' "${reject_output}"
if grep -q 'panicked at' "${reject_output}"; then
  echo "unknown-named-array-type fixture panicked instead of reporting a diagnostic" >&2
  exit 1
fi

# Lambda-actor and Duplex surface: type-checker-level fixtures (slice 2).
# These use `hew check` rather than `compile` because the HIR codegen
# path does not yet support actor expressions (slice 4 concern).

# Accept: tell-shaped lambda actor call dispatch.
"${HEW}" check "${ROOT}/tests/v05-vertical-slice/accept/lambda_callable_tell.hew"

# Accept: ask-shaped lambda actor call dispatch.
"${HEW}" check "${ROOT}/tests/v05-vertical-slice/accept/lambda_callable_ask.hew"

# Accept: lambda actor recursive self-call via let-binding name (§5.9 ratification 2).
"${HEW}" check "${ROOT}/tests/v05-vertical-slice/accept/lambda_self_recursion.hew"

# Reject: non-Send message type (E_DUPLEX_NON_SEND).
if "${HEW}" check "${ROOT}/tests/v05-vertical-slice/reject/duplex_non_send.hew" >"${reject_output}" 2>&1; then
  echo "expected duplex-non-send fixture to fail" >&2
  exit 1
fi
grep -q 'E_DUPLEX_NON_SEND' "${reject_output}"

# Reject: removed <- operator (E_OPERATOR_REMOVED).
if "${HEW}" check "${ROOT}/tests/v05-vertical-slice/reject/lambda_arrow_operator.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-arrow-operator fixture to fail" >&2
  exit 1
fi
grep -q 'E_OPERATOR_REMOVED' "${reject_output}"

# Accept: .send() on a lambda-actor handle is now allowed (allowed-secondary surface).
# Lambda-actor handles are Duplex<Msg, Reply> underneath; `.send()` routes to
# hew_duplex_send, the same symbol as call-syntax.  The old reject/lambda_method_send.hew
# file is kept for reference but now passes hew check.
if ! "${HEW}" check "${ROOT}/tests/v05-vertical-slice/accept/lambda_method_send.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-method-send fixture to pass; got:" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# Reject: ask-shaped actor body return type mismatch (E_LAMBDA_RETURN_TYPE_MISMATCH).
if "${HEW}" check "${ROOT}/tests/v05-vertical-slice/reject/lambda_return_mismatch.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-return-mismatch fixture to fail" >&2
  exit 1
fi
grep -q 'E_LAMBDA_RETURN_TYPE_MISMATCH' "${reject_output}"

# Reject: actor body returns Duplex handle (E_LAMBDA_SELF_ESCAPE).
if "${HEW}" check "${ROOT}/tests/v05-vertical-slice/reject/lambda_self_escape.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-self-escape fixture to fail" >&2
  exit 1
fi
grep -q 'E_LAMBDA_SELF_ESCAPE' "${reject_output}"

# Reject: removed spawn-lambda syntax (E_LEGACY_SPAWN_LAMBDA_SYNTAX).
if "${HEW}" check "${ROOT}/tests/v05-vertical-slice/reject/spawn_lambda_legacy.hew" >"${reject_output}" 2>&1; then
  echo "expected spawn-lambda-legacy fixture to fail" >&2
  exit 1
fi
grep -q 'E_LEGACY_SPAWN_LAMBDA_SYNTAX' "${reject_output}"
