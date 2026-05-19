#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW="${ROOT}/target/debug/hew"

cargo build -q -p hew-cli

mkdir -p "${ROOT}/.tmp"
accept_output="${ROOT}/.tmp/v05-vertical-slice-accept-output.txt"
reject_output="${ROOT}/.tmp/v05-vertical-slice-reject-output.txt"
stdout_output="${ROOT}/.tmp/v05-vertical-slice.stdout"
stderr_output="${ROOT}/.tmp/v05-vertical-slice.stderr"
trap 'rm -f "${accept_output}" "${reject_output}" "${stdout_output}" "${stderr_output}"' EXIT

compile_accept() {
  local fixture="$1"
  "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/accept/${fixture}.hew" >"${accept_output}" 2>&1
}

run_accept_expect_status() {
  local fixture="$1"
  local expected_status="$2"
  compile_accept "${fixture}"
  local bin="${ROOT}/.tmp/compile-out/${fixture}"
  local status=0
  if bash -c '"$1" >"$2" 2>"$3"' _ "${bin}" "${stdout_output}" "${stderr_output}" 2>/dev/null; then
    status=0
  else
    status=$?
  fi
  if [[ "${status}" -ne "${expected_status}" ]]; then
    echo "expected ${fixture} to exit ${expected_status}, got ${status}" >&2
    cat "${accept_output}" >&2
    cat "${stdout_output}" >&2
    cat "${stderr_output}" >&2
    exit 1
  fi
}

run_accept_expect_stdout() {
  local fixture="$1"
  run_accept_expect_status "${fixture}" 0
  diff -u "${ROOT}/tests/v05-vertical-slice/accept/${fixture}.expected" "${stdout_output}"
}

"${HEW}" compile --dump-mir raw "${ROOT}/tests/v05-vertical-slice/accept/string_return.hew" >"${accept_output}"
grep -q 'return_ty: String' "${accept_output}"

"${HEW}" compile --dump-mir raw "${ROOT}/tests/v05-vertical-slice/accept/01-arith.hew" >"${accept_output}"
grep -q 'return_ty: I64' "${accept_output}"

"${HEW}" compile "${ROOT}/tests/v05-vertical-slice/accept/arith_call.hew" >"${accept_output}" 2>&1
arith_bin="${ROOT}/.tmp/compile-out/arith_call"
if "${arith_bin}" >>"${accept_output}" 2>&1; then
  arith_status=0
else
  arith_status=$?
fi
if [[ "${arith_status}" -ne 5 ]]; then
  echo "expected arith_call fixture to exit 5, got ${arith_status}" >&2
  cat "${accept_output}" >&2
  exit 1
fi

"${HEW}" compile "${ROOT}/tests/v05-vertical-slice/accept/hello_println.hew" >"${accept_output}" 2>&1
hello_println_bin="${ROOT}/.tmp/compile-out/hello_println"
hello_stdout="${ROOT}/.tmp/hello_println.stdout"
trap 'rm -f "${accept_output}" "${reject_output}" "${stdout_output}" "${stderr_output}" "${hello_stdout}"' EXIT
"${hello_println_bin}" >"${hello_stdout}"
diff -u "${ROOT}/tests/v05-vertical-slice/accept/hello_println.expected" "${hello_stdout}"

run_accept_expect_status "assert" 0
run_accept_expect_status "assert_eq" 0
run_accept_expect_status "assert_ne" 0
run_accept_expect_status "sleep_ms" 0

run_accept_expect_status "assert_eq_fail" 134
grep -q 'assertion failed: assert_eq(4, 5)' "${stderr_output}"

run_accept_expect_status "exit_42" 42

# Actor body: increment(10) + increment(32) = 42.
run_accept_expect_status "actor_counter" 42

# Q87 slice 1 regression: same actor body as `actor_counter`, but the two
# `receive fn`s appear in reversed source order. Pre-Q87 the source-order
# `.enumerate()` msg_id derivation would have re-numbered the protocol and
# (in a multi-actor / multi-version setting) flipped the wire ABI. After
# slice 1 msg_ids are derived from the fully-qualified handler name, so this
# fixture must still exit 42.
run_accept_expect_status "actor_counter_reorder" 42

# Actor body with init + on(start): initial=9, boot increments to 10, increment(32) = 42.
# The exit code being 42 (not 41) proves on(start) fired before the first message.
run_accept_expect_status "actor_counter_init" 42

# Actor body with init + on(start) + on(stop): initial=9, boot increments to 10,
# increment(32) = 42. The on(stop) handler zeroes count after the final ask —
# the total() ask completes before teardown, so the returned value is 42 regardless.
# Exit code 42 proves the actor ran its full lifecycle (spawn → start → messages → stop).
run_accept_expect_status "actor_on_stop" 42

# select{} with two actor-ask arms + after-timer: FastWorker replies with 42
# immediately; SlowWorker sleeps 50 ms; after-arm deadline is 100 ms.
# FastWorker always wins under normal CI load. Exit code 42 proves the winner
# value is returned and the loser channel is cancelled without leaking.
run_accept_expect_status "actor_ask_race" 42

run_accept_expect_stdout "print_int"
run_accept_expect_stdout "print_bool"
run_accept_expect_stdout "print_f64"

run_accept_expect_status "panic" 101
grep -q 'panic fixture' "${stderr_output}"

run_accept_expect_status "directory_module_call" 7

if "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/reject/unresolved_symbol.hew" >"${reject_output}" 2>&1; then
  echo "expected unresolved symbol fixture to fail" >&2
  exit 1
fi
grep -q 'undefined variable' "${reject_output}"

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
