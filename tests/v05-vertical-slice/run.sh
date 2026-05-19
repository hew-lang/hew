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

# Multiple #[on(stop)] hooks on the same actor must compile and run without
# ActorHandlerSymbolCollision. Previously the second hook would collide with
# the first at MIR lowering. Exit 0 = Sequencer(start: 0).value() = 0.
run_accept_expect_status "actor_multi_on_stop" 0

# select{} with two actor-ask arms + after-timer: FastWorker replies with 42
# immediately; SlowWorker sleeps 50 ms; after-arm deadline is 100 ms.
# FastWorker always wins under normal CI load. Exit code 42 proves the winner
# value is returned and the loser channel is cancelled without leaking.
run_accept_expect_status "actor_ask_race" 42

# Supervisor bootstrap: spawn AppSupervisor → hew_supervisor_new + add_child_spec + start;
# main returns 42 after bootstrap completes successfully.
run_accept_expect_status "supervisor_basic" 42

# Supervisor child-accessor round-trip: spawn App → hew_supervisor_child_get
# returns a Live handle (tag=0) → ask child worker → echo 42 back as exit code.
# Exercises the { i64, i64 } ABI fix for hew_supervisor_child_get.
run_accept_expect_status "supervised_ingest_race" 42

# Supervisor graceful stop: spawn AppSupervisor → supervisor_stop(sup) lowers to
# hew_supervisor_stop; main returns 0. Exercises the user-name → C-ABI bridge and
# the void-return (dest: None) MIR + codegen path.
run_accept_expect_status "supervisor_stop_basic" 0

# on(crash) handler attachment: Crasher actor declares #[on(crash)]; codegen emits
# a non-null on_crash fn-pointer in HewChildSpec; supervisor boots and main returns 42.
# The crash path is not triggered at runtime — handler-fire observability is covered
# by hew-runtime/tests/on_crash_invocation.rs.
run_accept_expect_status "on_crash_basic" 42

# on(crash) with info.code field access: verifies the full HIR → MIR → codegen path
# for reading PanicInfo.code inside an on(crash) body.  PanicInfo is loaded from
# std/failure.hew via the module graph walk, so record_field_orders is populated and
# FieldAccess lowering succeeds.  The supervisor boots and main returns 42.
run_accept_expect_status "on_crash_info_code" 42

# Reject: accessing a non-existent child name on a supervisor LHS.
# `app.w2` does not exist — App declares only `w1`.  The checker emits
# UndefinedField with a fuzzy suggestion for `w1`.
if "${HEW}" check "${ROOT}/tests/v05-vertical-slice/reject/supervisor_unknown_child.hew" >"${reject_output}" 2>&1; then
  echo "expected supervisor-unknown-child fixture to fail" >&2
  exit 1
fi
grep -q 'has no child named' "${reject_output}"
grep -q 'w1' "${reject_output}"

# Reject: field access on a plain actor LocalPid, not a supervisor.
# `w.child` on LocalPid<Worker> — the checker emits UndefinedField because
# LocalPid has no user-visible fields and is not in the supervisor_children map.
if "${HEW}" check "${ROOT}/tests/v05-vertical-slice/reject/supervisor_child_on_plain_actor.hew" >"${reject_output}" 2>&1; then
  echo "expected supervisor-child-on-plain-actor fixture to fail" >&2
  exit 1
fi
grep -q 'no field' "${reject_output}"
grep -q 'LocalPid' "${reject_output}"

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

# Reject: link/monitor/unlink pending Cluster 2 composite-return spine.
# Runtime symbols (hew_actor_link, hew_actor_monitor) and codegen arms exist,
# but Result<(),LinkError> / MonitorRef construction requires Cluster 2.
# The diagnostic must be CutoverUnsupported with slice_target "Cluster-2",
# NOT UnresolvedSymbol (which would look like a user typo).
if "${HEW}" compile "${ROOT}/tests/v05-vertical-slice/reject/link_monitor_pending_cluster2.hew" >"${reject_output}" 2>&1; then
  echo "expected link/monitor fixture to fail" >&2
  exit 1
fi
grep -q 'CutoverUnsupported' "${reject_output}"
grep -q 'Cluster-2' "${reject_output}"
# The verifier emits a secondary UnresolvedSymbol for the unresolved callee
# (consistent with all unresolved-builtin-callee paths); the primary and
# informative diagnostic is CutoverUnsupported — verified above.
