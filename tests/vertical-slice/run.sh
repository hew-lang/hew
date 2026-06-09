#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW="${ROOT}/target/debug/hew"

# libhew.a is the combined runtime+stdlib static library linked into native outputs.
cargo build -q -p hew-lib
cargo build -q -p hew-cli

mkdir -p "${ROOT}/.tmp"
accept_output="${ROOT}/.tmp/vertical-slice-accept-output.txt"
reject_output="${ROOT}/.tmp/vertical-slice-reject-output.txt"
stdout_output="${ROOT}/.tmp/vertical-slice.stdout"
stderr_output="${ROOT}/.tmp/vertical-slice.stderr"
trap 'rm -f "${accept_output}" "${reject_output}" "${stdout_output}" "${stderr_output}"' EXIT

compile_accept() {
  local fixture="$1"
  "${HEW}" compile "${ROOT}/tests/vertical-slice/accept/${fixture}.hew" >"${accept_output}" 2>&1
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
  diff -u "${ROOT}/tests/vertical-slice/accept/${fixture}.expected" "${stdout_output}"
}

"${HEW}" compile --dump-mir raw "${ROOT}/tests/vertical-slice/accept/string_return.hew" >"${accept_output}"
grep -q 'return_ty: String' "${accept_output}"

"${HEW}" compile --dump-mir raw "${ROOT}/tests/vertical-slice/accept/01-arith.hew" >"${accept_output}"
grep -q 'return_ty: I64' "${accept_output}"

"${HEW}" compile "${ROOT}/tests/vertical-slice/accept/arith_call.hew" >"${accept_output}" 2>&1
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

"${HEW}" compile "${ROOT}/tests/vertical-slice/accept/hello_println.hew" >"${accept_output}" 2>&1
hello_println_bin="${ROOT}/.tmp/compile-out/hello_println"
hello_stdout="${ROOT}/.tmp/hello_println.stdout"
trap 'rm -f "${accept_output}" "${reject_output}" "${stdout_output}" "${stderr_output}" "${hello_stdout}"' EXIT
"${hello_println_bin}" >"${hello_stdout}"
diff -u "${ROOT}/tests/vertical-slice/accept/hello_println.expected" "${hello_stdout}"

run_accept_expect_status "assert" 0

# ---------------------------------------------------------------------------
# W4.002: HIR pre-pass Item::Machine coverage for FC-P0 sibling walkers
# ---------------------------------------------------------------------------

# Reject: `.recv()` inside a machine transition body must be rejected at
# HIR-lower time when compiling for wasm32. Exercises the Item::Machine arm
# added to `check_wasm_blocking_recv_gate`.
if "${HEW}" compile --target wasm32-unknown-unknown \
    "${ROOT}/tests/vertical-slice/reject/machine_wasm_blocking_recv.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected machine_wasm_blocking_recv fixture to fail" >&2
  exit 1
fi
grep -q 'Blocking channel receive operations are not supported on WASM32' "${reject_output}"

# Reject: `fork child = worker(42)` inside a machine state entry block must
# be rejected by the task-gate HIR pre-pass. Exercises the Item::Machine arm
# added to `check_task_gates`. Uses `hew compile` (not `hew check`) because
# the HIR pre-pass runs as part of the compile pipeline, not the type-check-
# only path.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/machine_task_gate_fork_args.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected machine_task_gate_fork_args fixture to fail" >&2
  exit 1
fi
grep -q 'TaskSpawnSignatureUnsupported\|spawned function must have zero arguments' "${reject_output}"

# Reject: spawned closures must not capture non-Send values. This fixture uses
# a real Checker-produced `Rc<i64>` capture fact and asserts the targeted HIR
# diagnostic rather than unrelated Rc construction or lowering diagnostics.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/spawned_closure_non_send_capture.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected spawned_closure_non_send_capture fixture to fail" >&2
  exit 1
fi
grep -q "spawned closure captures non-Send value 'r'" "${reject_output}"

run_accept_expect_status "assert_eq" 0
run_accept_expect_status "assert_ne" 0
run_accept_expect_status "sleep_ms" 0

run_accept_expect_status "assert_eq_fail" 134
grep -q 'assertion failed: assert_eq(4, 5)' "${stderr_output}"

run_accept_expect_status "exit_42" 42

# defer: basic (no effect on return), executes (exit override), LIFO, block scope
run_accept_expect_status "defer_basic" 7
run_accept_expect_status "defer_executes" 42
run_accept_expect_status "defer_lifo" 1
run_accept_expect_status "defer_block_scope" 7
# defer: early-return unwind, nested scopes, no double-run on tail
run_accept_expect_status "defer_early_return" 42
run_accept_expect_status "defer_nested_early_return" 10
run_accept_expect_status "defer_no_double_run" 5
# defer: tail-return value secured before scope-exit defers mutate referenced var
run_accept_expect_status "defer_secures_tail_return" 5
# defer: block-expression result secured before scope-exit defers mutate referenced var
run_accept_expect_status "defer_secures_block_result" 5
# explicit `return` must seal the basic block; post-return code must not run
run_accept_expect_status "return_terminates_early" 7

# ---------------------------------------------------------------------------
# W3.030 Stage 4 — User-resource `close` end-to-end coverage.
#
# `#[resource]` types declare `close` in a sibling inherent-impl block
# (Q-α-B ratified surface). The implicit drop contract dispatches
# `<T>::close` on every scope-exit path through the unified W3.021
# `ScopeExitPlan` stream. Stage 1 added HIR diagnostics for the surface;
# Stage 2 wired the codegen typed `DropDispatch::{RuntimeSymbol, UserFn}`
# dispatcher plus the no-third-arm verifier. Stage 4 ships e2e proof.
# ---------------------------------------------------------------------------

# V1 — close fires on normal tail return (`work` then close prints `7`).
run_accept_expect_stdout "user_resource_close_normal_return"

# V2 — close fires on the early-return exit path (run(true) -> 42 via
# `return`, close runs before the function exits).
run_accept_expect_stdout "user_resource_close_early_return"

# V8 — multiple distinct `#[resource]` types close in LIFO declaration
# order (`l` declared after `c`; `Lock::close` then `Conn::close`).
run_accept_expect_stdout "user_resource_close_multiple_types"

# V14 — WASI/WASM parity: V1 must compile under wasm32-unknown-unknown
# through the shared codegen pipeline. Behavioural parity is inherited
# from the shared MIR->LLVM lower; this gate pins that the wasm target
# does not regress on the user-resource drop path.
"${HEW}" compile --target wasm32-unknown-unknown \
  "${ROOT}/tests/vertical-slice/accept/user_resource_close_normal_return.hew" \
  >"${accept_output}" 2>&1
test -s "${ROOT}/.tmp/compile-out/user_resource_close_normal_return.wasm"

# V10 — `#[resource]` without any `close` body -> ResourceMissingClose.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/user_resource_missing_close.hew" \
    >"${reject_output}" 2>&1; then
  echo "W3.030 V10: expected user_resource_missing_close to fail" >&2
  exit 1
fi
grep -q 'ResourceMissingClose' "${reject_output}"

# V11 — inline `TypeBodyItem::Method` close on a `#[resource]` ->
# ResourceCloseSourceUnsupported (Q-α-B).
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/user_resource_inline_close_source.hew" \
    >"${reject_output}" 2>&1; then
  echo "W3.030 V11: expected user_resource_inline_close_source to fail" >&2
  exit 1
fi
grep -q 'ResourceCloseSourceUnsupported' "${reject_output}"

# V12 — inherent-impl `close` returning non-unit -> ResourceCloseMustReturnUnit
# (Q-β-C). Fallible cleanup composes via `defer`, not a non-unit close.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/user_resource_close_non_unit_return.hew" \
    >"${reject_output}" 2>&1; then
  echo "W3.030 V12: expected user_resource_close_non_unit_return to fail" >&2
  exit 1
fi
grep -q 'ResourceCloseMustReturnUnit' "${reject_output}"

# Actor body: increment(10) + increment(32) = 42.
run_accept_expect_status "actor_counter" 42

# Actor-only wasm smoke: `actor_counter` has actor state, so wasm compilation
# must keep resolving the actor state drop/clone setter pair.
"${HEW}" compile --target wasm32-unknown-unknown \
  "${ROOT}/tests/vertical-slice/accept/actor_counter.hew" >"${accept_output}" 2>&1
test -s "${ROOT}/.tmp/compile-out/actor_counter.wasm"

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

# Discarded link()/monitor() calls lower to hew_actor_link / hew_actor_monitor
# with dest=None and reach codegen.
run_accept_expect_status "link_monitor_discarded" 0

# on(crash) handler attachment: Crasher actor declares #[on(crash)]; codegen emits
# a non-null on_crash fn-pointer in HewChildSpec; supervisor boots and main returns 42.
# The crash path is not triggered at runtime — handler-fire observability is covered
# by hew-runtime/tests/on_crash_invocation.rs.
run_accept_expect_status "on_crash_basic" 42

# on(crash) with info.code field access: verifies the full HIR → MIR → codegen path
# for reading CrashInfo.code inside an on(crash) body.  CrashInfo is loaded from
# std/failure.hew via the module graph walk, so record_field_orders is populated and
# FieldAccess lowering succeeds.  The supervisor boots and main returns 42.
run_accept_expect_status "on_crash_info_code" 42

# `#[max_heap(N)]` wire-through — direct spawn path:
#   1. MIR dump confirms ActorLayout carries max_heap_bytes: Some(65536),
#      proving the annotation propagated from HIR through MIR.
#   2. Binary exits 42, proving codegen routed the spawn through
#      hew_actor_spawn_opts (arena_cap_bytes=65536) without breaking
#      actor functionality.
"${HEW}" compile --dump-mir raw "${ROOT}/tests/vertical-slice/accept/actor_max_heap_basic.hew" >"${accept_output}" 2>&1
grep -q 'max_heap_bytes: Some(' "${accept_output}"
grep -q '65536' "${accept_output}"
run_accept_expect_status "actor_max_heap_basic" 42

# `#[max_heap(N)]` wire-through — supervisor child path:
#   1. MIR dump confirms the supervisor bootstrap's SpawnActor instruction
#      carries max_heap_bytes: Some(131072), proving the post-loop pass
#      mirrored the cap from ActorLayout into SupervisorChildLayout, and
#      codegen emitted it into HewChildSpec.arena_cap_bytes.
#   2. Binary exits 42, proving the supervisor bootstrap path is
#      unaffected.
"${HEW}" compile --dump-mir raw "${ROOT}/tests/vertical-slice/accept/supervisor_max_heap.hew" >"${accept_output}" 2>&1
grep -q 'max_heap_bytes: Some(' "${accept_output}"
grep -q '131072' "${accept_output}"
run_accept_expect_status "supervisor_max_heap" 42

# Reject: accessing a non-existent child name on a supervisor LHS.
# `app.w2` does not exist — App declares only `w1`.  The checker emits
# UndefinedField with a fuzzy suggestion for `w1`.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/supervisor_unknown_child.hew" >"${reject_output}" 2>&1; then
  echo "expected supervisor-unknown-child fixture to fail" >&2
  exit 1
fi
grep -q 'has no child named' "${reject_output}"
grep -q 'w1' "${reject_output}"

# Reject: field access on a plain actor LocalPid, not a supervisor.
# `w.child` on LocalPid<Worker> — the checker emits UndefinedField because
# LocalPid has no user-visible fields and is not in the supervisor_children map.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/supervisor_child_on_plain_actor.hew" >"${reject_output}" 2>&1; then
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

if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/unresolved_symbol.hew" >"${reject_output}" 2>&1; then
  echo "expected unresolved symbol fixture to fail" >&2
  exit 1
fi
grep -q 'undefined variable' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/use_after_consume.hew" >"${reject_output}" 2>&1; then
  echo "expected use-after-consume fixture to fail" >&2
  exit 1
fi
grep -q 'UseAfterConsume' "${reject_output}"

# Reject: defer body references a binding that was moved/consumed earlier
# in the scope. The dataflow lattice must surface `UseAfterConsume` for
# the defer's lexical reference — Q205-B fail-closed boundary.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/defer_uses_moved_binding.hew" >"${reject_output}" 2>&1; then
  echo "expected defer-uses-moved-binding fixture to fail" >&2
  exit 1
fi
grep -q 'UseAfterConsume' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/unresolved_inference.hew" >"${reject_output}" 2>&1; then
  echo "expected unresolved-inference fixture to fail" >&2
  exit 1
fi
grep -q 'UnresolvedInferenceVar' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/unknown_named_type.hew" >"${reject_output}" 2>&1; then
  echo "expected unknown-named-type fixture to fail" >&2
  exit 1
fi
grep -q 'UnknownType' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/unknown_named_tuple_type.hew" >"${reject_output}" 2>&1; then
  echo "expected unknown-named-tuple-type fixture to fail" >&2
  exit 1
fi
grep -q 'UnknownType' "${reject_output}"
if grep -q 'panicked at' "${reject_output}"; then
  echo "unknown-named-tuple-type fixture panicked instead of reporting a diagnostic" >&2
  exit 1
fi

if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/unknown_named_array_type.hew" >"${reject_output}" 2>&1; then
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
"${HEW}" check "${ROOT}/tests/vertical-slice/accept/lambda_callable_tell.hew"

# Accept: ask-shaped lambda actor call dispatch.
"${HEW}" check "${ROOT}/tests/vertical-slice/accept/lambda_callable_ask.hew"

# Accept: lambda actor recursive self-call via let-binding name (§5.9 ratification 2).
"${HEW}" check "${ROOT}/tests/vertical-slice/accept/lambda_self_recursion.hew"

# Reject: non-Send message type (E_DUPLEX_NON_SEND).
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/duplex_non_send.hew" >"${reject_output}" 2>&1; then
  echo "expected duplex-non-send fixture to fail" >&2
  exit 1
fi
grep -q 'E_DUPLEX_NON_SEND' "${reject_output}"

# Reject: removed <- operator (E_OPERATOR_REMOVED).
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/lambda_arrow_operator.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-arrow-operator fixture to fail" >&2
  exit 1
fi
grep -q 'E_OPERATOR_REMOVED' "${reject_output}"

# Accept: .send() on a lambda-actor handle is now allowed (allowed-secondary surface).
# Lambda-actor handles are Duplex<Msg, Reply> underneath; `.send()` routes to
# hew_duplex_send, the same symbol as call-syntax.  The old reject/lambda_method_send.hew
# file is kept for reference but now passes hew check.
if ! "${HEW}" check "${ROOT}/tests/vertical-slice/accept/lambda_method_send.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-method-send fixture to pass; got:" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# Reject: ask-shaped actor body return type mismatch (E_LAMBDA_RETURN_TYPE_MISMATCH).
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/lambda_return_mismatch.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-return-mismatch fixture to fail" >&2
  exit 1
fi
grep -q 'E_LAMBDA_RETURN_TYPE_MISMATCH' "${reject_output}"

# Reject: actor body returns Duplex handle (E_LAMBDA_SELF_ESCAPE).
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/lambda_self_escape.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-self-escape fixture to fail" >&2
  exit 1
fi
grep -q 'E_LAMBDA_SELF_ESCAPE' "${reject_output}"

# Reject: removed spawn-lambda syntax (E_SPAWN_LAMBDA_SYNTAX_REMOVED).
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/spawn_lambda_removed.hew" >"${reject_output}" 2>&1; then
  echo "expected spawn-lambda-removed fixture to fail" >&2
  exit 1
fi
grep -q 'E_SPAWN_LAMBDA_SYNTAX_REMOVED' "${reject_output}"

# Reject: value-needed monitor() still needs Cluster 2 MonitorRef construction.
# Discarded statement-position calls compile, but expression contexts remain
# fail-closed at the MIR producer boundary.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/link_monitor_pending_cluster2.hew" >"${reject_output}" 2>&1; then
  echo "expected link/monitor fixture to fail" >&2
  exit 1
fi
grep -q 'NotYetImplemented' "${reject_output}"
grep -q 'MonitorRef' "${reject_output}"

# ---------------------------------------------------------------------------
# scope{} / fork — fail-closed surface pins
# ---------------------------------------------------------------------------

# Accept: actor-handler `scope { fork { worker(); } }` runs through the W4.010
# TaskEntry adapter for no-argument unit-returning free functions.
run_accept_expect_stdout "free_fn_actor_scope_spawn"

# Reject: top-level/default-callconv free-function task spawn has no enclosing
# ctx-bearing execution context to forward to the task wrapper. This must fail
# at MIR-lower time, before codegen.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/free_fn_scope_spawn_in_default.hew" >"${reject_output}" 2>&1; then
  echo "expected free-fn-scope-spawn-in-default fixture to fail" >&2
  exit 1
fi
grep -q 'E_NOT_YET_IMPLEMENTED' "${reject_output}"
grep -qF "cannot spawn \`worker\` from \`main\`" "${reject_output}"
grep -qF 'ctx-bearing execution context' "${reject_output}"
grep -qF 'W4.010-followup-caller-ctx-routing' "${reject_output}"

# Reject: generic free-function task spawn stays fail-closed. The caller is an
# actor handler so the generic diagnostic is not masked by the caller-context
# gate.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/free_fn_scope_spawn_generic.hew" >"${reject_output}" 2>&1; then
  echo "expected free-fn-scope-spawn-generic fixture to fail" >&2
  exit 1
fi
grep -q 'E_NOT_YET_IMPLEMENTED' "${reject_output}"
grep -qF 'generic free-function task spawning' "${reject_output}"

# Reject: `fork name = expr` outside any scope{} body.
# The type checker rejects this before HIR lowering — the construct is
# parser-only in the current build.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/fork_outside_scope.hew" >"${reject_output}" 2>&1; then
  echo "expected fork-outside-scope fixture to fail" >&2
  exit 1
fi
grep -qF 'parser-only in this build' "${reject_output}"

# Reject: removed `scope |s| { s.launch / s.spawn / s.cancel }` surface.
# Pins LESSONS row reject-scope-fork-collapse: the handle-based scope API was
# removed; the parser emits a targeted diagnostic directing users to the new
# `scope { fork name = call(...); }` form.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/scope_handle_legacy_launch.hew" >"${reject_output}" 2>&1; then
  echo "expected scope-handle-legacy-launch fixture to fail" >&2
  exit 1
fi
grep -qF "scope |s| { s.launch / s.spawn / s.cancel }' has been removed" "${reject_output}"

# Reject: fork-spawned callee returns a non-Unit value (here i64).
# Pins the fail-closed boundary at hew-mir/src/lower.rs direct_no_arg_unit_callee gate.
# Moves to accept/ when S2 lands value-bearing task propagation.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/fork_non_unit_return.hew" >"${reject_output}" 2>&1; then
  echo "expected fork-non-unit-return fixture to fail" >&2
  exit 1
fi
grep -q 'E_NOT_YET_IMPLEMENTED' "${reject_output}"
grep -qF 'spawned call' "${reject_output}"
grep -qF 'no-argument functions returning unit' "${reject_output}"

# Reject: fork-spawned callee takes arguments.
# Same gate as fork_non_unit_return; pins the no-args boundary.
# Moves to accept/ when S5 lands argument-bearing fork callees.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/fork_with_args.hew" >"${reject_output}" 2>&1; then
  echo "expected fork-with-args fixture to fail" >&2
  exit 1
fi
grep -q 'E_NOT_YET_IMPLEMENTED' "${reject_output}"
grep -qF 'spawned call' "${reject_output}"
grep -qF 'no-argument functions returning unit' "${reject_output}"

# Reject: `fork { ... }` block with multiple statements.
# Pins the fail-closed boundary at hew-mir/src/lower.rs:lower_fork_block_task.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/fork_multi_stmt.hew" >"${reject_output}" 2>&1; then
  echo "expected fork-multi-stmt fixture to fail" >&2
  exit 1
fi
grep -q 'E_NOT_YET_IMPLEMENTED' "${reject_output}"
grep -qF 'fork block cancellation child' "${reject_output}"
grep -qF 'exactly one statement' "${reject_output}"

# Reject: `after(duration) { ... }` with a non-empty timeout body.
# Pins the fail-closed boundary at hew-mir/src/lower.rs:lower_scope_deadline.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/scope_deadline_body.hew" >"${reject_output}" 2>&1; then
  echo "expected scope-deadline-body fixture to fail" >&2
  exit 1
fi
grep -q 'E_NOT_YET_IMPLEMENTED' "${reject_output}"
grep -qF 'scope deadline body' "${reject_output}"
grep -qF 'non-empty timeout bodies remain fail-closed' "${reject_output}"

# Reject: `for x in non_range_iterable` — only integer Range (a..b, a..=b) is
# supported in the current vertical slice.  Pins the fail-closed boundary at
# hew-hir/src/lower.rs non-range iterable arm.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/for_non_range_iterable.hew" >"${reject_output}" 2>&1; then
  echo "expected for-non-range-iterable fixture to fail" >&2
  exit 1
fi
grep -q 'E_NOT_YET_IMPLEMENTED' "${reject_output}"
grep -qF 'non-Range iterable' "${reject_output}"

# ---------------------------------------------------------------------------
# gen{} checker — typed generator blocks
# ---------------------------------------------------------------------------

# Accept: gen{} with yield expressions outside actor receive type-checks cleanly.
# HIR/MIR lowering is still fail-closed (no coroutine scheduler); hew check
# exercises the type-checker accept path.
if ! "${HEW}" check "${ROOT}/tests/vertical-slice/accept/gen_block_outside_receive.hew" >"${reject_output}" 2>&1; then
  echo "expected gen-block-outside-receive fixture to pass hew check; got:" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# Reject: empty gen{} has no yield expressions; yield type cannot be inferred.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/gen_block_empty.hew" >"${reject_output}" 2>&1; then
  echo "expected gen-block-empty fixture to fail" >&2
  exit 1
fi
grep -q 'E_EMPTY_GENERATOR' "${reject_output}"

# Reject: gen{} inside an actor receive handler is permanently forbidden.
# Pins GenBlockInActorReceive from fa8e8c64.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/genblock_in_actor_receive.hew" >"${reject_output}" 2>&1; then
  echo "expected genblock-in-actor-receive fixture to fail" >&2
  exit 1
fi
grep -q 'E_GENBLOCK_IN_ACTOR_RECEIVE' "${reject_output}"

# Accept: gen{} with a tail expression but no yield — Return component inferred as i64.
# Exercises the return_var inference path; E_EMPTY_GENERATOR must NOT fire.
if ! "${HEW}" check "${ROOT}/tests/vertical-slice/accept/gen_block_final_expr_returns.hew" >"${reject_output}" 2>&1; then
  echo "expected gen-block-final-expr-returns fixture to pass; got:" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# Accept: gen{} with explicit `return` but no yield — Return component inferred.
# Exercises Stmt::Return extraction of the R component from Generator<Y, R>.
if ! "${HEW}" check "${ROOT}/tests/vertical-slice/accept/gen_block_explicit_return.hew" >"${reject_output}" 2>&1; then
  echo "expected gen-block-explicit-return fixture to pass; got:" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# Accept: gen{} with both yield expressions and a tail-expression return.
# Both Yield and Return are inferred independently; no error.
if ! "${HEW}" check "${ROOT}/tests/vertical-slice/accept/gen_block_yields_and_returns.hew" >"${reject_output}" 2>&1; then
  echo "expected gen-block-yields-and-returns fixture to pass; got:" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# Reject: yield expressions with incompatible types — type mismatch (not EmptyGenerator).
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/gen_block_yield_type_mismatch.hew" >"${reject_output}" 2>&1; then
  echo "expected gen-block-yield-type-mismatch fixture to fail" >&2
  exit 1
fi
grep -q 'type mismatch' "${reject_output}"

# Reject: bare yield at function scope (not inside gen{}) — YieldOutsideGenerator.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/yield_outside_gen.hew" >"${reject_output}" 2>&1; then
  echo "expected yield-outside-gen fixture to fail" >&2
  exit 1
fi
grep -q 'outside of generator' "${reject_output}"

# ---------------------------------------------------------------------------
# Sink<T> / Stream<T> Wire-capability admissibility gate
# ---------------------------------------------------------------------------

# Accept: Sink<i64> and Stream<i64> pass the Wire-capability checker.
# i64 derives Encode + Decode (primitive Wire type); the admissibility gate
# must not reject these declarations.
if ! "${HEW}" check "${ROOT}/tests/vertical-slice/accept/sink_i64_typed.hew" >"${reject_output}" 2>&1; then
  echo "expected sink_i64_typed fixture to pass hew check; got:" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# Reject: Sink<LocalPid<Foo>> payload does not implement Encode + Decode.
# LocalPid derives Send + Sync + Copy + Clone but is not Wire-serialisable.
# The Wire-capability admissibility gate must emit SinkPayloadNotWire.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/sink_non_wire_payload.hew" >"${reject_output}" 2>&1; then
  echo "expected sink_non_wire_payload fixture to fail" >&2
  exit 1
fi
grep -qF 'Encode + Decode' "${reject_output}"

# Reject: literal payload subpattern in a constructor match arm.
# Shape::Line(1) must be rejected at check time with UnsupportedPayloadSubpattern
# rather than silently lowered as a wildcard that matches any payload value.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/enum_payload_literal_subpattern.hew" >"${reject_output}" 2>&1; then
  echo "expected enum-payload-literal-subpattern fixture to fail" >&2
  exit 1
fi
grep -q 'payload subpattern' "${reject_output}"
grep -q 'literal' "${reject_output}"
if grep -q 'panicked at' "${reject_output}"; then
  echo "enum-payload-literal-subpattern fixture panicked instead of reporting a diagnostic" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# Regex literal match-arm patterns
# ---------------------------------------------------------------------------

# Accept: `re"..."` in match-arm predicate position type-checks cleanly.
# The pattern syntax is validated at check time via the regex-syntax crate.
# End-to-end compilation requires HIR ExternBlock support for the
# auto-imported std::text::regex module (tracked as a follow-on lane).
if ! "${HEW}" check "${ROOT}/tests/vertical-slice/accept/regex_match_arm.hew" >"${reject_output}" 2>&1; then
  echo "expected regex_match_arm fixture to pass hew check; got:" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# Reject: malformed regex literal in match-arm position (E_INVALID_REGEX_LITERAL).
# The type checker validates regex syntax before HIR lowering.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/regex_invalid_pattern.hew" >"${reject_output}" 2>&1; then
  echo "expected regex_invalid_pattern fixture to fail" >&2
  exit 1
fi
grep -qF 'invalid regex pattern' "${reject_output}"

# ---------------------------------------------------------------------------
# W3.029 — user record/type ValueClass inference
# ---------------------------------------------------------------------------

# Accept: monomorphic user aggregate with all-BitCopy fields.
run_accept_expect_status "user_record_bitcopy" 42

# Accept: concrete generic user aggregate whose substituted fields are BitCopy.
run_accept_expect_status "generic_user_record_bitcopy" 42

# ---------------------------------------------------------------------------
# W3.032 Slice 3 — `Vec<Record>::contains` via codegen equality thunks
# ---------------------------------------------------------------------------

# Accept: layout-backed Vec<Point>::contains routes through the
# checker-authorized `hew_vec_contains_thunk` substrate.  The fixture
# asserts both true and false outcomes; exit 0 proves the thunk-driven
# equality kernel reports the correct membership.
run_accept_expect_status "vec_aggregate_contains" 0

# ---------------------------------------------------------------------------
# W3.004 — Vec::new turbofish + W3.013 — Vec range-slice sugar
# ---------------------------------------------------------------------------

# Accept (S0): Vec<i32> with type ascription (no turbofish) — confirms range-slice
# infrastructure baseline.  Exit 0 = empty vec created, no panic.
run_accept_expect_status "vec_new_ascription" 0

# Accept (S0): closed range slice `v[1..3]` returns a new Vec<i32> of length 2.
# Exit 2 = pop from the slice result.
run_accept_expect_status "vec_range_slice_closed" 2

# Accept (S0): open-left range slice `v[..2]` — first two elements.
# Exit 3 = first element of the slice result.
run_accept_expect_status "vec_range_slice_open_left" 3

# Accept (S0): open-right range slice `v[1..]` — all but first element.
# Exit 4 = first element of the slice result.
run_accept_expect_status "vec_range_slice_open_right" 4

# Accept (S0): full-range slice `v[..]` — clone of the whole vec.
# Exit 3 = length of the clone.
run_accept_expect_status "vec_range_slice_full" 3

# Accept (S0): inclusive range slice `v[0..=2]` — three elements.
# Exit 3 = length of the result.
run_accept_expect_status "vec_range_slice_inclusive" 3

# Accept (S1): Vec::<i64>::new() turbofish syntax with type annotation. Exit 0.
run_accept_expect_status "vec_new_turbofish_type" 0

# Accept (S1): Vec::<i64>::new() turbofish with push/pop chain. Exit 7.
run_accept_expect_status "vec_new_turbofish_method" 7

# Accept (S1): Vec<i64> push/pop/get/contains via catalog entries. Exit 10.
run_accept_expect_status "vec_i64_basic" 10

# Reject (S1): Vec::<i64, i32>::new() turbofish arity mismatch — Vec takes exactly
# 1 type argument; supplying 2 must produce a clear diagnostic.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/vec_new_turbofish_arity_mismatch.hew" \
    >"${reject_output}" 2>&1; then
  echo "W3.004: expected vec_new_turbofish_arity_mismatch to fail" >&2
  exit 1
fi
grep -q 'takes 1 type argument but 2 were supplied' "${reject_output}"

# Reject: concrete generic user aggregate with a heap-owning string field.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/user_record_non_bitcopy.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected user_record_non_bitcopy fixture to fail" >&2
  exit 1
fi
grep -q 'UnsupportedUserRecordValueClass' "${reject_output}"

# ---------------------------------------------------------------------------
# Generic enum monomorphisation — end-to-end acceptance fixtures
# ---------------------------------------------------------------------------

# Accept: Option<i64>::Some(42) — tuple-variant generic enum lowers end-to-end.
# Exercises HIR EnumLayoutRegistry → MIR layout-gather → codegen mangled-name
# lookup → link. Exit 42 proves the Some arm was matched and the payload read.
run_accept_expect_status "generic_enum_option_some" 42

# Accept: Option<i64>::None — unit arm of the same generic enum.
# Exit 99 proves the None arm matched (no payload confusion with Some).
run_accept_expect_status "generic_enum_option_none" 99

# Accept: Maybe<i64>::Just { value: 42 } — struct-variant (named-field) generic enum.
# Exit 42 proves named-field variant payloads lower through the generic path.
run_accept_expect_status "generic_enum_maybe_just" 42

# Accept: Result<i64, bool>::Ok(7) — two-type-parameter generic enum.
# Exit 7 proves the Ok arm matched and the i64 payload was read correctly.
run_accept_expect_status "generic_enum_result_ok" 7

# Accept: Option<Option<i64>>::Some(Some(5)) — nested generic instantiation.
# The HIR mono-pass must register both Option<i64> and Option<Option<i64>>
# layouts. Exit 5 proves both layouts lower and the nested match dispatched
# correctly.
run_accept_expect_status "generic_enum_nested_option" 5

# ---------------------------------------------------------------------------
# W3.028 Stage 1 — Composite-return spine: user functions returning enums
# ---------------------------------------------------------------------------

# Accept: fn maybe() -> Option<i64> { Some(42) } — aggregate return via ReturnSlot.
# Exit 42 proves the caller received the composite and destructured the Some payload.
run_accept_expect_status "composite_return_option_some" 42

# Accept: fn nothing() -> Option<i64> { None } — unit variant composite return.
# Exit 99 proves the None arm matched in the caller.
run_accept_expect_status "composite_return_option_none" 99

# Accept: Result<i64, i64> Ok(7) + Err(99) — two-type-parameter composite return.
# Exit 106 proves both Ok and Err variants return and destructure correctly.
run_accept_expect_status "composite_return_result" 106

# Reject: Option<string> composite return — heap-owning payload rejected at codegen.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/composite_return_heap_owning.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected composite_return_heap_owning fixture to fail" >&2
  exit 1
fi
grep -q 'composite return of heap-owning payload' "${reject_output}"

# WASM: composite return for Option<i64> compiles to wasm32.
"${HEW}" compile --target wasm32-unknown-unknown \
    "${ROOT}/tests/vertical-slice/accept/composite_return_option_some.hew" \
    >"${accept_output}" 2>&1

# WASM: composite return for Result<i64, i64> compiles to wasm32.
"${HEW}" compile --target wasm32-unknown-unknown \
    "${ROOT}/tests/vertical-slice/accept/composite_return_result.hew" \
    >"${accept_output}" 2>&1

# ---------------------------------------------------------------------------
# RemotePid<T>::tell — in-place Result<(), SendError> construction gate
# ---------------------------------------------------------------------------

# Accept: RemotePid<T>::tell on a synthetic pid (no peer online) must lower
# end-to-end and return SendError::NodeRoutingNotWired (variant 2) in the Err
# arm. Exit 42 asserts the correct tag/payload written by
# emit_remote_pid_tell_call. Regression in the in-place construction would
# produce a wrong SendError discriminant, exiting 1 or 2 instead.
run_accept_expect_status "remote_pid_tell" 42

# ---------------------------------------------------------------------------
# W2.006 Stage 1 — HewScope removal: scope{} MIR shape invariant.
# ---------------------------------------------------------------------------
# Accept fixture: `scope { fork { worker(); } }` inside an actor handler
# must:
#   1. typecheck cleanly (`hew check` exits 0).
#   2. produce a RawMirFunction whose instruction stream contains the
#      canonical hew_task_scope_* and hew_task_new symbols.
#   3. NOT mention the legacy hew_scope_* family anywhere in the dump.
#   4. compile and run end-to-end now that W4.010 synthesizes a TaskEntry
#      adapter for the free-function task body.
w2006_fixture="${ROOT}/tests/vertical-slice/accept/w2006_scope_spawn.hew"

"${HEW}" check "${w2006_fixture}" >"${accept_output}" 2>&1
grep -q ": OK$" "${accept_output}" || {
  echo "W2.006: expected hew check to print ': OK' on the scope_spawn fixture" >&2
  cat "${accept_output}" >&2
  exit 1
}

"${HEW}" compile --dump-mir raw "${w2006_fixture}" >"${accept_output}" 2>&1
grep -qF 'symbol: "hew_task_scope_new"' "${accept_output}" || {
  echo "W2.006: MIR dump must contain hew_task_scope_new" >&2
  cat "${accept_output}" >&2
  exit 1
}
grep -qF 'symbol: "hew_task_scope_spawn"' "${accept_output}" || {
  echo "W2.006: MIR dump must contain hew_task_scope_spawn" >&2
  cat "${accept_output}" >&2
  exit 1
}
grep -qF 'symbol: "hew_task_new"' "${accept_output}" || {
  echo "W2.006: MIR dump must contain hew_task_new (preceding hew_task_scope_spawn)" >&2
  cat "${accept_output}" >&2
  exit 1
}
grep -qF 'symbol: "hew_task_scope_destroy"' "${accept_output}" || {
  echo "W2.006: MIR dump must contain hew_task_scope_destroy" >&2
  cat "${accept_output}" >&2
  exit 1
}
# Legacy ABI must be fully removed.
if grep -qE 'symbol: "hew_scope_(spawn|new|create|free|destroy|cancel|is_cancelled|wait_all)"' "${accept_output}"; then
  echo "W2.006: legacy hew_scope_* symbol leaked into MIR dump — removal incomplete" >&2
  cat "${accept_output}" >&2
  exit 1
fi

run_accept_expect_status "w2006_scope_spawn" 0

# ---------------------------------------------------------------------------
# W3.025 Stage 1 — multi-file compile/run fixture baseline
# ---------------------------------------------------------------------------

# Accept: sibling module — all-pub, single-segment import, exit 42
# Layout: accept/sibling_module_call.hew imports accept/arith.hew
run_accept_expect_status "sibling_module_call" 42

# Accept: multi-level import (import lib::math;) — flat form lib/math.hew, exit 42
# Only the flat form exists (no lib/math/math.hew) so no ambiguity fires.
run_accept_expect_status "multilevel_import" 42

# Reject: unresolved module
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/unresolved_module.hew" \
  >"${reject_output}" 2>&1; then
  echo "W3.025: expected unresolved_module to fail" >&2
  exit 1
fi
grep -q 'does_not_exist' "${reject_output}"

# Reject: duplicate short module name (import alpha; import beta::alpha — both short name "alpha")
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/duplicate_short_name.hew" \
  >"${reject_output}" 2>&1; then
  echo "W3.025: expected duplicate_short_name to fail" >&2
  exit 1
fi
grep -q 'two imported modules share the short name' "${reject_output}"

# Reject: ambiguous module resolution (both flat ambig_mod.hew and dir ambig_mod/ambig_mod.hew exist)
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/ambiguous_module.hew" \
  >"${reject_output}" 2>&1; then
  echo "W3.025: expected ambiguous_module to fail" >&2
  exit 1
fi
grep -q 'is ambiguous' "${reject_output}"
grep -q 'Rename or remove one' "${reject_output}"

# WASM parity: must either succeed or emit a named WASM-TODO(#1451) diagnostic — never silent failure.
# Matches hew-cli/src/main.rs CodegenError::WasmUnsupportedSubstrate ("WASM target does not support").
if ! "${HEW}" compile --target wasm32-unknown-unknown \
    "${ROOT}/tests/vertical-slice/accept/directory_module_call.hew" \
    >"${accept_output}" 2>&1; then
  grep -qE 'WASM target does not support|wasm32' "${accept_output}" || {
    echo "W3.025: WASM multi-file compile failed silently (no named WASM diagnostic)" >&2
    cat "${accept_output}" >&2
    exit 1
  }
fi

# hew run direct invocation on multi-file fixture (same pipeline as hew compile)
if "${HEW}" run "${ROOT}/tests/vertical-slice/accept/directory_module_call.hew" \
  >"${stdout_output}" 2>"${stderr_output}"; then
  run_status=0
else
  run_status=$?
fi
if [[ "${run_status}" -ne 7 ]]; then
  echo "W3.025: hew run multi-file: expected exit 7, got ${run_status}" >&2
  exit 1
fi

# W3.041b: native-only layout-keyed HashMap/HashSet run-pass. WASM is
# deliberately covered by the codegen fail-closed substrate gate for
# hew_hashmap_*_layout / hew_hashset_*_layout.
(
  ulimit -v 524288
  timeout --kill-after=5s 30s "${HEW}" run "${ROOT}/examples/v05/hashmap_run_pass.hew"
)
