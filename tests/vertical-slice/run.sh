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
# Check-only fixture: `scope { fork { worker(); } }` inside an actor handler
# must:
#   1. typecheck cleanly (`hew check` exits 0).
#   2. produce a RawMirFunction whose instruction stream contains the
#      canonical hew_task_scope_* and hew_task_new symbols.
#   3. NOT mention the legacy hew_scope_* family anywhere in the dump.
# The full back-end link is blocked by an orthogonal SpawnTaskDirect callee
# constraint that predates W2.006; the check + MIR-shape gate is what this
# lane is responsible for.
w2006_fixture="${ROOT}/tests/vertical-slice/check-only/w2006_scope_spawn.hew"

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
