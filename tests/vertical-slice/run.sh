#!/usr/bin/env bash
set -euo pipefail

resolve_timeout() {
  if command -v timeout >/dev/null 2>&1; then
    command -v timeout
  elif command -v gtimeout >/dev/null 2>&1; then
    command -v gtimeout
  else
    echo "error: GNU timeout is required (install GNU coreutils: timeout/gtimeout)" >&2
    exit 127
  fi
}

TIMEOUT="$(resolve_timeout)"

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

run_fixture_path_expect_status() {
  local fixture_path="$1"
  local label="$2"
  local expected_status="$3"
  "${HEW}" compile "${fixture_path}" >"${accept_output}" 2>&1
  local bin
  bin="${ROOT}/.tmp/compile-out/$(basename "${fixture_path}" .hew)"
  local status=0
  # shellcheck disable=SC2016  # $1/$2/$3 are positional args to inner bash -c; single quotes deliberate.
  if "${TIMEOUT}" --kill-after=5s 30s bash -c '"$1" >"$2" 2>"$3"' _ "${bin}" "${stdout_output}" "${stderr_output}" 2>/dev/null; then
    status=0
  else
    status=$?
  fi
  if [[ "${status}" -ne "${expected_status}" ]]; then
    echo "expected ${label} to exit ${expected_status}, got ${status}" >&2
    cat "${accept_output}" >&2
    cat "${stdout_output}" >&2
    cat "${stderr_output}" >&2
    exit 1
  fi
}

run_accept_expect_status() {
  local fixture="$1"
  local expected_status="$2"
  echo "RUN ${fixture}"
  compile_accept "${fixture}"
  local bin="${ROOT}/.tmp/compile-out/${fixture}"
  local status=0
  # Time-bound the fixture binary: a non-terminating fixture (e.g. an actor
  # that never exits) must surface as a failure, not hang CI. 124/137 from
  # `timeout` then fail the exit-code assertion below rather than blocking.
  # shellcheck disable=SC2016  # $1/$2/$3 are positional args to the inner
  # `bash -c`, expanded there — the single quotes are deliberate.
  if "${TIMEOUT}" --kill-after=5s 30s bash -c '"$1" >"$2" 2>"$3"' _ "${bin}" "${stdout_output}" "${stderr_output}" 2>/dev/null; then
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
  echo "PASS ${fixture}"
}

run_accept_expect_stdout() {
  local fixture="$1"
  run_accept_expect_status "${fixture}" 0
  diff -u "${ROOT}/tests/vertical-slice/accept/${fixture}.expected" "${stdout_output}"
}

run_accept_expect_status_and_stdout() {
  local fixture="$1"
  local expected_status="$2"
  run_accept_expect_status "${fixture}" "${expected_status}"
  diff -u "${ROOT}/tests/vertical-slice/accept/${fixture}.expected" "${stdout_output}"
}

run_check_run_expect_stdout() {
  local fixture="$1"
  echo "RUN ${fixture}"
  "${HEW}" check "${ROOT}/tests/vertical-slice/accept/${fixture}.hew" >"${accept_output}" 2>&1
  local status=0
  if "${TIMEOUT}" --kill-after=5s 30s "${HEW}" run \
      "${ROOT}/tests/vertical-slice/accept/${fixture}.hew" \
      >"${stdout_output}" 2>"${stderr_output}"; then
    status=0
  else
    status=$?
  fi
  if [[ "${status}" -ne 0 ]]; then
    echo "expected ${fixture} hew run to exit 0, got ${status}" >&2
    cat "${accept_output}" >&2
    cat "${stdout_output}" >&2
    cat "${stderr_output}" >&2
    exit 1
  fi
  diff -u "${ROOT}/tests/vertical-slice/accept/${fixture}.expected" "${stdout_output}"
  echo "PASS ${fixture}"
}

expect_check_fail_contains() {
  local fixture_path="$1"
  local expected_substr="$2"
  local label="$3"
  if "${HEW}" check "${fixture_path}" >"${reject_output}" 2>&1; then
    echo "expected ${label} to fail closed under hew check" >&2
    exit 1
  fi
  grep -qF -- "${expected_substr}" "${reject_output}"
}

expect_check_fail_error_count() {
  local fixture_path="$1"
  local expected_count="$2"
  local label="$3"
  if "${HEW}" check "${fixture_path}" >"${reject_output}" 2>&1; then
    echo "expected ${label} to fail closed under hew check" >&2
    exit 1
  fi
  local actual_count
  actual_count="$(grep -Ec '^[^:]+:[0-9]+:[0-9]+: error:' "${reject_output}" || true)"
  if [[ "${actual_count}" -ne "${expected_count}" ]]; then
    echo "expected ${label} to emit ${expected_count} error(s), got ${actual_count}" >&2
    cat "${reject_output}" >&2
    exit 1
  fi
}

# Compile a fixture to a native binary (no memory cap — the LLVM codegen +
# clang/ld.lld link pipeline mmaps the multi-hundred-MB `libhew.a` and needs
# well over 1 GB of address space), then run ONLY the produced binary under a
# 512 MB virtual-memory cap.
#
# The cap is a best-effort guard that bounds the *fixture binary's* runtime
# heap so a collection-memory regression surfaces as a fail rather than a
# silent bloat; it is not part of the semantic oracle. It must NOT wrap the
# compile/link step: applying `ulimit -v 524288` around `hew run` (which
# compiles AND links before executing) throttled ld.lld's mmap of `libhew.a`
# and failed deterministically with "cannot open libhew.a: Cannot allocate
# memory". Splitting compile (uncapped) from run (capped) keeps the guard on
# the runtime where it belongs.
#
# GNU/Linux accepts `ulimit -v`; Darwin reports "Invalid argument", so on
# unsupported hosts the binary runs without the cap (the `|| true` swallows it).
#
# `HEW_WORKERS=2` pins the actor scheduler to two workers. The Hew runtime
# defaults to one worker thread per core, and each worker reserves multiple MB
# of stack address space; on a high-core host (e.g. 32 cores) those reservations
# alone exceed the 512 MB virtual cap and thread spawn fails with EAGAIN, which
# has nothing to do with the collection-heap regression being guarded. Pinning
# the worker count makes the cap host-independent without weakening the guard:
# these fixtures verify actor-teardown drop/free correctness, which is identical
# regardless of worker count.
#
# Echoes the binary's stdout so callers can capture it. Returns the binary's
# exit status.
run_native_under_memory_cap() {
  local fixture_path="$1"
  local base
  base="$(basename "${fixture_path}" .hew)"
  "${HEW}" compile "${fixture_path}" >/dev/null
  # shellcheck disable=SC2016  # $1 is a positional arg to the inner `bash -c`,
  # expanded there — the single quotes are deliberate.
  HEW_WORKERS=2 "${TIMEOUT}" --kill-after=5s 30s bash -c \
    'ulimit -v 524288 2>/dev/null || true; exec "$1"' _ \
    "${ROOT}/.tmp/compile-out/${base}"
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

# Accept: `fork child = worker(42)` inside a machine state entry block
# compiles end to end — machine entry blocks are spawn-capable contexts and
# arg-bearing named forks ride the fork-entry shim env. Compile-only (main
# never instantiates the machine); lower-level HIR tests continue to pin the
# Item::Machine task-gate walker directly.
compile_accept "machine_fork_args_spawn"

# Stage-2 lazy iterator wrappers: structural smoke. The fixture's wrapper
# records and `impl Iterator` blocks compile cleanly through the parser and
# the type checker, but the generic-T impl bodies do not yet have a MIR
# `ValueClass` lowering (deferred to the follow-on stages that wire
# `Vec::IntoIterator` and the for-loop desugar). The lock tests under
# `hew-types::iterator_lazy_wrappers` carry the binding shape — this
# command is documentary: the fixture is wired in so a future stage
# wraps it into a real accept run once MIR lowering catches up.
"${HEW}" check "${ROOT}/tests/vertical-slice/accept/iter_lazy_wrappers.hew" \
    >"${accept_output}" 2>&1

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

# Static trait dispatch through supertrait bounds:
# - reject missing concrete dispatch target at checker time, before MIR;
# - accept inline supermethod provision on the direct impl;
# - accept inline supermethod provision on an intermediate supertrait impl.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/static_trait_dispatch_missing_super_impl.hew" \
  "not its declared supertrait" \
  "static_trait_dispatch_missing_super_impl"
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/payload_enum_equality.hew" \
  "payload variants is not supported" \
  "payload_enum_equality"
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/builtin_payload_enum_equality.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected builtin_payload_enum_equality to fail closed under hew check" >&2
  exit 1
fi
# shellcheck disable=SC2016  # backtick-containing diagnostic string; not shell expansion.
grep -qF '`==` on enum `Option<i64>` with payload variants is not supported' "${reject_output}"
grep -qF 'let _ = a == b;' "${reject_output}"
if grep -qF 'E_CODEGEN_FRONT' "${reject_output}" || \
    grep -qF 'IntCmp lhs is not an integer' "${reject_output}"; then
  echo "builtin_payload_enum_equality leaked codegen-front diagnostics" >&2
  cat "${reject_output}" >&2
  exit 1
fi
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/builtin_payload_enum_inequality_result.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected builtin_payload_enum_inequality_result to fail closed under hew check" >&2
  exit 1
fi
# shellcheck disable=SC2016  # backtick-containing diagnostic strings; not shell expansion.
grep -qF '`!=` on enum `Result<i64, i64>` with payload variants is not supported' "${reject_output}"
grep -qF 'let _ = ok != err;' "${reject_output}"
if grep -qF 'E_CODEGEN_FRONT' "${reject_output}" || \
    grep -qF 'IntCmp lhs is not an integer' "${reject_output}"; then
  echo "builtin_payload_enum_inequality_result leaked codegen-front diagnostics" >&2
  cat "${reject_output}" >&2
  exit 1
fi
run_accept_expect_stdout "static_trait_dispatch_inline_supertrait"
run_accept_expect_stdout "static_trait_dispatch_intermediate_inline_supertrait"

run_accept_expect_status "assert_eq_fail" 134
grep -q 'assertion failed: assert_eq(4, 5)' "${stderr_output}"

run_accept_expect_status "exit_42" 42
run_accept_expect_status "for_vec_sum_42" 42
run_accept_expect_stdout "vec_string_for_each"
run_accept_expect_status "array_literal_int_sum" 6
run_accept_expect_stdout "array_literal_float_sum"
run_accept_expect_stdout "array_literal_string_for_each"
run_accept_expect_status "iter_manual_next_42" 42
run_accept_expect_status "gen_next_stack_bounded" 0
run_accept_expect_status "for_range_regression" 21

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
# Loop control flow: `break` / `continue` / bare `loop {}` completeness.
#
# Each fixture's exit code flows through the loop logic under test — the
# asserted value only holds if the runtime control flow is correct, and the
# regression case surfaces as a timeout (infinite loop) or a different exit
# code, never as a silent pass.
# ---------------------------------------------------------------------------

# `break` in `while` seals the block at the break site (exit 3, not ~100).
run_accept_expect_status "break_while" 3
# `continue` in `for`-range advances the counter via the increment block
# (Risk 1): 0 + 1 + 3 + 4 = 8 after skipping i == 2. A continue → header
# regression would skip the increment and hang at i == 2.
run_accept_expect_status "continue_for" 8
# bare `loop {}` exits only via `break` (exit 7; missing break would hang).
run_accept_expect_status "loop_break" 7
# `break` targets the innermost loop only (inner-only break -> 2 * 3 = 6).
run_accept_expect_status "break_nested" 6
# in-loop `defer` fires on the `break` path (cleanup-all-exits): exit 9, not 0.
run_accept_expect_status "defer_in_loop_break" 9
# labeled `break @outer` exits both the inner loop and the labeled outer loop,
# then runs code after the labeled loop.
run_accept_expect_status "labeled_break_outer" 16
# labeled `continue @outer` skips the rest of both loop bodies and advances the
# outer for-range counter.
run_accept_expect_status "labeled_continue_outer" 38
# labeled break flushes every intervening defer scope exactly once.
run_accept_expect_stdout "labeled_break_defer_window"

if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/labeled_break_unknown_label.hew" >"${reject_output}" 2>&1; then
  echo "expected labeled_break_unknown_label fixture to fail" >&2
  exit 1
fi
grep -qF "unknown loop label \`@nonexistent\`" "${reject_output}"

# WASM parity: the for-range `continue` CFG (increment block + overflow traps,
# the most complex loop-control shape) must compile under wasm32. Loop control
# is pure CFG/Goto, so behavioural parity is inherited from the shared
# MIR->LLVM lower; this gate pins that the wasm target does not regress on it.
"${HEW}" compile --target wasm32-unknown-unknown \
  "${ROOT}/tests/vertical-slice/accept/continue_for.hew" >"${accept_output}" 2>&1
test -s "${ROOT}/.tmp/compile-out/continue_for.wasm"


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

# shellcheck disable=SC2016  # backticks in the pattern are literal — they match
# the diagnostic text, not a command substitution.
expect_check_fail_contains \
    "${ROOT}/tests/vertical-slice/reject/user_impl_drop_unsupported.hew" \
    '`impl Drop` is not supported (its `drop` method would not run)' \
    "user impl Drop"
# shellcheck disable=SC2016  # backticks in strings are Hew diagnostic syntax, not subshell
expect_check_fail_contains \
    "${ROOT}/tests/vertical-slice/reject/const_ref_forward.hew" \
    'undefined variable `A`' \
    "const_ref_forward"

# Actor body: increment(10) + increment(32) = 42.
run_accept_expect_status "actor_counter" 42

# Actor field mutability: a `let` state field is writable only inside
# `init`; a receive-fn write must fail at check time and name the var fix.
# shellcheck disable=SC2016  # backticks in the pattern are literal — they match
# the diagnostic text, not a command substitution.
expect_check_fail_contains \
    "${ROOT}/tests/vertical-slice/reject/actor_let_field_assign.hew" \
    'cannot assign to immutable field `count`' \
    "actor_let_field_assign"

# Actor field mutability accept side: a `let` field set at spawn time is
# readable from handlers while the `var` field takes the writes.
# step=7 applied twice: exit 14.
run_accept_expect_status "actor_let_field_read" 14

# Actors on wasm32: the cooperative scheduler/mailbox ABI (`scheduler_wasm`) is
# present on `target_arch = "wasm32"`, so an actor program now compiles through the
# shared wasm codegen pipeline (previously rejected at HIR). This pins the gate
# lift; behavioural parity on the WASI run path is proven by the hew-sandbox-wasm
# actor parity suite. (`hew compile` supports only `wasm32-unknown-unknown`.)
"${HEW}" compile --target wasm32-unknown-unknown \
  "${ROOT}/tests/vertical-slice/accept/actor_counter.hew" \
  >"${accept_output}" 2>&1
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

# select{} with OWNED-string ask arms (#1739/#1735): FastWorker returns an owned
# `string` and wins; SlowWorker sleeps 50 ms and loses, its owned reply released
# on the loser teardown leg by the channel's registered destructor (no leak, no
# double-free). Exit code == "WINNER-OWNED-REPLY".len() == 18 proves the winner
# reply is consumed correctly while the abandoned owned reply is reclaimed.
run_accept_expect_status "ask_reply_owned_select_loser" 18

# Owned-string ask reply + `after` timeout (#1739/#1735): SlowWorker's owned
# reply always arrives after the 10 ms deadline, so the after-arm wins and the
# late owned reply lands on the cancelled channel (the hew_reply false leg),
# reaped by the registered destructor on shutdown. Exit code 7 (the after-arm
# value) proves the program completes past the abandoned-owned-reply teardown
# without crashing or hanging.
run_accept_expect_status "ask_reply_owned_timeout" 7

# join{} wait-ALL with two actor-ask branches: Doubler.compute(10)/(11)
# reply 20/22; the tuple binds (ra, rb) in declaration order. Exit code
# ra + rb = 42 proves both replies are materialised into the correct
# tuple elements and every reply channel is freed exactly once.
run_accept_expect_status "join_two_actors" 42

# join{} error propagation (HEW-SPEC-2026 §4.11.2): one branch traps
# (assert_eq false in Bad.compute). The trap propagates out of the join
# site instead of binding a tuple; the assertion-failure abort exits 134
# (SIGABRT), matching `assert_eq_fail`. Deterministic — independent of
# which branch replies first.
run_accept_expect_status "join_branch_trap" 134

# Reject (§4.11.2 actor-only join branches): `join { rx.recv() }` is NOT a
# valid join branch — a channel receive is not an actor receive-handler ask.
# The join-specific branch validator must reject it at CHECK time with
# `JoinBranchNotActorAsk` (declared but previously never triggered) — never a
# silent `Unit` lowering, never a late MIR error. Exercises the consumed form.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/join_branch_not_actor.hew" >"${reject_output}" 2>&1; then
  echo "expected join_branch_not_actor fixture to fail" >&2
  exit 1
fi
grep -q 'JoinBranchNotActorAsk' "${reject_output}"

# Regression guard: the join-branch rejection above is join-scoped and must NOT
# break `select`'s legitimate channel-receive arm. A `select { pat from
# rx.recv() ... }` still checks OK.
"${HEW}" check "${ROOT}/tests/vertical-slice/accept/select_recv_guard.hew" >"${accept_output}" 2>&1

# Reject (SELECT ship-3, HEW-SPEC §4.11.1): the task-await arm
# `<id> from await <expr>` is NOT one of the three sealed select forms
# (Task<T> remains unnameable in annotations even though `fork name = call()`
# binds typed handles), so the checker select-arm gate rejects it at CHECK
# time with the actor-ask form diagnostic — never a silent lowering, never a
# late MIR/codegen error.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/select_arm_await_task_dropped.hew" >"${reject_output}" 2>&1; then
  echo "expected select_arm_await_task_dropped fixture to fail" >&2
  exit 1
fi
grep -qF 'select arm source must be actor.method(args)' "${reject_output}"

# Reject (SELECT ship-3, HEW-SPEC §4.11.1): the stream-next arm
# `<id> from <stream>.recv()` over a Stream<T> is NOT a sealed select form — the
# `.recv()` arm is channel-only (Receiver<T>). A Stream<T> receiver is rejected
# by the checker select-arm gate (before MIR), so the diagnostic is the select-arm
# form error, not the owned-handle aggregate-extraction fail-closed.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/select_arm_stream_recv_dropped.hew" >"${reject_output}" 2>&1; then
  echo "expected select_arm_stream_recv_dropped fixture to fail" >&2
  exit 1
fi
grep -qF 'select arm source must be actor.method(args)' "${reject_output}"

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

# Regression guard (issue #382 Bug 1): supervised actor with init() block.
# Before the fix: restart_child_from_spec → hew_actor_spawn_opts →
# build_spawned_actor did not initialise the mailbox; supervised children with
# an init() block SIGSEGVed at the first dispatch.  The init() block generates
# Worker__init in codegen; the supervisor sets value=42 via HewChildSpec.init_state;
# the actor replies with 42, proving the init dispatch ran without crashing.
# (No WASM check needed: supervisor fixtures emit "Supervision tree operations are
# not supported on WASM32", which is already exercised by the supervisor_basic block.)
run_accept_expect_status "supervised_actor_init_block" 42

# Regression guard (issue #382 Bug 2): supervisor child access after restart.
# Before the fix: hew_supervisor_child_get returned a null pointer for Transient
# children, causing SIGSEGV.  After the fix: MIR lower_supervisor_child_get traps
# with code 206 on non-Live, and returns the handle safely on Live (tag=0).
# This test crashes the child, waits on hew_supervisor_wait_restart until the
# restart completion notification fires, then accesses sup.w1 and expects a
# Live handle (exit 7).
# A Transient result would fire SIGTRAP (exit 133) — a visible failure.
# (No WASM check needed: same reason as above.)
run_accept_expect_status "supervisor_child_after_restart" 7

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
run_check_run_expect_stdout "const_ref_init"

# W4.039 — bytes-to-string triple-ABI canonicalisation. Behavioural proof
# that `bytes.to_string()` routes through the canonical
# `hew_bytes_to_string(triple: BytesTriple) -> *mut c_char` runtime export
# (the catalog declares the LLVM extern with a single `bytes` parameter,
# materialised as `{ptr, i32, i32}`, which is ABI-equivalent to a
# `#[repr(C)] BytesTriple` passed by value). Asserts both exit-0 status
# and exact stdout `Hi\n`.
run_accept_expect_stdout "bytes_push_round_trip"

# W3 collections-sugar S2: bytes range slices. Each fixture asserts both the
# runtime-derived slice length (process status) and contents (`to_string()`).
run_accept_expect_status_and_stdout "bytes_slice_closed" 2
run_accept_expect_status_and_stdout "bytes_slice_open_left" 3
run_accept_expect_status_and_stdout "bytes_slice_open_right" 4
run_accept_expect_status_and_stdout "bytes_slice_full" 4
run_accept_expect_status_and_stdout "bytes_slice_open_right_loop" 10

run_accept_expect_stdout "regex_captures_find_all"
run_check_run_expect_stdout "stdlib_io_scanner_file_oracle"
run_accept_expect_stdout "tls_ffi_result_lowering"
run_accept_expect_stdout "template_compiled_free_function_p0"
run_accept_expect_stdout "template_oracle_02_compiled_substitution"
run_accept_expect_stdout "template_oracle_03_if_range"
run_accept_expect_stdout "template_negative_try_errors"
run_check_run_expect_stdout "unicode_oracle"
run_check_run_expect_stdout "unicode_error_oracle"

run_accept_expect_status "panic" 101
grep -q 'panic fixture' "${stderr_output}"

run_fixture_path_expect_status "${ROOT}/tests/vertical-slice/reject/std_panic_wrapper_regex_new_invalid.hew" "std_panic_wrapper_regex_new_invalid" 101
grep -q 'regex.new: invalid pattern' "${stderr_output}"
run_fixture_path_expect_status "${ROOT}/tests/vertical-slice/reject/std_panic_wrapper_fs_read_missing.hew" "std_panic_wrapper_fs_read_missing" 101
grep -q 'fs.read failed' "${stderr_output}"
run_fixture_path_expect_status "${ROOT}/tests/vertical-slice/reject/std_panic_wrapper_os_args_oob.hew" "std_panic_wrapper_os_args_oob" 101
grep -q 'os.args: index' "${stderr_output}"
run_fixture_path_expect_status "${ROOT}/tests/vertical-slice/reject/std_panic_wrapper_url_parse_invalid.hew" "std_panic_wrapper_url_parse_invalid" 101
grep -q 'url.parse: invalid URL' "${stderr_output}"
run_fixture_path_expect_status "${ROOT}/tests/vertical-slice/reject/std_panic_wrapper_cron_parse_invalid.hew" "std_panic_wrapper_cron_parse_invalid" 101
grep -q 'cron.parse: invalid expression' "${stderr_output}"
run_accept_expect_status "std_panic_wrappers_success" 0

run_accept_expect_status "directory_module_call" 7

if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/unresolved_symbol.hew" >"${reject_output}" 2>&1; then
  echo "expected unresolved symbol fixture to fail" >&2
  exit 1
fi
grep -q 'undefined variable' "${reject_output}"

expect_check_fail_error_count \
  "${ROOT}/tests/vertical-slice/reject/cascade_error_binary_operand.hew" \
  1 \
  "cascade_error_binary_operand"
# shellcheck disable=SC2016  # backtick-containing diagnostic strings; not shell expansion.
grep -qF 'undefined variable `undefined_var`' "${reject_output}"
# shellcheck disable=SC2016  # backtick-containing diagnostic strings; not shell expansion.
if grep -qF '<error>' "${reject_output}" || grep -qF 'cannot apply `+`' "${reject_output}"; then
  echo "cascade_error_binary_operand emitted a downstream binary-op diagnostic" >&2
  cat "${reject_output}" >&2
  exit 1
fi

expect_check_fail_error_count \
  "${ROOT}/tests/vertical-slice/reject/cascade_error_index_object.hew" \
  1 \
  "cascade_error_index_object"
# shellcheck disable=SC2016  # backtick-containing diagnostic strings; not shell expansion.
grep -qF 'undefined variable `undefined_var`' "${reject_output}"
# shellcheck disable=SC2016  # backtick-containing diagnostic strings; not shell expansion.
if grep -qF '<error>' "${reject_output}" || grep -qF 'cannot index into `<error>`' "${reject_output}"; then
  echo "cascade_error_index_object emitted a downstream index diagnostic" >&2
  cat "${reject_output}" >&2
  exit 1
fi

expect_check_fail_error_count \
  "${ROOT}/tests/vertical-slice/reject/real_binary_type_error_preserved.hew" \
  1 \
  "real_binary_type_error_preserved"
# shellcheck disable=SC2016  # backtick-containing diagnostic strings; not shell expansion.
grep -qF 'cannot apply `+` to `string` and `i64`' "${reject_output}"

expect_check_fail_error_count \
  "${ROOT}/tests/vertical-slice/reject/real_index_type_error_preserved.hew" \
  1 \
  "real_index_type_error_preserved"
# shellcheck disable=SC2016  # backtick-containing diagnostic strings; not shell expansion.
grep -qF 'cannot index into `i64`' "${reject_output}"

if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/use_after_consume.hew" >"${reject_output}" 2>&1; then
  echo "expected use-after-consume fixture to fail" >&2
  exit 1
fi
grep -q 'UseAfterConsume' "${reject_output}"

reject_check_use_after_consume() {
  local fixture="$1"
  if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/${fixture}.hew" >"${reject_output}" 2>&1; then
    echo "expected ${fixture} fixture to fail" >&2
    exit 1
  fi
  grep -q 'E_MIR_CHECK' "${reject_output}"
  grep -q 'used after it was consumed' "${reject_output}"
  grep -q 'UseAfterConsume' "${reject_output}"
}

for fixture in \
  move_into_tuple_string \
  move_into_record_string \
  move_into_tuple_vec \
  move_into_record_vec \
  move_into_tuple_owned_record \
  move_into_record_owned_record \
  move_into_tuple_resource \
  move_into_record_resource \
  move_into_enum_string \
  move_into_enum_vec \
  move_into_array_string \
  move_into_array_vec \
  actor_nested_handle_tuple_use_after_send \
  actor_nested_handle_bound_tuple_rx_use_after_send
do
  reject_check_use_after_consume "${fixture}"
done

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

# Lambda-actor and Duplex surface: runnable fixtures end-to-end through
# compile + execute, with diff-pinned stdout. The pre-fix path was
# `hew check` only — that caught front-end gaps but not the codegen
# uninit-handle, the ask null-deref decode, the small-msg stack
# over-read, or the run-time wrapper leak. The runnable surface now
# pins all four of those.

# Accept: tell-shaped lambda actor call dispatch — exercises spawn,
# `hew_lambda_actor_new`, env-less body synthesis, tell-send, and the
# wrapper-free path on release at process exit.
run_accept_expect_stdout "lambda_callable_tell"

# Accept: ask-shaped lambda actor call dispatch — exercises the reply
# channel, the B2 status-branched reply decode (Ok path), and the reply
# payload free. Stdout pins `Ok(10)` materialisation through the match.
run_accept_expect_stdout "lambda_callable_ask"

# Accept: ask-shaped lambda actor replying a record wider than one machine word.
# Pins the reply ABI at the record's exact byte width instead of the historical
# hard-coded i64 path that silently truncated `Pair { a, b }` to one field.
run_accept_expect_stdout "lambda_callable_ask_record_reply"

# Accept + run: block-wrapped actor-ask await (`await { actor.method() }`) must
# type-check as `Result<T, AskError>` and produce the correct reply value. Pre-
# fix the checker saw `Expr::Block` instead of `Expr::MethodCall` and fell
# through to the raw return type, producing "cannot match non-enum type" for
# `Ok`/`Err` arms.
run_check_run_expect_stdout "actor_ask_block_wrapped_await"

# Accept: lambda-actor binding literally named `log` pins the call-syntax
# dispatch routing — pre-fix the callee-binding check resolved `log("hi")`
# to the math `log` floor before the lambda-actor surface was consulted,
# misrouting the send through the f64 logarithm. The fixture exercises
# BOTH a tell-shaped and an ask-shaped binding named `log` so codegen
# parity (send dispatch + ask reply decode) is held by the same routing.
run_accept_expect_stdout "lambda_log_collision"

# Accept: lambda-actor multi-param dispatch through the packed-args
# anonymous-record wire (the same payload mechanism as declared-actor
# multi-arg sends). Covers i64+i64 ask, string+i64 ask, and string+i64
# tell. Single-param lambdas stay on the 8-byte single-vertebra wire —
# pinned by lambda_small_msg / lambda_callable_* below.
run_accept_expect_stdout "lambda_multi_param"

# Accept: lambda-actor message types narrower than 8 bytes (`bool`, `i32`)
# pin the 8-byte zero-padded wire format. Pre-fix, the sender allocated
# the spill at the source type's width (1 byte for `bool`, 4 for `i32`)
# while the runtime copied 8 bytes from it — a stack over-read; the
# body's msg-deserialise prologue then memcpy'd 8 bytes into the user
# param's same-narrow alloca — a stack over-write. Uses ask-shape so
# stdout ordering is deterministic regardless of dispatch scheduling.
run_accept_expect_stdout "lambda_small_msg"

# Accept: actor receive-method dispatch through a struct-field receiver.
# `actor B { let out: W; ... out.put(n) ... await out.get() }` — the field's
# bare actor-name type holds an actor handle (canonicalised to LocalPid<W> in
# HIR), so both the fire (`put`) and ask (`get`) calls route through the actor
# mailbox. Regression for `E_HIR: indirect call ... has no MIR dispatch path`,
# which fired before the field receiver was recognised as an actor send/ask.
run_accept_expect_stdout "actor_field_method_dispatch"

# Accept + run: declared-actor multi-arg tell through the packed-args
# anonymous-record wire. Mixed (string, i64) and double-heap-owning
# (string, string) payloads; the trailing ask fences stdout ordering.
# Each packed field is copied at exactly sizeof(field) into the record,
# the mailbox deep-copies sizeof(record) bytes, and the dispatch
# trampoline unpacks each param at its natural field offset.
run_accept_expect_stdout "actor_multi_arg_tell"

# Accept + run: declared-actor multi-arg ask — `await adder.compute(3, 4)`
# packs both args into one record and the reply rides the existing ask
# channel; covers i64+i64 and string+i64 ask payloads.
run_accept_expect_stdout "actor_multi_arg_ask"

# Accept + run: a channel handle nested inside a tuple `(Receiver<string>, string)`
# as an actor receive-fn parameter. Transfers the Receiver through the mailbox;
# the worker drains and closes it. Pins the recursive handle predicate: before
# the fix the tuple arg was lowered as Read (not Consume), and a later
# `rx.close()` in the caller compiled and double-closed the channel at runtime.
run_accept_expect_stdout "actor_nested_handle_tuple_transfer"

# Accept + run: user records named `Sender` and `Receiver` are not builtin
# channel handles. They must keep ordinary actor-send treatment and emit xnode
# codecs instead of being skipped by bare short name.
run_accept_expect_stdout "actor_channel_shadow_sender_codec"
grep -q '__hew_serialize_Sender' \
  "${ROOT}/.tmp/compile-out/actor_channel_shadow_sender_codec.ll"
grep -q '__hew_serialize_Receiver' \
  "${ROOT}/.tmp/compile-out/actor_channel_shadow_sender_codec.ll"

# Accept + run: a single-argument actor receive handler whose ONLY parameter is
# a process-local pid payload (`LocalPid<T>`). `echo.hear(this)` passes the
# pinger's own pid as the sole message arg; the handler routes an `ack` back
# through it. Before the fix this single-arg form was incorrectly fail-closed at
# codegen — the cross-node codec seeder eagerly tried to build a wire serializer
# for the non-serializable `LocalPid` payload and failed the WHOLE compile —
# even though the identical multi-arg form already worked and a same-node send
# never serializes. Pins the broadened codec-seeder skip (actor-pid/handle
# family), the sibling of the channel-handle skip. The companion reject fixture
# (actor_local_pid_remote_nonserializable) proves the cross-node direction stays
# forbidden at the checker.
run_accept_expect_stdout "actor_single_arg_pid_payload"

# Reject: an ask-shaped receive method called on a struct-field actor receiver
# must be `await`ed. `actor B { let out: W; ... out.get() }` invokes W's
# non-unit `get` without `await`; because the field receiver routes through the
# actor ask machinery (not a sync struct call), the checker fires the
# ask-without-await guard — fail-closed, never a silent sync call or an
# indirect-dispatch error.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/actor_field_ask_without_await.hew" >"${reject_output}" 2>&1; then
  echo "expected actor_field_ask_without_await fixture to fail" >&2
  exit 1
fi
# shellcheck disable=SC2016  # backticks in the pattern are literal — they match
# the CLI diagnostic's pretty-printed `Actor::method` / `await` names, not
# command substitution.
grep -qF 'actor ask `W::get` requires `await`' "${reject_output}"

# Reject: non-Send message type (E_DUPLEX_NON_SEND).
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/duplex_non_send.hew" >"${reject_output}" 2>&1; then
  echo "expected duplex-non-send fixture to fail" >&2
  exit 1
fi
grep -q 'E_DUPLEX_NON_SEND' "${reject_output}"

# Reject: non-Send ask reply type on a declared actor (E_DUPLEX_NON_SEND).
# Companion to duplex_non_send.hew (which gates the message): an ask-shaped
# reply crosses the actor boundary back to the caller, so it must be Send.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/ask_reply_non_send.hew" >"${reject_output}" 2>&1; then
  echo "expected ask-reply-non-send fixture to fail" >&2
  exit 1
fi
grep -q 'E_DUPLEX_NON_SEND' "${reject_output}"

# Reject: removed <- operator (E_OPERATOR_REMOVED).
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/lambda_arrow_operator.hew" >"${reject_output}" 2>&1; then
  echo "expected lambda-arrow-operator fixture to fail" >&2
  exit 1
fi
grep -q 'E_OPERATOR_REMOVED' "${reject_output}"

# Accept + run: `.send()` on a lambda-actor handle delivers the message.
# Lambda-actor handles are `Duplex<Msg, Reply>` underneath; `.send(msg)` on a
# `LambdaActorHandle` Place routes to the lambda-actor ABI
# (`hew_lambda_actor_send`), not the raw-duplex `hew_duplex_send` (which would
# type-pun the handle and silently drop the message). Statement-context send is
# fire-and-forget: the actor receives 42 and prints it.
run_accept_expect_stdout "lambda_method_send"

# Accept + run: value-context `.send()` on a lambda-actor handle materializes
# `Result<(), SendError>` AND still delivers the message. The checker types the
# tell-shaped send as `Result<(), SendError>`; MIR sizes the dest from that
# recorded type and codegen constructs the Result in place from the runtime's
# i32 status (D1 mapping). Exit 7 proves the Ok arm; stdout `99` proves the
# message was delivered (not dropped). Together with the statement-context
# fixture above this covers both send contexts.
run_accept_expect_status_and_stdout "lambda_send_result_ok" 7

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

# Accept + run: lambda-actor closure captures through the heap-boxed env
# record. BitCopy captures (tell + ask shapes) read the env field back on
# every dispatch; the recursive self-reference is a WEAK capture that is
# back-filled with the downgraded handle after construction and
# dispatches through hew_lambda_actor_weak_send (§5.9 ratification 2 —
# the body never keeps its own actor alive).
run_accept_expect_stdout "lambda_capture_tell"
run_accept_expect_stdout "lambda_capture_ask"
run_accept_expect_stdout "lambda_self_recursion"

# Accept + run: heap-owning `string` capture. The env owns an
# independent hew_string_clone (made at box time); the caller's binding
# stays usable after the spawn; two dispatches read the same env field;
# the synthesized state dropper frees the clone exactly once at actor
# shutdown. Validated under MallocScribble/PreScribble/GuardEdges.
run_accept_expect_stdout "lambda_capture_heap"

# Accept + run: fn-closure capturing an actor pid and sending through it.
# The pid is a BitCopy alias with no drop glue; the closure-shim builder
# carries the module's actor layout tables so the body's send resolves
# `actor_method_info` exactly as the parent would. Exit 42 = 10 + 32.
run_accept_expect_status "closure_pid_send" 42

# Accept + run: sends to the captured pid nested under `if` and `while`
# inside the closure body — per-site send resolution, not capture-time
# pre-resolution. Exit 42 = 39 (if-branch) + 3×1 (loop branch).
run_accept_expect_status "closure_pid_send_controlflow" 42

# Accept + run: alias soundness — the parent's pid binding stays live and
# usable after the closure captures it. Exit 42 = 40 (closure) + 2 (parent).
run_accept_expect_status "closure_pid_alias_parent" 42

# Accept + run: lambda actor capturing a declared-actor pid and forwarding.
# The pid rides the heap-boxed env as a no-drop field; the state dropper
# frees env bytes only. Exit 42 = 10 + 32 forwarded.
run_accept_expect_status "lambda_capture_pid_forward" 42

# Reject: capture env field classes are explicit — an owned aggregate
# capture (Vec, HashMap, record, owned handle) has no cross-boundary
# ownership protocol yet and must fail closed rather than shallow-copy
# heap pointers into the actor's env.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/lambda_capture_owned_aggregate.hew" \
  "CannotMaterializeClosureCapture" \
  "lambda_capture_owned_aggregate"

# Reject: a fn-closure capturing a LAMBDA-ACTOR handle (a Duplex, not a
# no-drop pid) and calling it has no materialization protocol through a
# closure env — the checker refuses with ClosureCapturesDuplexHandle
# (deliberate checker-authority wall) rather than an incidental
# CheckerBoundaryViolation. The fixture pins the authoritative error site
# (check_call in calls.rs, TypeErrorKind::ClosureCapturesDuplexHandle).
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/closure_capture_lambda_handle.hew" \
  "E_CLOSURE_CAPTURES_LAMBDA_HANDLE" \
  "closure_capture_lambda_handle"

# Reject: remote dispatch (RemotePid ask/tell) resolving to a multi-arg
# receive handler fails closed with E_REMOTE_PAYLOAD_UNSUPPORTED. The
# remote wire carries one message value; a multi-arg handler's local
# wire is the packed-args record. A first-param match between the two is
# a wire-width mismatch (short bytes into a full-record unpack on the
# receiving node), so both seams — MIR's remote ask matcher and
# codegen's hew_remote_pid_tell intercept — refuse at compile time. The
# cross-node payload serialization lane lands the positive path.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/actor_multi_arg_remote_unsupported.hew" \
  "E_REMOTE_PAYLOAD_UNSUPPORTED" \
  "actor_multi_arg_remote_unsupported"
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/actor_multi_arg_remote_tell_unsupported.hew" \
  "E_REMOTE_PAYLOAD_UNSUPPORTED" \
  "actor_multi_arg_remote_tell_unsupported"

# Reject: a process-local `LocalPid` payload must never cross a RemotePid
# (cross-node) boundary. `LocalPid<T>` is process-local — a bare `*mut HewActor`
# pointer with no on-wire representation — and carries neither Encode nor
# Decode, so it is NOT Serializable. Sending it through `RemotePid::tell` must
# fail closed at the checker's Serializable boundary, never ship a local pointer
# to another node. This is the guard for the broadened codec-seeder skip: the
# companion accept fixture (actor_single_arg_pid_payload) proves the SAME pid
# payload runs on a same-node send; broadening the skip removed dead codec
# weight only — the impossible cross-node case stays fail-closed here.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/actor_local_pid_remote_nonserializable.hew" \
  "must implement Serializable before it can cross a RemotePid boundary" \
  "actor_local_pid_remote_nonserializable"

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
# The type checker rejects this before HIR lowering — `fork` child bindings
# only have a spawn context inside a `scope { }` body.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/fork_outside_scope.hew" >"${reject_output}" 2>&1; then
  echo "expected fork-outside-scope fixture to fail" >&2
  exit 1
fi
# shellcheck disable=SC2016  # backticks in the pattern are literal — they match
# the diagnostic text, not a command substitution.
grep -qF 'only valid inside a `scope { }` body' "${reject_output}"

# Accept (TI-2 end to end): `fork name = call();` binds Task<()> inside a
# scope body; `await name` joins the child before the scope exits. The
# ask-shaped reply orders both println lines and the process exit.
run_accept_expect_stdout "fork_named_await_unit"

# Accept (arg-bearing fork): scalar args on a named fork and a moved-in heap
# string on a fork block both transfer through the fork-entry shim env.
run_accept_expect_stdout "fork_args_spawn"

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

# Reject: implicit-spawn callee takes arguments (non-named form stays nullary).
# Pins the HIR no-argument gate for non-named spawns in validate_task_spawn_call.
# Only `fork name = f(args)` (named form) accepts arguments; implicit scope-
# statement spawns and fork-block bodies must be nullary.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/fork_with_args.hew" >"${reject_output}" 2>&1; then
  echo "expected fork-with-args fixture to fail" >&2
  exit 1
fi
grep -q 'E_HIR' "${reject_output}"
grep -qF 'spawned call must have zero arguments' "${reject_output}"

# Reject: fork-block body with a type-mismatched argument (R1 pin).
# Pins the HIR fail-closed wall: fork { f(wrongtype); } must produce a
# source-spanned E_HIR diagnostic, never E_CODEGEN_FRONT.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/fork_block_arg_type_mismatch.hew" >"${reject_output}" 2>&1; then
  echo "expected fork-block-arg-type-mismatch fixture to fail" >&2
  exit 1
fi
grep -q 'E_HIR' "${reject_output}"
grep -qF 'fork block callee must have zero arguments' "${reject_output}"

# Reject: `fork { ... }` block with multiple statements.
# Pins the HIR fail-closed fork-block body boundary.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/fork_multi_stmt.hew" >"${reject_output}" 2>&1; then
  echo "expected fork-multi-stmt fixture to fail" >&2
  exit 1
fi
grep -q 'E_HIR' "${reject_output}"
grep -qF 'fork block body must contain exactly one statement or expression' "${reject_output}"

# Reject: `after(duration) { ... }` with a non-empty timeout body.
# Pins the HIR fail-closed deadline body boundary.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/scope_deadline_body.hew" >"${reject_output}" 2>&1; then
  echo "expected scope-deadline-body fixture to fail" >&2
  exit 1
fi
grep -q 'E_HIR' "${reject_output}"
grep -qF 'deadline body must be empty' "${reject_output}"

# Reject: `for x in non_iterable` — Vec<T> is accepted through IntoIterator,
# but values with no iterable contract must still fail closed.
if "${HEW}" compile "${ROOT}/tests/vertical-slice/reject/for_non_range_iterable.hew" >"${reject_output}" 2>&1; then
  echo "expected for-non-range-iterable fixture to fail" >&2
  exit 1
fi
grep -qF 'type is not iterable' "${reject_output}"

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

# Accept + run: generator bodies that read FREE VARIABLES — a `gen fn`'s formal
# parameter and a `gen { }` block's captured outer locals. Both travel through
# the runtime env channel (`hew_gen_ctx_create` deep-copies the env into the
# body thread). These compile to native binaries and run end-to-end, pinning
# the parameter-read repro (`counter(3)`) and the outer-local capture repro so
# the never-completed free-variable lowering cannot silently regress.
# Capture-free generators are covered by the gen_block check fixtures above and
# by tests/hew/generator_param_capture_test.hew.
run_accept_expect_stdout "gen_fn_param_capture"
run_accept_expect_stdout "gen_block_capture_outer"

# Accept: a `gen fn` with a bare named-fn reference parameter (`fn(i64)->i64`).
# A named-fn reference lowers to a two-word {code_ptr, env_ptr} fat pointer
# with env_ptr null by construction — safe to flat-copy across the generator
# thread boundary. Before the fix the fn-typed param was rejected as
# PersistentShare and poisoned sibling scalar params via all_materialisable=false.
run_accept_expect_stdout "gen_fn_fn_typed_param"

# Reject: a generator that captures a closure-with-env must still fail closed
# after the fn-typed-param gate was widened. A closure literal capturing an
# outer binding carries a heap-boxed env; flat-copying it across the thread
# boundary would shallow-alias the caller's env (double-free / UAF at teardown).
# The gate admits only null-env fns and empty-capture closures.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/gen_fn_closure_env_capture.hew" \
  "capture of opaque/owned value" \
  "gen_fn_closure_env_capture"

# Reject: a generator that captures an `#[opaque]` runtime handle as a free
# variable must fail closed. An opaque handle classifies as `BitCopy`
# (pointer-width, no drop), so the capture gate's `BitCopy` check alone would
# admit it — but `hew_gen_ctx_create` flat-`memcpy`s the env across the body
# thread, and shallow-copying an opaque handle aliases the caller's handle
# (use-after-free / double-free at teardown). The gate rejects any value
# transitively containing an opaque handle, not only non-`BitCopy` heap owners.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/gen_fn_capture_opaque_handle.hew" \
  "capture of opaque/owned value" \
  "gen_fn_capture_opaque_handle"

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

# Accept: literal payload subpattern in a constructor match arm compares the
# payload value. Shape::Line(1) must not silently lower as a wildcard that also
# matches Shape::Line(2); exit 2 proves the fallback binding arm handled it.
run_accept_expect_status "enum_payload_literal_subpattern" 2

# Reject: literal-only integer matches cover only selected values, never the
# full integer domain. These must fail under `hew check` until a catch-all arm is
# present, regardless of signedness or width.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/match_i32_literal_nonexhaustive.hew" \
  "non-exhaustive match: missing _" \
  "match_i32_literal_nonexhaustive"
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/match_u8_literal_nonexhaustive.hew" \
  "non-exhaustive match: missing _" \
  "match_u8_literal_nonexhaustive"
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/match_usize_literal_nonexhaustive.hew" \
  "non-exhaustive match: missing _" \
  "match_usize_literal_nonexhaustive"

# Accept: integer matches with a catch-all arm and a complete bool match remain
# exhaustive and run successfully end-to-end.
run_accept_expect_status "match_i32_literal_catchall_zero" 0
run_accept_expect_status "match_u8_literal_catchall" 0
run_accept_expect_status "match_bool_true_false_exhaustive" 0

# ---------------------------------------------------------------------------
# Regex literal match-arm patterns
# ---------------------------------------------------------------------------

# Typechecker-accept: `re"..."` in match-arm predicate position validates regex
# syntax, then `hew check` fails closed at codegen-front until regex literal
# handles are threaded through the pipeline.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/accept/regex_match_arm.hew" \
  "hew_regex_match: @hew_regex_handles global not found" \
  "regex_match_arm"

# Reject: malformed regex literal in match-arm position (E_INVALID_REGEX_LITERAL).
# The type checker validates regex syntax before HIR lowering.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/regex_invalid_pattern.hew" >"${reject_output}" 2>&1; then
  echo "expected regex_invalid_pattern fixture to fail" >&2
  exit 1
fi
grep -qF 'invalid regex pattern' "${reject_output}"

if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/string_embedded_nul.hew" >"${reject_output}" 2>&1; then
  echo "expected string_embedded_nul fixture to fail" >&2
  exit 1
fi
grep -qF 'embedded NUL (\0) in string literal is not supported by the null-terminated string ABI' "${reject_output}"
grep -qF 'string_embedded_nul.hew:2:13' "${reject_output}"

if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/unicode_rune_len_wrong_type.hew" >"${reject_output}" 2>&1; then
  echo "expected unicode_rune_len_wrong_type fixture to fail" >&2
  exit 1
fi
grep -q 'rune_len' "${reject_output}"

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
run_accept_expect_stdout "vec_generic_wrap_record_layout"
run_accept_expect_stdout "vec_generic_pair_record_layout"
run_accept_expect_stdout "vec_generic_holder_point_layout"
run_accept_expect_stdout "vec_generic_nested_wrap_layout"

# ---------------------------------------------------------------------------
# W3.043 — integer literal inference + record field mutability
# ---------------------------------------------------------------------------

# Accept: unsuffixed integer literals infer from adjacent concrete integer
# annotations, including narrower widths. Checker-only fixture: the current
# native MIR slice does not lower every narrow-width literal path yet.
if ! "${HEW}" check \
    "${ROOT}/tests/vertical-slice/accept/int_literal_inference.hew" \
    >"${accept_output}" 2>&1; then
  echo "W3.043: expected int_literal_inference to pass hew check" >&2
  cat "${accept_output}" >&2
  exit 1
fi

# Reject: out-of-range unsuffixed integer literal must fail against the
# adjacent annotation rather than silently defaulting to i64.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/int_literal_inference_overflow.hew" \
    >"${reject_output}" 2>&1; then
  echo "W3.043: expected int_literal_inference_overflow to fail" >&2
  exit 1
fi
grep -qF 'does not fit' "${reject_output}"

# Accept: mutable record bindings can be updated through functional update.
# Direct field-write acceptance is covered by hew-types checker tests; native
# MIR field-store lowering is outside this type-system polish lane.
if ! "${HEW}" check \
    "${ROOT}/tests/vertical-slice/accept/record_field_mutation_mut_binding.hew" \
    >"${accept_output}" 2>&1; then
  echo "W3.043: expected record_field_mutation_mut_binding to pass hew check" >&2
  cat "${accept_output}" >&2
  exit 1
fi

# Reject: record field writes through immutable bindings remain disallowed.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/record_field_mutation_immutable_binding.hew" \
    >"${reject_output}" 2>&1; then
  echo "W3.043: expected record_field_mutation_immutable_binding to fail" >&2
  exit 1
fi
grep -qF 'immutable binding' "${reject_output}"

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

# ---------------------------------------------------------------------------
# vec-generic-index — scalar `xs[i]` on Vec<T> for any supported element type
# ---------------------------------------------------------------------------

# Accept (S0): scalar index on Vec<string>. `xs[i]` returns a fresh retained
# owner (hew_vec_get_str refcount bump); the Vec keeps its own reference and is
# re-read after indexing, proving indexing CLONES rather than moves out. Exit
# 13 = "alpha".len() + "gamma".len() + xs.len() (5 + 5 + 3). Guards the
# generic-Vec-index drop balance end-to-end (a double-free would abort, a
# move-out would corrupt the re-read length).
run_accept_expect_status "vec_string_index" 13

# Accept (security-review follow-on): DISCARDED Vec<string> scalar index in a
# loop over HEAP strings. `xs[j];` retains a fresh owner via hew_vec_get_str
# each iteration; the general owned-string temp substrate's DISCARD path balances
# it with an inline per-iteration hew_string_drop. 4000 retained-then-dropped
# owners, then the Vec is reused. Exit 9 = xs[0].len() + xs.len() (5 + 4). A
# double-free would abort/segfault on the reuse; the structural no-leak
# (drop-emitted) proof is the MIR canary
# vec_string_discarded_scalar_index_in_loop_emits_one_drop_site.
run_accept_expect_status "vec_string_index_discard_loop" 9

# Accept (security-review follow-on): BOUND-and-used (`let y = xs[j]; y.len()`)
# and NESTED (`xs[j].len()`) Vec<string> scalar index in a loop over HEAP
# strings — the two shapes the re-review required the general owned-string temp
# substrate to balance (BOUND via a scope-exit CowHeap drop, NESTED via an inline
# drop). 8000 retained-then-released owners, then the Vec is reused. Exit 14 =
# xs[0].len() + xs[3].len() + xs.len() (5 + 5 + 4). A double-free would trip the
# runtime free_cstring header sentinel (abort) on the reuse; the structural
# exactly-one-drop proofs are the MIR canaries index_bound_releases_exactly_once
# and index_nested_in_loop_balances.
run_accept_expect_status "vec_string_index_use_loop" 14

# Accept (S0): scalar index control on Vec<i64> (BitCopy scalar element). Same
# bounds-checked lower_vec_index path, dispatching to hew_vec_get_i64. Exit 22
# = xs[1] + xs[2] - xs[0] (20 + 12 - 10).
run_accept_expect_status "vec_i64_index" 22

# Reject: scalar index on Vec<f32> is fail-closed — narrower scalars
# (i8/u8/i16/u16/f32/isize/usize) have no hew_vec_get_T getter yet, so the HIR
# element-type gate surfaces it as a compile-time diagnostic instead of a deep
# MIR NotYetImplemented.
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/reject/vec_index_unsupported_elem.hew" \
  "Vec<f32> scalar index (xs[i]) is not yet supported" \
  "vec_index_unsupported_elem"

# Accept (S1): Vec::<i64>::new() turbofish syntax with type annotation. Exit 0.
run_accept_expect_status "vec_new_turbofish_type" 0

# Accept (S1): Vec::<i64>::new() turbofish with push/pop chain. Exit 7.
run_accept_expect_status "vec_new_turbofish_method" 7

# Accept: HashMap::<K, V>::new() turbofish syntax.  `hew check` is enough here:
# the regression is the checker rejecting type args before constructor lowering.
"${TIMEOUT}" 30 "${HEW}" check "${ROOT}/tests/vertical-slice/accept/hashmap_new_turbofish_type.hew"

# Accept: HashSet::<T>::new() turbofish syntax.
"${TIMEOUT}" 30 "${HEW}" check "${ROOT}/tests/vertical-slice/accept/hashset_new_turbofish_type.hew"

# ---------------------------------------------------------------------------
# W3 collections-sugar S2: string codepoint index + slice (locked Q-CS1).
# ---------------------------------------------------------------------------
# Accept: ASCII codepoint slice. "hello"[1..4] = "ell" → byte_length = 3.
run_accept_expect_status "string_slice_codepoint" 3
# Accept: closed range prefix. "héllo"[0..2] = "hé" → byte_length = 3.
run_accept_expect_status_and_stdout "string_slice_closed_prefix" 3
# Accept: open-end codepoint slice. "héllo"[1..] = "éllo" → byte_length = 5.
run_accept_expect_status_and_stdout "string_slice_open_right" 5
# Accept: full-range codepoint slice. "hié"[..] = "hié" → byte_length = 4.
run_accept_expect_status_and_stdout "string_slice_full" 4
# Accept: ASCII single-element index path runs without panic and the
# accompanying slice [1..2] returns a 1-byte string.
run_accept_expect_status "string_index_codepoint" 1
# Accept: multi-byte UTF-8 codepoint slice. "héllo"[1..2] = "é" = 2 UTF-8
# bytes. A byte-clamping slice would yield 1 byte and either truncate
# or abort — this fixture proves we walk codepoints.
run_accept_expect_status "string_slice_multibyte" 2
# Accept (panic semantics): out-of-bounds codepoint slice ABORTS. Exit
# 134 = SIGABRT (libc::abort from hew_string_slice_codepoints). Q-CS1
# locks panic-on-OOB; no clamp / null / empty-string fallback.
run_accept_expect_status "string_slice_oob_panics" 134

# Accept (S1): Vec<i64> push/pop/get/contains via catalog entries. Exit 10.
run_accept_expect_status "vec_i64_basic" 10

# Accept (W3 Stage 3, typecheck-only): `impl<T> IntoIterator for Vec<T>` ships
# in std/builtins.hew. The checker must resolve `v.into_iter()` to
# `VecIter<T>` for both Vec<i32> and Vec<i64> (the two stdlib catalog
# instantiations). HIR-level dispatch wiring for user trait method calls is
# owned by Stage 4 (for-loop desugar) — this slice intentionally stops at
# typecheck. VecIter record MIR lowering is still NYI, so pin that diagnostic
# positively rather than swallowing the check exit code.
if "${TIMEOUT}" 30 "${HEW}" check \
    "${ROOT}/tests/vertical-slice/accept/vec_into_iter_typeck.hew" \
    >"${accept_output}" 2>&1; then
  echo "W3 Stage 3: expected VecIter MIR lowering to remain diagnostic until implemented" >&2
  cat "${accept_output}" >&2
  exit 1
fi
if grep -q "no method \`into_iter\`" "${accept_output}"; then
  echo "W3 Stage 3: Vec<T>::into_iter must resolve through IntoIterator impl" >&2
  cat "${accept_output}" >&2
  exit 1
fi
if grep -q "no field\|undefined type \`VecIter\`" "${accept_output}"; then
  echo "W3 Stage 3: VecIter<T> must be defined in std/builtins.hew" >&2
  cat "${accept_output}" >&2
  exit 1
fi
grep -q 'E_NOT_YET_IMPLEMENTED' "${accept_output}"
grep -q 'VecIter' "${accept_output}"

# Reject (S1): Vec::<i64, i32>::new() turbofish arity mismatch — Vec takes exactly
# 1 type argument; supplying 2 must produce a clear diagnostic.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/vec_new_turbofish_arity_mismatch.hew" \
    >"${reject_output}" 2>&1; then
  echo "W3.004: expected vec_new_turbofish_arity_mismatch to fail" >&2
  exit 1
fi
grep -q 'takes 1 type argument but 2 were supplied' "${reject_output}"

# Reject: HashMap takes key and value type arguments.
if "${TIMEOUT}" 30 "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/hashmap_new_turbofish_arity_mismatch.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected hashmap_new_turbofish_arity_mismatch to fail" >&2
  exit 1
fi
grep -q 'takes 2 type arguments but 1 was supplied' "${reject_output}"

# Reject: HashSet takes one element type argument.
if "${TIMEOUT}" 30 "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/hashset_new_turbofish_arity_mismatch.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected hashset_new_turbofish_arity_mismatch to fail" >&2
  exit 1
fi
grep -q 'takes 1 type argument but 2 were supplied' "${reject_output}"

# Accept: concrete generic user aggregate with a heap-owning string field.
run_accept_expect_status "user_record_non_bitcopy" 0

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

# Accept: builtin (stdlib) Option<i64> with bare `None`/`Some` — NO user
# `enum Option<T>` redeclaration (the W4.042 workaround). Exercises the checker
# now recording the bare `None`'s resolved type at its span so HIR stamps the
# concrete `Option<i64>` args and codegen resolves `Option$$i64` instead of
# tripping the D10 wall. Exit 99 proves the None arm drove the exit; exit 42
# proves the Some payload round-tripped through the match.
run_accept_expect_status "stdlib_option_none" 99
run_accept_expect_status "stdlib_option_some" 42
run_accept_expect_status "stdlib_option_predicates" 0

# WASM parity (W4.042): the bare-`None` builtin Option<i64> path must also lower
# under wasm32-unknown-unknown. The fix is pure checker-boundary type recording
# (hew-types) with no native-only codegen change, so behavioural parity is
# inherited from the shared MIR->LLVM lower; this gate pins that the wasm target
# emits a module for the stdlib bare-None path.
"${HEW}" compile --target wasm32-unknown-unknown \
  "${ROOT}/tests/vertical-slice/accept/stdlib_option_none.hew" \
  >"${accept_output}" 2>&1
test -s "${ROOT}/.tmp/compile-out/stdlib_option_none.wasm"

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

# W5.020 — heap-owning ENUM composite returns (`Result<T, string>` /
# `Option<string>` / user enums with an owned-payload variant). The move-out +
# caller-side tag-aware in-place drop spine transfers the active payload's single
# owner across the return boundary and drops it exactly once. (These were a
# reject fixture before W5.020.)

# Accept: Option<string> composite return — Some("hello") matched in the caller.
# Exit 1 proves the heap-owning composite crossed the boundary and matched.
run_accept_expect_status "composite_return_heap_owning" 1

# Runtime oracle: fn handle(...) -> Result<i64, string>, BOTH arms exercised.
# Ok arm returns a bitcopy i64; Err arm returns an owned string. Asserting the
# observable stdout proves both drop paths run end to end with correct output.
run_accept_expect_stdout "result_handler_heap_oracle"

# Runtime oracle: heap payload in the Ok position (Result<string, i64>) — the
# gate keys on which variant owns heap, not arm position.
run_accept_expect_stdout "result_ok_string_payload"

# Runtime oracle: `?` on a heap-owning Result return path. Unblocked by the gate
# lift with ZERO HIR change (the W3.044 `?` desugar was already type-agnostic).
run_accept_expect_stdout "try_op_heap_result"

# Double-free guard: 50k iterations each construct, return, and drop a
# heap-owning Result across both arms. A clean exit (no SIGABRT from the
# free_cstring header-sentinel guard) is the behavioural proof the active
# payload is released without a double-free. (Per-iteration loop-scope leak is a
# pre-existing W5.011-shared limitation, not a double-free — see the fixture
# header.) (raii-null-after-move, cleanup-all-exits, boundary-fail-closed.)
run_accept_expect_stdout "result_heap_drop_loop"

# I4 FFI allocator fix: stream.from_file on a missing path returns Err(string)
# where the string came from hew_stream_last_error(). The Hew drop spine frees
# it via hew_string_drop → free_cstring. Before the fix the raw libc::malloc
# allocation lacked the header sentinel and free_cstring called libc::abort.
# A clean exit (no SIGABRT) proves the alloc_cstring_from_str path is correct.
run_accept_expect_stdout "stream_last_error_drops_cleanly"

# Accept: tuple composite return carrying a heap-owned string. This used to
# fail closed before tuple composite-return drop coverage landed; exit 42 proves
# the caller receives the scalar field while the string payload remains owned.
run_accept_expect_status "tuple_heap_return" 42

# WASM: composite return for Option<i64> compiles to wasm32.
"${HEW}" compile --target wasm32-unknown-unknown \
    "${ROOT}/tests/vertical-slice/accept/composite_return_option_some.hew" \
    >"${accept_output}" 2>&1

# WASM: composite return for Result<i64, i64> compiles to wasm32.
"${HEW}" compile --target wasm32-unknown-unknown \
    "${ROOT}/tests/vertical-slice/accept/composite_return_result.hew" \
    >"${accept_output}" 2>&1

# WASM parity (W5.020): the heap-owning enum composite return + caller-side
# tag-aware in-place drop must emit on wasm32 too, not native-only. The
# `emit_one_elab_drop` consumer and `__hew_enum_drop_inplace_*` synthesis are
# backend-agnostic; this asserts the wasm path stays compiled (native-wasm-parity).
"${HEW}" compile --target wasm32-unknown-unknown \
    "${ROOT}/tests/vertical-slice/accept/result_handler_heap_oracle.hew" \
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

# Accept: Node::lookup(name) must expose the registered local actor as a
# RemotePid<T>, and pid.tell(msg) must deliver through the in-process
# send-by-id path.
run_accept_expect_status "node_lookup_tell" 0

# A640/S3: compile-time Serializable floor for RemotePid<T>::tell.
"${HEW}" check "${ROOT}/tests/vertical-slice/accept/positive_record_remote_tell.hew" \
    >"${accept_output}" 2>&1
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/negative_fn_msg_remote_tell.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected negative_fn_msg_remote_tell fixture to fail" >&2
  exit 1
fi
grep -q 'Serializable' "${reject_output}"
expect_check_fail_contains \
  "${ROOT}/tests/vertical-slice/accept/local_fn_msg_actor_method_allowed.hew" \
  "cross-node serialize: unsupported value type Function" \
  "local_fn_msg_actor_method_allowed"

# ---------------------------------------------------------------------------
# W2.006 Stage 1 — HewScope removal: scope{} MIR shape invariant.
# ---------------------------------------------------------------------------
# Accept fixture: `scope { fork { worker(); } }` inside an actor handler
# must:
#   1. typecheck cleanly (`hew check` exits 0).
#   2. produce a RawMirFunction whose instruction stream contains the
#      canonical TaskScope* and TaskNew runtime-call families.
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
grep -qF 'family: TaskScopeNew,' "${accept_output}" || {
  echo "W2.006: MIR dump must contain the TaskScopeNew runtime-call family" >&2
  cat "${accept_output}" >&2
  exit 1
}
grep -qF 'family: TaskScopeSpawn,' "${accept_output}" || {
  echo "W2.006: MIR dump must contain the TaskScopeSpawn runtime-call family" >&2
  cat "${accept_output}" >&2
  exit 1
}
grep -qF 'family: TaskNew,' "${accept_output}" || {
  echo "W2.006: MIR dump must contain the TaskNew runtime-call family (preceding TaskScopeSpawn)" >&2
  cat "${accept_output}" >&2
  exit 1
}
grep -qF 'family: TaskScopeDestroy,' "${accept_output}" || {
  echo "W2.006: MIR dump must contain the TaskScopeDestroy runtime-call family" >&2
  cat "${accept_output}" >&2
  exit 1
}
# Legacy ABI must be fully removed.
if grep -qE 'hew_scope_(spawn|new|create|free|destroy|cancel|is_cancelled|wait_all)' "${accept_output}"; then
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
run_native_under_memory_cap "${ROOT}/examples/v05/hashmap_run_pass.hew"

# W4.045: HashSet<i64> / HashSet<string> actor-state drop run-pass. Regression
# guard for the P0 use-after-free where scalar/string-element sets dropped a
# layout-keyed `HewLayoutHashSet*` through the legacy `hew_hashset_free`,
# corrupting the heap on actor shutdown. The actor holds two populated sets
# moved in from `main`; teardown frees them through the corrected
# `hew_hashset_free_layout`. Post-fix it prints `ok` and exits 0.
out="$(run_native_under_memory_cap "${ROOT}/examples/v05/hashset_actor_drop_run_pass.hew")"
if [[ "${out}" != "ok" ]]; then
  echo "W4.045: hashset actor drop run-pass: expected 'ok', got '${out}'" >&2
  exit 1
fi

# W5.001 (F0a): Vec<i64> + HashMap<string,i64> actor-state drop run-pass.
# Companion to the HashSet guard above. Pins the descriptor-derived collection
# memory consolidation: `collection_layout_witness` is the sole authority for
# Vec/HashMap clone+free symbol selection, so the constructor, clone, and free
# can never select mismatched ABI families (the W4.045 UAF class). The actor
# holds a populated map and vec moved in from `main`; teardown frees them
# through the witness-selected `hew_hashmap_free_layout` / `hew_vec_free`.
# Prints `ok` and exits 0. WASM is deliberately covered by the codegen
# fail-closed substrate gate (native-only layout family; tracked #1820).
out="$(run_native_under_memory_cap "${ROOT}/examples/v05/collection_actor_drop_run_pass.hew")"
if [[ "${out}" != "ok" ]]; then
  echo "W5.001: collection actor drop run-pass: expected 'ok', got '${out}'" >&2
  exit 1
fi

# W5.002 (F0b): Vec<string> actor-state drop run-pass. Pins the Vec migration
# onto the witness-managed clone/free pair (`hew_vec_clone_managed` /
# `hew_vec_free_managed`). The actor holds a populated `Vec<string>` (one heap
# `strdup` per slot) moved in from `main`; teardown frees each owned string
# through the witness-selected `hew_vec_free_managed`. A mismatched free would
# double-free or leak — so a clean `ok`/exit-0 is the load-bearing signal that
# the managed per-slot free is correct. WASM is deliberately covered by the
# native-only collection substrate gate (tracked #1820).
out="$(run_native_under_memory_cap "${ROOT}/examples/v05/vec_string_actor_drop_run_pass.hew")"
if [[ "${out}" != "ok" ]]; then
  echo "W5.002: vec<string> actor drop run-pass: expected 'ok', got '${out}'" >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# Q004: impl-vs-trait method signature equivalence checked at the impl site.
#
# Locks the diagnostic-authority property recorded in LESSONS row
# `diagnostic-trust` (P1): a wrong impl signature must be rejected locally at
# the impl method's span with `TraitImplSignatureMismatch`, not surface
# downstream as a confusing "type does not satisfy trait" cascade.
# ---------------------------------------------------------------------------

# Accept: impl signature structurally matches the trait after `Self`-sub and
# `Self::Item` projection (Container::unwrap → Holder::unwrap), so the program
# compiles and runs.
run_accept_expect_status "q004_trait_impl_sig_match" 7

q004_check_reject() {
  local fixture="$1"
  local detail_substr="$2"
  if "${HEW}" compile \
      "${ROOT}/tests/vertical-slice/reject/${fixture}.hew" \
      >"${reject_output}" 2>&1; then
    echo "Q004: expected ${fixture} fixture to fail" >&2
    cat "${reject_output}" >&2
    exit 1
  fi
  # Q004 diagnostics are rendered as human-readable messages anchored at the
  # impl method span ("impl method `Type::method` ..."). We check both the
  # common impl-method prefix and a discriminating per-fixture substring so a
  # regression to a generic "type does not satisfy trait" cascade is caught.
  grep -q 'error: impl method `' "${reject_output}" || {
    echo "Q004: ${fixture}: expected impl-method-anchored diagnostic" >&2
    cat "${reject_output}" >&2
    exit 1
  }
  grep -q -- "${detail_substr}" "${reject_output}" || {
    echo "Q004: ${fixture}: expected diagnostic to mention '${detail_substr}'" >&2
    cat "${reject_output}" >&2
    exit 1
  }
}

# shellcheck disable=SC2016  # backticks inside the patterns are literal — they
# match the CLI diagnostic's pretty-printed type names (e.g. `Option<i64>`),
# not command substitution.
q004_check_reject "q004_trait_impl_sig_wrong_return" \
  'returns `i64` but trait `Container` requires `Option<i64>`'
# shellcheck disable=SC2016
q004_check_reject "q004_trait_impl_sig_wrong_arity" \
  'has 1 parameter(s) but trait `Container` declares 0'
# shellcheck disable=SC2016
q004_check_reject "q004_trait_impl_sig_wrong_param" \
  'parameter `key` has type `string` but trait `Indexer` requires `i32`'

# ---------------------------------------------------------------------------
# W3.020 — #[opaque] runtime handles
# ---------------------------------------------------------------------------

# Accept: an #[opaque] handle lowers to a bare `ptr` and round-trips through the
# real `hew_deque_*` runtime FFI. main() pushes three elements; len == 3.
run_accept_expect_status "opaque_handle_ffi_round_trip" 3

# Reject: direct construction — an opaque handle has no constructible record
# layout (it lowers to `ptr`); only FFI may produce one.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/opaque_handle_construct.hew" >"${reject_output}" 2>&1; then
  echo "expected opaque_handle_construct fixture to fail" >&2
  exit 1
fi

# Reject: field access — an opaque handle has no fields.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/opaque_handle_field_access.hew" >"${reject_output}" 2>&1; then
  echo "expected opaque_handle_field_access fixture to fail" >&2
  exit 1
fi
grep -q 'no field' "${reject_output}"

# Reject: #[opaque] on a non-empty body.
if "${HEW}" check "${ROOT}/tests/vertical-slice/reject/opaque_handle_non_empty_body.hew" >"${reject_output}" 2>&1; then
  echo "expected opaque_handle_non_empty_body fixture to fail" >&2
  exit 1
fi
grep -q 'E_OPAQUE_TYPE_SHAPE' "${reject_output}"

# ---------------------------------------------------------------------------
# NEW-6b — `await … | after d` deadlines on suspendable actor asks
# ---------------------------------------------------------------------------

# Accept (worker-free deadline oracle): `await s.block() | after 60ms` where the
# callee parks worker-free on an empty channel recv and never replies. Under
# HEW_WORKERS=1 the deadline timer (its own ticker thread) cancels the in-flight
# ask and resumes the caller with Err(AskError::Timeout) → exit 7. Proves the
# deadline-before-reply race AND that the lone worker is freed + resumed by the
# timer (a blocking path would strand the worker / hang).
compile_accept "await_ask_deadline_timeout"
timeout_bin="${ROOT}/.tmp/compile-out/await_ask_deadline_timeout"
deadline_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${timeout_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  deadline_status=0
else
  deadline_status=$?
fi
if [[ "${deadline_status}" -ne 7 ]]; then
  echo "expected await_ask_deadline_timeout (HEW_WORKERS=1) to exit 7, got ${deadline_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (reply-before-deadline race): `await f.answer() | after 5000ms` where
# the callee replies immediately. The long deadline never fires and is cancelled
# on the reply → Ok(42) → exit 42 (no spurious timeout).
compile_accept "await_ask_deadline_ok"
ok_bin="${ROOT}/.tmp/compile-out/await_ask_deadline_ok"
ok_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${ok_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  ok_status=0
else
  ok_status=$?
fi
if [[ "${ok_status}" -ne 42 ]]; then
  echo "expected await_ask_deadline_ok (HEW_WORKERS=1) to exit 42, got ${ok_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (worker-free read-deadline oracle): `await conn.read() | after 60ms`
# where the peer stays silent. Under HEW_WORKERS=1 the read parks worker-free and
# the deadline timer resumes the actor with Err(IoError::TimedOut) -> exit 7.
compile_accept "await_read_deadline_deferred"
read_timeout_bin="${ROOT}/.tmp/compile-out/await_read_deadline_deferred"
read_deadline_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${read_timeout_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  read_deadline_status=0
else
  read_deadline_status=$?
fi
if [[ "${read_deadline_status}" -ne 7 ]]; then
  echo "expected await_read_deadline_deferred (HEW_WORKERS=1) to exit 7, got ${read_deadline_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (read-before-deadline race): peer writes immediately; the read completes
# first, cancels the long deadline, and binds Ok(bytes) -> exit 42.
compile_accept "await_read_deadline_ok"
read_ok_bin="${ROOT}/.tmp/compile-out/await_read_deadline_ok"
read_ok_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${read_ok_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  read_ok_status=0
else
  read_ok_status=$?
fi
if [[ "${read_ok_status}" -ne 42 ]]; then
  echo "expected await_read_deadline_ok (HEW_WORKERS=1) to exit 42, got ${read_ok_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (read_string worker-free timeout oracle): `await conn.read_string() | after 60ms`
# where the peer stays silent. Under HEW_WORKERS=1 the read parks worker-free and
# the deadline timer resumes the actor with Err(IoError::TimedOut) -> exit 7.
# Proves the to_string deadline path does NOT apply bytes-to-string on the timeout edge.
compile_accept "await_read_string_deadline_timeout"
read_str_timeout_bin="${ROOT}/.tmp/compile-out/await_read_string_deadline_timeout"
read_str_timeout_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${read_str_timeout_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  read_str_timeout_status=0
else
  read_str_timeout_status=$?
fi
if [[ "${read_str_timeout_status}" -ne 7 ]]; then
  echo "expected await_read_string_deadline_timeout (HEW_WORKERS=1) to exit 7, got ${read_str_timeout_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (read_string-before-deadline race): peer writes immediately; the read
# completes first, cancels the long deadline, and binds Ok(string) -> exit 42.
compile_accept "await_read_string_deadline_ok"
read_str_ok_bin="${ROOT}/.tmp/compile-out/await_read_string_deadline_ok"
read_str_ok_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${read_str_ok_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  read_str_ok_status=0
else
  read_str_ok_status=$?
fi
if [[ "${read_str_ok_status}" -ne 42 ]]; then
  echo "expected await_read_string_deadline_ok (HEW_WORKERS=1) to exit 42, got ${read_str_ok_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Reject (fail-closed): read deadlines require a suspendable context. A default
# `main` has no parkable continuation to resume on timeout.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_read_deadline_default_context.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_read_deadline_default_context fixture to fail" >&2
  exit 1
fi
grep -q 'non-suspendable context' "${reject_output}"

# Reject (fail-closed, non-suspendable): `await conn.read_string() | after d` in a
# plain function — same guard as the raw-read form above.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_read_string_deadline_non_suspendable.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_read_string_deadline_non_suspendable fixture to fail" >&2
  exit 1
fi
grep -q 'non-suspendable context' "${reject_output}"

# Reject (fail-closed, non-suspendable): `await ln.accept() | after d` in a
# plain function — no parkable continuation for the timer to resume.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_accept_deadline_non_suspendable.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_accept_deadline_non_suspendable fixture to fail" >&2
  exit 1
fi
grep -q 'non-suspendable context' "${reject_output}"

# Reject (fail-closed, deferred to v0.6): a deadline on a SUSPENDING-CLOSURE call.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_closure_deadline_deferred.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_closure_deadline_deferred fixture to fail" >&2
  exit 1
fi
grep -q 'on a non-await expression' "${reject_output}"

# Reject (fail-closed, deferred to v0.6): a deadline on a TASK-await. The
# `fork t = work()` binding type-checks; the HIR await-deadline gate rejects
# the `| after d` clause on the task-await at compile time.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_task_deadline_deferred.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_task_deadline_deferred fixture to fail" >&2
  exit 1
fi
grep -q 'task-await and suspending-closure deadlines are deferred' "${reject_output}"

# Accept (NEW-7 — recv-deadline ok): item arrives before deadline → Ok(Some(v)) → exit 42.
compile_accept "await_recv_deadline_ok"
recv_ok_bin="${ROOT}/.tmp/compile-out/await_recv_deadline_ok"
recv_ok_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${recv_ok_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  recv_ok_status=0
else
  recv_ok_status=$?
fi
if [[ "${recv_ok_status}" -ne 42 ]]; then
  echo "expected await_recv_deadline_ok (HEW_WORKERS=1) to exit 42, got ${recv_ok_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (NEW-7 — recv-deadline timeout): deadline fires before any send → Err(_) → exit 7.
compile_accept "await_recv_deadline_timeout"
recv_timeout_bin="${ROOT}/.tmp/compile-out/await_recv_deadline_timeout"
recv_timeout_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${recv_timeout_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  recv_timeout_status=0
else
  recv_timeout_status=$?
fi
if [[ "${recv_timeout_status}" -ne 7 ]]; then
  echo "expected await_recv_deadline_timeout (HEW_WORKERS=1) to exit 7, got ${recv_timeout_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (NEW-7 — recv-deadline closed): channel closed before deadline → Ok(None) → exit 5.
compile_accept "await_recv_deadline_closed"
recv_closed_bin="${ROOT}/.tmp/compile-out/await_recv_deadline_closed"
recv_closed_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${recv_closed_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  recv_closed_status=0
else
  recv_closed_status=$?
fi
if [[ "${recv_closed_status}" -ne 5 ]]; then
  echo "expected await_recv_deadline_closed (HEW_WORKERS=1) to exit 5, got ${recv_closed_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (accept-deadline worker-free timeout oracle): `await ln.accept() | after 60ms`
# where no client connects. Under HEW_WORKERS=1 the accept parks worker-free and
# the deadline timer resumes the actor with Err(IoError::TimedOut) -> exit 7.
compile_accept "await_accept_deadline_timeout"
accept_timeout_bin="${ROOT}/.tmp/compile-out/await_accept_deadline_timeout"
accept_timeout_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${accept_timeout_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  accept_timeout_status=0
else
  accept_timeout_status=$?
fi
if [[ "${accept_timeout_status}" -ne 7 ]]; then
  echo "expected await_accept_deadline_timeout (HEW_WORKERS=1) to exit 7, got ${accept_timeout_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (accept-before-deadline race): Acceptor creates a fresh listener,
# fires Connector to connect after 100 ms, then parks on the accept slot with a
# 5 s deadline. The connection arrives first, the accept completes with Ok(conn)
# -> Acceptor returns 42 -> main exits 42.
compile_accept "await_accept_deadline_ok"
accept_ok_bin="${ROOT}/.tmp/compile-out/await_accept_deadline_ok"
accept_ok_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${accept_ok_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  accept_ok_status=0
else
  accept_ok_status=$?
fi
if [[ "${accept_ok_status}" -ne 42 ]]; then
  echo "expected await_accept_deadline_ok (HEW_WORKERS=1) to exit 42, got ${accept_ok_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (NEW-7 — stream-recv-deadline ok): frame arrives before deadline → Ok(Some) → exit 1.
compile_accept "await_stream_recv_deadline_ok"
stream_recv_ok_bin="${ROOT}/.tmp/compile-out/await_stream_recv_deadline_ok"
stream_recv_ok_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${stream_recv_ok_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  stream_recv_ok_status=0
else
  stream_recv_ok_status=$?
fi
if [[ "${stream_recv_ok_status}" -ne 1 ]]; then
  echo "expected await_stream_recv_deadline_ok (HEW_WORKERS=1) to exit 1, got ${stream_recv_ok_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (NEW-7 — stream-recv-deadline timeout): deadline fires before any frame → Err(_) → exit 7.
compile_accept "await_stream_recv_deadline_timeout"
stream_recv_timeout_bin="${ROOT}/.tmp/compile-out/await_stream_recv_deadline_timeout"
stream_recv_timeout_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${stream_recv_timeout_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  stream_recv_timeout_status=0
else
  stream_recv_timeout_status=$?
fi
if [[ "${stream_recv_timeout_status}" -ne 7 ]]; then
  echo "expected await_stream_recv_deadline_timeout (HEW_WORKERS=1) to exit 7, got ${stream_recv_timeout_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (NEW-7 — element-type breadth: string): channel recv deadline with
# heap-owning string element type → Ok(Some) → exit 3.
compile_accept "await_recv_deadline_string_ok"
recv_string_ok_bin="${ROOT}/.tmp/compile-out/await_recv_deadline_string_ok"
recv_string_ok_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${recv_string_ok_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  recv_string_ok_status=0
else
  recv_string_ok_status=$?
fi
if [[ "${recv_string_ok_status}" -ne 3 ]]; then
  echo "expected await_recv_deadline_string_ok (HEW_WORKERS=1) to exit 3, got ${recv_string_ok_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Accept (NEW-7 — element-type breadth: record): channel recv deadline with
# struct/record element type (non-bitcopy layout-witness ABI) → Ok(Some(p)) → exit 30.
compile_accept "await_recv_deadline_record_ok"
recv_record_ok_bin="${ROOT}/.tmp/compile-out/await_recv_deadline_record_ok"
recv_record_ok_status=0
if "${TIMEOUT}" --kill-after=5s 30s env HEW_WORKERS=1 "${recv_record_ok_bin}" \
    >"${stdout_output}" 2>"${stderr_output}"; then
  recv_record_ok_status=0
else
  recv_record_ok_status=$?
fi
if [[ "${recv_record_ok_status}" -ne 30 ]]; then
  echo "expected await_recv_deadline_record_ok (HEW_WORKERS=1) to exit 30, got ${recv_record_ok_status}" >&2
  cat "${accept_output}" "${stdout_output}" "${stderr_output}" >&2
  exit 1
fi

# Reject (NEW-7 — non-suspendable): `await rx.recv() | after d` in a plain function.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_recv_deadline_non_suspendable.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_recv_deadline_non_suspendable fixture to fail" >&2
  exit 1
fi
grep -q 'non-suspendable context' "${reject_output}"

# Reject (NEW-6b — blocking caller): `await <actor>.<ask>() | after d` in `main`.
# Before the wall the deadline was silently dropped and the ask blocked
# until the reply.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_ask_deadline_non_suspendable.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_ask_deadline_non_suspendable fixture to fail" >&2
  exit 1
fi
grep -q 'blocking caller' "${reject_output}"

# Reject (NEW-7 — non-suspendable): `await stream.recv() | after d` in a plain function.
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_stream_recv_deadline_non_suspendable.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_stream_recv_deadline_non_suspendable fixture to fail" >&2
  exit 1
fi
grep -q 'non-suspendable context' "${reject_output}"

# Reject (fail-closed, deferred to v0.6): an actor-ask deadline with a NON-LITERAL
# duration. The codegen deadline is a compile-time constant, so a variable /
# computed duration fails CLOSED at CHECK time (not a runtime NYI, not a hang).
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_nonliteral_duration_deferred.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_nonliteral_duration_deferred fixture to fail" >&2
  exit 1
fi
grep -q 'non-literal duration' "${reject_output}"

# Reject (NEW-7 — non-literal duration): `await rx.recv() | after d` with a
# variable duration fails CLOSED at CHECK time (duration must be a literal).
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_recv_deadline_non_literal_duration.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_recv_deadline_non_literal_duration fixture to fail" >&2
  exit 1
fi
grep -q 'non-literal duration' "${reject_output}"

# Reject (NEW-7 — non-literal duration): `await stream.recv() | after d` with a
# variable duration fails CLOSED at CHECK time (duration must be a literal).
if "${HEW}" compile \
    "${ROOT}/tests/vertical-slice/reject/await_stream_recv_deadline_non_literal_duration.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected await_stream_recv_deadline_non_literal_duration fixture to fail" >&2
  exit 1
fi
grep -q 'non-literal duration' "${reject_output}"

# `lower_match_project` lowers non-BitCopy
# record/tuple match destructure (heap-owning fields), but a wildcard `_` on
# an *owned-aggregate* field (record/tuple/enum carrying heap payload) still
# fails closed — the inline-drop dispatcher only emits leaf release symbols
# (`hew_string_drop`, `hew_vec_free*`, `hew_hashmap_free_layout`,
# `hew_hashset_free_layout`, `hew_gen_free`, `hew_bytes_drop`), not
# `DropKind::RecordInPlace`. Loading such a field into a temp + inline-Drop
# would emit a wrong-ABI free, so MIR refuses. This pins the diagnostic.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/match_destructure_wildcard_owned_aggregate.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected match_destructure_wildcard_owned_aggregate fixture to fail" >&2
  exit 1
fi
grep -q 'match-destructure wildcard on owned aggregate field' "${reject_output}"

# A `BindingRef` scrutinee
# whose owned fields are destructured (full or partial) must transition to
# `Consumed` at the destructure site so the dataflow checker rejects a
# post-match read — without the consume mark, partial extraction would
# wildcard-drop `p.b` inline and then leak / UAF on a later `p.b` read.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/match_destructure_use_after_consume.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected match_destructure_use_after_consume fixture to fail" >&2
  exit 1
fi
grep -q 'UseAfterConsume' "${reject_output}"

# A `FieldAccess`
# / `TupleIndex` / `Index` / `Slice` (or captured `BindingRef`) scrutinee is
# re-readable through the same shape after the match. There is no binding
# for the dataflow checker to mark `Consumed`, so MIR refuses the shape
# fail-closed rather than emit a structurally undetectable UAF.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/match_destructure_projection_scrutinee.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected match_destructure_projection_scrutinee fixture to fail" >&2
  exit 1
fi
grep -q 'non-BitCopy match destructure on projection scrutinee' "${reject_output}"

# A temporary (fresh-value) scrutinee has no composite drop: with an
# all-wildcard arm nothing frees the discarded aggregate's owned fields, so
# every field would leak. MIR refuses fail-closed and tells the user to bind
# the scrutinee to a local first (whose composite drop frees the fields).
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/match_destructure_temporary_scrutinee.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected match_destructure_temporary_scrutinee fixture to fail" >&2
  exit 1
fi
grep -q 'non-BitCopy match destructure on temporary scrutinee' "${reject_output}"

# The same gate covers a temporary scrutinee with a binding arm: with no
# binding to consume-mark, the extracted binder stays tainted as a projection
# alias and its payload would leak. Refused fail-closed with the same
# temporary-scrutinee diagnostic.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/match_destructure_temporary_scrutinee_bound.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected match_destructure_temporary_scrutinee_bound fixture to fail" >&2
  exit 1
fi
grep -q 'non-BitCopy match destructure on temporary scrutinee' "${reject_output}"

# A guard on a record destructure arm has no fallthrough target: the project
# pattern is irrefutable, so the first arm is taken unconditionally and the
# guard would be silently ignored (a miscompile that runs the wrong arm).
# Rejected fail-closed; the condition belongs in the arm body or an enum match.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/match_destructure_guarded_record.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected match_destructure_guarded_record fixture to fail" >&2
  exit 1
fi
grep -q 'guarded record/tuple match destructure' "${reject_output}"

# The same gate covers a guard on a tuple destructure arm — the tuple project
# pattern is irrefutable exactly like the record case, so the guard is rejected
# fail-closed with the same diagnostic.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/match_destructure_guarded_tuple.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected match_destructure_guarded_tuple fixture to fail" >&2
  exit 1
fi
grep -q 'guarded record/tuple match destructure' "${reject_output}"

# Regression: file-imported trait impl methods emit as definitions, not
# external declarations. The fourth-pass (module-graph walk) previously
# re-lowered file-import impl blocks, producing duplicate HirItem::Function
# entries for the same unqualified method symbol and causing LLVM module
# verification to fail with "Global is external, but doesn't have external
# or weak linkage!".
run_check_run_expect_stdout file_import_trait_impl

# `.clone()` on a Copy type (i64, bool): emits a StyleSuggestion warning but
# compiles and runs correctly — the value is duplicated as-if by plain copy.
run_accept_expect_stdout copy_clone_warn

# User-defined record `.clone()`: BitCopy-field record produces an independent copy.
run_accept_expect_stdout record_clone_basic

# User-defined record `.clone()`: record with an owned string field deep-clones the string.
run_accept_expect_stdout record_clone_string_field

# User-defined record `.clone()`: original stays live and usable after the clone.
run_accept_expect_stdout record_clone_independence

# User-defined record `.clone()` on a record containing an opaque handle must be rejected.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/record_clone_unclonable_field.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected record_clone_unclonable_field fixture to fail" >&2
  exit 1
fi
grep -q 'contains an opaque field' "${reject_output}"

# ---------------------------------------------------------------------------
# let-destructure: record/struct product-type irrefutable patterns
# ---------------------------------------------------------------------------

# Accept: `let Point { x, y } = p` binds both fields; x=1 + y=2 = 3.
run_accept_expect_status "let_record_destructure_sums_fields" 3

# Accept: same desugar with 4 fields (Config-shaped record).
# Arithmetic over all four fields: (8080+3) + (3*30) + (8080+30) = 16283;
# 16283 mod 256 = 155.
run_accept_expect_status "let_record_destructure_multi_fields" 155

# Accept: record with an owned string field destructures without double-free.
# tag=7 is returned; run under MallocScribble=1 to catch use-after-free.
run_accept_expect_status "let_record_destructure_owned_field" 7

# Accept: destructure inside a called function and again at the call site.
# dot(Vec2{3,4}) = 25; norm_sq desugar + outer desugar both fire.
run_accept_expect_status "let_record_destructure_in_fn" 25

# Reject: enum-variant Constructor pattern in `let` binding must emit exactly
# one spanned error pointing to `if let` or `match`; no cascade.
expect_check_fail_error_count \
    "${ROOT}/tests/vertical-slice/reject/let_refutable_pattern_rejected.hew" \
    1 \
    "let_refutable_pattern_rejected"
# The error must reference `if let` or `match`.
expect_check_fail_contains \
    "${ROOT}/tests/vertical-slice/reject/let_refutable_pattern_rejected.hew" \
    "if let" \
    "let_refutable_pattern_rejected_message"
# No "has no binding" cascade must appear.
if "${HEW}" check \
    "${ROOT}/tests/vertical-slice/reject/let_refutable_pattern_rejected.hew" \
    >"${reject_output}" 2>&1; then
  echo "expected let_refutable_pattern_rejected to fail" >&2
  exit 1
fi
if grep -q 'has no binding' "${reject_output}"; then
  echo "let_refutable_pattern_rejected cascaded into 'has no binding'" >&2
  cat "${reject_output}" >&2
  exit 1
fi

# .wrapping_as_<W> and .saturating_as_<W> width-conversion methods.
# Tests exact values: wrapping narrowing/widening/sign-change and
# saturating clamp to [W::MIN, W::MAX].
run_check_run_expect_stdout wrapping_saturating_as_cast
