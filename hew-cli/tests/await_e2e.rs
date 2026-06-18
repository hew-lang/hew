//! Executed regression for the live non-blocking actor `await` (W6.010).
//!
//! Each `examples/actor/await_*.hew` fixture is COMPILED AND RUN, and its
//! stdout is asserted — under both the default worker pool AND `HEW_WORKERS=1`.
//! The single-worker run is the worker-freeing proof: one worker must suspend
//! the caller's continuation, run the callee, then resume — a blocking ask
//! would deadlock the lone worker. Before this target the await examples had no
//! executed CI guard (they were only parsed by the grammar fixtures).

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

/// Run an `examples/actor/<name>.hew` fixture via `hew run`, optionally setting
/// `HEW_WORKERS`, and assert it exits 0 with exactly `expected_stdout`.
fn run_await_example(name: &str, workers: Option<&str>, expected_stdout: &str) {
    require_codegen();

    let source: PathBuf = repo_root()
        .join("examples/actor")
        .join(format!("{name}.hew"));
    assert!(
        source.is_file(),
        "await example fixture missing: {}",
        source.display()
    );

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&source).current_dir(repo_root());
    if let Some(workers) = workers {
        command.env("HEW_WORKERS", workers);
    }

    let label = match workers {
        Some(w) => format!("hew run {name} (HEW_WORKERS={w})"),
        None => format!("hew run {name} (default pool)"),
    };
    // A lost wake would hang the program; the bounded runner turns a hang into a
    // test failure instead of an orphaned process.
    let output = support::run_bounded_command(command, label.clone());

    assert!(
        output.status.success(),
        "{label} should exit 0; stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_stdout,
        "{label} produced unexpected stdout",
    );
}

/// Assert a fixture produces the same output under the default pool AND under a
/// single worker (the worker-freeing edge).
fn run_await_example_both_pools(name: &str, expected_stdout: &str) {
    run_await_example(name, None, expected_stdout);
    run_await_example(name, Some("1"), expected_stdout);
}

#[test]
fn await_suspend_resume_binds_reply_under_both_pools() {
    run_await_example_both_pools("await_suspend_resume", "result=42\n");
}

#[test]
fn await_chain_completes_nested_suspend_chain_under_both_pools() {
    run_await_example_both_pools("await_chain", "result=31\n");
}

#[test]
fn await_fanout_rearms_continuation_across_dispatches_under_both_pools() {
    run_await_example_both_pools("await_fanout", "total=510\n");
}

#[test]
fn await_then_read_state_reads_live_resume_context_under_both_pools() {
    // The worker-freeing edge with a POST-await actor-state read: the resumed
    // handler reads `self.bias` after the await, which must come from the
    // resume-installed live context, not the unwound dispatch frame.
    run_await_example_both_pools("await_then_read_state", "result=142\n");
}

#[test]
fn fork_after_await_spawns_through_live_resume_context_under_both_pools() {
    // W6.010: a `scope { fork ... }` placed AFTER an await. The spawn site
    // (`SpawnTaskDirect`) snapshots supervisor/trace/cancel by dereferencing the
    // parent execution context. Before the fix the spawn passed the unwound
    // spilled `ctx` param, so the post-resume fork dereferenced the freed
    // dispatch frame — a deterministic misaligned-pointer crash in
    // `hew_cancel_token_new_child`. Routed through the resume-installed live
    // context, the spawn reads a valid context whose cancel/trace are fail-safe
    // null, so the child inherits no token (no deref) and the fork runs. The
    // structured scope joins the fork before the handler returns, so a correct
    // run prints the background line then the result under both pools.
    run_await_example_both_pools("await_then_fork", "background-ran\nresult=142\n");
}

#[test]
fn crash_after_sleep_resume_routes_reply_to_outer_under_both_pools() {
    // Crash-recovery regression for the resume re-entry: a handler that
    // `sleep_ms`-suspends twice, then `panic()`s, crashes while running as a
    // RESUMED continuation (driven on the scheduler resume edge, not the
    // fresh-dispatch frame). The resume edge must install the same `sigsetjmp`
    // crash-recovery frame the fresh dispatch does — otherwise the trap unwinds
    // past the worker frame and downs the whole process instead of crashing only
    // this actor. Post-fix the crash routes to the actor, the outer
    // `await reader.go(0)` resolves to `Err` (the empty crash fallback), and the
    // program completes under both pools.
    //
    // The two sleeps also pin the frame-reclamation edge: the crash-abandoned
    // coroutine frame (whose `sleep` resume edges already released their
    // await-cancel registrations) is reclaimed by freeing the frame block
    // WITHOUT re-running the `coro.destroy` cleanup outline — re-running it would
    // double-free the already-released registration (surfaced under
    // `MallocGuardEdges`).
    run_await_example_both_pools(
        "await_crash_after_sleep_resume",
        "reader-crash-fallback\nmain-done\n",
    );
}

#[test]
fn multiple_awaits_in_one_handler_resume_correctly_under_both_pools() {
    // Multi-suspend support: a single handler body may `await` more than once.
    // `Coordinator.two` awaits twice (84), `three` awaits three times (126), and
    // `loop_sum` awaits inside a `for` loop (168). Each yield-back-to-executor
    // resumes at the right point — the second-and-later await no longer runs off
    // into undefined code (the prior crash the single-fallthrough `coro.end`
    // epilogue fixes). The single-worker run is the worker-freeing edge: a
    // mis-resumed second await or a lost wake would crash or hang the lone
    // worker; correct totals under HEW_WORKERS=1 prove every park is matched by
    // exactly one resume across all suspends in the body.
    run_await_example_both_pools("await_multi", "two=84\nthree=126\nloop=168\n");
}

/// Run an `examples/net/<name>.hew` fixture via `hew run`, optionally setting
/// `HEW_WORKERS`, and assert it exits 0 with exactly `expected_stdout`. The net
/// fixtures are self-contained: `main` runs the TCP server and the actor runs
/// the suspendable client, so a single `hew run` is the whole oracle.
fn run_net_example(name: &str, workers: Option<&str>, expected_stdout: &str) {
    require_codegen();

    let source: PathBuf = repo_root().join("examples/net").join(format!("{name}.hew"));
    assert!(
        source.is_file(),
        "net example fixture missing: {}",
        source.display()
    );

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&source).current_dir(repo_root());
    if let Some(workers) = workers {
        command.env("HEW_WORKERS", workers);
    }

    let label = match workers {
        Some(w) => format!("hew run {name} (HEW_WORKERS={w})"),
        None => format!("hew run {name} (default pool)"),
    };
    // A closure-await that blocks instead of suspending would strand the lone
    // worker; the bounded runner turns that hang into a test failure.
    let output = support::run_bounded_command(command, label.clone());

    assert!(
        output.status.success(),
        "{label} should exit 0; stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_stdout,
        "{label} produced unexpected stdout",
    );
}

fn run_net_example_both_pools(name: &str, expected_stdout: &str) {
    run_net_example(name, None, expected_stdout);
    run_net_example(name, Some("1"), expected_stdout);
}

#[test]
fn closure_captured_await_suspends_and_frees_the_worker_under_both_pools() {
    // The suspendable-callee oracle (the real Slice-3): a closure that captures a
    // Connection and `await`s `read_string()` across the coroutine boundary. The
    // call site lowers to `Terminator::SuspendingCallClosure`, which drives the
    // callee coroutine and PARKS the calling actor while the read is pending. The
    // single-worker run is the worker-freeing proof: the lone worker suspends in
    // the closure await (freeing it for the scheduler) and the reactor resumes it
    // when the server's bytes land — a blocking closure read would strand the
    // worker and hang the bounded runner.
    run_net_example_both_pools(
        "probe_b2_closure_capture_await",
        "reader-received: hello-from-server\nserver-done\n",
    );
}

#[test]
fn closure_no_await_stays_on_the_direct_call_path_under_both_pools() {
    // The control: a closure that does NOT await keeps the direct
    // `Instr::CallClosure` path (the discriminator never records a non-suspending
    // closure). It must still run identically — proving the suspendable-callee
    // driver is not spuriously routing every closure call through the coroutine
    // machinery.
    run_net_example_both_pools(
        "probe_b3_closure_capture_noawait",
        "reader-received: hello-from-server\nserver-done\n",
    );
}

#[test]
fn closure_captured_await_twice_re_parks_the_caller_under_both_pools() {
    // The multi-suspend negative: a closure whose body `await`s the captured
    // Connection TWICE. The driver must re-park the calling actor on the SECOND
    // await as well as the first (the `.resume` edge that SIGSEGV'd in the
    // 2026-06-05 multi-suspend lesson). The first read binds the payload; the
    // second resumes on EOF (server close) and binds an empty string. Correct
    // totals under HEW_WORKERS=1 prove every park across both closure suspends is
    // matched by exactly one resume.
    run_net_example_both_pools(
        "probe_b2_closure_multi_await",
        "reader-received: payload|\nserver-done\n",
    );
}

#[test]
fn unit_returning_closure_await_completes_without_leaking_under_both_pools() {
    // CODE-1 regression: a UNIT-returning suspending closure (`|| { let _ = await
    // conn.read_string(); }`). The driver always retains an extra reply-channel
    // sender ref; a non-unit child's `hew_reply` releases it, but a unit body
    // deposits nothing, so the driver's finish path must release BOTH the sender
    // ref and the creator ref. Before the fix the unit finish path released only
    // one, leaking the retained sender ref. The program runs identically under
    // both pools — the leak was silent — but this pins the unit suspending-closure
    // path as exercised, and the matching runtime regression
    // (`unit_suspending_closure_finish_releases_both_channel_refs`) pins the
    // two-free ref-accounting the unit finish path now emits.
    run_net_example_both_pools(
        "probe_b2_closure_unit_await",
        "reader-unit-done\nserver-done\n",
    );
}

#[test]
fn outer_handler_crash_during_child_suspend_routes_reply_to_outer_under_both_pools() {
    // SEC-2 load-bearing regression: an outer ASK handler that crashes (panics)
    // DURING a child suspending-closure await — the panic fires inside the
    // driver's synchronous ramp call, while the reply-channel swap is open. The
    // longjmp bypasses the codegen swap-pop and driver-channel teardown.
    //
    // Pre-fix the scheduler crash-recovery edge read the swapped-in driver
    // channel, replied to it, and nulled the real outer reply channel, so
    // `await reader.go(0)` HUNG forever (the bounded runner turns that into a
    // failure). Post-fix the crash edge unwinds the open swap first, restoring
    // the outer reply routing and tearing the driver channel down, so the outer
    // ask resolves to `Err` (the empty crash fallback) and the program completes
    // under BOTH pools — proving the orphan/fallback reply routes to the real
    // outer channel, not the child's.
    run_net_example_both_pools(
        "probe_b2_closure_await_outer_crash",
        "reader-crash-fallback\nmain-done\n",
    );
}

#[test]
fn async_http_roundtrip_serves_and_fetches_under_both_pools() {
    // NEW-2 end-to-end oracle: a `Server` actor `await`s `listener.accept()`
    // (the new SuspendingAccept carrier) then `await`s the request bytes, and a
    // `Client` actor `await`s the response. The single-worker run is the
    // worker-freeing proof: one worker serves AND fetches only because every
    // `await` (accept + both read loops) suspends its handler — a blocking
    // accept or read would strand the lone worker and hang the bounded runner.
    run_net_example_both_pools(
        "await_http_roundtrip",
        "client-received: hi from hew\nmain-done\n",
    );
}

#[test]
fn async_http_codec_hardening_fails_closed() {
    // NEW-2 security revision regression: the async HTTP/1.1 codecs must fail
    // closed against DoS + smuggling inputs (no sockets — pure codec drive).
    // F2: oversized/over-declared requests/responses are rejected (413 / -1).
    // F3: malformed framing (bad request line / version / target, duplicate or
    // non-numeric Content-Length, Transfer-Encoding) is rejected with 400.
    // F4: CRLF/control chars in caller-provided builder fields are rejected so no
    // header is injected.
    // SF1: an overflowing numeric Content-Length fails closed (413 server / -1
    // client) instead of saturating to 0 and bypassing the body cap.
    // SF2: a malformed response status line (bad HTTP version, non-3-digit or
    // out-of-range status, missing CRLF-CRLF terminator) yields client status -1.
    // SF3: a request header line lacking a ':' separator is rejected with 400.
    // Any unrejected input panics an assertion mid-run, so a clean
    // `codec-hardening-ok` is the proof every bound held.
    run_net_example_both_pools("await_http_codec_hardening", "codec-hardening-ok\n");
}
