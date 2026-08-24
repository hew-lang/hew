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
use std::time::Duration;

use support::{hew_binary, repo_root, require_codegen, strip_ansi};

const CLOSURE_SUSPENSION_EXPECTED_FAILURE: &str =
    "E_NOT_YET_IMPLEMENTED: MIR lowering for suspension inside a closure is not implemented yet";

/// Compile one example to a native binary. The caller retains the tempdir for
/// as long as it needs to launch the artifact.
fn build_example(category: &str, name: &str) -> (tempfile::TempDir, PathBuf) {
    require_codegen();

    let source: PathBuf = repo_root()
        .join("examples")
        .join(category)
        .join(format!("{name}.hew"));
    assert!(
        source.is_file(),
        "example fixture missing: {}",
        source.display()
    );

    let dir = support::tempdir();
    let binary = hew_testutil::compiled_binary_path(dir.path(), name);
    let mut command = Command::new(hew_binary());
    command
        .arg("build")
        .arg(&source)
        .arg("-o")
        .arg(&binary)
        .current_dir(repo_root());
    let output = support::try_run_bounded_command(
        command,
        format!("hew build {}", source.display()),
        Duration::from_mins(2),
    )
    .unwrap_or_else(|error| panic!("{error}"));
    assert!(
        output.status.success(),
        "hew build {} should exit 0; stdout:\n{}\nstderr:\n{}",
        source.display(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        binary.is_file(),
        "missing built fixture: {}",
        binary.display()
    );
    (dir, binary)
}

fn assert_closure_suspension_rejected(category: &str, name: &str) {
    require_codegen();

    let expected_manifest_entry =
        format!("examples/{category}/{name}.hew # {CLOSURE_SUSPENSION_EXPECTED_FAILURE}");
    let manifest = std::fs::read_to_string(
        repo_root()
            .join("scripts")
            .join("hew-corpus-expected-failures.txt"),
    )
    .expect("read corpus expected-failures manifest");
    assert_eq!(
        manifest
            .lines()
            .filter(|line| line == &expected_manifest_entry)
            .count(),
        1,
        "the closure suspension probe must have one exact corpus expected-failure entry: \
         {expected_manifest_entry}"
    );

    let source = repo_root()
        .join("examples")
        .join(category)
        .join(format!("{name}.hew"));
    assert!(
        source.is_file(),
        "example fixture missing: {}",
        source.display()
    );

    let dir = support::tempdir();
    let binary = hew_testutil::compiled_binary_path(dir.path(), name);
    let mut command = Command::new(hew_binary());
    command
        .arg("build")
        .arg(&source)
        .arg("-o")
        .arg(&binary)
        .current_dir(repo_root());
    let output = support::try_run_bounded_command(
        command,
        format!("hew build {}", source.display()),
        Duration::from_mins(2),
    )
    .unwrap_or_else(|error| panic!("invoke hew build {}: {error}", source.display()));
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !output.status.success(),
        "hew build {} must reject closure suspension; stdout:\n{}\nstderr:\n{}",
        source.display(),
        String::from_utf8_lossy(&output.stdout),
        stderr,
    );
    assert!(
        stderr.contains("E_NOT_YET_IMPLEMENTED")
            && stderr.contains("suspension inside a closure")
            && stderr.contains(
                "function types do not yet carry the suspension metadata needed for every direct, \
                 nested, and higher-order invocation to select the matching driver"
            ),
        "hew build {} must report the precise closure suspension diagnostic:\n{}",
        source.display(),
        stderr,
    );
    assert!(
        !binary.exists(),
        "closure suspension rejection must not emit {}",
        binary.display()
    );
}

/// Launch a pre-built fixture under one worker-pool configuration.
fn run_example_binary(
    binary: &std::path::Path,
    name: &str,
    workers: Option<&str>,
    expected_exit_code: i32,
    expected_stdout: &str,
    expected_stderr: Option<&str>,
) {
    let mut command = Command::new(binary);
    command.current_dir(repo_root());
    if let Some(workers) = workers {
        command.env("HEW_WORKERS", workers);
    }

    let label = match workers {
        Some(w) => format!("run {name} (HEW_WORKERS={w})"),
        None => format!("run {name} (default pool)"),
    };
    // A lost wake would hang the program; the bounded runner turns a hang into a
    // test failure instead of an orphaned process.
    let output = support::run_bounded_command(command, label.clone());

    assert_eq!(
        output.status.code(),
        Some(expected_exit_code),
        "{label} should exit {expected_exit_code}; stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_stdout,
        "{label} produced unexpected stdout",
    );
    if let Some(expected_stderr) = expected_stderr {
        assert!(
            String::from_utf8_lossy(&output.stderr).contains(expected_stderr),
            "{label} stderr did not contain {expected_stderr:?}; stderr:\n{}",
            String::from_utf8_lossy(&output.stderr),
        );
    }
}

/// Assert a fixture produces the same output under the default pool AND under a
/// single worker (the worker-freeing edge).
fn run_await_example_both_pools(name: &str, expected_stdout: &str) {
    let (_dir, binary) = build_example("actor", name);
    run_example_binary(&binary, name, None, 0, expected_stdout, None);
    run_example_binary(&binary, name, Some("1"), 0, expected_stdout, None);
}

fn run_crashing_example_both_pools(
    category: &str,
    name: &str,
    expected_stdout: &str,
    expected_stderr: &str,
) {
    let (_dir, binary) = build_example(category, name);
    run_example_binary(
        &binary,
        name,
        None,
        1,
        expected_stdout,
        Some(expected_stderr),
    );
    run_example_binary(
        &binary,
        name,
        Some("1"),
        1,
        expected_stdout,
        Some(expected_stderr),
    );
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
    // program completes under both pools. The crash remains unrecovered, so the
    // completed program must still report status 1.
    //
    // The two sleeps also pin the frame-reclamation edge: the crash-abandoned
    // coroutine frame (whose `sleep` resume edges already released their
    // await-cancel registrations) is reclaimed by freeing the frame block
    // WITHOUT re-running the `coro.destroy` cleanup outline — re-running it would
    // double-free the already-released registration (surfaced under
    // `MallocGuardEdges`).
    run_crashing_example_both_pools(
        "actor",
        "await_crash_after_sleep_resume",
        "reader-crash-fallback\nmain-done\n",
        "reader crashed after sleep resume",
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

#[test]
fn select_in_handler_binds_first_ready_arm_under_both_pools() {
    // cut-select-waitset: a `select{}` inside an actor handler suspends the
    // racing continuation on a readiness waitset instead of busy-polling the
    // worker in `hew_select_first`. `Fast.compute(7)` (7 + 100) replies
    // immediately and wins; the `Slow` arm (sleeps 50ms) is the loser
    // (cancelled); the `after 500ms` safety net never fires. The single-worker
    // run is the worker-freeing proof: a blocking `hew_select_first` would
    // deadlock the lone worker (it would spin on the readiness flags while
    // pinning the very worker that must run the askees). The exact value (107)
    // proves the winner arm bound correctly under a real suspend/resume.
    run_await_example_both_pools("select_suspend_race", "winner=107\n");
}

#[test]
fn select_after_deadline_fires_timeout_branch_under_both_pools() {
    // cut-select-waitset: the `after` deadline arm fires the timeout branch. The
    // only ask arm is `Slow` (sleeps 200ms); the 20ms deadline wins. The
    // suspending select arms the deadline on the global timer wheel, suspends,
    // and the timer-fired wake resumes the coordinator into the AfterTimer
    // winner block — which cancels the still-pending Slow ask and runs the
    // `after` body. The exact value (-7) proves the deadline branch routed
    // correctly under both pools; a dropped timer arm or mis-routed timer wake
    // would print the wrong value or hang.
    run_await_example_both_pools("select_suspend_deadline", "deadline=-7\n");
}

#[test]
fn scope_deadline_runs_or_skips_the_timeout_body_under_both_pools() {
    // cut-task-sleep: `scope { } after(d) { body }` with a NON-EMPTY timeout body
    // races the scope's child join against the deadline. `timeout_wins` has a
    // 100ms child + a 20ms deadline (the deadline wins, the recovery body runs →
    // `timeout=7`); `work_wins` has a 10ms child + a 200ms deadline (the scope
    // completes first, the body is skipped → `work=0`). The SuspendingScopeDeadline
    // arbiter arms the deadline on the global timer wheel and wires the child join
    // as the completion arm; the first-ready CAS routes the resume edge. Under
    // HEW_WORKERS=1 the lone worker MUST suspend the actor (not block on the child
    // sleep / per-deadline thread), or both lines never print. The exact lines
    // prove the deadline-won edge runs the body and the join-won edge skips it.
    run_await_example_both_pools("scope_deadline_suspend", "timeout=7\nwork=0\n");
}

fn run_net_example_both_pools(name: &str, expected_stdout: &str) {
    let (_dir, binary) = build_example("net", name);
    run_example_binary(&binary, name, None, 0, expected_stdout, None);
    run_example_binary(&binary, name, Some("1"), 0, expected_stdout, None);
}

#[test]
fn closure_captured_await_is_rejected_until_closures_have_suspension_frames() {
    // Retain the captured-Connection semantic fixture while rc2 fails closed:
    // its declared string result must never receive the coroutine ramp address.
    assert_closure_suspension_rejected("net", "probe_b2_closure_capture_await");
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
fn closure_captured_multi_await_is_rejected_until_closures_have_suspension_frames() {
    // Retain the two-suspension semantic fixture so a future frame model must
    // still account for every re-park; rc2 rejects it before codegen.
    assert_closure_suspension_rejected("net", "probe_b2_closure_multi_await");
}

#[test]
fn unit_returning_closure_await_is_rejected_until_closures_have_suspension_frames() {
    // Unit results are not exempt from the missing closure frame model. Retain
    // the semantic fixture and reject it before its coroutine ramp can run.
    assert_closure_suspension_rejected("net", "probe_b2_closure_unit_await");
}

#[test]
fn closure_await_with_outer_crash_path_is_rejected_before_codegen() {
    // Keep the crash-routing semantic fixture for the eventual closure frame
    // model, but do not enter its unsupported suspension driver in rc2.
    assert_closure_suspension_rejected("net", "probe_b2_closure_await_outer_crash");
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
