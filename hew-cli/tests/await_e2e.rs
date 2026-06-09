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
