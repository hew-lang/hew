//! Executed regression for NEW-4: worker-free `await rx.recv()` over a
//! std/channel `Receiver<T>` and the `select { pat from rx.recv() }` arm.
//!
//! The `examples/channel/*.hew` fixtures are COMPILED AND RUN and their stdout
//! asserted under both the default pool AND `HEW_WORKERS=1` — the single-worker
//! run is the worker-freeing proof (a blocking recv would strand the lone
//! worker).
//!
//! This file used to also carry three `--dump-mir checked` oracles proving
//! `await rx.recv()` flips to the `SuspendingChannelRecv` carrier ONLY in an
//! execution-context caller (an actor handler), keeping the blocking
//! `Terminator::Call` path in `main`. `--dump-mir checked` no longer exists
//! (only `physical` does), and physical MIR has no channel-recv or
//! stream-next `PhysicalTerminator` variant yet (`hew-mir/src/physical.rs`'s
//! `PhysicalTerminator` enum has none; an unhandled `SuspendKind` fails
//! closed with a `PhysicalError` per `physical.rs:2746`, which is exactly
//! the "this call may suspend; use await or fork on this call" diagnostic
//! `std/channel/channel.hew`'s `hew_channel_recv_layout` hits today). With no
//! physical-dump marker to look for and no way to compile `await rx.recv()`
//! at all on this branch, those three oracles were deleted rather than
//! migrated. Lost coverage: the actor-vs-main suspend-flip negative control
//! (that `main` keeps the blocking `Call` path while an actor handler flips
//! to a suspend terminator). The positive that `await rx.recv()` runs
//! correctly at all is still pinned by
//! `await_recv_actor_binds_some_then_none_under_both_pools` below and by
//! `eval_e2e.rs`'s `for_await_*_drains_to_completion_under_single_worker`
//! tests; both currently fail on the same suspend gap and will resume
//! proving it once physical MIR grows a channel-recv terminator.

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

/// Run an `examples/channel/<name>.hew` fixture via `hew run`, optionally
/// setting `HEW_WORKERS`, asserting it exits 0 with exactly `expected_stdout`.
fn run_channel_example(name: &str, workers: Option<&str>, expected_stdout: &str) {
    require_codegen();

    let source: PathBuf = repo_root()
        .join("examples/channel")
        .join(format!("{name}.hew"));
    assert!(
        source.is_file(),
        "channel example fixture missing: {}",
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

fn run_channel_example_both_pools(name: &str, expected_stdout: &str) {
    run_channel_example(name, None, expected_stdout);
    run_channel_example(name, Some("1"), expected_stdout);
}

/// `await rx.recv()` in an actor handler binds the queued item, then `None` on
/// the closed channel. The single-worker pool proves the suspend ramp frees
/// the worker (a blocking recv would deadlock the lone worker).
#[test]
fn await_recv_actor_binds_some_then_none_under_both_pools() {
    run_channel_example_both_pools("await_recv_actor", "ping\nclosed\n");
}

/// A `select{}` with two channel-recv arms + an `after` safety net: the ready
/// channel's arm wins (i64 element), the loser arm's poll is cancelled, and the
/// after-timer never fires.
#[test]
fn select_channel_recv_arm_picks_ready_under_both_pools() {
    run_channel_example_both_pools("select_recv", "b:7\n");
}

/// Compile a Hew program to one MIR dump stage and return the dump.
fn mir_dump(source: &str, stage: &str) -> String {
    let dir = support::tempdir();
    let hew_src = dir.path().join("oracle.hew");
    std::fs::write(&hew_src, source).unwrap();
    let mut command = Command::new(hew_binary());
    command
        .arg("compile")
        .arg("--dump-mir")
        .arg(stage)
        .arg(&hew_src)
        .current_dir(repo_root());
    let out = support::run_bounded_command(command, format!("dump-mir {stage}"));
    assert!(
        out.status.success(),
        "dump-mir {stage} must succeed; stderr: {}",
        String::from_utf8_lossy(&out.stderr),
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn handler_section<'a>(dump: &'a str, name: &str) -> &'a str {
    let start = dump
        .find(name)
        .unwrap_or_else(|| panic!("{name} must be present in MIR dump"));
    let rest = &dump[start..];
    let end = rest.find("\nfn ").unwrap_or(rest.len());
    &rest[..end]
}

/// The `for await` cursor takes the receiver's ownership, while the sibling
/// sender remains live. A parked receive must close exactly that cursor and
/// sender — not the consumed source receiver — on coroutine destruction.
#[test]
fn parked_forawait_receiver_plan_closes_cursor_and_sender_once() {
    let source = "import std.channel.channel;\n\
         actor Drain {\n\
         \x20   receive fn run() {\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new(1) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
         \x20       tx.send(\"ready\");\n\
         \x20       for await item in rx { println(item); }\n\
         \x20   }\n\
         }\n";
    let raw = mir_dump(source, "raw");
    let elab = mir_dump(source, "elab");
    let raw_handler = handler_section(&raw, "fn Drain__recv__run");
    let cursor_bind = raw_handler
        .find("__hew_for_iter_")
        .expect("for-await must bind its synthetic cursor");
    assert!(
        raw_handler[..cursor_bind].contains(" rx ")
            && raw_handler[..cursor_bind].contains("intent=Consume"),
        "the cursor must consume the source receiver:\n{raw_handler}"
    );
    let cursor_move = raw_handler[cursor_bind..]
        .lines()
        .find_map(|line| line.trim().split_once(" = move "))
        .expect("cursor bind must be followed by its whole-value move");
    let cursor_place = cursor_move.0.trim();
    let elab_handler = handler_section(&elab, "fn Drain__recv__run");
    let suspend_plan_start = elab_handler
        .find("suspend[")
        .expect("for-await receiver must suspend");
    let suspend_plan = &elab_handler[suspend_plan_start..];
    let suspend_plan = &suspend_plan[..suspend_plan
        .find("\n    return[")
        .unwrap_or(suspend_plan.len())];
    assert_eq!(
        suspend_plan.matches("fn=rt(SenderClose)").count(),
        1,
        "the live sender must close exactly once:\n{suspend_plan}"
    );
    assert_eq!(
        suspend_plan.matches("fn=rt(ReceiverClose)").count(),
        1,
        "only the moved cursor receiver may close:\n{suspend_plan}"
    );
    assert!(
        suspend_plan.contains(&format!("drop {cursor_place} ty=Receiver<string> kind=resource fn=rt(ReceiverClose)")),
        "the sole receiver close must target the raw cursor move destination {cursor_place}:\n{suspend_plan}"
    );
}
