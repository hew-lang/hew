//! Executed regression for NEW-4: worker-free `await rx.recv()` over a
//! std/channel `Receiver<T>` and the `select { pat from rx.recv() }` arm.
//!
//! The `examples/channel/*.hew` fixtures are COMPILED AND RUN and their stdout
//! asserted under both the default pool AND `HEW_WORKERS=1` — the single-worker
//! run is the worker-freeing proof (a blocking recv would strand the lone
//! worker). Two MIR-dump oracles prove `await rx.recv()` flips to
//! `SuspendingChannelRecv` ONLY in an execution-context caller (an actor
//! handler), keeping the blocking `Terminator::Call` path in `main`.

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

/// Compile a Hew program to `--dump-mir checked` and return the dump.
fn mir_checked_dump(source: &str) -> String {
    let dir = support::tempdir();
    let hew_src = dir.path().join("oracle.hew");
    std::fs::write(&hew_src, source).unwrap();
    let out = support::run_hew_in(
        dir.path(),
        &[
            "compile",
            "--dump-mir",
            "checked",
            hew_src.to_str().unwrap(),
        ],
    );
    assert!(
        out.status.success(),
        "dump-mir checked must succeed; stderr: {}",
        String::from_utf8_lossy(&out.stderr),
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// True when the Checked-MIR dump carries the suspending channel-recv flip
/// carrier (`Terminator::SuspendingChannelRecv`).
///
/// The dump renderer is a presentation detail that has evolved: the
/// derived-`Debug` form prints the variant name `SuspendingChannelRecv`,
/// while the structured Checked-MIR renderer prints the instruction
/// mnemonic `suspend.channel_recv`. The flip's PRESENCE is the load-bearing
/// signal these oracles assert; match either rendering so the oracle stays
/// pinned to the MIR fact, not the formatter. Both mnemonics are distinct
/// from the blocking recv call (`hew_channel_recv_layout`), so the negative
/// oracle below cannot false-match the non-flipped path.
fn dump_has_channel_recv_suspend(dump: &str) -> bool {
    dump.contains("SuspendingChannelRecv") || dump.contains("suspend.channel_recv")
}

/// NEW-4 oracle: `await rx.recv()` in an actor handler (an execution-context
/// caller) flips to the `SuspendingChannelRecv` carrier.
#[test]
fn actor_await_recv_flips_to_suspending_channel_recv() {
    let dump = mir_checked_dump(
        "import std::channel::channel;\n\
         actor Worker {\n\
         \x20   receive fn run(unused: i64) {\n\
         \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(4);\n\
         \x20       tx.send(\"x\");\n\
         \x20       tx.close();\n\
         \x20       match await rx.recv() { Some(v) => {}, None => {}, }\n\
         \x20       rx.close();\n\
         \x20   }\n\
         }\n\
         fn main() { let w = spawn Worker(); w.run(0); }\n",
    );
    assert!(
        dump_has_channel_recv_suspend(&dump),
        "actor `await rx.recv()` must flip to SuspendingChannelRecv:\n{dump}"
    );
}

/// NEW-4 oracle: `await rx.recv()` from `main` (a context-free caller) must NOT
/// flip — it keeps the blocking `Terminator::Call` recv path.
#[test]
fn main_await_recv_does_not_flip() {
    let dump = mir_checked_dump(
        "import std::channel::channel;\n\
         fn main() {\n\
         \x20   let (tx, rx): (channel.Sender<i64>, channel.Receiver<i64>) = channel.new(4);\n\
         \x20   tx.send(7);\n\
         \x20   tx.close();\n\
         \x20   match await rx.recv() { Some(v) => {}, None => {}, }\n\
         \x20   rx.close();\n\
         }\n",
    );
    assert!(
        !dump_has_channel_recv_suspend(&dump),
        "`await rx.recv()` from main must NOT flip to SuspendingChannelRecv:\n{dump}"
    );
}
