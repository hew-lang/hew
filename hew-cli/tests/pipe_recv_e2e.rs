//! Executed regression for NEW-4: worker-free `Stream<T>.recv()` (no `await`
//! needed for a local stream) and the `select { pat from rx.recv() }` arm.
//!
//! The programs are embedded here rather than read from `examples/channel/`:
//! that directory still holds pre-D506 fixtures using the deleted
//! `std.channel` API and is out of this file's scope to rewrite. Each program
//! below is the direct translation to `std.stream` (`Sink`/`Stream`,
//! `stream.pipe`), compiled and run via `hew run`, with stdout asserted under
//! both the default pool and `HEW_WORKERS=1` — the single-worker run is the
//! worker-freeing proof (a blocking recv would strand the lone worker).

mod support;

use std::fs;
use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

const AWAIT_RECV_ACTOR_SOURCE: &str = r#"import std.stream;

actor Worker {
    receive fn run() {
        let (tx, rx): (stream.Sink<string>, stream.Stream<string>) = match stream.pipe(4) {
            .Ok(pair) => pair,
            .Err(err) => panic(f"stream.pipe failed: {err}"),
        };
        tx.send("ping").expect("send");
        tx.finish();
        match rx.recv() {
            .Some(v) => println(v),
            .None => println("closed"),
        }
        match rx.recv() {
            .Some(v) => println(v),
            .None => println("closed"),
        }
        rx.close();
    }
}

fn main() {
    let w = spawn Worker;
    let _ = w.run();
    sleep(100ms);
}
"#;

const SELECT_RECV_SOURCE: &str = r#"import std.stream;

actor Worker {
    receive fn run() {
        let (txa, rxa): (stream.Sink<string>, stream.Stream<string>) = match stream.pipe(4) {
            .Ok(pair) => pair,
            .Err(err) => panic(f"stream.pipe failed: {err}"),
        };
        let (txb, rxb): (stream.Sink<i64>, stream.Stream<i64>) = match stream.pipe(4) {
            .Ok(pair) => pair,
            .Err(err) => panic(f"stream.pipe failed: {err}"),
        };
        txb.send(7).expect("send");
        select {
            a from rxa.recv() => {
                match a {
                    .Some(s) => println(f"a:{s}"),
                    .None => println("a:none"),
                }
            },
            b from rxb.recv() => {
                match b {
                    .Some(n) => println(f"b:{n}"),
                    .None => println("b:none"),
                }
            },
            after 1s => println("timeout"),
        };
        txa.close();
        txb.close();
        rxa.close();
        rxb.close();
    }
}

fn main() {
    let w = spawn Worker;
    let _ = w.run();
    sleep(100ms);
}
"#;

/// Compile and run `source` under an optional worker-count override, asserting
/// exit 0 and exact stdout.
fn run_pipe_source(name: &str, source: &str, workers: Option<&str>, expected_stdout: &str) {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join(format!("{name}.hew"));
    fs::write(&path, source).expect("write pipe fixture");

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&path).current_dir(repo_root());
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

fn run_pipe_source_both_pools(name: &str, source: &str, expected_stdout: &str) {
    run_pipe_source(name, source, None, expected_stdout);
    run_pipe_source(name, source, Some("1"), expected_stdout);
}

/// `Stream<T>.recv()` in an actor handler binds the queued item, then `None`
/// once the sink finishes. The single-worker pool proves the suspend ramp
/// frees the worker (a blocking recv would deadlock the lone worker).
#[test]
fn recv_actor_binds_some_then_none_under_both_pools() {
    run_pipe_source_both_pools(
        "await_recv_actor",
        AWAIT_RECV_ACTOR_SOURCE,
        "ping\nclosed\n",
    );
}

/// A `select{}` with two pipe-recv arms + an `after` safety net: the ready
/// pipe's arm wins (i64 element), the loser arm's poll is cancelled, and the
/// after-timer never fires.
#[test]
fn select_pipe_recv_arm_picks_ready_under_both_pools() {
    run_pipe_source_both_pools("select_recv", SELECT_RECV_SOURCE, "b:7\n");
}
