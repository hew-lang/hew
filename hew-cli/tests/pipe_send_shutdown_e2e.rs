//! Executed regression for #3146: a blocking pipe send that is still full
//! when the runtime enters shutdown must fail closed with `Err(SendError.Closed)`
//! instead of parking its handler until the drain deadline.
//!
//! The fixture spawns a periodic handler that sends into a one-slot pipe
//! whose only reader is a `main` local that reads once and never closes.
//! `main` returns after a short sleep, dropping its `Stream` half; the third
//! send is still parked on the full pipe at that point (`#3380`'s bound: a
//! periodic handler's send does observe the pipe's capacity), and the drop
//! must resolve it as `Closed` rather than stranding the drain until it kills
//! the process 5s later.
//!
//! `sink.send()` returns a typed `Result` on the pipe surface (unlike the old
//! `std.channel` API, whose blocking send had no return value and whose
//! runtime printed its own `channel send abandoned during shutdown` /
//! `capacity 1` diagnostic on a parked send). That diagnostic was the old
//! channel implementation's own stderr surface, not part of the pipe design,
//! so it is gone; the fixture instead reports the `Err` itself and the test
//! asserts on the count and content of that report.

mod support;

use std::fs;
use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

/// A periodic handler filling a one-slot pipe that `main` reads once.
const FULL_SEND_AT_SHUTDOWN_SOURCE: &str = r#"import std.stream;

actor Pulse {
    let ready: stream.Sink<i64>,
    var count: i64 = 0,

    #[every(1ms)]
    receive fn tick() {
        count += 1;
        match ready.send(count) {
            .Ok(()) => println(f"sent {count}"),
            .Err(error) => println(f"send abandoned: {error}"),
        }
    }
}

fn main() {
    let (ready_tx, ready_rx): (stream.Sink<i64>, stream.Stream<i64>) = match stream.pipe(1) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let _p = spawn Pulse(ready: ready_tx, count: 0);
    let _ = ready_rx.recv();
    sleep(200ms);
    println("main done");
}
"#;

/// A send parked on a full pipe when shutdown starts fails closed instead of
/// surfacing as an unattributed drain timeout.
#[test]
fn full_send_at_shutdown_fails_closed_with_named_pipe() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("full_send_at_shutdown.hew");
    fs::write(&source, FULL_SEND_AT_SHUTDOWN_SOURCE).expect("write full-send fixture");

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&source).current_dir(repo_root());
    let output = support::run_bounded_command(command, "hew run full_send_at_shutdown");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        stdout.contains("main done"),
        "main must reach its end; stdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert!(
        !stderr.contains("shutdown drain timed out"),
        "a full send must not strand the drain; stdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert!(
        stdout.contains("send abandoned:"),
        "the parked send must resolve to Err instead of hanging; stdout:\n{stdout}\nstderr:\n{stderr}",
    );

    // #3380's bound: a periodic handler's send does observe the pipe's
    // capacity. Only the first two ticks fit in the one-slot pipe before
    // `main`'s single `recv()`; every send after that must park rather than
    // silently succeed. Without this count, a handler that never parks at all
    // would also pass the two assertions above trivially.
    let sent_count = stdout.matches("sent ").count();
    assert!(
        sent_count <= 3,
        "an unbounded handler send would report far more than a couple of \
         successes before `main done`; got {sent_count} in stdout:\n{stdout}",
    );
}

/// Negative control: backpressure under a live, draining receiver in the
/// running phase still parks and resumes. A shutdown-phase check that fired
/// while the runtime is running would turn this into a failure.
#[test]
fn full_send_with_draining_receiver_still_backpressures() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("full_send_backpressure.hew");
    fs::write(
        &source,
        r#"import std.stream;

actor Pump {
    let out: stream.Sink<i64>,

    receive fn go(count: i64) {
        for i in 0..count {
            out.send(i).expect("send");
        }
    }
}

fn main() {
    let (tx, rx): (stream.Sink<i64>, stream.Stream<i64>) = match stream.pipe(1) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let pump = spawn Pump(out: tx);
    let _ = fork pump.go(8);
    var seen: i64 = 0;
    for _i in 0..8 {
        if let .Some(_) = rx.recv() {
            seen += 1;
        }
    }
    println(f"seen {seen}");
    rx.close();
}
"#,
    )
    .expect("write backpressure fixture");

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&source).current_dir(repo_root());
    let output = support::run_bounded_command(command, "hew run full_send_backpressure");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "backpressure over a full pipe must still complete; stdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert_eq!(stdout, "seen 8\n", "stderr:\n{stderr}");
}
