//! Executed regression for #3146: a blocking channel send that is still full
//! when the runtime enters shutdown must fail closed with an attributed
//! diagnostic instead of parking its handler until the drain deadline.
//!
//! The fixture spawns a periodic handler that sends into a one-slot channel
//! whose only receiver is a `main` local that reads once and never closes. The
//! third send finds the ring full and, before the fix, parked the scheduler
//! worker inside the handler: the shutdown drain refuses to join a worker still
//! in a handler, so the process died through the 5s drain timeout with nothing
//! naming the channel or the actor.

mod support;

use std::fs;
use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

/// A periodic handler filling a one-slot channel that `main` reads once.
const FULL_SEND_AT_SHUTDOWN_SOURCE: &str = r#"import std.channel.channel;

actor Pulse {
    let ready: channel.Sender<i64>,
    var count: i64 = 0,

    #[every(1ms)]
    receive fn tick() {
        count += 1;
        ready.send(count);
    }
}

fn main() {
    let (ready_tx, ready_rx): (channel.Sender<i64>, channel.Receiver<i64>) = match channel.new(1) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let _p = spawn Pulse(ready: ready_tx, count: 0);
    let _ = ready_rx.recv();
    sleep(200ms);
    println("main done");
}
"#;

/// A send parked on a full channel when shutdown starts fails closed, naming
/// the channel, rather than surfacing as an unattributed drain timeout.
#[test]
fn full_send_at_shutdown_fails_closed_with_named_channel() {
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
        stderr.contains("channel send abandoned during shutdown"),
        "the abandoned send must name itself; stdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert!(
        stderr.contains("capacity 1"),
        "the diagnostic must name the channel's capacity; stdout:\n{stdout}\nstderr:\n{stderr}",
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
        r#"import std.channel.channel;

actor Pump {
    let out: channel.Sender<i64>,

    receive fn go(count: i64) {
        for i in 0..count {
            out.send(i);
        }
    }
}

fn main() {
    let (tx, rx): (channel.Sender<i64>, channel.Receiver<i64>) = match channel.new(1) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let pump = spawn Pump(out: tx);
    pump.go(8);
    var seen: i64 = 0;
    for _i in 0..8 {
        match rx.recv() {
            .Some(_) => { seen += 1; },
            .None => {},
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
        "backpressure over a full channel must still complete; stdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert_eq!(stdout, "seen 8\n", "stderr:\n{stderr}");
}
