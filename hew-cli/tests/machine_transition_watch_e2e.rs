//! Machine transition watch over select channel arms — e2e baseline and
//! fail-closed gap pins.
//!
//! The supported observation pattern for machine state transitions is the
//! notification channel: the owner steps the machine (a value type) and
//! publishes each observed transition into a std/channel `Sender`; the
//! observer waits with the sealed `from ... recv()` select arm composed
//! with `after`. This file pins the green baseline
//! (`examples/machine/transition_watch_baseline.hew`) and the named
//! fail-closed refusal for the cross-actor handoff gap:
//!
//! - channel handles (`Sender`/`Receiver`) as actor message arguments are
//!   routed into the cross-node serializer and refused
//!   (`E_NOT_YET_IMPLEMENTED: cross-node serialize`). Local handle
//!   transfer flips this pin to a green run.
//!
//! Each pinned refusal is an executable contract: the gap fails closed
//! with a named diagnostic — never a miscompile — until the surface lands.

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

/// Run an `examples/machine/<name>.hew` fixture via `hew run`, asserting it
/// exits 0 with exactly `expected_stdout`.
fn run_machine_example(name: &str, expected_stdout: &str) {
    require_codegen();

    let source: PathBuf = repo_root()
        .join("examples/machine")
        .join(format!("{name}.hew"));
    assert!(
        source.is_file(),
        "machine example fixture missing: {}",
        source.display()
    );

    let mut command = Command::new(hew_binary());
    command.arg("run").arg(&source).current_dir(repo_root());
    let label = format!("hew run examples/machine/{name}.hew");
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

/// Baseline: a single actor owns the machine, publishes the transition as a
/// string, and receives it through the sealed channel-recv select arm.
#[test]
fn transition_watch_baseline_prints_transition() {
    run_machine_example("transition_watch_baseline", "Created -> Initialising\n");
}

/// A `channel.Receiver<string>` as a receive-fn parameter source. The
/// cross-actor watch handoff needs this exact shape green.
fn receiver_param_source() -> &'static str {
    "import std::channel::channel;\n\
     \n\
     actor Observer {\n\
     \x20   receive fn watch(rx: channel.Receiver<string>) {\n\
     \x20       match rx.recv() {\n\
     \x20           Some(v) => println(v),\n\
     \x20           None => println(\"closed\"),\n\
     \x20       }\n\
     \x20       rx.close();\n\
     \x20   }\n\
     }\n\
     \n\
     fn main() {\n\
     \x20   let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(4);\n\
     \x20   tx.send(\"hello\");\n\
     \x20   tx.close();\n\
     \x20   let obs = spawn Observer;\n\
     \x20   obs.watch(rx);\n\
     \x20   sleep_ms(200);\n\
     }\n"
}

/// Gap pin: a channel handle as an actor message argument is refused with
/// the named cross-node-serialize diagnostic (it never miscompiles). The
/// local handle-transfer stage flips this to a green `hew run`.
#[test]
fn channel_receiver_actor_message_arg_fails_closed() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("receiver_param.hew");
    std::fs::write(&source, receiver_param_source()).unwrap();

    let output = support::run_hew_in(dir.path(), &["compile", source.to_str().unwrap()]);

    assert!(
        !output.status.success(),
        "channel.Receiver as an actor message arg must fail closed today; \
         it compiled instead:\n{}",
        support::describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("E_NOT_YET_IMPLEMENTED")
            && stderr.contains("cross-node serialize")
            && stderr.contains("channel.Receiver"),
        "expected the named cross-node-serialize refusal for channel.Receiver; \
         got:\n{stderr}",
    );
}
