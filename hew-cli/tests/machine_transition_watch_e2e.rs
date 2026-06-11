//! Machine transition watch over select channel arms — e2e baseline and
//! fail-closed gap pins.
//!
//! The supported observation pattern for machine state transitions is the
//! notification channel: the owner steps the machine (a value type) and
//! publishes each observed transition into a std/channel `Sender`; the
//! observer waits with the sealed `from ... recv()` select arm composed
//! with `after`. This file pins the green baseline
//! (`examples/machine/transition_watch_baseline.hew`), the local
//! channel-handle transfer through actor messages, and its ownership
//! contract:
//!
//! - a `Sender`/`Receiver` actor message argument transfers the retained
//!   handle to the receiving handler (the local mailbox copies the handle
//!   pointer; no cross-node codec is emitted for handle-bearing
//!   handlers);
//! - the caller binding is consumed at the send site — any later use
//!   (`close()`, a second send) is refused with `UseAfterConsume`, the
//!   double-close / racing-owner guard;
//! - cross-node transfer is statically unreachable: `RemotePid` exposes
//!   no receive-fn dispatch, and tell/ask payloads are
//!   Serializable-enforced (channel handles are not Serializable).

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

/// Write `source` to a tempdir, `hew run` it under the poisoned-allocator
/// pair (`MallocScribble`/`MallocPreScribble`), and assert exit 0 with
/// exactly `expected_stdout`. A producer-side double release of an arm
/// payload would crash under the scribbled allocator before stdout settles.
fn run_inline_scribbled(label: &str, source: &str, expected_stdout: &str) {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join(format!("{label}.hew"));
    std::fs::write(&path, source).unwrap();

    let mut command = Command::new(hew_binary());
    command
        .arg("run")
        .arg(&path)
        .current_dir(dir.path())
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1");
    let output = support::run_bounded_command(command, label.to_string());

    assert!(
        output.status.success(),
        "{label} should exit 0 under MallocScribble; stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_stdout,
        "{label} produced unexpected stdout",
    );
}

/// Record channel element received through the sealed select arm, with the
/// arm body's field reads spanning Call-terminated blocks (the f-string).
/// Pins the select-arm init-check CFG fix end to end: before the arm-body
/// edges landed this refused with a false `InitialisedBeforeUse`.
#[test]
fn select_record_element_cross_block_arm_runs_clean() {
    run_inline_scribbled(
        "select_record_element",
        "import std::channel::channel;\n\
         \n\
         record Transition {\n\
         \x20   from_state: string,\n\
         \x20   to_state: string\n\
         }\n\
         \n\
         actor Combined {\n\
         \x20   receive fn run() {\n\
         \x20       let (tx, rx): (channel.Sender<Transition>, channel.Receiver<Transition>) = channel.new(4);\n\
         \x20       tx.send(Transition { from_state: \"Created\", to_state: \"Initialising\" });\n\
         \x20       tx.close();\n\
         \x20       select {\n\
         \x20           t from rx.recv() => {\n\
         \x20               match t {\n\
         \x20                   Some(tr) => println(f\"{tr.from_state} -> {tr.to_state}\"),\n\
         \x20                   None => println(\"closed\"),\n\
         \x20               }\n\
         \x20           },\n\
         \x20           after 1s => println(\"timeout\"),\n\
         \x20       };\n\
         \x20       rx.close();\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let c = spawn Combined;\n\
         \x20   c.run();\n\
         \x20   sleep_ms(200);\n\
         }\n",
        "Created -> Initialising\n",
    );
}

/// Heap-payload enum channel element through the sealed select arm. Pins
/// the queue-carrier element thunk seeding: before `Sender`/`Receiver`/
/// `Stream` elements were seeded like `Vec` elements, the owned-element
/// witness referenced `__hew_enum_{clone,drop}_inplace_*` thunks with no
/// body and llvm-verify refused the module.
#[test]
fn select_enum_element_thunks_resolve_and_run_clean() {
    run_inline_scribbled(
        "select_enum_element",
        "import std::channel::channel;\n\
         \n\
         enum Transition {\n\
         \x20   Moved { from_state: string, to_state: string };\n\
         }\n\
         \n\
         actor Combined {\n\
         \x20   receive fn run() {\n\
         \x20       let (tx, rx): (channel.Sender<Transition>, channel.Receiver<Transition>) = channel.new(4);\n\
         \x20       tx.send(Transition::Moved { from_state: \"Created\", to_state: \"Initialising\" });\n\
         \x20       tx.close();\n\
         \x20       select {\n\
         \x20           t from rx.recv() => {\n\
         \x20               match t {\n\
         \x20                   Some(t2) => {\n\
         \x20                       match t2 {\n\
         \x20                           Transition::Moved { from_state, to_state } => println(f\"{from_state} -> {to_state}\"),\n\
         \x20                       }\n\
         \x20                   },\n\
         \x20                   None => println(\"closed\"),\n\
         \x20               }\n\
         \x20           },\n\
         \x20           after 1s => println(\"timeout\"),\n\
         \x20       };\n\
         \x20       rx.close();\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let c = spawn Combined;\n\
         \x20   c.run();\n\
         \x20   sleep_ms(200);\n\
         }\n",
        "Created -> Initialising\n",
    );
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

/// A channel handle as an actor message argument transfers locally: the
/// observer receives the live receiver, drains it, and closes it. Was the
/// stage-0 cross-node-serialize refusal pin before handle-bearing handlers
/// stopped emitting xnode codecs.
#[test]
fn channel_receiver_actor_message_arg_transfers_locally() {
    run_inline_scribbled(
        "receiver_param_transfer",
        receiver_param_source(),
        "hello\n",
    );
}

/// Ownership contract: the caller binding is consumed by the transfer.
/// A later `rx.close()` would double-close the channel the new owner now
/// holds — refused with the named `UseAfterConsume` diagnostic.
#[test]
fn channel_handle_use_after_transfer_refused() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("use_after_transfer.hew");
    std::fs::write(
        &source,
        "import std::channel::channel;\n\
         \n\
         actor Observer {\n\
         \x20   receive fn watch(rx: channel.Receiver<string>, label: string) {\n\
         \x20       rx.close();\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(4);\n\
         \x20   tx.close();\n\
         \x20   let obs = spawn Observer;\n\
         \x20   obs.watch(rx, \"watch\");\n\
         \x20   rx.close();\n\
         \x20   sleep_ms(100);\n\
         }\n",
    )
    .unwrap();

    let output = support::run_hew_in(dir.path(), &["compile", source.to_str().unwrap()]);

    assert!(
        !output.status.success(),
        "rx.close() after transferring rx must be refused; it compiled:\n{}",
        support::describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("used after it was consumed") && stderr.contains("`rx`"),
        "expected the UseAfterConsume refusal on rx; got:\n{stderr}",
    );
}

/// The lane's target composition: a machine-owning service publishes
/// record transitions into a channel whose receiver was handed to a
/// separate observer actor through a message; the observer selects on
/// transitions with an `after` safety net and reacts to the Faulted edge.
#[test]
fn cross_actor_record_transition_watch_runs_clean() {
    run_inline_scribbled(
        "cross_actor_transition_watch",
        "import std::concurrency::lifecycle;\n\
         import std::channel::channel;\n\
         \n\
         record Transition {\n\
         \x20   from_state: string,\n\
         \x20   to_state: string\n\
         }\n\
         \n\
         actor Service {\n\
         \x20   receive fn drive(tx: channel.Sender<Transition>) {\n\
         \x20       var lc: lifecycle.Lifecycle<i64> = lifecycle.Lifecycle::Created;\n\
         \x20       let before1 = lc.state_name();\n\
         \x20       lc.step(Initialise);\n\
         \x20       let after1 = lc.state_name();\n\
         \x20       if before1 != after1 {\n\
         \x20           tx.send(Transition { from_state: before1, to_state: after1 });\n\
         \x20       }\n\
         \x20       let before2 = lc.state_name();\n\
         \x20       lc.step(lifecycle.LifecycleEvent::Crashed { error: Error::Code(7) });\n\
         \x20       let after2 = lc.state_name();\n\
         \x20       if before2 != after2 {\n\
         \x20           tx.send(Transition { from_state: before2, to_state: after2 });\n\
         \x20       }\n\
         \x20       tx.close();\n\
         \x20   }\n\
         }\n\
         \n\
         actor Observer {\n\
         \x20   receive fn watch(rx: channel.Receiver<Transition>) {\n\
         \x20       var waiting = true;\n\
         \x20       while waiting {\n\
         \x20           select {\n\
         \x20               t from rx.recv() => {\n\
         \x20                   match t {\n\
         \x20                       Some(tr) => {\n\
         \x20                           println(f\"{tr.from_state} -> {tr.to_state}\");\n\
         \x20                           if tr.to_state == \"Faulted\" {\n\
         \x20                               println(\"observer: child faulted\");\n\
         \x20                           }\n\
         \x20                       },\n\
         \x20                       None => {\n\
         \x20                           println(\"watch closed\");\n\
         \x20                           waiting = false;\n\
         \x20                       },\n\
         \x20                   }\n\
         \x20               },\n\
         \x20               after 2s => {\n\
         \x20                   println(\"timeout\");\n\
         \x20                   waiting = false;\n\
         \x20               },\n\
         \x20           };\n\
         \x20       }\n\
         \x20       rx.close();\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let (tx, rx): (channel.Sender<Transition>, channel.Receiver<Transition>) = channel.new(8);\n\
         \x20   let obs = spawn Observer;\n\
         \x20   obs.watch(rx);\n\
         \x20   let svc = spawn Service;\n\
         \x20   svc.drive(tx);\n\
         \x20   sleep_ms(300);\n\
         }\n",
        "Created -> Initialising\nInitialising -> Faulted\nobserver: child faulted\nwatch closed\n",
    );
}
