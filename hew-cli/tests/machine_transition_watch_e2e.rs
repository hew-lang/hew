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

/// The full cross-actor watch: the owner drives a Lifecycle machine in
/// actor state, publishes transitions (attach snapshot first), the
/// observer receives the channel end through an actor message and selects
/// with the `after` safety net, reacting to the Running edge and tearing
/// down on the close-driven `None` arm.
#[test]
fn select_on_transition_example_runs_clean() {
    run_machine_example(
        "select_on_transition",
        "Created -> Created\nCreated -> Initialising\nInitialising -> Running\nobserver: child is up\nwatch closed\n",
    );
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
         \x20   sleep(200ms);\n\
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
         \x20   sleep(200ms);\n\
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
     \x20   sleep(200ms);\n\
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
         \x20   sleep(100ms);\n\
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

/// The target composition: a machine-owning service publishes
/// record transitions into a channel whose receiver was handed to a
/// separate observer actor through a message; the observer selects on
/// transitions with an `after` safety net and reacts to the Faulted edge.
///
/// Main blocks on a COMPLETION channel the observer closes out after its
/// watch loop ends — a deterministic seam, not a `sleep` that races the
/// observer under load. The `after 2s` arm must never fire here: every
/// wake the select observes carries a real transition (or the close), so
/// a `timeout` line in the output is the stale-wake fabricated-timeout
/// regression (a wake with no readiness routed to the `after` arm while
/// the deadline is unexpired), not a slow machine.
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
         \x20   receive fn watch(rx: channel.Receiver<Transition>, done: channel.Sender<i64>) {\n\
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
         \x20       done.send(1);\n\
         \x20       done.close();\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let (tx, rx): (channel.Sender<Transition>, channel.Receiver<Transition>) = channel.new(8);\n\
         \x20   let (done_tx, done_rx): (channel.Sender<i64>, channel.Receiver<i64>) = channel.new(1);\n\
         \x20   let obs = spawn Observer;\n\
         \x20   obs.watch(rx, done_tx);\n\
         \x20   let svc = spawn Service;\n\
         \x20   svc.drive(tx);\n\
         \x20   let _ = done_rx.recv();\n\
         \x20   done_rx.close();\n\
         }\n",
        "Created -> Initialising\nInitialising -> Faulted\nobserver: child faulted\nwatch closed\n",
    );
}

/// The other direction of the timeout-honesty contract: a GENUINE deadline
/// expiry (no sender ever publishes, the channel stays open) must still
/// take the `after` arm. The resume-edge gate that refuses to fabricate a
/// timeout on a stale wake consults the deadline arbiter — this pins that
/// a real `TimedOut` still routes to the `after` body and is not
/// re-suspended into a hang (main would block on the completion channel
/// forever and the bounded runner would kill it).
#[test]
fn select_after_genuine_expiry_takes_after_arm() {
    run_inline_scribbled(
        "select_after_genuine_expiry",
        "import std::channel::channel;\n\
         \n\
         actor Observer {\n\
         \x20   receive fn watch(rx: channel.Receiver<i64>, done: channel.Sender<i64>) {\n\
         \x20       select {\n\
         \x20           v from rx.recv() => {\n\
         \x20               match v {\n\
         \x20                   Some(_) => println(\"value\"),\n\
         \x20                   None => println(\"closed\"),\n\
         \x20               }\n\
         \x20           },\n\
         \x20           after 400ms => println(\"timeout\"),\n\
         \x20       };\n\
         \x20       rx.close();\n\
         \x20       done.send(1);\n\
         \x20       done.close();\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let (tx, rx): (channel.Sender<i64>, channel.Receiver<i64>) = channel.new(4);\n\
         \x20   let (done_tx, done_rx): (channel.Sender<i64>, channel.Receiver<i64>) = channel.new(1);\n\
         \x20   let obs = spawn Observer;\n\
         \x20   obs.watch(rx, done_tx);\n\
         \x20   let _ = done_rx.recv();\n\
         \x20   done_rx.close();\n\
         \x20   tx.close();\n\
         }\n",
        "timeout\n",
    );
}

/// A heap-payload machine held in actor state: step into the
/// string-payload `Failed` state and back out. The store-back rides the
/// state-field overwrite-release path, so the old state's payload is
/// released before the new value lands — a double release would crash
/// under the scribbled allocator.
#[test]
fn heap_payload_machine_actor_field_steps_clean() {
    run_inline_scribbled(
        "heap_machine_field",
        "machine Conn {\n\
         \x20   events {\n\
         \x20       Connect;\n\
         \x20       Fail { reason: string; }\n\
         \x20       Reset;\n\
         \x20   }\n\
         \n\
         \x20   state Idle;\n\
         \x20   state Open;\n\
         \x20   state Failed { reason: string; }\n\
         \n\
         \x20   on Connect: Idle => Open {\n\
         \x20       Open\n\
         \x20   }\n\
         \x20   on Fail: Open => Failed {\n\
         \x20       Conn::Failed { reason: event.reason }\n\
         \x20   }\n\
         \x20   on Reset: Failed => Idle {\n\
         \x20       Idle\n\
         \x20   }\n\
         \x20   on Connect: _ => _ {\n\
         \x20       state\n\
         \x20   }\n\
         \x20   on Fail: _ => _ {\n\
         \x20       state\n\
         \x20   }\n\
         \x20   on Reset: _ => _ {\n\
         \x20       state\n\
         \x20   }\n\
         }\n\
         \n\
         actor Holder {\n\
         \x20   var c: Conn = Conn::Idle;\n\
         \n\
         \x20   receive fn drive() {\n\
         \x20       c.step(Connect);\n\
         \x20       println(c.state_name());\n\
         \x20       c.step(ConnEvent::Fail { reason: \"boom\" });\n\
         \x20       println(c.state_name());\n\
         \x20       c.step(Reset);\n\
         \x20       println(c.state_name());\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let h = spawn Holder;\n\
         \x20   h.drive();\n\
         \x20   sleep(150ms);\n\
         }\n",
        "Open\nFailed\nIdle\n",
    );
}

/// Machine-state actors as SUPERVISOR children stay fail-closed: the
/// child-init slice admits only integer/bool/float literal field
/// defaults, and a machine field's default is a state constructor. The
/// restart-clone surface for machine state opens when that slice widens —
/// this pin fails first.
#[test]
fn supervisor_child_with_machine_state_fails_closed() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("machine_child.hew");
    std::fs::write(
        &source,
        "machine Light {\n\
         \x20   events {\n\
         \x20       Flip;\n\
         \x20   }\n\
         \x20   state Off;\n\
         \x20   state On;\n\
         \x20   on Flip: Off => On { On }\n\
         \x20   on Flip: On => Off { Off }\n\
         }\n\
         \n\
         actor Worker {\n\
         \x20   var l: Light = Light::Off;\n\
         \x20   receive fn ping() {}\n\
         }\n\
         \n\
         supervisor Pool {\n\
         \x20   strategy: one_for_one;\n\
         \x20   intensity: 3 within 60s;\n\
         \n\
         \x20   child w: Worker();\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let sup = spawn Pool;\n\
         \x20   sleep(20ms);\n\
         }\n",
    )
    .unwrap();

    let output = support::run_hew_in(dir.path(), &["compile", source.to_str().unwrap()]);

    assert!(
        !output.status.success(),
        "machine-state supervisor child must fail closed today; it compiled:\n{}",
        support::describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("E_NOT_YET_IMPLEMENTED") && stderr.contains("supported child init values"),
        "expected the named child-init refusal; got:\n{stderr}",
    );
}

/// The snapshot watch with full pattern fidelity: machine values travel as
/// channel elements through the sealed select arm and the received
/// snapshot pattern-matches on state variants, including the heap-payload
/// `Failed { reason }` extraction. Multi-iteration: two snapshots, then
/// the close-driven `None` teardown. Pins the deferred (post-machine)
/// enum-registration pass — before it, `Option<Conn>`'s payload was sized
/// against the still-opaque machine struct, the recv decode wrote past the
/// alloca, and the second iteration lost its wake.
#[test]
fn machine_snapshot_select_watch_matches_state_variants() {
    run_inline_scribbled(
        "machine_snapshot_select",
        "\
         // Snapshot watch: machine values as channel elements through the sealed\n\
         // select arm, state-variant pattern matching on the received snapshot.\n\
         import std::channel::channel;\n\
         \n\
         machine Conn {\n\
         \x20   events {\n\
         \x20       Connect;\n\
         \x20       Fail { reason: string; }\n\
         \x20   }\n\
         \n\
         \x20   state Idle;\n\
         \x20   state Open;\n\
         \x20   state Failed { reason: string; }\n\
         \n\
         \x20   on Connect: Idle => Open {\n\
         \x20       Open\n\
         \x20   }\n\
         \x20   on Fail: Open => Failed {\n\
         \x20       Conn::Failed { reason: event.reason }\n\
         \x20   }\n\
         \x20   on Connect: _ => _ {\n\
         \x20       state\n\
         \x20   }\n\
         \x20   on Fail: _ => _ {\n\
         \x20       state\n\
         \x20   }\n\
         }\n\
         \n\
         actor Owner {\n\
         \x20   receive fn run() {\n\
         \x20       let (tx, rx): (channel.Sender<Conn>, channel.Receiver<Conn>) = channel.new(4);\n\
         \x20       var c: Conn = Conn::Idle;\n\
         \x20       c.step(Connect);\n\
         \x20       tx.send(c);\n\
         \x20       c.step(ConnEvent::Fail { reason: \"peer reset\" });\n\
         \x20       tx.send(c);\n\
         \x20       tx.close();\n\
         \x20       var waiting = true;\n\
         \x20       while waiting {\n\
         \x20           select {\n\
         \x20               snap from rx.recv() => {\n\
         \x20                   match snap {\n\
         \x20                       Some(s) => {\n\
         \x20                           match s {\n\
         \x20                               Conn::Failed { reason } => println(f\"failed: {reason}\"),\n\
         \x20                               Conn::Open => println(\"open\"),\n\
         \x20                               Conn::Idle => println(\"idle\"),\n\
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
         \x20   let o = spawn Owner;\n\
         \x20   o.run();\n\
         \x20   sleep(300ms);\n\
         }\n\
         ",
        "open\nfailed: peer reset\nwatch closed\n",
    );
}

/// `Vec<machine>` is fail-closed at compile time: machine values are valid
/// as channel/queue elements but have no Vec-construction thunk path today.
/// Admitting them into Vec would compile and then panic at runtime (the
/// owned-element admission widening for channels must not bleed into Vec);
/// this pin ensures the compile-time refusal and named diagnostic survive.
/// The machine has a heap-carrying state variant (string payload) so it is
/// not Copy and reaches the admissibility gate.
#[test]
fn vec_machine_element_refuses_at_compile_time() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("vec_machine.hew");
    std::fs::write(
        &source,
        "machine Conn {\n\
         \x20   events {\n\
         \x20       Connect;\n\
         \x20       Fail { reason: string; }\n\
         \x20   }\n\
         \x20   state Idle;\n\
         \x20   state Open;\n\
         \x20   state Failed { reason: string; }\n\
         \x20   on Connect: Idle => Open { Open }\n\
         \x20   on Fail: Open => Failed { Conn::Failed { reason: event.reason } }\n\
         \x20   on Connect: _ => _ { state }\n\
         \x20   on Fail: _ => _ { state }\n\
         }\n\
         \n\
         actor Owner {\n\
         \x20   receive fn run() {\n\
         \x20       var conns: Vec<Conn> = [];\n\
         \x20       var c: Conn = Conn::Idle;\n\
         \x20       conns.push(c);\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let _ = spawn Owner;\n\
         \x20   sleep(20ms);\n\
         }\n",
    )
    .unwrap();

    let output = support::run_hew_in(dir.path(), &["compile", source.to_str().unwrap()]);

    assert!(
        !output.status.success(),
        "Vec<machine> must be refused at compile time; it compiled:\n{}",
        support::describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("machine values cannot be `Vec` elements"),
        "expected the named machine-Vec refusal diagnostic; got:\n{stderr}",
    );
}

/// A channel handle nested inside a tuple `(Receiver<i64>, i64)` is still a
/// single-owner resource. Sending the tuple to an actor transfers the handle;
/// the caller binding `rx` must be `UseAfterConsume` after the send.
///
/// Before the fix, `checked_span_is_channel_handle` only matched the top-level
/// type — a tuple arg was lowered as `Read` (`CowShare` in MIR), the caller
/// binding stayed live, and `rx.close()` compiled silently and crashed at
/// runtime (SIGABRT from double-close, SIGSEGV from a freed-then-read pointer).
#[test]
fn nested_channel_handle_in_tuple_use_after_send_refused() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("nested_handle_tuple.hew");
    std::fs::write(
        &source,
        "import std::channel::channel;\n\
         \n\
         actor Worker {\n\
         \x20   receive fn accept(pair: (channel.Receiver<i64>, i64)) {\n\
         \x20       let (rx, _n) = pair;\n\
         \x20       rx.close();\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let (tx, rx): (channel.Sender<i64>, channel.Receiver<i64>) = channel.new(4);\n\
         \x20   tx.close();\n\
         \x20   let w = spawn Worker;\n\
         \x20   w.accept((rx, 42));\n\
         \x20   rx.close();\n\
         }\n",
    )
    .unwrap();

    let output = support::run_hew_in(dir.path(), &["compile", source.to_str().unwrap()]);

    assert!(
        !output.status.success(),
        "rx.close() after transferring rx in a tuple must be refused; it compiled:\n{}",
        support::describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("used after it was consumed") && stderr.contains("`rx`"),
        "expected UseAfterConsume on rx; got:\n{stderr}",
    );
}

/// Positive contract: sending a tuple `(Receiver<string>, string)` to an actor
/// compiles and runs correctly — the handle is transferred (not double-closed),
/// the worker drains and closes the channel, and both sides produce the
/// expected output.
#[test]
fn nested_channel_handle_in_tuple_transfers_correctly() {
    run_inline_scribbled(
        "nested_handle_tuple_transfer",
        "import std::channel::channel;\n\
         \n\
         actor Worker {\n\
         \x20   receive fn deliver(pair: (channel.Receiver<i64>, i64)) {\n\
         \x20       let (rx, n) = pair;\n\
         \x20       rx.close();\n\
         \x20       println(f\"worker got {n}\");\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let (tx, rx): (channel.Sender<i64>, channel.Receiver<i64>) = channel.new(4);\n\
         \x20   tx.close();\n\
         \x20   let w = spawn Worker;\n\
         \x20   w.deliver((rx, 99));\n\
         \x20   sleep(100ms);\n\
         \x20   println(\"done\");\n\
         }\n",
        "worker got 99\ndone\n",
    );
}
