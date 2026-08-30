//! End-to-end behaviour for #3122: the compiler-synthesized `<Machine>Event`
//! companion enum goes through the same `Send` classification as an
//! equivalent hand-written enum — payloads decide, no machine-path blanket
//! allowance.
//!
//! `POSITIVE` is HEW-SPEC-2026 §3.11.7's worked example (an actor with a
//! machine field whose handler takes the machine's own event enum): every
//! event is payload-free, so `TcpStateEvent` is trivially `Send` and the
//! program must compile and run. `NEGATIVE` is the control: an event
//! carrying a non-`Send` payload (`Rc<i64>`) must keep the companion enum
//! non-`Send`, refused with the same diagnostic (`report_invalid_actor_send`)
//! a hand-written enum would get, naming the enum at the send call site.

mod support;

use std::path::Path;

use support::{run_hew_in, strip_ansi, tempdir};

const POSITIVE: &str = r"machine TcpState {
    events {
        Syn;
        Ack;
        Reset;
    }

    state Closed;
    state SynReceived;
    state Established;

    on Syn: Closed => SynReceived {
        TcpState.SynReceived
    }
    on Ack: Closed => Closed reenter {
        TcpState.Closed
    }
    on Syn: SynReceived => SynReceived reenter {
        TcpState.SynReceived
    }
    on Ack: SynReceived => Established {
        TcpState.Established
    }
    on Syn: Established => Established reenter {
        TcpState.Established
    }
    on Ack: Established => Established reenter {
        TcpState.Established
    }
    on Reset: _ => Closed {
        TcpState.Closed
    }
}

actor ConnectionManager {
    var tcp: TcpState = TcpState.Closed;

    receive fn handle(event: TcpStateEvent) {
        tcp.step(event);
        println(tcp.state_name());
    }
}

fn main() {
    let cm = spawn ConnectionManager;
    cm.handle(TcpStateEvent.Syn);
    cm.handle(TcpStateEvent.Ack);
    sleep(100ms);
}
";

const NEGATIVE: &str = r#"machine Sensor {
    events {
        Reading { value: Rc<i64>; }
        Reset;
    }

    state Idle;
    state Active;

    on Reading: _ => Active {
        Sensor.Active
    }
    on Reset: _ => Idle {
        Sensor.Idle
    }
}

actor Collector {
    receive fn handle(event: SensorEvent) {
        println("handled");
    }
}

fn main() {
    let c = spawn Collector;
    c.handle(SensorEvent.Reset);
}
"#;

fn write_source(dir: &Path, name: &str, source: &str) -> std::path::PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("fixture must be writable");
    path
}

/// The spec's worked example: a payload-free event enum is trivially `Send`,
/// so the actor handler compiles and the program runs to completion,
/// stepping the machine through both states.
#[test]
fn payload_free_machine_event_enum_is_send_and_runs() {
    let dir = tempdir();
    let path = write_source(dir.path(), "spec_3_11_7.hew", POSITIVE);
    let source = path.to_str().expect("UTF-8 path");

    let run = run_hew_in(dir.path(), &["run", source]);
    let rendered = strip_ansi(&format!(
        "{}{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    ));
    assert!(
        run.status.success(),
        "HEW-SPEC-2026 §3.11.7's actor-over-machine example must compile and run:\n{rendered}"
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("SynReceived"),
        "the handler must observe the Syn transition:\n{stdout}"
    );
    assert!(
        stdout.contains("Established"),
        "the handler must observe the Ack transition:\n{stdout}"
    );
}

/// Negative control: an event enum with a non-`Send` payload (`Rc<i64>`)
/// must NOT be granted `Send` just because it is a machine's companion enum.
/// The refusal must be the same `E_INVALID_SEND` diagnostic a hand-written
/// enum carrying the same payload would get, naming the enum at the send
/// call site — not a machine-specific error and not a silent accept.
#[test]
fn machine_event_enum_with_non_send_payload_is_rejected() {
    let dir = tempdir();
    let path = write_source(dir.path(), "sensor_reject.hew", NEGATIVE);
    let source = path.to_str().expect("UTF-8 path");

    let check = run_hew_in(dir.path(), &["check", source]);
    let rendered = strip_ansi(&format!(
        "{}{}",
        String::from_utf8_lossy(&check.stdout),
        String::from_utf8_lossy(&check.stderr)
    ));
    assert!(
        !check.status.success(),
        "an event carrying a non-Send payload must keep the companion enum non-Send:\n{rendered}"
    );
    assert!(
        rendered.contains("cannot send `SensorEvent` to actor: type is not Send"),
        "the refusal must name the event enum at the send call site:\n{rendered}"
    );
}
