//! Rejection oracle for a `#[resource]` held inside a `machine` state.
//!
//! The drop-obligation lattice's `MachineStatePayload` position has no wired
//! release: a handle carried in a machine state payload is closed neither on
//! transition nor at scope exit. Two silent-leak shapes lived here — a
//! `reenter` that carried a payload field through unchanged, and a machine
//! value ending its scope in a state still holding the handle. Rather than
//! compiling either into a leak, the checker rejects the machine declaration
//! outright (`check_machine_state_resource_payloads` in
//! `hew-types/src/check/admissibility.rs`).
//!
//! This oracle pins that fail-closed floor: both shapes must stop in the
//! checker with the diagnostic naming the machine, the state, and the resource
//! type, and must leave no native artifacts behind. When machine drop
//! elaboration lands and the payloads are released tag-aware, these revert to
//! leak-slope oracles proving the exactly-once close.

mod support;

use std::path::Path;
use std::process::Command;

use support::{hew_binary, repo_root};

const REENTER_CARRY_SOURCE: &str = r#"
#[opaque]
type Dq {}

#[resource]
type Handle { raw: Dq; }

impl Handle {
    fn close(self) { unsafe { hew_deque_free(self.raw) }; print("C"); }
}

extern "C" {
    fn hew_deque_new() -> Dq;
    fn hew_deque_free(consume dq: Dq);
}

machine Session {
    events { Open; UseIt; }
    state Idle;
    state Active { h: Handle; }

    on Open: Idle => Active { Active { h: Handle { raw: unsafe { hew_deque_new() } } } }
    on UseIt: Active => Active reenter { Active { h: self.h } }

    default { state }
}

fn main() {
    var s = Session::Idle;
    s.step(SessionEvent::Open);
    for _ in 0..6 {
        s.step(SessionEvent::UseIt);
        print("R");
    }
}
"#;

const RELEASE_PATH_SOURCE: &str = r#"
#[opaque]
type Dq {}

#[resource]
type Handle { raw: Dq; }

impl Handle {
    fn close(self) { unsafe { hew_deque_free(self.raw) }; print("C"); }
}

extern "C" {
    fn hew_deque_new() -> Dq;
    fn hew_deque_free(consume dq: Dq);
}

machine Mixed {
    events { Open; Touch; }
    state Idle;
    state Active { h: Handle; label: string; }

    on Open: Idle => Active {
        Active { h: Handle { raw: unsafe { hew_deque_new() } }, label: "live".to_upper() }
    }
    on Touch: Active => Active reenter { Active { h: self.h, label: self.label } }

    default { state }
}

machine Plain {
    events { Open; Shut; }
    state Idle;
    state Live { h: Handle; }

    on Open: Idle => Live { Live { h: Handle { raw: unsafe { hew_deque_new() } } } }
    on Shut: Live => Idle { Idle }

    default { state }
}

fn main() {
    var s = Mixed::Idle;
    s.step(MixedEvent::Open);
    var p = Plain::Idle;
    p.step(PlainEvent::Open);
    p.step(PlainEvent::Shut);
}
"#;

/// Compile `source` expecting the checker to reject it, and assert every
/// diagnostic in `expected_diagnostics` appears. A rejected program has no
/// leak slope to measure; the assertion is that the rejection fires before
/// any native artifact exists.
fn assert_rejected_in_checker(source: &str, name: &str, expected_diagnostics: &[&str]) {
    let dir = tempfile::Builder::new()
        .prefix(&format!("{name}-"))
        .tempdir()
        .expect("tempdir");
    let source_path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&source_path, source).expect("write machine resource source");
    let emit_dir = dir.path().join("emit");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            emit_dir.to_str().expect("emit path utf-8"),
            source_path.to_str().expect("source path utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        !output.status.success(),
        "{name}: a machine state payload holding a `#[resource]` must be rejected \
         in the checker, not compiled into a silent leak; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    for expected in expected_diagnostics {
        assert!(
            combined.contains(expected),
            "{name}: diagnostic must name the machine, state, and resource:\n\
             expected substring: {expected}\nactual output:\n{combined}"
        );
    }
    assert!(
        combined.contains("is not released on transition or scope exit"),
        "{name}: diagnostic must state why the machine is rejected:\n{combined}"
    );
    assert_no_native_artifacts(&emit_dir, name);
}

fn assert_no_native_artifacts(emit_dir: &Path, name: &str) {
    assert!(
        !emit_dir.exists()
            || std::fs::read_dir(emit_dir)
                .expect("read emit dir")
                .next()
                .is_none(),
        "{name}: checker rejection must not leave MIR/LLVM/native artifacts in {}",
        emit_dir.display()
    );
}

#[test]
fn machine_reenter_carrying_resource_is_rejected_in_checker() {
    assert_rejected_in_checker(
        REENTER_CARRY_SOURCE,
        "machine_reenter_resource",
        &["machine `Session` state `Active` holds `#[resource]`/`#[linear]` value `Handle`"],
    );
}

#[test]
fn machine_state_resource_release_paths_are_rejected_in_checker() {
    assert_rejected_in_checker(
        RELEASE_PATH_SOURCE,
        "machine_release_paths",
        &[
            "machine `Mixed` state `Active` holds `#[resource]`/`#[linear]` value `Handle`",
            "machine `Plain` state `Live` holds `#[resource]`/`#[linear]` value `Handle`",
        ],
    );
}
