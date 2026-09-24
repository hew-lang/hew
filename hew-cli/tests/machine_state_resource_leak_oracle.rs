//! Rejection oracle for a `#[resource]` held directly inside a `machine` state.
//!
//! A step stages a copy of its machine until it commits, so a fault before
//! commit leaves the caller's machine intact. A direct `#[resource]` payload
//! has no independent value copy, so the checker refuses the transition that
//! takes it, naming the machine, state, field and type, rather than letting
//! the step reach lowering. Purity itself no longer restricts the payload
//! types (D530); an `Rc` payload is admitted and exercised by the
//! `machine-rc-resource-payload` core-acceptance case.
//!
//! This oracle pins that fail-closed floor: both shapes must stop in the
//! checker and leave no native artifacts behind. When a step can take an
//! affine receiver and hand it back on a pre-commit fault, these revert to
//! leak-slope oracles proving the exactly-once close.

mod support;

use std::path::Path;
use std::process::Command;

use support::{hew_binary, repo_root};

const REENTER_CARRY_SOURCE: &str = r#"
#[resource]
type Handle { id: i64, }

impl Handle {
    fn close(consume self) { print("C"); }
}

machine Session {
    events { Open, UseIt, }
    state Idle,
    state Active { h: Handle, },

    on Open: Idle => Active { h: Handle { id: 1 } }
    on UseIt: Active => Active reenter { h: state.h }

    default { state }
}

fn main() {
    var s: Session = .Idle;
    let _ = s.step(.Open);
    for _ in 0..6 {
        let _ = s.step(.UseIt);
        print("R");
    }
}
"#;

const RELEASE_PATH_SOURCE: &str = r#"
#[resource]
type Handle { id: i64, }

impl Handle {
    fn close(consume self) { print("C"); }
}

machine Mixed {
    events { Open, Touch, }
    state Idle,
    state Active { h: Handle, label: string, },

    on Open: Idle => Active { h: Handle { id: 1 }, label: "live" }
    on Touch: Active => Active reenter { h: state.h, label: state.label }

    default { state }
}

machine Plain {
    events { Open, Shut, }
    state Idle,
    state Live { h: Handle, },

    on Open: Idle => Live { h: Handle { id: 2 } }
    on Shut: Live => Idle,

    default { state }
}

fn main() {
    var s: Mixed = .Idle;
    let _ = s.step(.Open);
    var p: Plain = .Idle;
    let _ = p.step(.Open);
    let _ = p.step(.Shut);
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
         in the checker until a step can stage it; stdout: {}\nstderr: {}",
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
            "{name}: diagnostic must name the rejected machine:\n\
             expected substring: {expected}\nactual output:\n{combined}"
        );
    }
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
        &["machine `Session` state `Active` field `h` holds `Handle`"],
    );
}

#[test]
fn machine_state_resource_release_paths_are_rejected_in_checker() {
    assert_rejected_in_checker(
        RELEASE_PATH_SOURCE,
        "machine_release_paths",
        &[
            "machine `Mixed` state `Active` field `h` holds `Handle`",
            "machine `Plain` state `Live` field `h` holds `Handle`",
        ],
    );
}
