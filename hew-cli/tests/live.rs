//! Actual source input, worker execution and continued authoring after a fault.

#![cfg(any(target_os = "linux", target_os = "macos"))]

use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};

#[test]
fn persistent_native_authoring_keeps_actor_identity_and_clears_bindings() {
    let compiler = Path::new(env!("CARGO_BIN_EXE_hew"));
    let directory = compiler.parent().unwrap();
    let runtime = directory.join(if cfg!(target_os = "macos") {
        "libhew.dylib"
    } else {
        "libhew.so"
    });
    let project = tempfile::tempdir().unwrap();
    std::fs::write(
        project.path().join("helpers.hew"),
        "fn loaded_value() -> i64 { 9 }\n",
    )
    .unwrap();
    let mut process = Command::new(compiler)
        .args(["tool", "live", "--runtime"])
        .arg(runtime)
        .arg("--project")
        .arg(project.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    process
        .stdin
        .take()
        .unwrap()
        .write_all(
            br#"
:load helpers.hew
loaded_value()
actor Counter { var value: i64 = 40; receive fn next() -> i64 { value += 1; value } }
let counter = spawn Counter;
counter.next().expect("counter")
counter.next().expect("counter")
var count: i64 = 40;
count += 2; panic("stop");
count
let letters = "a";
letters[4]
letters
unknown_name
:type count
stop(counter); stopped(counter);
:clear
:session
var count: i64 = 7;
count
:quit
"#,
        )
        .unwrap();
    let output = process.wait_with_output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(output.status.success(), "{stderr}");
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        "9\n41\n42\n42\na\ni64\n7\n"
    );
    assert!(stderr.contains("stop"), "{stderr}");
    assert!(stderr.contains("IndexOutOfBounds (205)"), "{stderr}");
    assert!(stderr.contains("unknown_name"), "{stderr}");
    assert!(stderr.contains(":1:1: error:"), "{stderr}");
}

#[test]
fn live_service_edits_preserve_state_and_reject_invalid_replacements() {
    let compiler = Path::new(env!("CARGO_BIN_EXE_hew"));
    let runtime = compiler
        .parent()
        .unwrap()
        .join(if cfg!(target_os = "macos") {
            "libhew.dylib"
        } else {
            "libhew.so"
        });
    let project = Path::new(env!("CARGO_MANIFEST_DIR")).join("../examples/services");
    let mut process = Command::new(compiler)
        .args(["tool", "live", "--runtime"])
        .arg(runtime)
        .arg("--project")
        .arg(project)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    process
        .stdin
        .take()
        .unwrap()
        .write_all(
            br#"
import breaker;
let service = spawn breaker.BackendService;
let circuit = spawn breaker.CircuitBreaker { backend: service.backend };
fn issue(circuit: breaker.CircuitBreaker, id: i64) -> i64 { circuit.request(id).expect("breaker").expect("backend") }
issue(circuit, 1)
fn issue(circuit: breaker.CircuitBreaker, id: i64) -> i64 { circuit.request(id).expect("breaker").expect("backend") + 1 }
issue(circuit, 2)
fn issue(circuit: breaker.CircuitBreaker, id: i64) -> i64 { "invalid replacement" }
issue(circuit, 3)
circuit.accepted_count().expect("count")
service.backend.set_healthy(false).expect("backend");
circuit.request(4).expect("breaker").is_err()
service.backend.set_healthy(true).expect("backend");
issue(circuit, 5)
circuit.accepted_count().expect("count")
stop(circuit); stopped(circuit); stop(service); stopped(service);
:clear
:quit
"#,
        )
        .unwrap();
    let output = process.wait_with_output().unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(output.status.success(), "{stderr}");
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        "10\n21\n31\n3\ntrue\n51\n4\n",
        "{stderr}"
    );
}
