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
