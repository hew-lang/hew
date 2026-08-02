//! Red counterfactual: an opaque close wrapper with extra side effects is not
//! the checker-admitted canonical release and must not earn automatic drop.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root};

const INVALID_RESOURCE_MODULE: &str = r#"
#[resource]
#[opaque]
pub type Value {}

impl Value {
    fn close(consuming self) {
        unsafe { hew_json_free(self) };
        println("closed");
    }
}

extern "C" {
    fn hew_json_from_null() -> Value;
    fn hew_json_free(consume value: Value);
}
"#;

#[test]
fn opaque_close_with_extra_side_effect_is_not_a_canonical_release() {
    let dir = tempfile::Builder::new()
        .prefix("opaque-close-counterfactual-")
        .tempdir()
        .expect("tempdir");
    let module_dir = dir.path().join("std/encoding");
    std::fs::create_dir_all(&module_dir).expect("create exact owner module path");
    std::fs::write(module_dir.join("json.hew"), INVALID_RESOURCE_MODULE)
        .expect("write invalid resource module");
    let root = dir.path().join("main.hew");
    std::fs::write(&root, "import \"std/encoding/json.hew\";\nfn main() {}\n")
        .expect("write root module");

    let output = Command::new(hew_binary())
        .args(["check", root.to_str().expect("source path utf-8")])
        .current_dir(dir.path())
        .env("HEWPATH", repo_root())
        .output()
        .expect("run counterfactual check");
    assert!(
        !output.status.success(),
        "an opaque close that performs work after release must be rejected"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("canonical close does not match")
            || stderr.contains("OpaqueResourceCloseMismatch"),
        "expected exact lifecycle mismatch:\n{}",
        describe_output(&output)
    );
}
