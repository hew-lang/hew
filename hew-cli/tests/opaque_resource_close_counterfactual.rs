//! Canonicality of an opaque resource's `close` body is retired: the checker
//! no longer requires a single free call with no other side effects. What
//! still fails closed is ordinary affine discipline inside that body — a
//! `close` that consumes `self` twice is a use-after-move like anywhere else.
//! A `close` that runs and never frees anything is admitted; that is a real
//! coverage gap (no double-free protection against a resource silently
//! leaking through a no-op close), pinned here rather than left implicit.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root};

fn write_fixture(dir: &std::path::Path, close_body: &str) -> std::path::PathBuf {
    let module_dir = dir.join("std/encoding");
    std::fs::create_dir_all(&module_dir).expect("create exact owner module path");
    let module = format!(
        r#"
#[resource]
#[opaque]
pub type Value {{}}

impl Value {{
    fn close(consume self) {{
        {close_body}
    }}
}}

extern "C" {{
    fn hew_json_from_null() -> Value;
    fn hew_json_free(consume value: Value);
}}
"#
    );
    std::fs::write(module_dir.join("json.hew"), module).expect("write resource module");
    let root = dir.join("main.hew");
    std::fs::write(&root, "import \"std/encoding/json.hew\";\nfn main() {}\n")
        .expect("write root module");
    root
}

#[test]
fn opaque_close_that_consumes_self_twice_is_rejected() {
    let dir = tempfile::Builder::new()
        .prefix("opaque-close-counterfactual-")
        .tempdir()
        .expect("tempdir");
    let root = write_fixture(
        dir.path(),
        "unsafe { hew_json_free(self) };\n        unsafe { hew_json_free(self) };",
    );

    let output = Command::new(hew_binary())
        .args(["check", root.to_str().expect("source path utf-8")])
        .current_dir(dir.path())
        .env("HEWPATH", repo_root())
        .output()
        .expect("run counterfactual check");
    assert!(
        !output.status.success(),
        "an opaque close that consumes self twice must be rejected"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("use of moved value `self`"),
        "expected a use-after-move refusal naming self:\n{}",
        describe_output(&output)
    );
}

/// Coverage gap, not a claim of correct behaviour: a `close` body that runs
/// and never calls the underlying free function still checks clean. Nothing
/// in the checker requires an opaque resource's declared close to actually
/// release the resource it wraps.
#[test]
fn opaque_close_that_never_frees_is_admitted() {
    let dir = tempfile::Builder::new()
        .prefix("opaque-close-counterfactual-")
        .tempdir()
        .expect("tempdir");
    let root = write_fixture(dir.path(), r#"println("closed without freeing");"#);

    let output = Command::new(hew_binary())
        .args(["check", root.to_str().expect("source path utf-8")])
        .current_dir(dir.path())
        .env("HEWPATH", repo_root())
        .output()
        .expect("run counterfactual check");
    assert!(
        output.status.success(),
        "a close that never frees is currently admitted; if that changes, \
         tighten this test rather than delete it:\n{}",
        describe_output(&output)
    );
}
