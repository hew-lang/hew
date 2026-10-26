//! Executed copy, mutation and cleanup of imported opaque JSON values.
//! The copied vector must preserve its original while both real payloads are released.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const VEC_RESOURCE: &str = r#"
import std.encoding.json;

fn release_vec() -> i64 {
    let handles = [json.from_int(7), json.from_int(9)];
    var copied = handles;
    copied.set(0, json.from_int(11));
    handles[0].get_int().expect("original integer")
        + copied[0].get_int().expect("copied integer")
}

fn main() {
    var total = 0;
    for _ in 0..500 { total = total + release_vec(); }
    println(total);
}
"#;

#[test]
fn copied_opaque_json_vectors_preserve_independent_values_and_cleanup() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("opaque-resource-vec-")
        .tempdir()
        .expect("tempdir");
    let source = dir.path().join("vec_resource.hew");
    std::fs::write(&source, VEC_RESOURCE).expect("write Hew fixture");
    let binary = hew_testutil::compiled_binary_path(dir.path(), "vec_resource");
    let compiled = Command::new(hew_binary())
        .arg("build")
        .arg(&source)
        .arg("-o")
        .arg(&binary)
        .current_dir(repo_root())
        .output()
        .expect("compile Vec<json.Value> fixture");
    assert!(
        compiled.status.success(),
        "Vec<json.Value> fixture must compile:\n{}",
        describe_output(&compiled)
    );
    let output = Command::new(&binary)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run Vec<json.Value> fixture");
    assert!(
        output.status.success(),
        "poisoned Vec<json.Value> loop must run clean:\n{}",
        describe_output(&output)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "9000\n");
}
