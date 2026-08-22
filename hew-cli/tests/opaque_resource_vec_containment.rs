//! Native containment oracle for the real imported
//! `std.encoding.json.Value` opaque lifecycle.
//!
//! The emitted descriptor proves exact-close selection and a null clone slot;
//! the poisoned native loop exercises 1,000 real JSON allocations/releases and
//! catches a duplicate close or stale-slot walk.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const VEC_RESOURCE: &str = r#"
import std.encoding.json;

fn release_vec() {
    let handles = [json.null(), json.null()];
}

fn main() {
    for i in 0..500 { release_vec(); }
    println("released=1000");
}
"#;

#[test]
fn vec_of_exact_imported_opaque_resources_uses_exact_drop_only_descriptor() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("opaque-resource-vec-")
        .tempdir()
        .expect("tempdir");
    let source = dir.path().join("vec_resource.hew");
    std::fs::write(&source, VEC_RESOURCE).expect("write Hew fixture");
    let compiled = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-llvm",
            "--emit-dir",
            dir.path().to_str().expect("emit dir utf-8"),
            source.to_str().expect("source path utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("compile Vec<json.Value> fixture");
    assert!(
        compiled.status.success(),
        "Vec<json.Value> fixture must compile:\n{}",
        describe_output(&compiled)
    );
    let stdout = String::from_utf8_lossy(&compiled.stdout);
    let binary = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .map(PathBuf::from)
        .expect("compiler must report native artifact");
    let ll = std::fs::read_to_string(dir.path().join("vec_resource.ll"))
        .expect("read Vec resource LLVM IR");
    assert!(
        ll.contains(
            "@\"__hew_vec_elem_layout_resource_std$encoding$json$Value_drop_only\" = private constant { i64, i64, i8, ptr, ptr } { i64 8, i64 8, i8 2, ptr null, ptr @\"__hew_vec_resource_std$encoding$json$Value_drop_inplace\" }"
        ),
        "exact imported resource descriptor must be clone-null/drop-present"
    );
    let drop_start = ll
        .find("define internal void @\"__hew_vec_resource_std$encoding$json$Value_drop_inplace\"")
        .expect("resource drop callback");
    let drop_body = &ll[drop_start..];
    let drop_body = &drop_body[..drop_body.find("\n}").expect("drop callback end")];
    assert!(
        drop_body.contains("call i8 @\"std.encoding.json.Value::close\""),
        "drop callback must invoke the exact imported close symbol"
    );
    assert!(
        drop_body.contains("store ptr null"),
        "drop callback must neutralize the consumed slot"
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
    assert_eq!(String::from_utf8_lossy(&output.stdout), "released=1000\n");
}
