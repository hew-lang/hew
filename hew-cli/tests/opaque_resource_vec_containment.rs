//! Native containment oracle for the real imported
//! `std.encoding.json.Value` opaque lifecycle.
//!
//! `json.Value` is `#[opaque]` but not `#[resource]`
//! (`std/encoding/json/json.hew:67`), so a `Vec<json.Value>` gets an ordinary
//! clone-and-drop element descriptor through `hew_json_clone`/`hew_json_free`,
//! not a drop-only one. (A drop-only, clone-refusing descriptor is what an
//! affine `#[resource]` element would get; if `Value` is meant to be affine,
//! the missing marker is a separate defect, not this test's subject.) The
//! emitted descriptor proves the real clone and close symbols are wired; the
//! poisoned native loop exercises 1,000 real JSON allocations/releases and
//! catches a double-free or stale-slot walk.

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
fn vec_of_exact_imported_opaque_handles_uses_the_real_clone_and_close_descriptor() {
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

    // Element descriptors are index-keyed now (reachability-gated emission,
    // universal mangling), not type-named, so locate this Vec's descriptor by
    // its `hew_vec_new_with_elem_layout` use rather than by a type-derived
    // symbol.
    let new_call_at = ll
        .find("call ptr @hew_vec_new_with_elem_layout(ptr @")
        .expect("Vec construction must select an element-layout descriptor");
    let after_at = &ll[new_call_at..];
    let descriptor_name = after_at
        .strip_prefix("call ptr @hew_vec_new_with_elem_layout(ptr @")
        .and_then(|rest| rest.split(')').next())
        .expect("descriptor global name");

    let descriptor_decl = format!("@{descriptor_name} = internal constant {{ i64, i64, i8, ptr, ptr, ptr }} {{ i64 8, i64 8, i8 2, ptr @{descriptor_name}_clone, ptr @{descriptor_name}_drop, ptr null }}");
    assert!(
        ll.contains(&descriptor_decl),
        "json.Value is opaque, not a resource, so its Vec element descriptor \
         must carry both a real clone and a real drop function:\n{descriptor_decl}\nfull IR:\n{ll}"
    );

    let clone_start = ll
        .find(&format!("define internal i32 @{descriptor_name}_clone"))
        .expect("descriptor clone callback");
    let clone_body = &ll[clone_start..];
    let clone_body = &clone_body[..clone_body.find("\n}").expect("clone callback end")];
    assert!(
        clone_body.contains("call ptr @hew_json_clone("),
        "clone callback must invoke the real json clone symbol:\n{clone_body}"
    );

    let drop_start = ll
        .find(&format!("define internal void @{descriptor_name}_drop"))
        .expect("descriptor drop callback");
    let drop_body = &ll[drop_start..];
    let drop_body = &drop_body[..drop_body.find("\n}").expect("drop callback end")];
    assert!(
        drop_body.contains("call void @hew_json_free("),
        "drop callback must invoke the real json close symbol:\n{drop_body}"
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
