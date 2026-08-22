//! Runtime proof for a direct-call match payload that moves through a vector
//! before a later field return.

#![cfg(unix)]

mod support;

use std::process::Command;

use support::leak_slope::{compile_to_native, measure_leaks_exact};
use support::{describe_output, repo_root, require_codegen};

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1) and the Darwin poisoned allocator"
)]
#[test]
fn direct_call_vec_escape_releases_every_owner() {
    require_codegen();
    let source = std::fs::read_to_string(repo_root().join("tests/obligation-advisory/rC.hew"))
        .expect("read rC fixture");
    let directory = tempfile::Builder::new()
        .prefix("direct-call-vec-escape-")
        .tempdir()
        .expect("create fixture output directory");
    let binary = compile_to_native(&source, directory.path(), "rC");

    let witness = Command::new(&binary)
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1")
        .output()
        .expect("run rC work witness");
    assert!(
        witness.status.success(),
        "rC must finish one million transfers under the poisoned allocator:\n{}",
        describe_output(&witness)
    );
    assert_eq!(
        String::from_utf8_lossy(&witness.stdout),
        "sec-1\ncred\nskipped\n",
        "rC must reach every post-loop witness"
    );
    assert_eq!(
        measure_leaks_exact(&binary),
        (0, 0),
        "rC must release the returned field, its record sibling, the vector, and both carriers"
    );
}
