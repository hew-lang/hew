//! Exact leak and poisoned-allocator oracle for projection reads through a
//! heap-owning record field.

#![cfg(unix)]

mod support;

use support::leak_slope::{compile_to_native, measure_leaks_exact, run_under_malloc_scribble};
use support::{describe_output, require_codegen};

const TUPLE_SCALAR_SOURCE: &str =
    include_str!("../../tests/vertical-slice/accept/record_tuple_scalar_projection_drop.hew");

fn compile_fixture(prefix: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(prefix)
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(TUPLE_SCALAR_SOURCE, dir.path(), "tuple_scalar_projection");
    (dir, bin)
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn tuple_scalar_projection_is_exactly_leak_clean() {
    let (_dir, bin) = compile_fixture("record-tuple-scalar-leaks-");
    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "a scalar tuple projection must leave zero leaked nodes and bytes"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn tuple_scalar_projection_drops_each_owner_once() {
    let (_dir, bin) = compile_fixture("record-tuple-scalar-scribble-");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "tuple scalar projection double-freed or read poisoned memory:\n{}",
        describe_output(&output)
    );
}
