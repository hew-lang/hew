//! Exact leak and poisoned-allocator oracle for projection reads through a
//! heap-owning record field.

#![cfg(unix)]

mod support;

use support::leak_slope::{compile_to_native, measure_leaks_exact, run_under_malloc_scribble};
use support::{describe_output, require_codegen};

const CASES: &[(&str, &str)] = &[
    (
        "array_heap_move",
        include_str!("../../tests/vertical-slice/accept/record_array_heap_projection_move.hew"),
    ),
    (
        "array_heap_read",
        include_str!("../../tests/vertical-slice/accept/record_array_heap_projection_read.hew"),
    ),
    (
        "array_scalar_move",
        include_str!("../../tests/vertical-slice/accept/record_array_scalar_projection_move.hew"),
    ),
    (
        "array_scalar_read",
        include_str!("../../tests/vertical-slice/accept/record_array_scalar_projection_read.hew"),
    ),
    (
        "enum_heap_move",
        include_str!("../../tests/vertical-slice/accept/record_enum_heap_projection_move.hew"),
    ),
    (
        "enum_heap_read",
        include_str!("../../tests/vertical-slice/accept/record_enum_heap_projection_read.hew"),
    ),
    (
        "enum_scalar_move",
        include_str!("../../tests/vertical-slice/accept/record_enum_scalar_projection_move.hew"),
    ),
    (
        "enum_scalar_read",
        include_str!("../../tests/vertical-slice/accept/record_enum_scalar_projection_read.hew"),
    ),
    (
        "nested_heap_move",
        include_str!("../../tests/vertical-slice/accept/record_nested_heap_projection_move.hew"),
    ),
    (
        "nested_heap_read",
        include_str!("../../tests/vertical-slice/accept/record_nested_heap_projection_read.hew"),
    ),
    (
        "nested_scalar_move",
        include_str!("../../tests/vertical-slice/accept/record_nested_scalar_projection_move.hew"),
    ),
    (
        "nested_scalar_read",
        include_str!("../../tests/vertical-slice/accept/record_nested_scalar_projection_read.hew"),
    ),
    (
        "tuple_heap_move",
        include_str!("../../tests/vertical-slice/accept/record_tuple_heap_projection_move.hew"),
    ),
    (
        "tuple_heap_read",
        include_str!("../../tests/vertical-slice/accept/record_tuple_heap_projection_read.hew"),
    ),
    (
        "tuple_scalar_move",
        include_str!("../../tests/vertical-slice/accept/record_tuple_scalar_projection_move.hew"),
    ),
    (
        "tuple_scalar_read",
        include_str!("../../tests/vertical-slice/accept/record_tuple_scalar_projection_drop.hew"),
    ),
];

fn compile_fixture(
    prefix: &str,
    name: &str,
    source: &str,
) -> (tempfile::TempDir, std::path::PathBuf) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(prefix)
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), name);
    (dir, bin)
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn projection_matrix_is_exactly_leak_clean() {
    for &(name, source) in CASES {
        let (_dir, bin) = compile_fixture("record-projection-leaks-", name, source);
        assert_eq!(
            measure_leaks_exact(&bin),
            (0, 0),
            "{name} must leave zero leaked nodes and bytes"
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn projection_matrix_drops_each_owner_once() {
    for &(name, source) in CASES {
        let (_dir, bin) = compile_fixture("record-projection-scribble-", name, source);
        let output = run_under_malloc_scribble(&bin);
        assert!(
            output.status.success(),
            "{name} double-freed or read poisoned memory:\n{}",
            describe_output(&output)
        );
    }
}
