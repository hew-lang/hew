//! Leak oracle for cross-function `Vec<owned-record>` release.
//!
//! The constructing function registers the owned element descriptor, but the
//! final owner may live in a different function whose per-function harvest set
//! has never seen the element key. Both affected release sites must classify
//! the element structurally:
//!
//! - a `for`-in generator-yield binding releases each yielded Vec through
//!   `hew_vec_free_owned`;
//! - a record reassignment overriding a field releases the replaced Vec through
//!   `hew_vec_free_owned`.
//!
//! The ABI requires `hew_vec_free_owned` so every `HeapRow.payload` collection
//! is released. The current runtime keeps `hew_vec_free` descriptor-aware for
//! compatibility, so the oracle also pins the emitted symbol directly; its
//! leak slopes were falsified against a buffer-only compatibility-free neuter.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn generator_yield_source(frames: usize) -> String {
    format!(
        "record HeapRow {{\n\
         \x20   payload: [i64],\n\
         \x20   value: i64,\n\
         }}\n\
         \n\
         fn make_rows(value: i64) -> Vec<HeapRow> {{\n\
         \x20   [HeapRow {{ payload: [value, value + 1], value: value }}]\n\
         }}\n\
         \n\
         gen fn batches() -> Vec<HeapRow> {{\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       yield make_rows(i);\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   var total: i64 = 0;\n\
         \x20   for rows in batches() {{\n\
         \x20       total = total + 1;\n\
         \x20   }}\n\
         \x20   if total != {frames} {{ return 81; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn field_reassignment_source(frames: usize) -> String {
    format!(
        "record HeapRow {{\n\
         \x20   payload: [i64],\n\
         \x20   value: i64,\n\
         }}\n\
         \n\
         record Holder {{\n\
         \x20   rows: Vec<HeapRow>,\n\
         \x20   total: i64,\n\
         }}\n\
         \n\
         fn make_rows(value: i64) -> Vec<HeapRow> {{\n\
         \x20   [HeapRow {{ payload: [value, value + 1], value: value }}]\n\
         }}\n\
         \n\
         fn replace_holder(old: Holder, next: Holder) -> Holder {{\n\
         \x20   var current = old;\n\
         \x20   current = next;\n\
         \x20   current\n\
         }}\n\
         \n\
         fn churn() -> i64 {{\n\
         \x20   var holder = Holder {{ rows: make_rows(0), total: 0 }};\n\
         \x20   for i in 0..{frames} {{\n\
         \x20       let next = Holder {{ rows: make_rows(i), total: i + 1 }};\n\
         \x20       holder = replace_holder(holder, next);\n\
         \x20   }}\n\
         \x20   holder.total\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let total = churn();\n\
         \x20   if total != {frames} {{ return 82; }}\n\
         \x20   0\n\
         }}\n"
    )
}

fn assert_no_double_free(shape_name: &str, source: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("owned-vec-cross-fn-{shape_name}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), shape_name);
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "{shape_name}: owned Vec elements must be released exactly once;\n{}",
        describe_output(&output)
    );
}

fn llvm_function_body<'a>(ir: &'a str, name: &str) -> &'a str {
    let marker = format!("@{name}(");
    let start = ir
        .lines()
        .enumerate()
        .find_map(|(line_no, line)| {
            (line.starts_with("define ") && line.contains(&marker)).then_some(line_no)
        })
        .unwrap_or_else(|| panic!("missing LLVM function `{name}`"));
    let byte_start = ir
        .match_indices('\n')
        .nth(start.saturating_sub(1))
        .map_or(0, |(idx, _)| idx + 1);
    let rest = &ir[byte_start..];
    let byte_end = rest
        .find("\n}\n")
        .unwrap_or_else(|| panic!("unterminated LLVM function `{name}`"))
        + 3;
    &rest[..byte_end]
}

fn assert_owned_release_symbol(source: &str, fixture: &str, function: &str) {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("owned-vec-symbol-{fixture}-"))
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(source, dir.path(), fixture);
    let ir = std::fs::read_to_string(bin.with_extension("ll")).expect("read emitted LLVM IR");
    let body = llvm_function_body(&ir, function);
    assert!(
        body.contains("call void @hew_vec_free_owned("),
        "{function} must release the unharvested owned-element Vec through \
         `hew_vec_free_owned`:\n{body}"
    );
    assert!(
        !body.contains("call void @hew_vec_free("),
        "{function} must not select the plain Vec release for an owned-record element:\n{body}"
    );
}

#[test]
fn affected_release_sites_emit_owned_symbol() {
    assert_owned_release_symbol(&generator_yield_source(3), "generator_symbol", "main");
    assert_owned_release_symbol(
        &field_reassignment_source(3),
        "field_symbol",
        "replace_holder",
    );
}

#[test]
fn generator_yield_owned_vec_payload_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_vec_generator_yield", generator_yield_source);
}

#[test]
fn field_reassignment_owned_vec_payload_leak_slope_below_tolerance() {
    assert_frame_slope_below_tolerance("owned_vec_field_reassignment", field_reassignment_source);
}

#[test]
fn affected_release_paths_do_not_double_free() {
    assert_no_double_free("generator_yield_df", &generator_yield_source(50));
    assert_no_double_free("field_reassignment_df", &field_reassignment_source(50));
}
