//! Nested-collection exactly-once ownership oracles for #2545 and #2546.
//!
//! The two regressions are duals: local `Vec<Vec<T>>` already drops recursively,
//! so a cursor over `rows[i]` must borrow the indexed inner Vec; actor-state
//! `Vec<Vec<T>>` must use the same recursive clone/free protocol at teardown.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const LOCAL_INDEXED_INNER_SOURCE: &str = "\
fn main() -> i64 {
    let rows: Vec<Vec<i64>> = Vec::new();
    let a: Vec<i64> = Vec::new();
    a.push(1);
    a.push(2);
    rows.push(a);
    let b: Vec<i64> = Vec::new();
    b.push(3);
    b.push(4);
    rows.push(b);
    var sum: i64 = 0;
    for v in rows[0] {
        sum = sum + v;
    }
    sum
}
";

fn actor_nested_vec_source(rows: usize) -> String {
    format!(
        "actor Buckets {{\n\
         \x20   var rows: Vec<Vec<i64>>;\n\
         \n\
         \x20   receive fn count() -> i64 {{\n\
         \x20       rows.len()\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let rows: Vec<Vec<i64>> = Vec::new();\n\
         \x20   for i in 0..{rows} {{\n\
         \x20       let row: Vec<i64> = Vec::new();\n\
         \x20       row.push(i);\n\
         \x20       row.push(i + 1);\n\
         \x20       rows.push(row);\n\
         \x20   }}\n\
         \x20   let buckets = spawn Buckets(rows: rows);\n\
         \x20   sleep(500ms);\n\
         \x20   match await buckets.count() {{\n\
         \x20       Ok(v) => v,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

#[test]
fn local_indexed_inner_iteration_frees_each_vec_once() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("nested-collection-local-index-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        LOCAL_INDEXED_INNER_SOURCE,
        dir.path(),
        "local_indexed_inner",
    );
    let output = run_under_malloc_scribble(&bin);

    assert_eq!(
        output.status.code(),
        Some(3),
        "indexed inner-Vec iteration must borrow the handle and leave the recursive outer \
         Vec drop as its sole freer;\n{}",
        describe_output(&output)
    );
}

#[test]
fn actor_nested_vec_teardown_has_no_per_row_leak_slope() {
    assert_frame_slope_below_tolerance("actor_nested_vec_teardown", actor_nested_vec_source);
}
