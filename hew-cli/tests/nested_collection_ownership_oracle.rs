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

// Shape 1: local `Vec<Vec<T>>` iterated over the OUTER vec. The loop variable
// `row` is an inner Vec still owned by `rows`, so it must BORROW — the recursive
// `hew_vec_free_owned` drop of `rows` is the sole freer. A per-iteration free of
// `row` would double-free against that recursion; the scribble allocator turns a
// double-free into an abort, so a clean exit proves the loop variable borrows.
const LOCAL_OUTER_ITER_SOURCE: &str = "\
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
    for row in rows {
        sum = sum + row[0];
    }
    sum
}
";

// Shape 6: single-element actor-state `Vec<Vec<T>>`. Pre-fix this read 0 leaks
// only by coincidence (the one inner vec happened to be freed by a for-loop
// cursor); with the recursive owned teardown it is genuinely freed once by the
// state drop. Runs under the scribble allocator so any double-free aborts.
const ACTOR_SINGLE_INNER_SOURCE: &str = "\
actor Holder {
    var rows: Vec<Vec<i64>>;

    receive fn count() -> i64 {
        rows.len()
    }
}

fn main() -> i64 {
    let rows: Vec<Vec<i64>> = Vec::new();
    let inner: Vec<i64> = Vec::new();
    inner.push(7);
    inner.push(8);
    rows.push(inner);
    let holder = spawn Holder(rows: rows);
    sleep(500ms);
    match await holder.count() {
        Ok(v) => v,
        Err(_) => -1,
    }
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

// Shape 4: actor-state `Vec<Vec<T>>` iterated over the OUTER vec INSIDE a receive
// handler, then torn down. Exercises the loop variable over a state-owned inner
// vec (must borrow) followed by the recursive state teardown (must free each
// inner once). A per-iteration free would double-free against the teardown; an
// under-free at teardown would show as a per-row leak slope.
fn actor_nested_vec_iterate_source(rows: usize) -> String {
    format!(
        "actor Buckets {{\n\
         \x20   var rows: Vec<Vec<i64>>;\n\
         \n\
         \x20   receive fn sum_firsts() -> i64 {{\n\
         \x20       var s: i64 = 0;\n\
         \x20       for row in rows {{\n\
         \x20           s = s + row[0];\n\
         \x20       }}\n\
         \x20       s\n\
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
         \x20   match await buckets.sum_firsts() {{\n\
         \x20       Ok(_) => 0,\n\
         \x20       Err(_) => -1,\n\
         \x20   }}\n\
         }}\n"
    )
}

// `Vec<HashMap<K, V>>` actor state: the same owned-descriptor routing must free
// each inner HashMap via `hew_hashmap_free_layout` at teardown. Pins the
// HashMap-element branch of the witness routing (not just the `Vec<Vec<T>>` one).
fn actor_nested_hashmap_source(rows: usize) -> String {
    format!(
        "actor Registry {{\n\
         \x20   var buckets: Vec<HashMap<string, i64>>;\n\
         \n\
         \x20   receive fn total() -> i64 {{\n\
         \x20       buckets.len()\n\
         \x20   }}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let buckets: Vec<HashMap<string, i64>> = Vec::new();\n\
         \x20   for i in 0..{rows} {{\n\
         \x20       let m: HashMap<string, i64> = HashMap::new();\n\
         \x20       m.insert(\"a\", i);\n\
         \x20       m.insert(\"b\", i + 1);\n\
         \x20       buckets.push(m);\n\
         \x20   }}\n\
         \x20   let reg = spawn Registry(buckets: buckets);\n\
         \x20   sleep(500ms);\n\
         \x20   match await reg.total() {{\n\
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

#[test]
fn local_outer_iteration_borrows_each_inner_vec() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("nested-collection-local-outer-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(LOCAL_OUTER_ITER_SOURCE, dir.path(), "local_outer_iter");
    let output = run_under_malloc_scribble(&bin);

    assert_eq!(
        output.status.code(),
        Some(4),
        "iterating the outer Vec must borrow each inner-Vec loop variable and leave the \
         recursive outer drop as the sole freer;\n{}",
        describe_output(&output)
    );
}

#[test]
fn actor_single_inner_vec_teardown_runs_clean() {
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("nested-collection-single-inner-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(ACTOR_SINGLE_INNER_SOURCE, dir.path(), "actor_single_inner");
    let output = run_under_malloc_scribble(&bin);

    assert_eq!(
        output.status.code(),
        Some(1),
        "a single-element actor-state Vec<Vec<T>> must tear down cleanly (freed once by the \
         recursive state drop, not by loop coincidence);\n{}",
        describe_output(&output)
    );
}

#[test]
fn actor_state_outer_iteration_has_no_per_row_leak_slope() {
    assert_frame_slope_below_tolerance(
        "actor_state_outer_iteration",
        actor_nested_vec_iterate_source,
    );
}

#[test]
fn actor_nested_hashmap_teardown_has_no_per_row_leak_slope() {
    assert_frame_slope_below_tolerance(
        "actor_nested_hashmap_teardown",
        actor_nested_hashmap_source,
    );
}
