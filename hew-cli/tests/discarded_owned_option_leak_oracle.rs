//! Leak and exactly-once guards for discarded caller-owned `Option<T>` results.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn discarded_remove_source(frames: usize) -> String {
    format!(
        "fn one() {{\n\
         \x20   let m: HashMap<i64, string> = HashMap::new();\n\
         \x20   m.insert(1, \"discard-remove\".to_upper());\n\
         \x20   m.remove(1);\n\
         }}\n\
         fn main() {{\n\
         \x20   for i in 0..{frames} {{ one(); }}\n\
         }}\n"
    )
}

fn wildcard_remove_source(frames: usize) -> String {
    format!(
        "fn one() {{\n\
         \x20   let m: HashMap<i64, string> = HashMap::new();\n\
         \x20   m.insert(1, \"wildcard-remove\".to_upper());\n\
         \x20   let _ = m.remove(1);\n\
         }}\n\
         fn main() {{\n\
         \x20   for i in 0..{frames} {{ one(); }}\n\
         }}\n"
    )
}

fn discarded_get_source(frames: usize) -> String {
    format!(
        "fn one() {{\n\
         \x20   let m: HashMap<i64, string> = HashMap::new();\n\
         \x20   m.insert(1, \"discard-get\".to_upper());\n\
         \x20   m.get(1);\n\
         }}\n\
         fn main() {{\n\
         \x20   for i in 0..{frames} {{ one(); }}\n\
         }}\n"
    )
}

fn none_control_source(frames: usize) -> String {
    format!(
        "fn one() {{\n\
         \x20   let m: HashMap<i64, string> = HashMap::new();\n\
         \x20   m.remove(1);\n\
         }}\n\
         fn main() {{\n\
         \x20   for i in 0..{frames} {{ one(); }}\n\
         }}\n"
    )
}

const SCRIBBLE_SOURCE: &str = "\
fn main() {\n\
\x20   let m: HashMap<string, string> = HashMap::new();\n\
\x20   m.insert(\"k\", \"still-owned\".to_upper());\n\
\x20   m.get(\"k\");\n\
\x20   print(m[\"k\"]);\n\
}\n";

#[test]
fn discarded_hashmap_remove_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("discarded_hashmap_remove", discarded_remove_source);
}

#[test]
fn wildcard_hashmap_remove_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("wildcard_hashmap_remove", wildcard_remove_source);
}

#[test]
fn discarded_hashmap_get_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("discarded_hashmap_get", discarded_get_source);
}

#[test]
fn discarded_none_control_has_flat_leak_slope() {
    assert_frame_slope_below_tolerance("discarded_none_control", none_control_source);
}

#[test]
fn discarded_get_does_not_release_the_maps_copy() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("discarded-owned-option-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(SCRIBBLE_SOURCE, dir.path(), "discarded_get_scribble");
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "discarded get must release only its clone, not the map's live value:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "STILL-OWNED",
        "the map's copy must remain readable after the discarded clone is released"
    );
}
