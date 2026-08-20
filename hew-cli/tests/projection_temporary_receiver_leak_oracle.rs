//! Leak and double-free oracle for owned temporaries used as method receivers.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

fn projection_shapes_source(frames: usize) -> String {
    format!(
        "fn inspect(values: Vec<string>) -> i64 {{\n\
         \x20   var count = 0;\n\
         \x20   for value in values {{\n\
         \x20       if value.len() > 0 {{ count = count + 1; }}\n\
         \x20   }}\n\
         \x20   count\n\
         }}\n\
         fn make_vec(seed: i64) -> Vec<string> {{\n\
         \x20   [f\"made-{{seed}}-a\", f\"made-{{seed}}-b\", f\"made-{{seed}}-c\"]\n\
         }}\n\
         fn frame(seed: i64) -> i64 {{\n\
         \x20   let map: HashMap<string, string> = HashMap.new();\n\
         \x20   let set: HashSet<string> = HashSet.new();\n\
         \x20   let values: Vec<string> = Vec.new();\n\
         \x20   var i = 0;\n\
         \x20   while i < 3 {{\n\
         \x20       map.insert(f\"key-{{seed}}-{{i}}\", f\"value-{{seed}}-{{i}}\");\n\
         \x20       set.insert(f\"member-{{seed}}-{{i}}\");\n\
         \x20       values.push(f\"item-{{seed}}-{{i}}\");\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   let receiver = map.keys().len()\n\
         \x20       + map.values().len()\n\
         \x20       + set.clone().len()\n\
         \x20       + values[..].len()\n\
         \x20       + make_vec(seed).len();\n\
         \x20   var iterated = 0;\n\
         \x20   for key in map.keys() {{\n\
         \x20       if key.len() > 0 {{ iterated = iterated + 1; }}\n\
         \x20   }}\n\
         \x20   let argument = inspect(map.keys());\n\
         \x20   let bound = map.keys();\n\
         \x20   receiver + iterated + argument + bound.len()\n\
         }}\n\
         fn main() -> i64 {{\n\
         \x20   var total = 0;\n\
         \x20   var i = 0;\n\
         \x20   while i < {frames} {{\n\
         \x20       total = total + frame(i);\n\
         \x20       i = i + 1;\n\
         \x20   }}\n\
         \x20   if total == {expected} {{ 0 }} else {{ 95 }}\n\
         }}\n",
        expected = frames * 24,
    )
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn all_temporary_shapes_have_flat_leak_slope() {
    assert_frame_slope_below_tolerance("projection_temporary_receiver", projection_shapes_source);
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn all_temporary_shapes_release_exactly_once() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("projection-temporary-receiver-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &projection_shapes_source(200),
        dir.path(),
        "projection_temporary_receiver_exactly_once",
    );
    let output = run_under_malloc_scribble(&bin);

    assert!(
        output.status.success(),
        "temporary receiver, for-loop, call-argument, and binding owners must each release once:\n{}",
        describe_output(&output)
    );
}
