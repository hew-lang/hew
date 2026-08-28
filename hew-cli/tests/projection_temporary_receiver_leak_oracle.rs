//! Leak and double-free oracle for owned temporaries used as method receivers.

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::Command;

use support::leak_slope::{
    assert_frame_slope_below_tolerance, compile_to_native, run_under_malloc_scribble,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const RECORD_PROJECTION_SOURCE: &str = r#"
type Snap { label: string, n: i64 }

fn make_snap(seed: i64) -> Snap {
    Snap { label: f"snap-{seed}", n: seed }
}

fn direct_projection(seed: i64) {
    println(make_snap(seed).label);
}

fn named_projection(seed: i64) {
    let snap = make_snap(seed);
    println(snap.label);
}

fn main() {
    direct_projection(6);
    named_projection(7);
}
"#;

fn dump_mir(source: &str, stage: &str, dir: &Path) -> String {
    let path = dir.join("record_projection_temporary.hew");
    std::fs::write(&path, source).expect("write projection temporary source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--dump-mir",
            stage,
            path.to_str().expect("Hew source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|error| panic!("invoke hew compile --dump-mir {stage}: {error}"));
    assert!(
        output.status.success(),
        "{stage} MIR dump failed:\n{}",
        describe_output(&output)
    );
    String::from_utf8(output.stdout).expect("MIR dump is UTF-8")
}

fn function_section<'a>(dump: &'a str, marker: &str) -> &'a str {
    let start = dump
        .find(marker)
        .unwrap_or_else(|| panic!("missing function marker `{marker}`:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ")
        .or_else(|| tail.find("\ndefine "))
        .map_or(tail, |next| &tail[..next])
}

fn assert_one_drop_per_reachable_exit(section: &str, needle: &str, owner: &str) {
    let mut counts = Vec::new();
    for line in section.lines() {
        if line.starts_with("    ") && !line.starts_with("      ") && line.ends_with(" ->") {
            counts.push(0);
        } else if line.starts_with("      ") && line.contains(needle) {
            *counts
                .last_mut()
                .expect("a drop-plan entry follows its exit header") += 1;
        }
    }
    assert!(
        counts.iter().all(|count| *count <= 1),
        "{owner} must have at most one release on each mutually exclusive exit; \
         per-exit counts were {counts:?}:\n{section}"
    );
    assert_eq!(
        counts.iter().sum::<usize>(),
        2,
        "{owner} must release on the post-construction unwind and normal return exits:\n{section}"
    );
}

#[test]
fn direct_record_projection_completes_parent_and_leaf_drop_authorities() {
    let dir = tempfile::Builder::new()
        .prefix("record-projection-temporary-structural-")
        .tempdir()
        .expect("tempdir");
    let raw = dump_mir(RECORD_PROJECTION_SOURCE, "raw", dir.path());
    let elaborated = dump_mir(RECORD_PROJECTION_SOURCE, "elab", dir.path());

    let direct_raw = function_section(&raw, "fn direct_projection");
    let direct_elab = function_section(&elaborated, "fn direct_projection");
    assert_eq!(
        direct_raw
            .matches("ty=string fn=release(hew_string_drop)")
            .count(),
        1,
        "the retained field read-copy must keep exactly one inline release:\n{direct_raw}"
    );
    assert_one_drop_per_reachable_exit(
        direct_elab,
        "kind=record_in_place",
        "the anonymous call-result record",
    );
    assert!(
        direct_elab.contains("__hew_temp_projection_parent"),
        "the direct projection must complete the exact provisional parent owner:\n{direct_elab}"
    );

    let named_raw = function_section(&raw, "fn named_projection");
    let named_elab = function_section(&elaborated, "fn named_projection");
    assert_eq!(
        named_raw
            .matches("ty=string fn=release(hew_string_drop)")
            .count(),
        1,
        "the named control must retain the same one read-copy release:\n{named_raw}"
    );
    assert_one_drop_per_reachable_exit(
        named_elab,
        "kind=record_in_place",
        "the named control record",
    );
    assert!(
        !named_elab.contains("__hew_temp_projection_parent"),
        "the named control must use its ordinary binding owner:\n{named_elab}"
    );
}

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
         \x20       + make_vec(seed).clone().len();\n\
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
