//! Exact ownership oracle for the JSON, TOML, and YAML `Value` resources.
//!
//! Each family is exercised at low and high iteration counts. Every frame
//! transfers a child into a container, takes an independent deep-cloned getter,
//! closes the original root, reads and closes the clone, and transfers another
//! child through an invalid parent. The exact-zero leak check catches missed
//! releases; the poisoned-allocator run catches duplicate releases.

#![cfg(unix)]

mod support;

use support::describe_output;
use support::leak_slope::{
    compile_to_native, measure_leaks_exact, run_probe_witness, run_under_malloc_scribble,
    HIGH_FRAMES, LOW_FRAMES,
};
use support::require_codegen;

fn source(family: &str, frames: usize) -> String {
    let (new_container, insert, getter, invalid) = match family {
        "json" => (
            "json.object()",
            "value.with(\"child\", json.from_int(7));",
            "value.get_field(\"child\")",
            "json.parse(\"{\")",
        ),
        "toml" => (
            "toml.array()",
            "value.push(toml.from_int(7));",
            "value.array_get(0)",
            "toml.parse(\"=\")",
        ),
        "yaml" => (
            "yaml.object()",
            "value.with(\"child\", yaml.from_int(7));",
            "value.get_field(\"child\")",
            "yaml.parse(\"[\")",
        ),
        _ => panic!("unknown value-tree family {family}"),
    };
    format!(
        "import std.encoding.{family};\n\
         fn main() {{\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       let value = {new_container};\n\
         \x20       {insert}\n\
         \x20       let child = {getter};\n\
         \x20       value.close();\n\
         \x20       println(child.get_int());\n\
         \x20       child.close();\n\
         \x20       let invalid = {invalid};\n\
         \x20       invalid.with(\"discarded\", {family}.from_int(9));\n\
         \x20       invalid.free();\n\
         \x20   }}\n\
         }}\n"
    )
}

fn try_parse_match_source(family: &str, frames: usize, valid: bool) -> String {
    let (document, arms) = match (family, valid) {
        ("json", true) => (
            r#"{\"n\":7}"#,
            "Ok(parsed) => { let child = parsed.get_field(\"n\"); let n = child.get_int(); child.close(); parsed.close(); n }, Err(_) => return 1",
        ),
        ("toml", true) => (
            "n = 7",
            "Ok(parsed) => { let child = parsed.get_field(\"n\"); let n = child.get_int(); child.close(); parsed.close(); n }, Err(_) => return 1",
        ),
        ("yaml", true) => (
            "n: 7",
            "Ok(parsed) => { let child = parsed.get_field(\"n\"); let n = child.get_int(); child.close(); parsed.close(); n }, Err(_) => return 1",
        ),
        ("json", false) => (
            "{",
            "Ok(parsed) => { parsed.close(); return 1; }, Err(_) => 7",
        ),
        ("toml", false) => (
            "=",
            "Ok(parsed) => { parsed.close(); return 1; }, Err(_) => 7",
        ),
        ("yaml", false) => (
            "[",
            "Ok(parsed) => { parsed.close(); return 1; }, Err(_) => 7",
        ),
        _ => panic!("unknown value-tree family {family}"),
    };
    format!(
        "import std.encoding.{family};\n\
         fn main() -> i32 {{\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       let n = match {family}.try_parse(\"{document}\") {{\n\
         \x20           {arms}\n\
         \x20       }};\n\
         \x20       println(n);\n\
         \x20   }}\n\
         \x20   0\n\
         }}\n"
    )
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn low_and_high_value_trees_are_exactly_leak_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("value-tree-resource-leaks-")
        .tempdir()
        .expect("tempdir");
    for family in ["json", "toml", "yaml"] {
        for frames in [LOW_FRAMES, HIGH_FRAMES] {
            let bin = compile_to_native(
                &source(family, frames),
                dir.path(),
                &format!("{family}_value_tree_{frames}"),
            );
            assert_eq!(
                run_probe_witness(&bin, &[]),
                frames,
                "{family} must execute every requested ownership frame"
            );
            assert_eq!(
                measure_leaks_exact(&bin),
                (0, 0),
                "{family} must release every owner after {frames} frames"
            );
        }
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn high_value_trees_do_not_double_free_or_read_poison() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("value-tree-resource-scribble-")
        .tempdir()
        .expect("tempdir");
    for family in ["json", "toml", "yaml"] {
        let bin = compile_to_native(
            &source(family, HIGH_FRAMES),
            dir.path(),
            &format!("{family}_value_tree_scribble"),
        );
        let output = run_under_malloc_scribble(&bin);
        assert!(
            output.status.success(),
            "{family} owners must remain valid until their one release:\n{}",
            describe_output(&output)
        );
        assert_eq!(
            String::from_utf8_lossy(&output.stdout).lines().count(),
            HIGH_FRAMES,
            "{family} poisoned-allocator probe must execute every frame"
        );
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "exact leak oracle needs macOS leaks(1); absent capability must be a counted skip"
)]
#[test]
fn try_parse_match_results_are_exactly_leak_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("value-tree-try-parse-leaks-")
        .tempdir()
        .expect("tempdir");
    for (case, valid) in [("success", true), ("handled_error", false)] {
        for family in ["json", "toml", "yaml"] {
            for frames in [LOW_FRAMES, HIGH_FRAMES] {
                let bin = compile_to_native(
                    &try_parse_match_source(family, frames, valid),
                    dir.path(),
                    &format!("{family}_try_parse_{case}_{frames}"),
                );
                assert_eq!(
                    run_probe_witness(&bin, &[]),
                    frames,
                    "{family} {case} must execute every requested match-result frame"
                );
                assert_eq!(
                    measure_leaks_exact(&bin),
                    (0, 0),
                    "{family} {case} must release every owner after {frames} frames"
                );
            }
        }
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn try_parse_match_results_do_not_double_free_or_read_poison() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("value-tree-try-parse-scribble-")
        .tempdir()
        .expect("tempdir");
    for (case, valid) in [("success", true), ("handled_error", false)] {
        for family in ["json", "toml", "yaml"] {
            let bin = compile_to_native(
                &try_parse_match_source(family, HIGH_FRAMES, valid),
                dir.path(),
                &format!("{family}_try_parse_{case}_scribble"),
            );
            let output = run_under_malloc_scribble(&bin);
            assert!(
                output.status.success(),
                "{family} {case} owners must survive until their one release:\n{}",
                describe_output(&output)
            );
            assert_eq!(
                String::from_utf8_lossy(&output.stdout).lines().count(),
                HIGH_FRAMES,
                "{family} {case} probe must execute every frame"
            );
        }
    }
}
