//! Exact ownership oracle for the JSON, TOML, and YAML `Value` resources.
//!
//! Each family is exercised at low and high iteration counts. Every frame
//! transfers a child into a container, takes an independent getter for that
//! child, reads it, and drives a malformed parse so the error path's release
//! runs too. `json` and `yaml` carry an `#[opaque]` `Value` with automatic
//! cleanup and report access through `Result`; `toml` keeps its explicit
//! `close()` carrier and fluent consuming builders. The exact-zero leak check
//! catches missed releases; the poisoned-allocator run catches duplicate
//! releases.

#![cfg(unix)]

mod support;

use support::describe_output;
use support::leak_slope::{
    compile_to_native, measure_leaks_exact, run_probe_witness, run_under_malloc_scribble,
    HIGH_FRAMES, LOW_FRAMES,
};
use support::require_codegen;

/// One ownership frame per family: build a container, transfer a fresh child
/// into it, take an independent getter for that child, read it, and let the
/// container go out of scope. `json` and `yaml` carry automatic cleanup on an
/// `#[opaque]` `Value` and report access through `Result`; `toml` keeps its
/// explicit `close()` carrier and its fluent consuming builders. Each frame
/// also drives a malformed parse so the error path's release runs too. Every
/// frame prints exactly one line, which is the probe's work witness.
fn source(family: &str, frames: usize) -> String {
    let body = match family {
        "json" | "yaml" => format!(
            "\x20       var value = {family}.object();\n\
             \x20       match value.set(\"child\", {family}.from_int(7)) {{\n\
             \x20           .Ok(_) => {{}}\n\
             \x20           .Err(_) => {{ return 1; }}\n\
             \x20       }}\n\
             \x20       match value.get_field(\"child\") {{\n\
             \x20           .Ok(.Some(child)) => {{\n\
             \x20               match child.get_int() {{\n\
             \x20                   .Ok(n) => {{ println(f\"{{n}}\"); }}\n\
             \x20                   .Err(_) => {{ return 1; }}\n\
             \x20               }}\n\
             \x20           }}\n\
             \x20           .Ok(.None) => {{ return 1; }}\n\
             \x20           .Err(_) => {{ return 1; }}\n\
             \x20       }}\n\
             \x20       match {family}.parse(\"{invalid}\") {{\n\
             \x20           .Ok(_) => {{ return 1; }}\n\
             \x20           .Err(_) => {{}}\n\
             \x20       }}\n",
            family = family,
            invalid = if family == "json" { "{" } else { "[" },
        ),
        "toml" => "\x20       var value = toml.array();\n\
             \x20       value = value.push(toml.from_int(7));\n\
             \x20       let child = value.array_get(0);\n\
             \x20       println(f\"{child.get_int()}\");\n\
             \x20       child.close();\n\
             \x20       value.close();\n\
             \x20       match toml.parse(\"=\") {\n\
             \x20           .Ok(parsed) => { parsed.close(); return 1; }\n\
             \x20           .Err(_) => {}\n\
             \x20       }\n"
            .to_string(),
        _ => panic!("unknown value-tree family {family}"),
    };
    format!(
        "import std.encoding.{family};\n\
         fn main() -> i32 {{\n\
         \x20   for _ in 0..{frames} {{\n\
         {body}\
         \x20   }}\n\
         \x20   0\n\
         }}\n"
    )
}

/// The `parse` match-result shape on its own: a fresh owner arrives in the
/// `.Ok` arm and must be released exactly once, while the `.Err` arm must
/// release nothing. Every frame prints exactly one line.
fn parse_match_source(family: &str, frames: usize, valid: bool) -> String {
    let document = match (family, valid) {
        ("json", true) => r#"{\"n\":7}"#,
        ("toml", true) => "n = 7",
        ("yaml", true) => "n: 7",
        ("json", false) => "{",
        ("toml", false) => "=",
        ("yaml", false) => "[",
        _ => panic!("unknown value-tree family {family}"),
    };
    let arms = match (family, valid) {
        ("toml", true) => "\x20           .Ok(parsed) => {\n\
             \x20               let child = parsed.get_field(\"n\");\n\
             \x20               let n = child.get_int();\n\
             \x20               child.close();\n\
             \x20               parsed.close();\n\
             \x20               n\n\
             \x20           }\n\
             \x20           .Err(_) => { return 1; }\n"
            .to_string(),
        ("toml", false) => "\x20           .Ok(parsed) => { parsed.close(); return 1; }\n\
             \x20           .Err(_) => 7\n"
            .to_string(),
        (_, true) => "\x20           .Ok(parsed) => {\n\
             \x20               match parsed.get_field(\"n\") {\n\
             \x20                   .Ok(.Some(child)) => {\n\
             \x20                       match child.get_int() {\n\
             \x20                           .Ok(v) => v,\n\
             \x20                           .Err(_) => { return 1; }\n\
             \x20                       }\n\
             \x20                   }\n\
             \x20                   .Ok(.None) => { return 1; }\n\
             \x20                   .Err(_) => { return 1; }\n\
             \x20               }\n\
             \x20           }\n\
             \x20           .Err(_) => { return 1; }\n"
            .to_string(),
        (_, false) => "\x20           .Ok(_) => { return 1; }\n\
             \x20           .Err(_) => 7\n"
            .to_string(),
    };
    format!(
        "import std.encoding.{family};\n\
         fn main() -> i32 {{\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       let n = match {family}.parse(\"{document}\") {{\n\
         {arms}\
         \x20       }};\n\
         \x20       println(f\"{{n}}\");\n\
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
fn parse_match_results_are_exactly_leak_clean() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("value-tree-parse-leaks-")
        .tempdir()
        .expect("tempdir");
    for (case, valid) in [("success", true), ("handled_error", false)] {
        for family in ["json", "toml", "yaml"] {
            for frames in [LOW_FRAMES, HIGH_FRAMES] {
                let bin = compile_to_native(
                    &parse_match_source(family, frames, valid),
                    dir.path(),
                    &format!("{family}_parse_{case}_{frames}"),
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
fn parse_match_results_do_not_double_free_or_read_poison() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("value-tree-parse-scribble-")
        .tempdir()
        .expect("tempdir");
    for (case, valid) in [("success", true), ("handled_error", false)] {
        for family in ["json", "toml", "yaml"] {
            let bin = compile_to_native(
                &parse_match_source(family, HIGH_FRAMES, valid),
                dir.path(),
                &format!("{family}_parse_{case}_scribble"),
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
