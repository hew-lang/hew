//! Leak oracles for both structural-clone drop orders.

#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::Command;

use support::leak_slope::{measure_leaks, require_leaks_tool};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const ITERATIONS: usize = 48;
const FLOOR_TOLERANCE: usize = 2;

fn source(body: &str) -> String {
    format!(
        "\
fn make(seed: string) -> (Vec<string>, string) {{
    let original: (Vec<string>, string) = (Vec::new(), seed + \"-tail\");
    original.0.push(seed + \"-item\");
{body}
}}

fn main() -> i64 {{
    var total: i64 = 0;
    for _ in 0..{ITERATIONS} {{
        let kept = make(\"seed\");
        total = total + kept.0[0].len() + kept.1.len();
    }}
    if total != {expected} {{ return 71; }}
    0
}}
",
        expected = ITERATIONS * 18,
    )
}

fn control_source() -> String {
    source("    original")
}

fn keep_clone_source() -> String {
    source(
        "\
    let copy = clone original;
    copy",
    )
}

fn keep_original_source() -> String {
    source(
        "\
    let copy = clone original;
    if copy.0[0].len() != 9 { return original; }
    original",
    )
}

fn compile_to_native(source: &str, dir: &std::path::Path, name: &str) -> PathBuf {
    let hew_src = dir.join(format!("{name}.hew"));
    std::fs::write(&hew_src, source).expect("write Hew source");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit-dir utf-8"),
            hew_src.to_str().expect("Hew source utf-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        output.status.success(),
        "hew compile failed for {name}:\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let binary = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("no native output for {name}:\n{stdout}"));
    PathBuf::from(binary)
}

fn assert_drop_order_has_no_clone_leak(name: &str, fixture: &str) {
    require_leaks_tool();
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix(&format!("structural-clone-{name}-"))
        .tempdir()
        .expect("tempdir");
    let control = compile_to_native(&control_source(), dir.path(), "control");
    let fixture = compile_to_native(fixture, dir.path(), name);
    let control_leaks = measure_leaks(&control);
    let fixture_leaks = measure_leaks(&fixture);
    assert!(
        fixture_leaks <= control_leaks + FLOOR_TOLERANCE,
        "{name}: structural clone leaked {fixture_leaks} nodes against control floor \
         {control_leaks} (tolerance {FLOOR_TOLERANCE})"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; absence must be a counted skip"
)]
#[test]
fn dropping_original_before_returned_clone_is_balanced() {
    assert_drop_order_has_no_clone_leak("keep_clone", &keep_clone_source());
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)`; absence must be a counted skip"
)]
#[test]
fn dropping_clone_before_returned_original_is_balanced() {
    assert_drop_order_has_no_clone_leak("keep_original", &keep_original_source());
}
