//! Allocator oracle for imported machine payloads crossing call boundaries.
//!
//! A heap-owning state travels importer-to-module through `bounce`, then back
//! through its return carrier. A second value travels module-to-importer
//! through `make` and crosses a local return carrier before scope exit. The
//! LOW/HIGH allocation slope catches a missing release in either direction;
//! the poisoned allocator catches an over-release or use-after-free.

#![cfg(unix)]

mod support;

use std::path::{Path, PathBuf};
use std::process::Command;

use support::leak_slope::{
    measure_leaks, require_leaks_tool, run_under_malloc_scribble, HIGH_FRAMES, LOW_FRAMES,
    SLOPE_TOLERANCE,
};
use support::{describe_output, hew_binary, repo_root, require_codegen};

const MODULE_SOURCE: &str = r"
pub machine PayloadLifecycle {
    events { Reset, }
    state Empty,
    state Loaded { label: string, },
    on Reset: Loaded => Empty { Empty }
    default { state }
}

pub fn make(label: string) -> PayloadLifecycle {
        PayloadLifecycle.Loaded { label: label }
}

pub fn bounce(value: PayloadLifecycle) -> PayloadLifecycle {
    value
}
";

fn importer_source(frames: usize) -> String {
    format!(
        r#"
import runner.machine_defs;
import std.string;

type Envelope {{ value: machine_defs.PayloadLifecycle, marker: i64, }}

fn local_bounce(value: machine_defs.PayloadLifecycle) -> machine_defs.PayloadLifecycle {{
    value
}}

fn label_of(value: machine_defs.PayloadLifecycle) -> string {{
    match value {{
        machine_defs.PayloadLifecycle.Loaded {{ label }} => label,
        machine_defs.PayloadLifecycle.Empty => "empty",
    }}
}}

fn main() -> i64 {{
    var i: i64 = 0;
    while i < {frames} {{
        let outbound = machine_defs.make(string.repeat("caller", 16));
        let returned = machine_defs.bounce(outbound);
        let envelope = Envelope {{ value: returned, marker: i }};
        if label_of(envelope.value).len() != 96 {{ return 71; }}

        let inbound = machine_defs.make(string.repeat("callee", 16));
        let carried = local_bounce(inbound);
        if label_of(carried).len() != 96 {{ return 72; }}

        println(i);
        i = i + 1;
    }}
    0
}}
"#
    )
}

fn compile_imported_fixture(dir: &Path, frames: usize, name: &str) -> PathBuf {
    require_codegen();
    let runner = dir.join("runner");
    std::fs::create_dir_all(&runner).expect("create imported module directory");
    std::fs::write(runner.join("machine_defs.hew"), MODULE_SOURCE)
        .expect("write imported machine module");
    let source = dir.join(format!("{name}.hew"));
    std::fs::write(&source, importer_source(frames)).expect("write imported machine fixture");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit directory utf-8"),
            source.to_str().expect("source path utf-8"),
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
    stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: ").map(PathBuf::from))
        .unwrap_or_else(|| panic!("no native artifact for {name}:\n{stdout}"))
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator"
)]
#[test]
fn imported_machine_payload_release_slope_stays_flat() {
    require_leaks_tool();
    let dir = tempfile::Builder::new()
        .prefix("imported-machine-release-slope-")
        .tempdir()
        .expect("tempdir");
    let low = compile_imported_fixture(dir.path(), LOW_FRAMES, "low");
    let high = compile_imported_fixture(dir.path(), HIGH_FRAMES, "high");
    for (binary, frames) in [(&low, LOW_FRAMES), (&high, HIGH_FRAMES)] {
        let witness = run_under_malloc_scribble(binary);
        assert!(
            witness.status.success(),
            "imported machine release witness failed:\n{}",
            describe_output(&witness)
        );
        assert_eq!(
            String::from_utf8_lossy(&witness.stdout).lines().count(),
            frames,
            "the release-slope witness must execute every transfer"
        );
    }
    let low_leaks = measure_leaks(&low);
    let high_leaks = measure_leaks(&high);
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "imported machine payload release grew with transfers: low={low_leaks}, high={high_leaks}"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "double-free oracle needs the Darwin poisoned allocator"
)]
#[test]
fn imported_machine_payload_is_not_released_twice() {
    let dir = tempfile::Builder::new()
        .prefix("imported-machine-double-release-")
        .tempdir()
        .expect("tempdir");
    let binary = compile_imported_fixture(dir.path(), HIGH_FRAMES, "double_release");
    let output = run_under_malloc_scribble(&binary);
    assert!(
        output.status.success(),
        "imported machine payload must survive both transfer directions exactly once:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).lines().count(),
        HIGH_FRAMES,
        "the poisoned-allocator probe must execute every transfer"
    );
}
