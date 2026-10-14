//! Execution authority for `std::sort`'s iterative merge cores.
//!
//! The Hew-suite tests pin the exact comparison ceilings. This target runs the
//! same production entry points as standalone native O0/O2 programs and under
//! Wasmtime, covering copied-output semantics and owned-string teardown.

mod support;

use std::process::Output;

use support::{describe_output, repo_root};

const SOURCES: [&str; 2] = [
    include_str!("../../tests/core-acceptance/cases/sort-values.hew"),
    include_str!("../../tests/core-acceptance/cases/float-sort-values.hew"),
];

const EXPECTED: &str = "sort-authority=0\n";

fn run_native(text: &str, opt_level: &str) -> Output {
    support::require_codegen();
    let dir = support::tempdir();
    let source = dir.path().join("sort_complexity_native.hew");
    std::fs::write(&source, text).expect("write native sort authority source");

    let mut command = support::hew_command();
    command
        .arg("run")
        .arg(&source)
        .env("HEW_OPT_LEVEL", opt_level)
        .current_dir(repo_root());
    support::run_bounded_command(command, format!("sort authority native O{opt_level}"))
}

fn assert_authority_output(output: &Output, target: &str) {
    assert!(
        output.status.success(),
        "sort authority failed for {target}\n{}",
        describe_output(output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        EXPECTED,
        "sort authority stdout mismatch for {target}\n{}",
        describe_output(output)
    );
}

#[test]
fn sort_merge_authority_runs_at_native_o0_and_o2() {
    for source in SOURCES {
        assert_authority_output(&run_native(source, "0"), "native O0");
        assert_authority_output(&run_native(source, "2"), "native O2");
    }
}

#[test]
fn sort_merge_authority_runs_under_wasmtime() {
    support::require_wasi_runner();
    for text in SOURCES {
        let dir = support::tempdir();
        let source = dir.path().join("sort_complexity_wasi.hew");
        std::fs::write(&source, text).expect("write WASI sort authority source");

        let output = support::bounded_hew_command(
            [
                "run",
                source.to_str().expect("WASI source path UTF-8"),
                "--target",
                "wasm32-wasi",
            ],
            repo_root(),
            "sort authority Wasmtime",
        );
        assert_authority_output(&output, "Wasmtime");
    }
}
