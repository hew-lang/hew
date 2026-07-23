//! Execution authority for `std::sort`'s iterative merge cores.
//!
//! The Hew-suite tests pin the exact comparison ceilings. This target runs the
//! same production entry points as standalone native O0/O2 programs and under
//! Wasmtime, covering copied-output semantics and owned-string teardown.

mod support;

use std::process::Output;

use support::{describe_output, repo_root};

const SOURCE: &str = r#"import std::sort;

fn check_ints() -> i64 {
    let reverse: Vec<i64> = Vec::new();
    for i in 0 .. 1024 {
        reverse.push(1023 - i);
    }
    let (reverse_out, reverse_comparisons) = sort.sort_ints_counted(reverse);
    if reverse_comparisons > 1024 * 10 { return 10; }
    for i in 0 .. 1024 {
        if reverse_out[i] != i { return 11; }
        if reverse[i] != 1023 - i { return 12; }
    }

    let non_power: Vec<i64> = Vec::new();
    for i in 0 .. 17 {
        non_power.push((i * 7) % 5);
    }
    let non_power_out = sort.sort_ints(non_power);
    var before_sum = 0;
    var after_sum = 0;
    var before_squares = 0;
    var after_squares = 0;
    for i in 0 .. 17 {
        if i > 0 && non_power_out[i - 1] > non_power_out[i] { return 13; }
        before_sum = before_sum + non_power[i];
        after_sum = after_sum + non_power_out[i];
        before_squares = before_squares + non_power[i] * non_power[i];
        after_squares = after_squares + non_power_out[i] * non_power_out[i];
        if non_power[i] != (i * 7) % 5 { return 14; }
    }
    if before_sum != after_sum || before_squares != after_squares { return 15; }

    let equal: Vec<i64> = Vec::new();
    for _i in 0 .. 33 { equal.push(7); }
    let equal_out = sort.sort_ints(equal);
    for i in 0 .. 33 {
        if equal_out[i] != 7 || equal[i] != 7 { return 16; }
    }
    0
}

fn reverse_key(i: i64) -> string {
    match i % 8 {
        0 => "hotel",
        1 => "golf",
        2 => "foxtrot",
        3 => "echo",
        4 => "delta",
        5 => "charlie",
        6 => "bravo",
        _ => "alpha",
    }
}

fn check_strings() -> i64 {
    let duplicate_reverse: Vec<string> = Vec::new();
    for i in 0 .. 256 {
        duplicate_reverse.push(reverse_key(i));
    }
    let (out, comparisons) = sort.sort_strings_counted(duplicate_reverse);
    if comparisons > 256 * 8 { return 20; }
    for i in 1 .. 256 {
        if out[i - 1] > out[i] { return 21; }
    }
    for i in 0 .. 32 {
        if out[i] != "alpha" { return 22; }
        if out[32 + i] != "bravo" { return 23; }
        if out[64 + i] != "charlie" { return 24; }
        if out[96 + i] != "delta" { return 25; }
        if out[128 + i] != "echo" { return 26; }
        if out[160 + i] != "foxtrot" { return 27; }
        if out[192 + i] != "golf" { return 28; }
        if out[224 + i] != "hotel" { return 29; }
    }
    if duplicate_reverse[0] != "hotel" || duplicate_reverse[7] != "alpha" {
        return 30;
    }
    if duplicate_reverse[248] != "hotel" || duplicate_reverse[255] != "alpha" {
        return 31;
    }

    let non_power: Vec<string> = Vec::new();
    for i in 0 .. 17 {
        let value = match i % 3 {
            0 => "charlie",
            1 => "alpha",
            _ => "bravo",
        };
        non_power.push(value);
    }
    let non_power_out = sort.sort_strings(non_power);
    for i in 1 .. 17 {
        if non_power_out[i - 1] > non_power_out[i] { return 32; }
    }
    for i in 0 .. 17 {
        let expected = match i % 3 {
            0 => "charlie",
            1 => "alpha",
            _ => "bravo",
        };
        if non_power[i] != expected { return 33; }
    }
    0
}

fn main() {
    let int_status = check_ints();
    if int_status != 0 {
        println(f"sort-authority={int_status}");
        return;
    }
    let string_status = check_strings();
    println(f"sort-authority={string_status}");
}
"#;

const EXPECTED: &str = "sort-authority=0\n";

fn run_native(opt_level: &str) -> Output {
    support::require_codegen();
    let dir = support::tempdir();
    let source = dir.path().join("sort_complexity_native.hew");
    std::fs::write(&source, SOURCE).expect("write native sort authority source");

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
    assert_authority_output(&run_native("0"), "native O0");
    assert_authority_output(&run_native("2"), "native O2");
}

// WINDOWS-TODO: requires the Wasmtime runtime, which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn sort_merge_authority_runs_under_wasmtime() {
    support::require_wasi_runner();
    let dir = support::tempdir();
    let source = dir.path().join("sort_complexity_wasi.hew");
    std::fs::write(&source, SOURCE).expect("write WASI sort authority source");

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
