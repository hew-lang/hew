//! Exercise the PDB generated for a real Windows Hew failure.
#![cfg(windows)]

mod support;

use std::path::{Path, PathBuf};
use std::process::Command;
use support::{hew_binary, require_codegen, run_bounded_command, tempdir};

const SOURCE: &str = include_str!("../../tests/packaged-sdk/debug-failure.hew");

fn build(dir: &Path) -> PathBuf {
    require_codegen();
    let source = dir.join("p.hew");
    std::fs::write(&source, SOURCE).unwrap();
    let binary = dir.join("p.exe");
    let mut command = Command::new(hew_binary());
    command
        .args(["build", "-g"])
        .arg(&source)
        .arg("-o")
        .arg(&binary);
    let output = run_bounded_command(command, "build Windows debug fixture");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        binary.with_extension("pdb").is_file(),
        "generated PDB is missing"
    );
    binary
}

#[test]
fn recorded_failure_address_resolves_through_the_generated_pdb() {
    let dir = tempdir();
    let binary = build(dir.path());
    let log = dir.path().join("failure.ndjson");
    let mut command = Command::new(&binary);
    command
        .env("HEW_BACKTRACE", "1")
        .env("HEW_FAILURE_LOG", &log);
    let run = run_bounded_command(command, "run Windows debug fixture");
    assert!(!run.status.success());
    let text = std::fs::read_to_string(&log).expect("failure log");
    let record: serde_json::Value =
        serde_json::from_str(text.lines().next().expect("failure record")).unwrap();
    assert_eq!(record["message"], "too big", "{record}");
    let frame = record["frames"]
        .as_array()
        .unwrap()
        .iter()
        .find(|frame| frame["hew_fn"] == "level2")
        .unwrap_or_else(|| panic!("level2 frame missing: {record}"));
    let pc = frame["pc"].as_str().expect("frame address");
    let base = record["build"]["image_base"].as_str().expect("image base");
    assert_ne!(base, "0x0");
    // Opening the PDB itself proves the result did not come from the compact
    // Hew tables in the executable. Opening the PE also checks its sidecar ID.
    for input in [binary.clone(), binary.with_extension("pdb")] {
        let mut command = Command::new(hew_binary());
        command
            .arg("symbolize")
            .arg(input)
            .arg(pc)
            .arg(format!("--base={base}"))
            .arg("--return-addresses");
        let result = run_bounded_command(command, "symbolize Windows failure");
        let stdout = String::from_utf8_lossy(&result.stdout);
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(
            stdout.contains("level2") && stdout.contains("p.hew:3"),
            "{stdout}"
        );
    }
}

#[test]
fn imported_failure_address_resolves_to_its_own_source_in_the_pdb() {
    require_codegen();
    let dir = tempdir();
    let source = dir.path().join("main.hew");
    let imported = dir.path().join("helper.hew");
    std::fs::write(
        &source,
        "import helper;\nfn main() { println(helper.level2(4)); }\n",
    )
    .unwrap();
    std::fs::write(
        &imported,
        "pub fn level2(x: i64) -> i64 {\n    let y = x * 3;\n    if y == 12 { panic(\"too big\"); }\n    y\n}\n",
    )
    .unwrap();
    let binary = dir.path().join("imported.exe");
    let mut command = Command::new(hew_binary());
    command
        .args(["build", "-g"])
        .arg(source)
        .arg("-o")
        .arg(&binary);
    let result = run_bounded_command(command, "build imported Windows debug fixture");
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );

    let log = dir.path().join("imported-failure.ndjson");
    let mut command = Command::new(&binary);
    command
        .env("HEW_BACKTRACE", "1")
        .env("HEW_FAILURE_LOG", &log);
    let result = run_bounded_command(command, "run imported Windows debug fixture");
    assert!(!result.status.success());
    let text = std::fs::read_to_string(&log).expect("failure log");
    let record: serde_json::Value = serde_json::from_str(text.lines().next().unwrap()).unwrap();
    let frame = record["frames"]
        .as_array()
        .unwrap()
        .iter()
        .find(|frame| frame["hew_fn"] == "helper.level2")
        .unwrap_or_else(|| panic!("imported frame missing: {record}"));
    let pc = frame["pc"].as_str().expect("frame address");
    let base = record["build"]["image_base"].as_str().expect("image base");
    let mut command = Command::new(hew_binary());
    command
        .arg("symbolize")
        .arg(binary.with_extension("pdb"))
        .arg(pc)
        .arg(format!("--base={base}"))
        .arg("--return-addresses");
    let result = run_bounded_command(command, "symbolize imported Windows failure PDB");
    let stdout = String::from_utf8_lossy(&result.stdout);
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(
        stdout.contains("helper.level2") && stdout.contains("helper.hew:3"),
        "{stdout}"
    );
}

#[test]
#[ignore = "requires the Windows Debugging Tools component (run by the release gate)"]
fn cdb_stops_at_source_and_reads_a_live_local() {
    let dir = tempdir();
    let binary = build(dir.path());
    let mut command = Command::new("cdb.exe");
    // A source line can cover several disjoint instructions. CDB defaults to
    // rejecting those matches unless ambiguous breakpoint resolution is enabled.
    command
        .args([
            "-lines",
            "-c",
            "dx @$debuggerRootNamespace.Debugger.Settings.EngineInitialization.ResolveAmbiguousBreakpoints = true; bu `p.hew:3`; g",
        ])
        .arg(binary);
    let output = hew_testutil::run_command_bounded_with_stdin(
        &mut command,
        "cdb source breakpoint",
        std::time::Duration::from_mins(5),
        b"dv\nk\nq\n",
    )
    .expect("run cdb");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "{stdout}\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        stdout.contains("Breakpoint") && stdout.contains("level2"),
        "{stdout}"
    );
    assert!(stdout.lines().any(is_twelve_local), "{stdout}");
}

fn is_twelve_local(line: &str) -> bool {
    let Some((name, value)) = line.split_once('=') else {
        return false;
    };
    if name.split_whitespace().last() != Some("y") {
        return false;
    }
    let Some(value) = value.split_whitespace().next() else {
        return false;
    };
    let value = value.replace('`', "");
    let parsed = if let Some(hex) = value.strip_prefix("0x") {
        u64::from_str_radix(hex, 16)
    } else {
        value.strip_prefix("0n").unwrap_or(&value).parse()
    };
    parsed == Ok(12)
}

#[test]
fn cdb_local_match_requires_the_actual_variable_and_value() {
    assert!(is_twelve_local("    y = 0n12"));
    assert!(is_twelve_local("    int64 y = 0x00000000`0000000c"));
    assert!(!is_twelve_local("    other = 0n12"));
    assert!(!is_twelve_local("    y = 0n112"));
    assert!(!is_twelve_local("runtime y at 0x12"));
}
