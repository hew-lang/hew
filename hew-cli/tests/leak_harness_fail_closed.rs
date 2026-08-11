//! Counterfactual-red tests for the macOS leak-oracle harness.
//!
//! These tests inject failed inspector commands and incomplete work
//! observations into the same verdict helpers used by the live
//! `leaks --atExit` corpus. Each counterfactual must produce a named error; a
//! missing capability or incomplete measurement may never become a green
//! oracle.

#![cfg(unix)]

mod support;

use std::path::Path;
use std::process::Command;
use std::time::Duration;

use support::leak_slope::{try_measure_leaks_command, validate_work_witness, LeakProbe};

fn shell_command(script: &str) -> Command {
    let mut command = Command::new("/bin/sh");
    command.args(["-c", script]);
    command
}

fn assert_measurement_red(label: &str, result: Result<(usize, usize), String>, expected: &str) {
    let error = result.unwrap_err();
    eprintln!("counterfactual RED [{label}]: {error}");
    assert!(
        error.contains(expected),
        "{label}: expected error containing {expected:?}, got:\n{error}"
    );
    assert!(
        error.contains("must not report success"),
        "{label}: fail-closed diagnostic lost its no-silent-green invariant:\n{error}"
    );
}

#[test]
fn missing_leaks_binary_is_red() {
    let command = Command::new("/definitely/not/a/real/leaks-binary");
    assert_measurement_red(
        "missing leaks",
        try_measure_leaks_command(command, "missing-tool-probe", Duration::from_secs(1)),
        "could not inspect",
    );
}

#[test]
fn declined_leaks_attach_is_red() {
    assert_measurement_red(
        "declined attach",
        try_measure_leaks_command(
            shell_command("echo 'attach denied' >&2; exit 1"),
            "declined-probe",
            Duration::from_secs(1),
        ),
        "declined to attach",
    );
}

#[test]
fn leaks_output_without_summary_is_red() {
    assert_measurement_red(
        "no summary",
        try_measure_leaks_command(
            shell_command("printf '%s\\n' 'leaks inspection started'"),
            "no-summary-probe",
            Duration::from_secs(1),
        ),
        "emitted no `Process <pid>",
    );
}

#[test]
fn malformed_leaks_summary_is_red() {
    assert_measurement_red(
        "malformed summary",
        try_measure_leaks_command(
            shell_command("printf '%s\\n' 'Process 42: bananas for many bytes.'"),
            "malformed-summary-probe",
            Duration::from_secs(1),
        ),
        "emitted no `Process <pid>",
    );
}

#[test]
fn leaks_timeout_is_red() {
    assert_measurement_red(
        "timeout",
        try_measure_leaks_command(
            shell_command("sleep 5"),
            "timeout-probe",
            Duration::from_millis(50),
        ),
        "could not inspect",
    );
}

#[test]
fn canonical_leaks_summary_is_accepted() {
    let measurement = try_measure_leaks_command(
        shell_command("printf '%s\\n' 'Process 42: 0 leaks for 0 total leaked bytes.'"),
        "canonical-summary-probe",
        Duration::from_secs(1),
    )
    .expect("canonical leaks summary must remain measurable");
    assert_eq!(measurement, (0, 0));
}

#[test]
fn shrinking_high_probe_work_witness_is_red() {
    let result = validate_work_witness(
        "shrinking-witness",
        3,
        50,
        LeakProbe {
            leak_nodes: 0,
            program_lines: 3,
        },
        LeakProbe {
            leak_nodes: 0,
            program_lines: 2,
        },
        None,
        Path::new("low-probe"),
        Path::new("high-probe"),
    );
    let error = result.unwrap_err();
    eprintln!("counterfactual RED [shrinking witness]: {error}");
    assert!(error.contains("WORK WITNESS"));
    assert!(error.contains("FEWER"));
    assert!(error.contains("measuring nothing"));
}

#[test]
fn incomplete_exact_work_witness_is_red() {
    let result = validate_work_witness(
        "incomplete-exact-witness",
        3,
        50,
        LeakProbe {
            leak_nodes: 0,
            program_lines: 3,
        },
        LeakProbe {
            leak_nodes: 0,
            program_lines: 3,
        },
        Some((3, 50)),
        Path::new("low-probe"),
        Path::new("high-probe"),
    );
    let error = result.unwrap_err();
    eprintln!("counterfactual RED [incomplete exact witness]: {error}");
    assert!(error.contains("WORK WITNESS"));
    assert!(error.contains("not the 50"));
    assert!(error.contains("not a slope sample"));
}
