//! Sandbox VM host for compiled Hew tests.
//!
//! The shared checker chooses test entries; the browser SIR projection emits
//! one package per selected entry, and Node runs each package in its own
//! process with the same timeout and report path as native tests.

use super::discovery::TestCase;
use super::runner::{CompiledTestArtifact, Schedule};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

/// Locate the installed VM host, or the package built in this checkout.
///
/// # Errors
/// Returns a setup hint when the JavaScript runtime package is absent.
pub fn resolve_runner(project_dir: &Path) -> Result<PathBuf, String> {
    let mut candidates = Vec::new();
    if let Some(path) = std::env::var_os("HEW_VM_RUNNER") {
        candidates.push(PathBuf::from(path));
    }
    candidates.push(project_dir.join("node_modules/@hew-lang/sandbox-vm/cli/test-runner.mjs"));
    candidates.push(project_dir.join("hew-sandbox-vm/cli/test-runner.mjs"));
    candidates
        .push(Path::new(env!("CARGO_MANIFEST_DIR")).join("../hew-sandbox-vm/cli/test-runner.mjs"));
    candidates
        .into_iter()
        .find(|path| {
            path.is_file()
                && path
                    .parent()
                    .is_some_and(|parent| parent.join("../dist/interpreter/index.js").is_file())
        })
        .ok_or_else(|| {
            "sandbox VM host is missing; install @hew-lang/sandbox-vm or run `make sandbox-vm-deps` in the Hew checkout".into()
        })
}

pub(super) fn compile_test(tests: &[&TestCase]) -> Result<CompiledTestArtifact, String> {
    let test = tests
        .first()
        .ok_or("VM test file has no selected entries")?;
    let source_path = Path::new(&test.file);
    let source = std::fs::read_to_string(source_path)
        .map_err(|error| format!("cannot read {}: {error}", source_path.display()))?;
    let selections = tests.iter().map(|test| test.occurrence).collect::<Vec<_>>();
    let companion = test.companion.as_deref().map(Path::new);
    // The test file's own package, wherever `hew test` was run from.
    let source_dir = source_path.parent().unwrap_or(Path::new("."));
    let project_dir = source_dir
        .ancestors()
        .find(|dir| dir.join("hew.toml").is_file())
        .unwrap_or(source_dir);
    let output = hew_wasm::sandbox::compile_tests_to_sandbox_bytecode(
        &source,
        source_path,
        &selections,
        companion,
        project_dir,
    )
    .map_err(|error| error.to_string())?;
    let errors = output
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == "error")
        .map(|diagnostic| format!("{}: {}", diagnostic.phase, diagnostic.message))
        .collect::<Vec<_>>();
    if !errors.is_empty() {
        return Err(errors.join("\n"));
    }
    if output.bytecodes.len() != tests.len() {
        return Err(format!(
            "sandbox compiler selected {} entries for {} tests",
            output.bytecodes.len(),
            tests.len()
        ));
    }
    let emit_dir = tempfile::Builder::new()
        .prefix("hew_test_vm_")
        .tempdir()
        .map_err(|error| format!("cannot create VM test directory: {error}"))?;
    let mut packages = Vec::with_capacity(output.bytecodes.len());
    for (ordinal, bytecode) in output.bytecodes.iter().enumerate() {
        let path = emit_dir.path().join(format!("test-{ordinal}.json"));
        let bytes = serde_json::to_vec(bytecode)
            .map_err(|error| format!("cannot serialize sandbox test: {error}"))?;
        std::fs::write(&path, bytes)
            .map_err(|error| format!("cannot write sandbox test package: {error}"))?;
        packages.push(path);
    }
    Ok(CompiledTestArtifact::Vm {
        _emit_dir: emit_dir,
        packages,
    })
}

#[allow(
    clippy::too_many_arguments,
    reason = "the VM command needs independent package, runner, scheduler, budget, timeout, report, scratch, and capture inputs"
)]
pub(super) fn execute_test(
    package: &Path,
    runner: &Path,
    (schedule, seed): (Schedule, u64),
    step_budget: u64,
    timeout: Duration,
    report: &Path,
    trace_path: Option<&Path>,
    scratch: &Path,
    capture: bool,
    merge_output: bool,
) -> Result<crate::process::BinaryRunOutcome, String> {
    let mut command = Command::new("node");
    command
        .arg(runner)
        .arg(package)
        .arg("--schedule")
        .arg(schedule.as_str())
        .arg("--seed")
        .arg(seed.to_string())
        .arg("--step-budget")
        .arg(step_budget.to_string())
        .env("HEW_TEST_REPORT", report)
        .env("TMPDIR", scratch)
        .env("TMP", scratch)
        .env("TEMP", scratch);
    if let Some(path) = trace_path {
        command.env("HEW_TEST_TRACE_PATH", path);
    } else {
        command.env_remove("HEW_TEST_TRACE_PATH");
    }
    if capture {
        if merge_output {
            crate::process::run_command_captured_merged(&mut command, timeout)
        } else {
            crate::process::run_command_captured(&mut command, timeout)
        }
    } else {
        crate::process::run_command_uncaptured(&mut command, timeout)
    }
}
