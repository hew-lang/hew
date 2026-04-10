//! Bounded `wasmtime` wrapper for `hew run --target wasm32-wasi`.

use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::Duration;

pub(crate) enum WasiRunOutcome {
    Exited(ExitStatus),
    Timeout,
}

/// Result of a captured WASI module execution (stdout/stderr collected).
pub(crate) enum WasiCapturedOutcome {
    /// Process exited successfully; captured stdout.
    Success { stdout: String },
    /// Process exited unsuccessfully; captured stderr for diagnosis.
    Failed { stderr: String },
    /// Process exceeded the timeout and was terminated.
    Timeout,
}

pub(crate) fn run_module(
    module_path: &Path,
    program_args: &[String],
    timeout: Option<Duration>,
) -> Result<WasiRunOutcome, String> {
    let wasmtime = find_wasmtime().ok_or_else(|| {
        "cannot find wasmtime. Install wasmtime or add it to PATH to use `hew run --target wasm32-wasi`".to_string()
    })?;

    let mut command = Command::new(wasmtime);
    command.arg("run").arg(module_path);
    command.args(program_args);

    let mut child = command
        .spawn()
        .map_err(|e| format!("cannot launch wasmtime: {e}"))?;

    #[cfg(unix)]
    crate::signal::forward_signals_to_child(child.id());

    let outcome = match timeout {
        Some(timeout) => crate::process::wait_for_child_with_timeout(
            &mut child,
            timeout,
            crate::process::TimeoutKillTarget::Child,
        )?,
        None => crate::process::ChildWaitOutcome::Exited(
            child
                .wait()
                .map_err(|e| format!("cannot wait for wasmtime: {e}"))?,
        ),
    };

    match outcome {
        crate::process::ChildWaitOutcome::Exited(status) => Ok(WasiRunOutcome::Exited(status)),
        crate::process::ChildWaitOutcome::Timeout => Ok(WasiRunOutcome::Timeout),
    }
}

/// Run a WASM module under wasmtime with captured stdout/stderr.
///
/// Unlike [`run_module`], this variant pipes both output streams so that the
/// caller (e.g. `hew eval --target wasm32-wasi`) can capture the output
/// instead of forwarding it to the terminal.
pub(crate) fn run_module_captured(
    module_path: &Path,
    timeout: Duration,
) -> Result<WasiCapturedOutcome, String> {
    let wasmtime = find_wasmtime().ok_or_else(|| {
        "cannot find wasmtime. Install wasmtime or add it to PATH to use `--target wasm32-wasi`"
            .to_string()
    })?;

    let mut command = Command::new(wasmtime);
    command.arg("run").arg(module_path);

    match crate::process::run_command_captured(&mut command, timeout)? {
        crate::process::BinaryRunOutcome::Success { stdout } => Ok(WasiCapturedOutcome::Success {
            stdout: stdout.replace("\r\n", "\n"),
        }),
        crate::process::BinaryRunOutcome::Failed { stderr, .. } => {
            Ok(WasiCapturedOutcome::Failed { stderr })
        }
        crate::process::BinaryRunOutcome::Timeout => Ok(WasiCapturedOutcome::Timeout),
    }
}

fn find_wasmtime() -> Option<PathBuf> {
    if tool_available("wasmtime") {
        return Some(PathBuf::from("wasmtime"));
    }

    let binary_name = format!("wasmtime{}", crate::platform::exe_suffix());
    [std::env::var_os("HOME"), std::env::var_os("USERPROFILE")]
        .into_iter()
        .flatten()
        .map(PathBuf::from)
        .map(|home| home.join(".wasmtime/bin").join(&binary_name))
        .find(|candidate| candidate.exists())
}

fn tool_available(name: &str) -> bool {
    Command::new(name)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}
