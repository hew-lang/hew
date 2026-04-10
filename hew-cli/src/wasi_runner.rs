//! Bounded `wasmtime` wrapper for `hew run --target wasm32-wasi`.

use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::Duration;

pub(crate) enum WasiRunOutcome {
    Exited(ExitStatus),
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
