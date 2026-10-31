//! Bounded `wasmtime` wrapper for WASI eval and `hew run --target wasm32-wasi`.

use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::Duration;

pub(crate) enum WasiRunOutcome {
    Exited(ExitStatus),
    Timeout,
}

/// Result of a captured WASI module execution (stdout/stderr collected).
#[derive(Debug)]
pub(crate) enum WasiCapturedOutcome {
    /// Process exited successfully; captured stdout.
    Success { stdout: String },
    /// Process exited unsuccessfully; stdout produced before failure and exit code are preserved.
    Failed {
        stdout: String,
        stderr: String,
        exit_code: i32,
    },
    /// Process exceeded the timeout and was terminated.
    Timeout,
}

pub(crate) fn run_module(
    module_path: &Path,
    program_args: &[String],
    timeout: Option<Duration>,
    deterministic: Option<&str>,
) -> Result<WasiRunOutcome, String> {
    let wasmtime = find_wasmtime().ok_or_else(|| {
        "cannot find wasmtime. Install wasmtime or add it to PATH to use `hew run --target wasm32-wasi`".to_string()
    })?;

    let mut command = Command::new(wasmtime);
    command.arg("run");
    reserve_engine_stack(&mut command);
    if let Some(config) = deterministic {
        command
            .arg("--env")
            .arg(format!("HEW_DETERMINISTIC={config}"));
    }
    command.arg(module_path);
    command.args(program_args);

    // Keep wasmtime in the terminal's foreground group so stdin remains
    // readable. The CLI owns only its direct process in this mode.
    let outcome = crate::process::run_command_interactive(&mut command, timeout, true)?;

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
    command.arg("run");
    reserve_engine_stack(&mut command);
    command.arg(module_path);

    match crate::process::run_command_captured(&mut command, timeout)? {
        crate::process::BinaryRunOutcome::Success { stdout } => Ok(WasiCapturedOutcome::Success {
            stdout: stdout.replace("\r\n", "\n"),
        }),
        crate::process::BinaryRunOutcome::Failed {
            stdout,
            stderr,
            exit_code,
            signal: _,
        } => Ok(WasiCapturedOutcome::Failed {
            stdout: stdout.replace("\r\n", "\n"),
            stderr: stderr.replace("\r\n", "\n"),
            exit_code,
        }),
        crate::process::BinaryRunOutcome::Timeout => Ok(WasiCapturedOutcome::Timeout),
    }
}

/// Native stack the engine's main thread may grow to while running a module.
#[cfg(unix)]
const ENGINE_STACK_BYTES: libc::rlim_t = 64 << 20;

/// Give wasmtime enough native stack that a module's own entry checks, which
/// guard its 1 MiB linear-memory stack, fail a deep recursion with
/// `StackOverflow` before the engine's call-stack limit traps the instance
/// (HEW-SPEC-2026 §2.2). The child's main thread may grow to
/// [`ENGINE_STACK_BYTES`], and wasm may use three quarters of it.
#[cfg(unix)]
fn reserve_engine_stack(command: &mut Command) {
    use std::os::unix::process::CommandExt;

    let mut limit = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // SAFETY: `limit` is valid writable storage for one rlimit.
    if unsafe { libc::getrlimit(libc::RLIMIT_STACK, &raw mut limit) } != 0 {
        return;
    }
    let available = if limit.rlim_max == libc::RLIM_INFINITY {
        ENGINE_STACK_BYTES
    } else {
        limit.rlim_max.min(ENGINE_STACK_BYTES)
    };
    if limit.rlim_cur != libc::RLIM_INFINITY && limit.rlim_cur < available {
        let raised = libc::rlimit {
            rlim_cur: available,
            rlim_max: limit.rlim_max,
        };
        // SAFETY: setrlimit is async-signal-safe and touches only the child's
        // own limits between fork and exec.
        unsafe {
            command.pre_exec(move || {
                if libc::setrlimit(libc::RLIMIT_STACK, &raw const raised) != 0 {
                    return Err(std::io::Error::last_os_error());
                }
                Ok(())
            });
        }
    }
    let usable = if limit.rlim_cur == libc::RLIM_INFINITY {
        ENGINE_STACK_BYTES
    } else {
        available.max(limit.rlim_cur)
    };
    command
        .arg("-W")
        .arg(format!("max-wasm-stack={}", usable / 4 * 3));
}

/// Windows keeps the engine's defaults.
#[cfg(not(unix))]
fn reserve_engine_stack(_command: &mut Command) {}

/// Returns the path to `wasmtime` if it is available, using the same lookup
/// order as the production `run_module` / `run_module_captured` functions.
///
/// Exposed so that callers (e.g. test skip guards) can use consistent
/// availability detection without duplicating the search logic.
pub(crate) fn find_wasmtime() -> Option<PathBuf> {
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
