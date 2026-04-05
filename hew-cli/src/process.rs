//! Bounded child-process execution helpers for native Hew binaries.

use std::io::Read;
use std::path::Path;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

/// Result of running a native binary under a timeout.
#[derive(Debug)]
pub(crate) enum BinaryRunOutcome {
    /// The process exited successfully and produced stdout.
    Success { stdout: String },
    /// The process exited unsuccessfully and produced captured output.
    Failed { stdout: String, stderr: String },
    /// The process exceeded the timeout and was terminated.
    Timeout,
}

/// Result of waiting for a child process under a timeout.
#[derive(Debug)]
pub(crate) enum ChildWaitOutcome {
    /// The child exited before the timeout expired.
    Exited(ExitStatus),
    /// The child exceeded the timeout and was terminated.
    Timeout,
}

/// How a timed-out child should be terminated.
#[derive(Clone, Copy, Debug)]
pub(crate) enum TimeoutKillTarget {
    /// Kill only the direct child process.
    Child,
    /// Kill the child's process group.
    ProcessGroup,
}

/// Parse a `--timeout` value expressed in seconds.
pub(crate) fn timeout_from_seconds(seconds: u64) -> Result<Duration, String> {
    if seconds == 0 {
        Err("--timeout must be at least 1 second".to_string())
    } else {
        Ok(Duration::from_secs(seconds))
    }
}

/// Format a timeout duration using the existing CLI text style.
pub(crate) fn format_timeout(timeout: Duration) -> String {
    if timeout.as_millis() > 0 && timeout.as_millis() < 1_000 {
        format!("{}ms", timeout.as_millis())
    } else {
        format!("{}s", timeout.as_secs())
    }
}

/// Execute a native binary with bounded wall-clock time.
pub(crate) fn run_binary_with_timeout(
    binary: &Path,
    timeout: Duration,
) -> Result<BinaryRunOutcome, String> {
    let mut command = Command::new(binary);
    command.stdout(Stdio::piped()).stderr(Stdio::piped());

    let mut child = spawn_bounded_child(&mut command)?;
    match wait_for_child_with_timeout(&mut child, timeout, TimeoutKillTarget::ProcessGroup)? {
        ChildWaitOutcome::Exited(status) => {
            let (stdout, stderr) = collect_child_output(&mut child)?;
            if status.success() {
                Ok(BinaryRunOutcome::Success { stdout })
            } else {
                Ok(BinaryRunOutcome::Failed { stdout, stderr })
            }
        }
        ChildWaitOutcome::Timeout => Ok(BinaryRunOutcome::Timeout),
    }
}

/// Wait for a child process to exit before `timeout`, terminating it otherwise.
pub(crate) fn wait_for_child_with_timeout(
    child: &mut Child,
    timeout: Duration,
    kill_target: TimeoutKillTarget,
) -> Result<ChildWaitOutcome, String> {
    let start = Instant::now();

    loop {
        match child.try_wait() {
            Ok(Some(status)) => return Ok(ChildWaitOutcome::Exited(status)),
            Ok(None) => {
                if start.elapsed() > timeout {
                    terminate_timed_out_child(child, kill_target)?;
                    return Ok(ChildWaitOutcome::Timeout);
                }
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(e) => return Err(format!("cannot poll child process: {e}")),
        }
    }
}

fn collect_child_output(child: &mut Child) -> Result<(String, String), String> {
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| "child stdout pipe missing".to_string())
        .and_then(|stream| read_pipe(stream, "stdout"))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| "child stderr pipe missing".to_string())
        .and_then(|stream| read_pipe(stream, "stderr"))?;
    Ok((stdout, stderr))
}

fn read_pipe<T: Read>(mut stream: T, name: &str) -> Result<String, String> {
    let mut bytes = Vec::new();
    stream
        .read_to_end(&mut bytes)
        .map_err(|e| format!("cannot read child {name}: {e}"))?;
    Ok(String::from_utf8_lossy(&bytes).into_owned())
}

fn terminate_timed_out_child(
    child: &mut Child,
    kill_target: TimeoutKillTarget,
) -> Result<(), String> {
    kill_timed_out_child(child, kill_target)?;
    child
        .wait()
        .map_err(|e| format!("cannot reap timed-out child process: {e}"))?;
    Ok(())
}

#[cfg(unix)]
fn spawn_bounded_child(command: &mut Command) -> Result<Child, String> {
    use std::os::unix::process::CommandExt;

    // SAFETY: `pre_exec` runs in the child process after `fork` and before
    // `exec`. `setpgid(0, 0)` only mutates the child's own process-group
    // membership so timed-out executions can be terminated as a group.
    unsafe {
        command.pre_exec(|| {
            if libc::setpgid(0, 0) == 0 {
                Ok(())
            } else {
                Err(std::io::Error::last_os_error())
            }
        });
    }

    command
        .spawn()
        .map_err(|e| format!("cannot spawn child process: {e}"))
}

#[cfg(not(unix))]
fn spawn_bounded_child(command: &mut Command) -> Result<Child, String> {
    command
        .spawn()
        .map_err(|e| format!("cannot spawn child process: {e}"))
}

fn kill_child_only(child: &mut Child) -> Result<(), String> {
    match child.kill() {
        Ok(()) => Ok(()),
        Err(kill_error) => match child.try_wait() {
            Ok(Some(_)) => Ok(()),
            Ok(None) => Err(format!("cannot kill timed-out child process: {kill_error}")),
            Err(wait_error) => Err(format!(
                "cannot kill timed-out child process: {kill_error}; \
                 failed to confirm child state: {wait_error}"
            )),
        },
    }
}

#[cfg(unix)]
#[allow(
    clippy::cast_possible_wrap,
    reason = "PIDs fit in i32 on all supported Unix platforms"
)]
fn kill_timed_out_child(child: &mut Child, kill_target: TimeoutKillTarget) -> Result<(), String> {
    match kill_target {
        TimeoutKillTarget::Child => kill_child_only(child),
        TimeoutKillTarget::ProcessGroup => {
            let process_group = child.id() as i32;
            // SAFETY: `killpg` targets the child-created process group. If the
            // group is already gone, `ESRCH` is treated as success and `wait()`
            // reaps the child.
            let result = unsafe { libc::killpg(process_group, libc::SIGKILL) };
            if result == 0 {
                return Ok(());
            }

            let group_error = std::io::Error::last_os_error();
            if group_error.raw_os_error() == Some(libc::ESRCH) {
                return Ok(());
            }

            kill_child_only(child).map_err(|kill_error| {
                format!(
                    "cannot kill timed-out child process group: {group_error}; \
                     fallback child kill failed: {kill_error}"
                )
            })
        }
    }
}

#[cfg(not(unix))]
fn kill_timed_out_child(child: &mut Child, _kill_target: TimeoutKillTarget) -> Result<(), String> {
    kill_child_only(child)
}
