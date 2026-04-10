//! Bounded child-process execution helpers for native Hew binaries.

use std::io::Read;
use std::path::Path;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::thread::JoinHandle;
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
///
/// Both stdout and stderr are drained concurrently in background threads to
/// prevent pipe-buffer deadlocks when a process produces large output on
/// either stream before exiting.
pub(crate) fn run_binary_with_timeout(
    binary: &Path,
    timeout: Duration,
) -> Result<BinaryRunOutcome, String> {
    let mut command = Command::new(binary);
    command.stdout(Stdio::piped()).stderr(Stdio::piped());

    let mut child = spawn_bounded_child(&mut command)?;
    let drain = ConcurrentChildOutput::spawn(&mut child)?;
    let start = Instant::now();

    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                let (stdout, stderr) = drain.finish()?;
                if status.success() {
                    return Ok(BinaryRunOutcome::Success { stdout });
                }
                return Ok(BinaryRunOutcome::Failed { stdout, stderr });
            }
            Ok(None) => {
                if start.elapsed() > timeout {
                    let tree_killed =
                        terminate_timed_out_child(&mut child, TimeoutKillTarget::ProcessGroup)?;
                    drain.abandon(tree_killed);
                    return Ok(BinaryRunOutcome::Timeout);
                }
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(e) => return Err(format!("cannot poll child process: {e}")),
        }
    }
}

/// Drains stdout and stderr from a child process concurrently.
///
/// Each pipe is read on its own background thread so that a child filling one
/// pipe buffer cannot prevent the other from being drained, avoiding the
/// classic deadlock where `try_wait` never returns because the child is blocked
/// on a full pipe.
pub(crate) struct ConcurrentChildOutput {
    stdout: ChildPipeReader,
    stderr: ChildPipeReader,
}

impl ConcurrentChildOutput {
    /// Spawn background drain threads for both pipes.
    ///
    /// Takes ownership of the child's stdout and stderr handles, so they must
    /// not have been taken previously.
    pub(crate) fn spawn(child: &mut Child) -> Result<Self, String> {
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| "child stdout pipe missing".to_string())?;
        let stderr = child
            .stderr
            .take()
            .ok_or_else(|| "child stderr pipe missing".to_string())?;

        Ok(Self {
            stdout: ChildPipeReader::spawn(stdout, "stdout"),
            stderr: ChildPipeReader::spawn(stderr, "stderr"),
        })
    }

    /// Join both drain threads and return `(stdout, stderr)`.
    pub(crate) fn finish(self) -> Result<(String, String), String> {
        Ok((self.stdout.finish()?, self.stderr.finish()?))
    }

    /// Dispose of the drain threads on the timeout path.
    ///
    /// `tree_killed` must be `true` iff the entire process tree — including
    /// all descendants that could hold a write end of the pipes — has been
    /// terminated before this is called.  When that is the case the reader
    /// threads will observe EOF promptly and can be joined safely.
    ///
    /// When `tree_killed` is `false` (only the direct child was killed),
    /// joining could block indefinitely if a descendant still holds a pipe
    /// write end.  The threads are detached instead: the thread continues in
    /// the background and finishes when the OS eventually closes the pipe.
    /// In practice this path is only reached on unusual targets where neither
    /// a Unix process group nor a Windows job-tree kill is available.
    pub(crate) fn abandon(self, tree_killed: bool) {
        if tree_killed {
            // All write-end holders are dead; safe to join and discard output.
            let _ = self.finish();
        }
        // else: drop without joining — descendants may still hold pipe handles.
    }
}

struct ChildPipeReader {
    name: &'static str,
    handle: JoinHandle<Result<String, String>>,
}

impl ChildPipeReader {
    fn spawn<T>(stream: T, name: &'static str) -> Self
    where
        T: Read + Send + 'static,
    {
        Self {
            name,
            handle: std::thread::spawn(move || drain_pipe(stream, name)),
        }
    }

    fn finish(self) -> Result<String, String> {
        self.handle
            .join()
            .map_err(|_| format!("child {} reader panicked", self.name))?
    }
}

fn drain_pipe<T: Read>(mut stream: T, name: &str) -> Result<String, String> {
    let mut bytes = Vec::new();
    stream
        .read_to_end(&mut bytes)
        .map_err(|e| format!("cannot read child {name}: {e}"))?;
    Ok(String::from_utf8_lossy(&bytes).into_owned())
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

/// Returns `true` if the entire process tree was terminated (all processes
/// that could hold a write end of a pipe are dead), `false` if only the
/// direct child was killed.
fn terminate_timed_out_child(
    child: &mut Child,
    kill_target: TimeoutKillTarget,
) -> Result<bool, String> {
    let tree_killed = kill_timed_out_child(child, kill_target)?;
    child
        .wait()
        .map_err(|e| format!("cannot reap timed-out child process: {e}"))?;
    Ok(tree_killed)
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
fn kill_timed_out_child(child: &mut Child, kill_target: TimeoutKillTarget) -> Result<bool, String> {
    match kill_target {
        TimeoutKillTarget::Child => {
            kill_child_only(child)?;
            Ok(false)
        }
        TimeoutKillTarget::ProcessGroup => {
            let process_group = child.id() as i32;
            // SAFETY: `killpg` targets the child-created process group. If the
            // group is already gone, `ESRCH` is treated as success and `wait()`
            // reaps the child.
            let result = unsafe { libc::killpg(process_group, libc::SIGKILL) };
            if result == 0 {
                return Ok(true);
            }

            let group_error = std::io::Error::last_os_error();
            if group_error.raw_os_error() == Some(libc::ESRCH) {
                // Group already gone — all processes (and their pipe handles)
                // are already dead.
                return Ok(true);
            }

            kill_child_only(child).map_err(|kill_error| {
                format!(
                    "cannot kill timed-out child process group: {group_error}; \
                     fallback child kill failed: {kill_error}"
                )
            })?;
            Ok(false)
        }
    }
}

/// Kill the process tree rooted at `child` on Windows using `taskkill /T /F`.
///
/// Returns `true` if the tree was definitively terminated (exit 0 or exit 128
/// meaning the process was already gone), so the caller knows it is safe to
/// join pipe-drain threads.  Returns `false` if `taskkill` was unavailable or
/// reported partial/unexpected failure, in which case a child-only kill is
/// attempted as a fallback and drain threads must be detached to avoid a hang.
#[cfg(windows)]
fn kill_timed_out_child(child: &mut Child, kill_target: TimeoutKillTarget) -> Result<bool, String> {
    match kill_target {
        TimeoutKillTarget::Child => {
            kill_child_only(child)?;
            Ok(false)
        }
        TimeoutKillTarget::ProcessGroup => {
            let pid = child.id();
            match Command::new("taskkill")
                .args(["/T", "/F", "/PID", &pid.to_string()])
                .status()
            {
                Ok(s) if s.success() => Ok(true),
                // Exit 128: "The process with PID <n> not found." — already gone,
                // so its pipe handles are closed.
                Ok(s) if s.code() == Some(128) => Ok(true),
                Ok(s) => {
                    // Unexpected taskkill failure; fall back to child-only kill.
                    kill_child_only(child).map_err(|kill_error| {
                        format!(
                            "taskkill exited with {s}; \
                             fallback child kill also failed: {kill_error}"
                        )
                    })?;
                    Ok(false)
                }
                Err(spawn_error) => {
                    // taskkill.exe not available; fall back to child-only kill.
                    kill_child_only(child).map_err(|kill_error| {
                        format!(
                            "cannot spawn taskkill: {spawn_error}; \
                             fallback child kill also failed: {kill_error}"
                        )
                    })?;
                    Ok(false)
                }
            }
        }
    }
}

#[cfg(not(any(unix, windows)))]
fn kill_timed_out_child(
    child: &mut Child,
    _kill_target: TimeoutKillTarget,
) -> Result<bool, String> {
    kill_child_only(child)?;
    Ok(false)
}
