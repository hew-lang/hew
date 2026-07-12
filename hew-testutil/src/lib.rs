//! Shared integration-test helpers.

use fd_lock::RwLock;
use std::fmt;
use std::fs::{self, OpenOptions};
use std::io::{ErrorKind, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Output, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::time::{Duration, Instant};

/// Default wall-clock deadline for Hew execution tests.
pub const DEFAULT_EXEC_TIMEOUT: Duration = Duration::from_secs(30);

/// Per-stream capture cap for bounded process output.
///
/// Four MiB is large enough for useful failure diagnostics while preventing a
/// tight infinite-output fixture from growing the test process without bound.
pub const MAX_CAPTURED_BYTES: usize = 4 * 1024 * 1024;

const OUTPUT_READ_CHUNK_BYTES: usize = 8 * 1024;
const POLL_INTERVAL: Duration = Duration::from_millis(10);
// Grace between killing the process tree and waiting for drain threads to see EOF.
const KILL_GRACE: Duration = Duration::from_secs(5);

/// Error returned by bounded process execution.
#[derive(Debug)]
pub enum BoundedExecError {
    /// The process exceeded its wall-clock deadline and was terminated.
    Timeout {
        /// Human-readable command or fixture name.
        label: String,
        /// Configured wall-clock deadline.
        timeout: Duration,
        /// Time elapsed before returning.
        elapsed: Duration,
        /// Captured stdout, capped at [`MAX_CAPTURED_BYTES`] plus a marker.
        stdout: Vec<u8>,
        /// Captured stderr, capped at [`MAX_CAPTURED_BYTES`] plus a marker.
        stderr: Vec<u8>,
    },
    /// Any non-timeout execution infrastructure failure.
    Failed {
        /// Human-readable command or fixture name.
        label: String,
        /// Failure detail.
        message: String,
    },
}

impl BoundedExecError {
    fn failed(label: &str, message: impl Into<String>) -> Self {
        Self::Failed {
            label: label.to_string(),
            message: message.into(),
        }
    }

    /// Returns true when this error represents a deadline kill.
    #[must_use]
    pub fn is_timeout(&self) -> bool {
        matches!(self, Self::Timeout { .. })
    }
}

impl fmt::Display for BoundedExecError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Timeout {
                label,
                timeout,
                elapsed,
                stdout,
                stderr,
            } => write!(
                formatter,
                "`{label}` exceeded {} wall-clock deadline (elapsed {}); process tree was killed\nstdout:\n{}\nstderr:\n{}",
                format_duration(*timeout),
                format_duration(*elapsed),
                String::from_utf8_lossy(stdout),
                String::from_utf8_lossy(stderr),
            ),
            Self::Failed { label, message } => write!(formatter, "`{label}` failed: {message}"),
        }
    }
}

impl std::error::Error for BoundedExecError {}

/// Run a command with captured stdout/stderr, bounded output, and a wall-clock deadline.
///
/// # Errors
///
/// Returns [`BoundedExecError::Timeout`] if the deadline expires after killing
/// the process tree, or [`BoundedExecError::Failed`] if spawning, polling,
/// stdin writing, or pipe draining fails.
pub fn run_command_bounded(
    command: &mut Command,
    label: impl Into<String>,
    timeout: Duration,
) -> Result<Output, BoundedExecError> {
    run_command_bounded_impl(command, label, timeout, None)
}

/// Run a command with stdin input plus captured stdout/stderr under a deadline.
///
/// # Errors
///
/// Returns [`BoundedExecError::Timeout`] if the deadline expires after killing
/// the process tree, or [`BoundedExecError::Failed`] if spawning, polling,
/// stdin writing, or pipe draining fails.
pub fn run_command_bounded_with_stdin(
    command: &mut Command,
    label: impl Into<String>,
    timeout: Duration,
    stdin: &[u8],
) -> Result<Output, BoundedExecError> {
    run_command_bounded_impl(command, label, timeout, Some(stdin.to_vec()))
}

fn run_command_bounded_impl(
    command: &mut Command,
    label: impl Into<String>,
    timeout: Duration,
    stdin: Option<Vec<u8>>,
) -> Result<Output, BoundedExecError> {
    let label = label.into();
    command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let start = Instant::now();
    let deadline = start + timeout;
    let mut bounded = BoundedChild::spawn(command, &label)?;
    let output = ConcurrentOutput::spawn(&mut bounded.child, &label)?;
    let stdin = StdinWriter::spawn(&mut bounded.child, stdin, &label)?;
    let timing = RunTiming {
        start,
        deadline,
        timeout,
        label: &label,
    };

    loop {
        match bounded.child.try_wait().map_err(|error| {
            BoundedExecError::failed(&label, format!("cannot poll child: {error}"))
        })? {
            Some(status) => {
                return finish_exited_child(bounded, &output, stdin, status, timing);
            }
            None if Instant::now() >= deadline => {
                let tree_killed = bounded.terminate_process_group(&label)?;
                return finish_timed_out_child(&output, timing, tree_killed);
            }
            None => sleep_until_next_poll(deadline),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct RunTiming<'a> {
    start: Instant,
    deadline: Instant,
    timeout: Duration,
    label: &'a str,
}

fn finish_exited_child(
    mut bounded: BoundedChild,
    output: &ConcurrentOutput,
    stdin: StdinWriter,
    status: ExitStatus,
    timing: RunTiming<'_>,
) -> Result<Output, BoundedExecError> {
    let (stdout, stderr) = match output.finish_until(timing.deadline, timing.label)? {
        OutputFinish::Complete { stdout, stderr } => (stdout, stderr),
        OutputFinish::Incomplete { stdout, stderr } => {
            let tree_killed = bounded.terminate_process_group(timing.label)?;
            let (stdout, stderr) =
                output.finish_after_kill(tree_killed, timing.label, stdout, stderr)?;
            return Err(BoundedExecError::Timeout {
                label: timing.label.to_string(),
                timeout: timing.timeout,
                elapsed: timing.start.elapsed(),
                stdout,
                stderr,
            });
        }
    };

    if let Some(message) = stdin.finish_until(timing.deadline, timing.label)? {
        return Err(BoundedExecError::failed(timing.label, message));
    }

    Ok(Output {
        status,
        stdout,
        stderr,
    })
}

fn finish_timed_out_child(
    output: &ConcurrentOutput,
    timing: RunTiming<'_>,
    tree_killed: bool,
) -> Result<Output, BoundedExecError> {
    let (stdout, stderr) = output.finish_after_kill(tree_killed, timing.label, None, None)?;
    Err(BoundedExecError::Timeout {
        label: timing.label.to_string(),
        timeout: timing.timeout,
        elapsed: timing.start.elapsed(),
        stdout,
        stderr,
    })
}

fn sleep_until_next_poll(deadline: Instant) {
    let now = Instant::now();
    if now < deadline {
        std::thread::sleep(POLL_INTERVAL.min(deadline.duration_since(now)));
    }
}

fn format_duration(duration: Duration) -> String {
    if duration.as_millis() > 0 && duration.as_millis() < 1_000 {
        format!("{}ms", duration.as_millis())
    } else {
        format!("{}s", duration.as_secs())
    }
}

/// Registers a `pre_exec` hook that moves the spawned child into a fresh,
/// independent process group (`setpgid(0, 0)`) before it execs.
///
/// [`run_command_bounded`] applies this internally. Exists as its own public
/// step for callers that must spawn now and reap later -- e.g. a
/// barrier-synchronized test harness that spawns several long-lived
/// children, releases them together, and only then waits -- rather than
/// `run_command_bounded`'s single spawn-poll-capture-return call. Without
/// this, killing only the direct child on a later timeout leaves any
/// grandchild it forked (a `cargo`/linker invocation, say) running and free
/// to keep mutating a shared artifact after the caller believes the
/// process is gone.
///
/// Must be called before [`Command::spawn`].
#[cfg(unix)]
pub fn own_process_group(command: &mut Command) {
    use std::os::unix::process::CommandExt;

    // SAFETY: this runs in the child after fork and before exec. It only
    // moves the child into a fresh process group so a later group-wide kill
    // also reaches any grandchild the child itself forks.
    unsafe {
        command.pre_exec(|| {
            if libc::setpgid(0, 0) == 0 {
                Ok(())
            } else {
                Err(std::io::Error::last_os_error())
            }
        });
    }
}

/// Kills and reaps every process in `child`'s process group (established
/// via [`own_process_group`]), not just `child` itself, so a grandchild
/// process cannot outlive the group leader's termination. Returns
/// `Ok(true)` if the whole group was killed via `killpg`, `Ok(false)` if
/// `killpg` failed for a reason other than "already gone" (`ESRCH`) and a
/// direct child-only kill was used as a fallback.
///
/// # Errors
///
/// Returns `Err` if neither the group kill nor the child-only fallback
/// succeeds, or if reaping `child` afterward fails.
#[cfg(unix)]
pub fn terminate_process_group(child: &mut Child, label: &str) -> Result<bool, String> {
    let process_group = child.id().cast_signed();
    // SAFETY: `own_process_group` put the child in a process group whose
    // PGID is the child's own PID (the group leader). ESRCH means the group
    // is already gone.
    let result = unsafe { libc::killpg(process_group, libc::SIGKILL) };
    let group_killed = if result != 0 {
        let group_error = std::io::Error::last_os_error();
        if group_error.raw_os_error() == Some(libc::ESRCH) {
            true
        } else {
            kill_child_only(child, label).map_err(|kill_error| {
                format!(
                    "cannot kill process group {process_group}: {group_error}; fallback child kill failed: {kill_error}"
                )
            })?;
            false
        }
    } else {
        true
    };
    child
        .wait()
        .map_err(|error| format!("cannot reap child after kill: {error}"))?;
    Ok(group_killed)
}

#[derive(Debug)]
struct BoundedChild {
    child: Child,
    #[cfg(windows)]
    job: Option<windows_job::WindowsJob>,
}

impl BoundedChild {
    #[cfg(unix)]
    fn spawn(command: &mut Command, label: &str) -> Result<Self, BoundedExecError> {
        own_process_group(command);

        let child = command.spawn().map_err(|error| {
            BoundedExecError::failed(label, format!("cannot spawn child: {error}"))
        })?;
        Ok(Self { child })
    }

    #[cfg(windows)]
    fn spawn(command: &mut Command, label: &str) -> Result<Self, BoundedExecError> {
        use std::os::windows::process::CommandExt;

        const CREATE_SUSPENDED: u32 = 0x0000_0004;

        match windows_job::WindowsJob::new() {
            Err(_) => {
                let child = command.spawn().map_err(|error| {
                    BoundedExecError::failed(label, format!("cannot spawn child: {error}"))
                })?;
                Ok(Self { child, job: None })
            }
            Ok(job) => {
                let mut child =
                    command
                        .creation_flags(CREATE_SUSPENDED)
                        .spawn()
                        .map_err(|error| {
                            BoundedExecError::failed(label, format!("cannot spawn child: {error}"))
                        })?;
                let job = job.assign(&child).ok().map(|()| job);
                if let Err(error) = windows_job::resume_child_process(&child) {
                    let _ = child.kill();
                    let _ = child.wait();
                    return Err(BoundedExecError::failed(
                        label,
                        format!("cannot resume suspended child: {error}"),
                    ));
                }
                Ok(Self { child, job })
            }
        }
    }

    #[cfg(not(any(unix, windows)))]
    fn spawn(command: &mut Command, label: &str) -> Result<Self, BoundedExecError> {
        let child = command.spawn().map_err(|error| {
            BoundedExecError::failed(label, format!("cannot spawn child: {error}"))
        })?;
        Ok(Self { child })
    }

    #[cfg(unix)]
    fn terminate_process_group(&mut self, label: &str) -> Result<bool, BoundedExecError> {
        terminate_process_group(&mut self.child, label)
            .map_err(|message| BoundedExecError::failed(label, message))
    }

    #[cfg(windows)]
    fn terminate_process_group(&mut self, label: &str) -> Result<bool, BoundedExecError> {
        match self.job.as_ref() {
            Some(job) => {
                job.terminate().map_err(|error| {
                    BoundedExecError::failed(label, format!("cannot terminate job object: {error}"))
                })?;
                self.child.wait().map_err(|error| {
                    BoundedExecError::failed(
                        label,
                        format!("cannot reap child after timeout: {error}"),
                    )
                })?;
                Ok(true)
            }
            None => {
                let tree_killed = windows_kill_taskkill(&mut self.child, label)?;
                self.child.wait().map_err(|error| {
                    BoundedExecError::failed(
                        label,
                        format!("cannot reap child after timeout: {error}"),
                    )
                })?;
                Ok(tree_killed)
            }
        }
    }

    #[cfg(not(any(unix, windows)))]
    fn terminate_process_group(&mut self, label: &str) -> Result<bool, BoundedExecError> {
        kill_child_only(&mut self.child, label).map_err(|error| {
            BoundedExecError::failed(label, format!("cannot kill child after timeout: {error}"))
        })?;
        self.child.wait().map_err(|error| {
            BoundedExecError::failed(label, format!("cannot reap child after timeout: {error}"))
        })?;
        Ok(false)
    }
}

fn kill_child_only(child: &mut Child, _label: &str) -> Result<(), String> {
    match child.kill() {
        Ok(()) => Ok(()),
        Err(kill_error) => match child.try_wait() {
            Ok(Some(_)) => Ok(()),
            Ok(None) => Err(format!("cannot kill child: {kill_error}")),
            Err(wait_error) => Err(format!(
                "cannot kill child: {kill_error}; failed to confirm child state: {wait_error}"
            )),
        },
    }
}

#[cfg(windows)]
fn windows_kill_taskkill(child: &mut Child, label: &str) -> Result<bool, BoundedExecError> {
    let pid = child.id();
    match Command::new("taskkill")
        .args(["/T", "/F", "/PID", &pid.to_string()])
        .status()
    {
        Ok(status) if status.success() => Ok(true),
        Ok(status) => {
            kill_child_only(child, label).map_err(|kill_error| {
                BoundedExecError::failed(
                    label,
                    format!(
                        "taskkill exited with {status}; fallback child kill failed: {kill_error}"
                    ),
                )
            })?;
            Ok(false)
        }
        Err(error) => {
            kill_child_only(child, label).map_err(|kill_error| {
                BoundedExecError::failed(
                    label,
                    format!(
                        "cannot spawn taskkill: {error}; fallback child kill failed: {kill_error}"
                    ),
                )
            })?;
            Ok(false)
        }
    }
}

#[derive(Debug)]
struct ConcurrentOutput {
    stdout: PipeReader,
    stderr: PipeReader,
}

#[derive(Debug)]
enum OutputFinish {
    Complete {
        stdout: Vec<u8>,
        stderr: Vec<u8>,
    },
    Incomplete {
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
    },
}

impl ConcurrentOutput {
    fn spawn(child: &mut Child, label: &str) -> Result<Self, BoundedExecError> {
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| BoundedExecError::failed(label, "child stdout pipe missing"))?;
        let stderr = child
            .stderr
            .take()
            .ok_or_else(|| BoundedExecError::failed(label, "child stderr pipe missing"))?;
        Ok(Self {
            stdout: PipeReader::spawn(stdout, "stdout"),
            stderr: PipeReader::spawn(stderr, "stderr"),
        })
    }

    fn finish_until(
        &self,
        deadline: Instant,
        label: &str,
    ) -> Result<OutputFinish, BoundedExecError> {
        let stdout = self.stdout.recv_until(deadline, label)?;
        let stderr = self.stderr.recv_until(deadline, label)?;
        match (stdout, stderr) {
            (Some(stdout), Some(stderr)) => Ok(OutputFinish::Complete { stdout, stderr }),
            (stdout, stderr) => Ok(OutputFinish::Incomplete { stdout, stderr }),
        }
    }

    fn finish_after_kill(
        &self,
        tree_killed: bool,
        label: &str,
        stdout: Option<Vec<u8>>,
        stderr: Option<Vec<u8>>,
    ) -> Result<(Vec<u8>, Vec<u8>), BoundedExecError> {
        let deadline = if tree_killed {
            Instant::now() + KILL_GRACE
        } else {
            Instant::now()
        };
        let stdout = match stdout {
            Some(stdout) => stdout,
            None => self
                .stdout
                .recv_until(deadline, label)?
                .unwrap_or_else(|| abandoned_capture_marker("stdout")),
        };
        let stderr = match stderr {
            Some(stderr) => stderr,
            None => self
                .stderr
                .recv_until(deadline, label)?
                .unwrap_or_else(|| abandoned_capture_marker("stderr")),
        };
        Ok((stdout, stderr))
    }
}

#[derive(Debug)]
struct PipeReader {
    name: &'static str,
    receiver: Receiver<Result<Vec<u8>, String>>,
}

impl PipeReader {
    fn spawn<T>(stream: T, name: &'static str) -> Self
    where
        T: Read + Send + 'static,
    {
        let (sender, receiver) = mpsc::channel();
        std::thread::spawn(move || {
            let _ = sender.send(drain_pipe(stream, name));
        });
        Self { name, receiver }
    }

    fn recv_until(
        &self,
        deadline: Instant,
        label: &str,
    ) -> Result<Option<Vec<u8>>, BoundedExecError> {
        let Some(remaining) = remaining_until(deadline) else {
            return Ok(None);
        };
        match self.receiver.recv_timeout(remaining) {
            Ok(Ok(bytes)) => Ok(Some(bytes)),
            Ok(Err(message)) => Err(BoundedExecError::failed(label, message)),
            Err(RecvTimeoutError::Timeout) => Ok(None),
            Err(RecvTimeoutError::Disconnected) => Err(BoundedExecError::failed(
                label,
                format!("child {} reader exited without output", self.name),
            )),
        }
    }
}

fn drain_pipe<T: Read>(mut stream: T, name: &str) -> Result<Vec<u8>, String> {
    let mut captured = Vec::new();
    let mut chunk = [0; OUTPUT_READ_CHUNK_BYTES];
    let mut truncated = false;

    loop {
        let read = stream
            .read(&mut chunk)
            .map_err(|error| format!("cannot read child {name}: {error}"))?;
        if read == 0 {
            break;
        }

        let remaining = MAX_CAPTURED_BYTES.saturating_sub(captured.len());
        if remaining == 0 {
            truncated = true;
            continue;
        }

        let keep = remaining.min(read);
        captured.extend_from_slice(&chunk[..keep]);
        if keep < read {
            truncated = true;
        }
    }

    if truncated {
        captured.extend_from_slice(truncation_marker().as_bytes());
    }

    Ok(captured)
}

fn truncation_marker() -> String {
    format!("\n[output truncated at {MAX_CAPTURED_BYTES} bytes]\n")
}

fn abandoned_capture_marker(name: &str) -> Vec<u8> {
    format!("\n[{name} capture abandoned after timeout]\n").into_bytes()
}

/// Serialize `cargo build -p hew-lib` across all parallel nextest processes and
/// return the resolved static-library path (`libhew.a` / `hew.lib`).
///
/// One `fd_lock` write-lock plus a `NEXTEST_RUN_ID`-keyed stamp means the first
/// caller in a nextest run performs the (fast, Cargo-fingerprinted) build and
/// every later caller — in any crate — takes the stamped fast path. With no
/// second writer left, Cargo's non-atomic uplift window can no longer race the
/// linker's `open()` of `libhew.a`.
///
/// # Errors
///
/// Returns `Err` if the lock file cannot be opened/locked, if
/// `cargo build -p hew-lib` fails or cannot be spawned, or if the expected
/// static-library artifact is missing after a successful build.
pub fn ensure_hew_lib_built() -> Result<PathBuf, String> {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")) // = <repo>/hew-testutil
        .parent()
        .ok_or("hew-testutil must live under the workspace root")?
        .to_path_buf();
    let (target_dir, profile) = target_dir_and_profile();
    let lib_path = target_dir.join(&profile).join(hew_lib_name());
    ensure_built_serialized(&target_dir, &profile, &lib_path, |td, prof| {
        run_cargo_build_hew_lib(&repo_root, td, prof)
    })?;
    Ok(lib_path)
}

fn hew_lib_name() -> &'static str {
    if cfg!(windows) {
        "hew.lib"
    } else {
        "libhew.a"
    }
}

/// Testable core: `build_fn` is injected so the unit test can stub `cargo`.
///
/// The stamp read, the build, the artifact presence check, and the stamp
/// write all happen inside the `fd_lock` write guard — no TOCTOU window
/// between "stamp matches" and "build" and "artifact present". The lock is
/// intentionally held across `build_fn` (that IS the serialization); the
/// only hazard would be re-entrancy, which is absent here because `build_fn`
/// either shells `cargo` as a subprocess or (in tests) only touches files.
fn ensure_built_serialized(
    target_dir: &Path,
    profile: &str,
    artifact: &Path,
    build_fn: impl FnOnce(&Path, &str) -> Result<(), String>,
) -> Result<(), String> {
    fs::create_dir_all(target_dir).map_err(|e| format!("mkdir {}: {e}", target_dir.display()))?;
    let run_id =
        std::env::var("NEXTEST_RUN_ID").unwrap_or_else(|_| format!("pid:{}", std::process::id()));
    let lock_path = target_dir.join("hew-lib-bootstrap.lock");
    let stamp_path = target_dir.join(format!("hew-lib-bootstrap-{profile}.stamp"));
    let fresh = || fs::read_to_string(&stamp_path).is_ok_and(|s| s == run_id) && artifact.is_file();
    if fresh() {
        return Ok(()); // pre-lock fast path
    }
    let lock_file = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(false)
        .open(&lock_path)
        .map_err(|e| format!("open lock {}: {e}", lock_path.display()))?;
    let mut lock = RwLock::new(lock_file);
    let _guard = lock
        .write()
        .map_err(|e| format!("lock {}: {e}", lock_path.display()))?;
    if fresh() {
        return Ok(()); // re-check under lock
    }
    build_fn(target_dir, profile)?; // build UNDER the lock (by design)
    if !artifact.is_file() {
        return Err(format!(
            "build succeeded but {} was not created",
            artifact.display()
        ));
    }
    fs::write(&stamp_path, &run_id)
        .map_err(|e| format!("write stamp {}: {e}", stamp_path.display()))
}

fn target_dir_and_profile() -> (PathBuf, String) {
    if let Ok(exe) = std::env::current_exe() {
        // <target>/<profile>/deps/<bin>
        if let Some(profile_dir) = exe.parent().and_then(Path::parent) {
            if let (Some(p), Some(t)) = (
                profile_dir.file_name().and_then(|s| s.to_str()),
                profile_dir.parent(),
            ) {
                return (t.to_path_buf(), p.to_string());
            }
        }
    }
    let target = std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .unwrap()
                .join("target")
        },
        PathBuf::from,
    );
    (
        target,
        if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        }
        .to_string(),
    )
}

fn run_cargo_build_hew_lib(
    repo_root: &Path,
    target_dir: &Path,
    profile: &str,
) -> Result<(), String> {
    let mut cmd = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
    cmd.args(["build", "-q", "-p", "hew-lib"])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root);
    match profile {
        // dev/test both land in target/debug
        "debug" => {}
        "release" => {
            cmd.arg("--release");
        }
        other => {
            cmd.args(["--profile", other]); // e.g. CI release-lib
        }
    }
    #[cfg(target_os = "macos")]
    {
        // port verbatim from build_codegen_artifacts
        let dep = std::env::var("MACOSX_DEPLOYMENT_TARGET")
            .ok()
            .filter(|v| !v.is_empty())
            .unwrap_or_else(|| "13.0".to_string());
        cmd.env("MACOSX_DEPLOYMENT_TARGET", dep);
    }
    let out = cmd
        .output()
        .map_err(|e| format!("spawn cargo build -p hew-lib: {e}"))?;
    if !out.status.success() {
        return Err(format!(
            "cargo build -p hew-lib failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ));
    }
    Ok(())
}

#[cfg(test)]
mod hew_lib_bootstrap_tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// N threads race `ensure_built_serialized` on one tempdir; the injected
    /// build stub must run exactly once (`fd_lock` serializes the writers, the
    /// stamp fast-path short-circuits everyone after the first winner).
    #[test]
    fn concurrent_callers_build_exactly_once() {
        let dir = tempfile::tempdir().expect("create tempdir");
        let target_dir = dir.path().to_path_buf();
        let artifact = target_dir.join("libhew-stub.a");
        let build_count = AtomicUsize::new(0);

        std::thread::scope(|scope| {
            let handles: Vec<_> = (0..8)
                .map(|_| {
                    let target_dir = &target_dir;
                    let artifact = &artifact;
                    let build_count = &build_count;
                    scope.spawn(move || {
                        ensure_built_serialized(target_dir, "debug", artifact, |_td, _prof| {
                            build_count.fetch_add(1, Ordering::SeqCst);
                            fs::write(artifact, b"stub archive")
                                .map_err(|e| format!("write stub artifact: {e}"))
                        })
                    })
                })
                .collect();
            for handle in handles {
                handle
                    .join()
                    .expect("bootstrap thread should not panic")
                    .expect("bootstrap thread should succeed");
            }
        });

        assert_eq!(
            build_count.load(Ordering::SeqCst),
            1,
            "build stub should run exactly once across concurrent callers"
        );
        assert!(artifact.is_file(), "stub artifact should be present");
    }
}

#[cfg(test)]
#[cfg(unix)]
mod tests {
    use super::*;

    #[test]
    fn bounded_exec_helper_kills_infinite_output_child() {
        let mut command = Command::new("sh");
        command.arg("-c").arg("yes 0123456789abcdef");

        let timeout = Duration::from_secs(3);
        let started = Instant::now();
        let error = run_command_bounded(&mut command, "infinite-output fixture", timeout)
            .expect_err("infinite-output child should hit the bounded exec timeout");
        let elapsed = started.elapsed();

        let BoundedExecError::Timeout {
            stdout,
            stderr,
            elapsed: reported_elapsed,
            ..
        } = error
        else {
            panic!("expected timeout from infinite-output child, got {error:?}");
        };

        let marker = truncation_marker();
        assert!(
            elapsed < Duration::from_secs(8),
            "bounded exec should return promptly after timeout, elapsed {elapsed:?}"
        );
        assert!(
            reported_elapsed < Duration::from_secs(8),
            "reported timeout elapsed should stay bounded, got {reported_elapsed:?}"
        );
        assert!(
            stderr.is_empty(),
            "fixture should not write stderr: {}",
            String::from_utf8_lossy(&stderr)
        );
        assert!(
            stdout.ends_with(marker.as_bytes()),
            "stdout should end with truncation marker"
        );
        assert_eq!(
            stdout.len(),
            MAX_CAPTURED_BYTES + marker.len(),
            "stdout should be capped plus the marker"
        );
    }

    /// Heartbeat interval used by the process-group probe tests below --
    /// fast enough to observe several beats quickly, slow enough to keep
    /// the probe's own I/O negligible.
    const HEARTBEAT_INTERVAL: Duration = Duration::from_millis(20);
    /// How many heartbeats must be observed before either probe acts,
    /// proving the grandchild is genuinely alive and pumping repeatedly --
    /// not a single write that could race the read.
    const MIN_HEARTBEATS_BEFORE_ACTION: u64 = 3;
    /// Bounds waiting for the grandchild to start heart-beating at all --
    /// a hang backstop, not the proof itself.
    const HEARTBEAT_STARTUP_DEADLINE: Duration = Duration::from_secs(5);
    /// Post-kill settling window for the positive test: `killpg` is
    /// immediate, but a write the grandchild had already entered before
    /// the signal landed can still complete afterward. Waiting this long
    /// before taking the "final" baseline absorbs that race so a
    /// straggling in-flight write is never mistaken for survival.
    const POST_KILL_SETTLE_WINDOW: Duration = Duration::from_millis(200);
    /// Fixed stability window checked *after* settling: the positive
    /// test's proof is that the count taken after this wait matches the
    /// settled baseline exactly, not that it merely stayed below some
    /// threshold -- so any genuine straggler write still fails it.
    const POST_KILL_STABILITY_WINDOW: Duration = Duration::from_millis(300);
    /// Bounds the negative control's wait for the *specific event* of
    /// further heartbeat growth -- a hang backstop for a deterministic
    /// bounded poll, not a fixed sleep-then-single-check window: a
    /// genuinely surviving grandchild grows past the pre-kill count well
    /// within this bound regardless of scheduler jitter, while a broken
    /// negative control (grandchild actually died too) hangs until the
    /// deadline and fails for a structural reason, not timing luck.
    const POST_KILL_GROWTH_DEADLINE: Duration = Duration::from_secs(5);

    /// Owns the heartbeat probe's direct child and process group from the
    /// moment it is spawned, so the whole group -- direct child and the
    /// grandchild it backgrounds -- is always killed and reaped on `Drop`,
    /// including when a startup wait (`await_min_heartbeats`) or an
    /// assertion panics before either test method below runs its own
    /// explicit teardown. `terminate_group`/`kill_direct_child_only` mark
    /// the direct child as already reaped so `Drop` does not attempt to
    /// wait on it twice; `Drop` always retries the group-wide `killpg`
    /// regardless, which is a harmless no-op (`ESRCH`) once the group is
    /// already gone and is exactly what reclaims the negative control's
    /// deliberately-orphaned grandchild afterward.
    struct HeartbeatProbe {
        child: Child,
        pgid: i32,
        reaped: bool,
    }

    impl HeartbeatProbe {
        /// Spawns a direct child that backgrounds a grandchild shell
        /// heart-beating (appending one byte to `heartbeat_path` every
        /// `HEARTBEAT_INTERVAL`) and blocks on `wait` -- mirroring a race
        /// child that spawns its own long-running `cargo`/`hew`
        /// subprocess and waits on it. `own_process_group` repositions
        /// the direct child into a fresh process group before it execs,
        /// so the grandchild it forks afterward inherits that same
        /// group.
        fn spawn(heartbeat_path: &Path) -> Self {
            let mut command = Command::new("sh");
            command.arg("-c").arg(format!(
                "sh -c 'while :; do printf x >> \"{path}\"; sleep {interval}; done' & wait",
                path = heartbeat_path.display(),
                interval = HEARTBEAT_INTERVAL.as_secs_f64(),
            ));
            own_process_group(&mut command);
            let child = command.spawn().expect("spawn heartbeat probe child");
            let pgid = child.id().cast_signed();
            HeartbeatProbe {
                child,
                pgid,
                reaped: false,
            }
        }

        /// Kills the whole process group (direct child and grandchild)
        /// and reaps the direct child -- the action the positive test
        /// proves.
        fn terminate_group(&mut self) -> Result<bool, String> {
            let result = terminate_process_group(&mut self.child, "heartbeat probe");
            self.reaped = true;
            result
        }

        /// Kills only the direct child, deliberately leaving the
        /// grandchild (and thus the process group) running -- the
        /// negative control's setup step. `Drop` still reclaims the
        /// orphaned grandchild's group afterward via `killpg`.
        fn kill_direct_child_only(&mut self) {
            let _ = self.child.kill();
            let _ = self.child.wait();
            self.reaped = true;
        }
    }

    impl Drop for HeartbeatProbe {
        fn drop(&mut self) {
            // Unconditional, best-effort group cleanup: the panic-safety
            // backstop for a startup/assertion panic (nothing terminated
            // yet), the reclaim step for the negative control's
            // deliberately-orphaned grandchild, and a harmless no-op
            // (ESRCH) after an already-successful `terminate_group`.
            // SAFETY: signal-only, no memory access.
            unsafe {
                libc::killpg(self.pgid, libc::SIGKILL);
            }
            if !self.reaped {
                let _ = self.child.wait();
            }
        }
    }

    /// Heartbeat count so far: each beat appends exactly one byte, so the
    /// file's length is the count directly -- no parsing, and no races
    /// beyond ordinary single-writer append semantics.
    fn heartbeat_count(path: &Path) -> u64 {
        fs::metadata(path).map_or(0, |metadata| metadata.len())
    }

    /// Bounded wait (a hang backstop, not the proof) for at least `min`
    /// heartbeats to accumulate.
    fn await_min_heartbeats(path: &Path, min: u64, deadline: Duration) -> u64 {
        let start = Instant::now();
        loop {
            let count = heartbeat_count(path);
            if count >= min {
                return count;
            }
            assert!(
                start.elapsed() < deadline,
                "heartbeat probe never reached {min} beats before the startup deadline"
            );
            std::thread::sleep(Duration::from_millis(5));
        }
    }

    /// Proves the process-group guarantee `libhew_link_race.rs`'s
    /// hardening depends on with a portable, observable side effect
    /// instead of PID liveness probing (which is prone to a just-killed
    /// process briefly remaining a valid `kill(pid, 0)` target as a
    /// zombie pending reap by its new parent): `own_process_group` must
    /// put a grandchild the direct child forks into the *same* group, and
    /// `terminate_process_group` must stop that grandchild too, not just
    /// the direct child -- otherwise a self-fork race child's own
    /// `cargo`/`hew`/linker invocation could outlive a timeout kill and
    /// keep mutating `libhew.a` after the caller believes the process is
    /// gone.
    ///
    /// `direct_child_only_kill_leaves_grandchild_heartbeating` below is
    /// this test's negative control: the identical setup, killed the old
    /// (direct-child-only) way, keeps heart-beating -- proving this test
    /// actually discriminates rather than passing regardless of what the
    /// kill call does.
    #[test]
    fn terminate_process_group_stops_grandchild_heartbeat() {
        let scratch = tempfile::tempdir().expect("create scratch dir");
        let heartbeat_path = scratch.path().join("heartbeat");
        fs::write(&heartbeat_path, b"").expect("create heartbeat file");

        let mut probe = HeartbeatProbe::spawn(&heartbeat_path);
        await_min_heartbeats(
            &heartbeat_path,
            MIN_HEARTBEATS_BEFORE_ACTION,
            HEARTBEAT_STARTUP_DEADLINE,
        );

        probe
            .terminate_group()
            .expect("terminate_process_group should succeed");

        // Let any write already in flight when the signal landed finish
        // before treating a reading as the settled baseline.
        std::thread::sleep(POST_KILL_SETTLE_WINDOW);
        let settled_baseline = heartbeat_count(&heartbeat_path);

        std::thread::sleep(POST_KILL_STABILITY_WINDOW);
        let after_stability_window = heartbeat_count(&heartbeat_path);

        assert_eq!(
            after_stability_window, settled_baseline,
            "heartbeat grew from {settled_baseline} to {after_stability_window} bytes \
             after settling -- the grandchild survived terminate_process_group"
        );
    }

    /// Negative control for the test above: killing only the direct child
    /// (the pre-hardening pattern) must leave the grandchild's process
    /// group untouched, so its heartbeat keeps growing past the pre-kill
    /// count -- proving `terminate_process_group_stops_grandchild_heartbeat`
    /// actually discriminates rather than passing regardless of what the
    /// kill call does. Growth is proven with a bounded poll for that
    /// specific event, not a fixed sleep-then-single-check window, so it
    /// cannot false-fail under scheduler contention.
    #[test]
    fn direct_child_only_kill_leaves_grandchild_heartbeating() {
        let scratch = tempfile::tempdir().expect("create scratch dir");
        let heartbeat_path = scratch.path().join("heartbeat");
        fs::write(&heartbeat_path, b"").expect("create heartbeat file");

        let mut probe = HeartbeatProbe::spawn(&heartbeat_path);
        await_min_heartbeats(
            &heartbeat_path,
            MIN_HEARTBEATS_BEFORE_ACTION,
            HEARTBEAT_STARTUP_DEADLINE,
        );

        probe.kill_direct_child_only();
        let count_at_kill = heartbeat_count(&heartbeat_path);

        let count_after_growth = await_min_heartbeats(
            &heartbeat_path,
            count_at_kill + 1,
            POST_KILL_GROWTH_DEADLINE,
        );

        assert!(
            count_after_growth > count_at_kill,
            "heartbeat did not grow past {count_at_kill} bytes within the bounded \
             deadline after a direct-child-only kill; this negative control should show \
             the grandchild surviving, or the positive test above proves nothing"
        );

        // `probe`'s Drop reclaims the deliberately-orphaned grandchild's
        // process group via killpg.
    }
}

#[derive(Debug)]
struct StdinWriter {
    receiver: Option<Receiver<Result<(), String>>>,
}

impl StdinWriter {
    fn spawn(
        child: &mut Child,
        input: Option<Vec<u8>>,
        label: &str,
    ) -> Result<Self, BoundedExecError> {
        let Some(mut stdin) = child.stdin.take() else {
            return Err(BoundedExecError::failed(label, "child stdin pipe missing"));
        };

        let Some(input) = input else {
            drop(stdin);
            return Ok(Self { receiver: None });
        };

        let (sender, receiver) = mpsc::channel();
        std::thread::spawn(move || {
            let result = match stdin.write_all(&input) {
                Ok(()) => Ok(()),
                Err(error)
                    if matches!(error.kind(), ErrorKind::BrokenPipe | ErrorKind::WriteZero) =>
                {
                    Ok(())
                }
                Err(error) => Err(format!("cannot write child stdin: {error}")),
            };
            drop(stdin);
            let _ = sender.send(result);
        });

        Ok(Self {
            receiver: Some(receiver),
        })
    }

    fn finish_until(
        self,
        deadline: Instant,
        label: &str,
    ) -> Result<Option<String>, BoundedExecError> {
        let Some(receiver) = self.receiver else {
            return Ok(None);
        };
        let Some(remaining) = remaining_until(deadline) else {
            return Ok(Some(
                "child stdin writer did not finish before deadline".to_string(),
            ));
        };
        match receiver.recv_timeout(remaining) {
            Ok(Ok(())) => Ok(None),
            Ok(Err(message)) => Ok(Some(message)),
            Err(RecvTimeoutError::Timeout) => Ok(Some(
                "child stdin writer did not finish before deadline".to_string(),
            )),
            Err(RecvTimeoutError::Disconnected) => Err(BoundedExecError::failed(
                label,
                "child stdin writer exited without reporting status",
            )),
        }
    }
}

fn remaining_until(deadline: Instant) -> Option<Duration> {
    let now = Instant::now();
    (now < deadline).then(|| deadline.duration_since(now))
}

#[cfg(windows)]
mod windows_job {
    use std::os::windows::io::AsRawHandle;
    use std::process::Child;

    type Handle = *mut core::ffi::c_void;
    type Bool = i32;
    type Dword = u32;

    const FALSE: Bool = 0;

    extern "system" {
        fn CreateJobObjectW(
            lp_job_attributes: *mut core::ffi::c_void,
            lp_name: *const u16,
        ) -> Handle;
        fn AssignProcessToJobObject(h_job: Handle, h_process: Handle) -> Bool;
        fn TerminateJobObject(h_job: Handle, u_exit_code: u32) -> Bool;
        fn CloseHandle(h_object: Handle) -> Bool;
        fn GetLastError() -> Dword;
    }

    #[link(name = "ntdll")]
    extern "system" {
        fn NtResumeProcess(process_handle: Handle) -> i32;
    }

    pub(super) fn resume_child_process(child: &Child) -> Result<(), String> {
        let handle = child.as_raw_handle() as Handle;
        // SAFETY: `handle` is valid for the lifetime of `child`.
        let status = unsafe { NtResumeProcess(handle) };
        if status < 0 {
            // SAFETY: GetLastError has no preconditions.
            let code = unsafe { GetLastError() };
            Err(format!(
                "NtResumeProcess failed: NTSTATUS {status:#010x} (last error {code})"
            ))
        } else {
            Ok(())
        }
    }

    #[derive(Debug)]
    pub(super) struct WindowsJob(Handle);

    // SAFETY: The handle remains valid until Drop closes it.
    unsafe impl Send for WindowsJob {}
    // SAFETY: The handle remains valid until Drop closes it.
    unsafe impl Sync for WindowsJob {}

    impl WindowsJob {
        pub(super) fn new() -> Result<Self, String> {
            // SAFETY: null security attributes and a null name are valid.
            let handle = unsafe { CreateJobObjectW(core::ptr::null_mut(), core::ptr::null()) };
            if handle.is_null() {
                // SAFETY: GetLastError has no preconditions.
                let code = unsafe { GetLastError() };
                Err(format!("CreateJobObjectW failed: error {code}"))
            } else {
                Ok(Self(handle))
            }
        }

        pub(super) fn assign(&self, child: &Child) -> Result<(), String> {
            let process_handle = child.as_raw_handle() as Handle;
            // SAFETY: both handles are valid at this point.
            let ok = unsafe { AssignProcessToJobObject(self.0, process_handle) };
            if ok == FALSE {
                // SAFETY: GetLastError has no preconditions.
                let code = unsafe { GetLastError() };
                Err(format!("AssignProcessToJobObject failed: error {code}"))
            } else {
                Ok(())
            }
        }

        pub(super) fn terminate(&self) -> Result<(), String> {
            // SAFETY: self.0 is a live Job Object handle until Drop.
            let ok = unsafe { TerminateJobObject(self.0, 1) };
            if ok == FALSE {
                // SAFETY: GetLastError has no preconditions.
                let code = unsafe { GetLastError() };
                Err(format!("TerminateJobObject failed: error {code}"))
            } else {
                Ok(())
            }
        }
    }

    impl Drop for WindowsJob {
        fn drop(&mut self) {
            // SAFETY: self.0 is a valid handle obtained from CreateJobObjectW.
            unsafe { CloseHandle(self.0) };
        }
    }
}
