//! Shared integration-test helpers.

use fd_lock::RwLock;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::fs::{self, OpenOptions};
use std::io::{ErrorKind, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Output, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::time::{Duration, Instant};

/// Default wall-clock deadline for Hew execution tests.
pub const DEFAULT_EXEC_TIMEOUT: Duration = Duration::from_secs(30);

/// Return the host-native executable path for a compiled Hew program.
#[must_use]
pub fn compiled_binary_path(dir: &Path, name: &str) -> PathBuf {
    dir.join(format!("{name}{}", std::env::consts::EXE_SUFFIX))
}

/// Per-stream capture cap for bounded process output.
///
/// Four MiB is large enough for useful failure diagnostics while preventing a
/// tight infinite-output fixture from growing the test process without bound.
pub const MAX_CAPTURED_BYTES: usize = 4 * 1024 * 1024;

const OUTPUT_READ_CHUNK_BYTES: usize = 8 * 1024;
const POLL_INTERVAL: Duration = Duration::from_millis(10);
// Grace between killing the process tree and waiting for drain threads to see EOF.
const KILL_GRACE: Duration = Duration::from_secs(5);
const SPAWN_RETRY_TIMEOUT: Duration = Duration::from_secs(2);
const SPAWN_RETRY_INTERVAL: Duration = Duration::from_millis(10);
const TEST_NO_BUILD_ENV: &str = "HEW_TEST_NO_BUILD";
const SHARED_TEST_ARTIFACTS: &str = include_str!("../shared-test-artifacts.tsv");

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SharedArtifactKind {
    HostBin,
    HostStaticlib,
    CrossStaticlib,
    WasmStaticlib,
}

#[derive(Clone, Copy, Debug)]
struct SharedArtifactSpec<'a> {
    key: &'a str,
    kind: SharedArtifactKind,
    package: &'a str,
    unix_archive: &'a str,
    windows_archive: &'a str,
    cargo_args: &'a str,
    make_target: &'a str,
}

impl<'a> SharedArtifactSpec<'a> {
    fn archive(self) -> &'a str {
        let archive = if cfg!(windows) {
            self.windows_archive
        } else {
            self.unix_archive
        };
        // Every field is borrowed from the compile-time table.
        archive
    }

    fn cargo_args(self) -> impl Iterator<Item = &'a str> {
        self.cargo_args
            .split_ascii_whitespace()
            .filter(|arg| *arg != "-")
    }
}

fn shared_artifact_specs() -> impl Iterator<Item = Result<SharedArtifactSpec<'static>, String>> {
    SHARED_TEST_ARTIFACTS
        .lines()
        .filter(|line| !line.is_empty() && !line.starts_with('#') && !line.starts_with("gate\t"))
        .map(|line| {
            let fields = line.split('\t').collect::<Vec<_>>();
            if fields.len() != 7 {
                return Err(format!(
                    "shared test artifact row has {} fields instead of 7: {line}",
                    fields.len()
                ));
            }
            let kind = match fields[1] {
                "host-bin" => SharedArtifactKind::HostBin,
                "host-staticlib" => SharedArtifactKind::HostStaticlib,
                "cross-staticlib" => SharedArtifactKind::CrossStaticlib,
                "wasm-staticlib" => SharedArtifactKind::WasmStaticlib,
                other => return Err(format!("unknown shared test artifact kind `{other}`")),
            };
            Ok(SharedArtifactSpec {
                key: fields[0],
                kind,
                package: fields[2],
                unix_archive: fields[3],
                windows_archive: fields[4],
                cargo_args: fields[5],
                make_target: fields[6],
            })
        })
}

fn shared_artifact_spec(
    key: &str,
    expected_kind: SharedArtifactKind,
) -> Result<SharedArtifactSpec<'static>, String> {
    let mut found = None;
    for spec in shared_artifact_specs() {
        let spec = spec?;
        if spec.key == key {
            if found.is_some() {
                return Err(format!("duplicate shared test artifact key `{key}`"));
            }
            found = Some(spec);
        }
    }
    let spec = found.ok_or_else(|| format!("shared test artifact `{key}` is not inventoried"))?;
    if spec.kind != expected_kind {
        return Err(format!(
            "shared test artifact `{key}` has kind {:?}, expected {expected_kind:?}",
            spec.kind
        ));
    }
    if spec.package.is_empty() || spec.archive().is_empty() || spec.make_target.is_empty() {
        return Err(format!(
            "shared test artifact `{key}` has an empty required field"
        ));
    }
    Ok(spec)
}

/// Retry only a temporarily resource-blocked spawn. Once a child exists its
/// exit status is returned unchanged and never triggers another execution.
fn spawn_with_retry(mut spawn: impl FnMut() -> std::io::Result<Child>) -> std::io::Result<Child> {
    let deadline = Instant::now() + SPAWN_RETRY_TIMEOUT;
    loop {
        match spawn() {
            Err(error) if error.kind() == ErrorKind::WouldBlock && Instant::now() < deadline => {
                std::thread::sleep(SPAWN_RETRY_INTERVAL);
            }
            result => return result,
        }
    }
}

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

/// Result of compiling a Hew source while retaining its textual LLVM IR.
#[derive(Debug)]
pub struct CompileWithIr {
    /// Captured compiler output.
    pub output: Output,
    /// Path to the explicitly requested LLVM IR sidecar.
    pub ll_path: PathBuf,
}

/// Run a configured `hew compile` command with textual LLVM IR explicitly emitted.
///
/// The caller configures the source, output directory, and any target-specific
/// arguments before calling this helper. `ll_path` identifies the expected
/// sidecar for the supplied source.
///
/// # Errors
///
/// Returns an error if the compiler process cannot be spawned or awaited.
pub fn compile_with_ir(
    command: &mut Command,
    ll_path: impl Into<PathBuf>,
) -> std::io::Result<CompileWithIr> {
    command.arg("--emit-llvm");
    let output = command.output()?;
    Ok(CompileWithIr {
        output,
        ll_path: ll_path.into(),
    })
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
        .env(TEST_NO_BUILD_ENV, "1")
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

        let child = spawn_with_retry(|| command.spawn()).map_err(|error| {
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
                let child = spawn_with_retry(|| command.spawn()).map_err(|error| {
                    BoundedExecError::failed(label, format!("cannot spawn child: {error}"))
                })?;
                Ok(Self { child, job: None })
            }
            Ok(job) => {
                command.creation_flags(CREATE_SUSPENDED);
                let mut child = spawn_with_retry(|| command.spawn()).map_err(|error| {
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
        let child = spawn_with_retry(|| command.spawn()).map_err(|error| {
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
        if let Some(job) = self.job.as_ref() {
            job.terminate().map_err(|error| {
                BoundedExecError::failed(label, format!("cannot terminate job object: {error}"))
            })?;
            self.child.wait().map_err(|error| {
                BoundedExecError::failed(label, format!("cannot reap child after timeout: {error}"))
            })?;
            Ok(true)
        } else {
            let tree_killed = windows_kill_taskkill(&mut self.child, label)?;
            self.child.wait().map_err(|error| {
                BoundedExecError::failed(label, format!("cannot reap child after timeout: {error}"))
            })?;
            Ok(tree_killed)
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

/// Return the resolved `libhew.a` / `hew.lib`, building it only outside a test
/// run.
///
/// Under nextest, or when `HEW_TEST_NO_BUILD=1`, this is verify-only: the
/// archive and its content certificate must already have been published by the
/// enclosing test gate. Outside a test run, the serialized bootstrap remains
/// available for standalone tools and explicitly invoked proving tests.
///
/// # Errors
///
/// Returns `Err` if the archive is missing or uncertified during a test run, or
/// if an allowed standalone build fails.
pub fn ensure_hew_lib_built() -> Result<PathBuf, String> {
    let spec = shared_artifact_spec("hew-lib", SharedArtifactKind::HostStaticlib)?;
    let repo_root = workspace_root()?;
    let (target_dir, profile) = target_dir_and_profile(&repo_root);
    let lib_path = target_dir.join(&profile).join(spec.archive());
    let debug_dir = target_dir.join(&profile);
    let certificate = debug_dir.join(".hew-libhew-freshness-v1");
    let verify_only = test_run_no_build();
    ensure_built_serialized(
        &target_dir,
        &profile,
        "hew-lib",
        &lib_path,
        || {
            profile != "debug"
                || if verify_only {
                    verify_hew_lib_certificate(&repo_root, &debug_dir)
                } else {
                    certificate.is_file()
                }
        },
        |td, prof| run_cargo_build_hew_lib(&repo_root, td, prof, spec.package),
    )?;
    Ok(lib_path)
}

#[cfg(test)]
fn hew_lib_name() -> &'static str {
    shared_artifact_spec("hew-lib", SharedArtifactKind::HostStaticlib)
        .expect("hew-lib must be present in the shared test artifact table")
        .archive()
}

fn test_run_no_build() -> bool {
    std::env::var_os("NEXTEST_RUN_ID").is_some()
        || std::env::var(TEST_NO_BUILD_ENV).is_ok_and(|value| value == "1")
}

fn verify_hew_lib_certificate(repo_root: &Path, debug_dir: &Path) -> bool {
    let python = std::env::var_os("PYTHON").unwrap_or_else(|| {
        if cfg!(windows) {
            OsString::from("python")
        } else {
            OsString::from("python3")
        }
    });
    Command::new(python)
        .arg(repo_root.join("scripts/libhew-freshness.py"))
        .args(["verify", "--debug-dir"])
        .arg(debug_dir)
        .current_dir(repo_root)
        .output()
        .is_ok_and(|output| output.status.success())
}

/// Verify every concrete shared artifact named by the test artifact table.
///
/// This is the contract counterpart to Make's table-derived builder. Cross
/// archives are concrete only on hosts where the test suite can demand them:
/// both native macOS slices, or the opposite Linux architecture when its
/// multiarch sysroot is installed.
///
/// # Errors
///
/// Returns `Err` if the inventory is malformed, an expected artifact is
/// absent, or the debug host library lacks a valid freshness certificate.
pub fn verify_shared_test_artifacts() -> Result<Vec<PathBuf>, String> {
    let repo_root = workspace_root()?;
    let (target_dir, profile) = target_dir_and_profile(&repo_root);
    let mut verified = Vec::new();

    for spec in shared_artifact_specs() {
        let spec = spec?;
        let paths = match spec.kind {
            SharedArtifactKind::HostBin | SharedArtifactKind::HostStaticlib => {
                vec![target_dir.join(&profile).join(spec.archive())]
            }
            SharedArtifactKind::WasmStaticlib => vec![target_dir
                .join(WASM_TARGET)
                .join(&profile)
                .join(spec.archive())],
            SharedArtifactKind::CrossStaticlib => supported_cross_targets()
                .into_iter()
                .map(|target| target_dir.join(target).join(&profile).join(spec.archive()))
                .collect(),
        };
        for path in paths {
            if !path.is_file() {
                return Err(format!(
                    "shared test artifact `{}` is absent at {} (builder target `{}`)",
                    spec.key,
                    path.display(),
                    spec.make_target
                ));
            }
            if spec.key == "hew-lib"
                && profile == "debug"
                && !verify_hew_lib_certificate(&repo_root, &target_dir.join(&profile))
            {
                return Err(format!(
                    "shared test artifact `{}` is not certified at {}",
                    spec.key,
                    path.display()
                ));
            }
            verified.push(path);
        }
    }

    Ok(verified)
}

fn supported_cross_targets() -> Vec<&'static str> {
    if cfg!(target_os = "macos") {
        let host = if cfg!(target_arch = "aarch64") {
            "aarch64-apple-darwin"
        } else {
            "x86_64-apple-darwin"
        };
        return ["aarch64-apple-darwin", "x86_64-apple-darwin"]
            .into_iter()
            .filter(|target| *target != host)
            .collect();
    }

    #[cfg(target_os = "linux")]
    {
        let environment = if cfg!(target_env = "musl") {
            "musl"
        } else {
            "gnu"
        };
        let (target, sysroot) = match (std::env::consts::ARCH, environment) {
            ("aarch64", "musl") => ("x86_64-unknown-linux-musl", "/usr/x86_64-linux-musl"),
            ("aarch64", _) => ("x86_64-unknown-linux-gnu", "/usr/x86_64-linux-gnu"),
            ("x86_64", "musl") => ("aarch64-unknown-linux-musl", "/usr/aarch64-linux-musl"),
            ("x86_64", _) => ("aarch64-unknown-linux-gnu", "/usr/aarch64-linux-gnu"),
            _ => return Vec::new(),
        };
        if Path::new(sysroot).is_dir() {
            return vec![target];
        }
    }

    Vec::new()
}

/// Return the resolved `hew` binary path, building it only outside a test run.
///
/// This mirrors [`ensure_hew_lib_built`] for the compiler-driver binary that the
/// `*_exec` integration tests invoke. Without it, a cold `target/` lets the
/// per-test `hew_command` fall back to a `cargo run` INSIDE the bounded
/// deadline; if a concurrent `cargo`/`nextest` invocation holds the build-lock,
/// that fallback blocks on `Blocking waiting for file lock on build directory`
/// and burns the whole budget, producing a false timeout (hew-lang/hew#1887).
/// The enclosing test gate builds the binary before starting the runner. The
/// serialized bootstrap remains available to standalone callers.
///
/// # Errors
///
/// Returns `Err` if a test run finds the binary absent, or if an allowed
/// standalone build fails.
pub fn ensure_hew_bin_built() -> Result<PathBuf, String> {
    let spec = shared_artifact_spec("hew-bin", SharedArtifactKind::HostBin)?;
    let repo_root = workspace_root()?;
    let (target_dir, profile) = target_dir_and_profile(&repo_root);
    let bin_path = target_dir.join(&profile).join(spec.archive());
    ensure_built_serialized(
        &target_dir,
        &profile,
        "hew-bin",
        &bin_path,
        || true,
        |td, prof| run_cargo_build_hew_bin(&repo_root, td, prof, spec),
    )?;
    Ok(bin_path)
}

/// Return a cross-compiled `target/<target>/<profile>/libhew.a`, building it
/// only outside a test run.
///
/// The cross-target archive is as much a shared-target artifact as the host
/// one: every nextest process linking a cross-target fixture reads it while any
/// unserialized sibling could be inside Cargo's non-atomic uplift window.
///
/// `still_current` is consulted in addition to artifact presence. A test gate
/// must prebuild an archive that passes this check; a standalone caller may
/// rebuild a stale archive under the shared lock.
///
/// # Errors
///
/// Returns `Err` if a test run finds the archive absent or stale, or if an
/// allowed standalone build fails.
pub fn ensure_hew_lib_built_for_target(
    target: &str,
    still_current: impl Fn(&Path) -> bool,
) -> Result<PathBuf, String> {
    let spec = shared_artifact_spec("hew-lib-cross", SharedArtifactKind::CrossStaticlib)?;
    let repo_root = workspace_root()?;
    let (target_dir, profile) = target_dir_and_profile(&repo_root);
    let lib_path = target_dir.join(target).join(&profile).join(spec.archive());
    let artifact = lib_path.clone();
    ensure_built_serialized(
        &target_dir,
        &profile,
        &format!("hew-lib-{target}"),
        &lib_path,
        || still_current(&artifact),
        |td, prof| run_cargo_build_cross_target_hew_lib(&repo_root, td, prof, target, spec.package),
    )?;
    Ok(lib_path)
}

/// Return the resolved native `hew-runtime` static library, building it only
/// outside a test run.
///
/// `hew-lib` has its own authority ([`ensure_hew_lib_built`]) because it also
/// publishes a freshness certificate. This covers the other shared-target
/// staticlib, which link tests read while a sibling process could be inside
/// Cargo's non-atomic uplift of the same file.
///
/// Test runs are verify-only: their gate must build this archive before
/// starting the test runner. Outside a test run, callers retain the serialized
/// bootstrap as a convenience for focused development commands.
///
/// # Errors
///
/// Returns `Err` if a test run finds the archive absent, if the lock cannot be
/// taken outside a test run, if the build fails or cannot be spawned, or if the
/// archive is absent after a successful build.
pub fn ensure_host_runtime_built() -> Result<PathBuf, String> {
    let spec = shared_artifact_spec("hew-runtime", SharedArtifactKind::HostStaticlib)?;
    let repo_root = workspace_root()?;
    let (target_dir, profile) = target_dir_and_profile(&repo_root);
    let archive_path = target_dir.join(&profile).join(spec.archive());
    ensure_built_serialized(
        &target_dir,
        &profile,
        "host-hew-runtime",
        &archive_path,
        || true,
        |td, prof| {
            let mut cmd = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
            cmd.args(["build", "-q", "-p", spec.package])
                .args(spec.cargo_args())
                .args(profile_args(prof))
                .env("CARGO_TARGET_DIR", td)
                .current_dir(&repo_root);
            let out = cmd
                .output()
                .map_err(|e| format!("spawn cargo build -p {}: {e}", spec.package))?;
            if !out.status.success() {
                return Err(describe(&format!("cargo build -p {}", spec.package), &out));
            }
            Ok(())
        },
    )?;
    Ok(archive_path)
}

/// Resolve an inventoried wasm32-wasip1 staticlib, building it only outside a
/// test run.
///
/// Same hazard, same spine as [`ensure_hew_lib_built`]: `wasm-ld` reads these
/// archives while a sibling process could be rewriting them. Test runs are
/// verify-only and require the gate to prebuild the archive. Outside a test
/// run, Cargo can also exit 0 without producing the staticlib when a cached
/// rlib leaves a stale fingerprint, so a `cargo clean -p <package> --target
/// wasm32-wasip1` and one retry happen inside the lock.
///
/// # Errors
///
/// Returns `Err` if the lock cannot be taken, if the build (or its clean and
/// retry) fails or cannot be spawned, or if the archive is still absent
/// afterwards.
fn ensure_wasm_staticlib_built(key: &str) -> Result<PathBuf, String> {
    let spec = shared_artifact_spec(key, SharedArtifactKind::WasmStaticlib)?;
    let repo_root = workspace_root()?;
    let (target_dir, profile) = target_dir_and_profile(&repo_root);
    let archive_path = target_dir
        .join(WASM_TARGET)
        .join(&profile)
        .join(spec.archive());
    let archive_path_for_build = archive_path.clone();
    ensure_built_serialized(
        &target_dir,
        &profile,
        &format!("wasm-{}", spec.package),
        &archive_path,
        || true,
        |td, prof| {
            run_cargo_build_wasm_staticlib(
                &repo_root,
                td,
                prof,
                spec.package,
                &spec.cargo_args().collect::<Vec<_>>(),
                &archive_path_for_build,
            )
        },
    )?;
    Ok(archive_path)
}

/// Return the prebuilt wasm32-wasip1 runtime archive.
///
/// # Errors
///
/// Returns `Err` when the shared artifact is absent during a test run, or when
/// an allowed standalone build fails.
pub fn ensure_wasm_runtime_built() -> Result<PathBuf, String> {
    ensure_wasm_staticlib_built("hew-runtime-wasi")
}

/// Return the prebuilt wasm32-wasip1 standard-library archive.
///
/// # Errors
///
/// Returns `Err` when the shared artifact is absent during a test run, or when
/// an allowed standalone build fails.
pub fn ensure_wasm_std_built() -> Result<PathBuf, String> {
    ensure_wasm_staticlib_built("hew-std-wasi")
}

/// Build a throwaway fixture crate into a target directory it owns outright.
///
/// This is the only sanctioned test-time `cargo build` that is NOT serialized,
/// because it is not a writer of the workspace target directory at all. That is
/// enforced rather than documented: a `target_dir` inside the workspace target
/// directory is rejected, so this authority cannot be repurposed into a second
/// unlocked writer of the shared artifacts.
///
/// # Errors
///
/// Returns `Err` if `target_dir` is not private to the caller, or if `cargo`
/// cannot be spawned. A build that runs and fails is returned as `Ok` with a
/// non-success status so the caller can attach its own fixture diagnostics.
pub fn cargo_build_isolated(
    manifest_path: &Path,
    target_dir: &Path,
    extra_args: &[&str],
) -> Result<Output, String> {
    let repo_root = workspace_root()?;
    let (shared_target, _) = target_dir_and_profile(&repo_root);
    let resolved = target_dir
        .canonicalize()
        .unwrap_or_else(|_| target_dir.to_path_buf());
    let shared = shared_target
        .canonicalize()
        .unwrap_or_else(|_| shared_target.clone());
    if resolved.starts_with(&shared) {
        return Err(format!(
            "cargo_build_isolated requires a private target directory; {} is inside the \
             shared workspace target {} -- use ensure_hew_lib_built / \
             ensure_hew_lib_built_for_target / an inventoried WASI helper instead",
            target_dir.display(),
            shared_target.display()
        ));
    }
    let mut cmd = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
    cmd.arg("build")
        .args(extra_args)
        .arg("--manifest-path")
        .arg(manifest_path)
        .env("CARGO_TARGET_DIR", target_dir);
    if let Some(dir) = manifest_path.parent() {
        cmd.current_dir(dir);
    }
    cmd.output().map_err(|e| {
        format!(
            "spawn cargo build --manifest-path {}: {e}",
            manifest_path.display()
        )
    })
}

const WASM_TARGET: &str = "wasm32-wasip1";

/// Cargo command for wasm32-wasip1 builds with coverage instrumentation
/// scrubbed.
///
/// Under `cargo llvm-cov`, test processes inherit the instrumentation
/// environment -- a `RUSTC_WRAPPER` shim driven by `__CARGO_LLVM_COV_*` vars
/// and/or `-C instrument-coverage` in `RUSTFLAGS`. wasm32-wasip1 ships no
/// profiler runtime, so an inherited instrumented build fails with "can't find
/// crate for `profiler_builtins`". These archives execute under wasmtime and
/// contribute no host coverage, so the instrumentation environment is dropped
/// rather than honoured.
fn wasm_cargo_command() -> Command {
    let mut command = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
    for var in [
        "RUSTFLAGS",
        "CARGO_ENCODED_RUSTFLAGS",
        "CARGO_BUILD_RUSTFLAGS",
        "LLVM_PROFILE_FILE",
        "RUSTC_WRAPPER",
        "__CARGO_LLVM_COV_RUSTC_WRAPPER",
        "__CARGO_LLVM_COV_RUSTC_WRAPPER_RUSTFLAGS",
        "__CARGO_LLVM_COV_RUSTC_WRAPPER_CRATE_NAMES",
        "__CARGO_LLVM_COV_RUSTC_WRAPPER_PRE_EXISTING",
        "CARGO_LLVM_COV",
    ] {
        command.env_remove(var);
    }
    // Explicitly override any user-level Cargo `build.rustc-wrapper` setting.
    // Merely removing the environment variable lets Cargo fall back to that
    // config (for example `sccache`), making the hermetic bootstrap depend on a
    // developer-local daemon that may be unavailable in CI sandboxes.
    command.env("RUSTC_WRAPPER", "");
    command
}

fn profile_args(profile: &str) -> Vec<OsString> {
    match profile {
        // dev/test both land in target/debug
        "debug" => Vec::new(),
        "release" => vec![OsString::from("--release")],
        other => vec![OsString::from("--profile"), OsString::from(other)],
    }
}

fn describe(label: &str, out: &Output) -> String {
    format!(
        "{label} failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

fn run_cargo_build_cross_target_hew_lib(
    repo_root: &Path,
    target_dir: &Path,
    profile: &str,
    target: &str,
    package: &str,
) -> Result<(), String> {
    let mut cmd = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
    cmd.args(["build", "-q", "-p", package, "--target", target])
        .args(profile_args(profile))
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root);
    let out = cmd
        .output()
        .map_err(|e| format!("spawn cargo build -p hew-lib --target {target}: {e}"))?;
    if !out.status.success() {
        return Err(describe(
            &format!("cargo build -p hew-lib --target {target}"),
            &out,
        ));
    }
    Ok(())
}

fn run_cargo_build_wasm_staticlib(
    repo_root: &Path,
    target_dir: &Path,
    profile: &str,
    package: &str,
    extra_cargo_args: &[&str],
    archive_path: &Path,
) -> Result<(), String> {
    let build = |cmd: &mut Command| -> Result<(), String> {
        cmd.args(["build", "-q", "-p", package, "--target", WASM_TARGET])
            .args(extra_cargo_args)
            .args(profile_args(profile))
            .env("CARGO_TARGET_DIR", target_dir)
            .current_dir(repo_root);
        let out = cmd
            .output()
            .map_err(|e| format!("spawn cargo build -p {package} --target {WASM_TARGET}: {e}"))?;
        if !out.status.success() {
            return Err(describe(
                &format!("cargo build -p {package} --target {WASM_TARGET}"),
                &out,
            ));
        }
        Ok(())
    };
    build(&mut wasm_cargo_command())?;
    if archive_path.is_file() {
        return Ok(());
    }

    // Cargo can exit 0 without producing the staticlib when a stale fingerprint
    // (for example a CI cache hit that only cached the rlib) convinces it that
    // nothing needs rebuilding. Clean this package's wasm artifacts and retry
    // once, still under the caller's lock. `ensure_built_serialized` reports
    // the artifact's continued absence if the retry does not produce it.
    let clean = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
        .args(["clean", "-q", "-p", package, "--target", WASM_TARGET])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root)
        .output()
        .map_err(|e| format!("spawn cargo clean -p {package} --target {WASM_TARGET}: {e}"))?;
    if !clean.status.success() {
        return Err(describe(
            &format!("cargo clean -p {package} --target {WASM_TARGET}"),
            &clean,
        ));
    }
    build(&mut wasm_cargo_command())
}

fn workspace_root() -> Result<PathBuf, String> {
    Ok(
        Path::new(env!("CARGO_MANIFEST_DIR")) // = <repo>/hew-testutil
            .parent()
            .ok_or("hew-testutil must live under the workspace root")?
            .to_path_buf(),
    )
}

#[cfg(test)]
fn hew_bin_name() -> &'static str {
    if cfg!(windows) {
        "hew.exe"
    } else {
        "hew"
    }
}

fn run_cargo_build_hew_bin(
    repo_root: &Path,
    target_dir: &Path,
    profile: &str,
    spec: SharedArtifactSpec<'_>,
) -> Result<(), String> {
    let mut cmd = Command::new(std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into()));
    cmd.args(["build", "-q", "-p", spec.package])
        .args(spec.cargo_args())
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
        let dep = std::env::var("MACOSX_DEPLOYMENT_TARGET")
            .ok()
            .filter(|v| !v.is_empty())
            .unwrap_or_else(|| "13.0".to_string());
        cmd.env("MACOSX_DEPLOYMENT_TARGET", dep);
    }
    let out = cmd
        .output()
        .map_err(|e| format!("spawn cargo build -p hew-cli --bin hew: {e}"))?;
    if !out.status.success() {
        return Err(format!(
            "cargo build -p hew-cli --bin hew failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ));
    }
    Ok(())
}

/// Testable core: `build_fn` is injected so the unit test can stub `cargo`.
/// `extra_fresh` lets the debug archive require its companion certificate
/// without making binary and release-profile bootstraps pay that authority.
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
    key: &str,
    artifact: &Path,
    extra_fresh: impl Fn() -> bool,
    build_fn: impl FnOnce(&Path, &str) -> Result<(), String>,
) -> Result<(), String> {
    ensure_built_serialized_inner(
        target_dir,
        profile,
        key,
        artifact,
        extra_fresh,
        build_fn,
        test_run_no_build(),
    )
}

fn ensure_built_serialized_inner(
    target_dir: &Path,
    profile: &str,
    key: &str,
    artifact: &Path,
    extra_fresh: impl Fn() -> bool,
    build_fn: impl FnOnce(&Path, &str) -> Result<(), String>,
    verify_only: bool,
) -> Result<(), String> {
    if verify_only {
        if artifact.is_file() && extra_fresh() {
            return Ok(());
        }
        return Err(format!(
            "test run requires a present, certified shared artifact at {}; run `make stdlib` before the test gate",
            artifact.display()
        ));
    }
    fs::create_dir_all(target_dir).map_err(|e| format!("mkdir {}: {e}", target_dir.display()))?;
    let run_id =
        std::env::var("NEXTEST_RUN_ID").unwrap_or_else(|_| format!("pid:{}", std::process::id()));
    // One lock file for every shared-target bootstrap: whichever artifact a
    // participant found missing, it serializes against all the others, so no
    // two `cargo` invocations can overlap in this target directory.
    let lock_path = target_dir.join("hew-lib-bootstrap.lock");
    // The stamp is per ARTIFACT, not per target directory. A single
    // `hew-lib-bootstrap-{profile}.stamp` shared by every caller let the first
    // bootstrap of a run certify artifacts it never built: `hew` stamping the
    // run made a stale-but-present `libhew.a` read as fresh, and the freshness
    // certificate is only checked for presence, so the staleness survived. Each
    // artifact now proves its own build.
    let stamp_path = target_dir.join(format!(".hew-bootstrap-{key}-{profile}.stamp"));
    let fresh = || {
        fs::read_to_string(&stamp_path).is_ok_and(|s| s == run_id)
            && artifact.is_file()
            && extra_fresh()
    };
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

fn target_dir_and_profile(workspace_root: &Path) -> (PathBuf, String) {
    let current_exe = std::env::current_exe().ok();
    let cargo_target_dir = std::env::var_os("CARGO_TARGET_DIR");
    target_dir_and_profile_from_sources(
        current_exe.as_deref(),
        cargo_target_dir.as_deref(),
        workspace_root,
        cfg!(debug_assertions),
    )
}

fn target_dir_and_profile_from_sources(
    current_exe: Option<&Path>,
    cargo_target_dir: Option<&OsStr>,
    workspace_root: &Path,
    debug_assertions: bool,
) -> (PathBuf, String) {
    // <target>/<profile>/deps/<bin>
    if let Some(authority) = current_exe.and_then(target_dir_and_profile_from_exe) {
        return authority;
    }
    let target = cargo_target_dir.map_or_else(
        || workspace_root.join("target"),
        |configured| {
            let configured = Path::new(configured);
            if configured.is_absolute() {
                configured.to_path_buf()
            } else {
                workspace_root.join(configured)
            }
        },
    );
    (
        target,
        if debug_assertions { "debug" } else { "release" }.to_string(),
    )
}

fn target_dir_and_profile_from_exe(exe: &Path) -> Option<(PathBuf, String)> {
    let deps_dir = exe.parent()?;
    if deps_dir.file_name()? != OsStr::new("deps") {
        return None;
    }
    let profile_dir = deps_dir.parent()?;
    let profile = profile_dir.file_name()?.to_str()?.to_string();
    let target_dir = profile_dir.parent()?.to_path_buf();
    Some((target_dir, profile))
}

fn run_cargo_build_hew_lib(
    repo_root: &Path,
    target_dir: &Path,
    profile: &str,
    package: &str,
) -> Result<(), String> {
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut cargo_args = vec![
        OsString::from("build"),
        OsString::from("-q"),
        OsString::from("-p"),
        OsString::from(package),
    ];
    match profile {
        // dev/test both land in target/debug
        "debug" => {}
        "release" => {
            cargo_args.push(OsString::from("--release"));
        }
        other => {
            cargo_args.extend([OsString::from("--profile"), OsString::from(other)]);
            // e.g. CI release-lib
        }
    }

    // The debug archive is consumed by the Make freshness gate.  Publish its
    // certificate under the same fd_lock that serializes the Cargo uplift, so
    // test helpers cannot leave a freshly built archive uncoupled from that
    // gate.  Keep non-debug profiles on their own artifact authority.
    let mut cmd = if profile == "debug" {
        let python = std::env::var_os("PYTHON").unwrap_or_else(|| {
            if cfg!(windows) {
                OsString::from("python")
            } else {
                OsString::from("python3")
            }
        });
        let mut helper = Command::new(python);
        helper
            .arg(repo_root.join("scripts/libhew-freshness.py"))
            .args(["build", "--debug-dir"])
            .arg(target_dir.join(profile))
            .arg("--")
            .arg(cargo)
            .args(&cargo_args);
        helper
    } else {
        let mut cargo_command = Command::new(cargo);
        cargo_command.args(&cargo_args);
        cargo_command
    };
    cmd.env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root);
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

    #[test]
    fn target_authority_follows_normal_target_layout() {
        let target = PathBuf::from("workspace").join("target");
        let exe = target.join("debug").join("deps").join("e2e-test");
        assert_eq!(
            target_dir_and_profile_from_exe(&exe),
            Some((target, "debug".to_string()))
        );
    }

    #[test]
    fn target_authority_follows_alternate_target_layout() {
        let target = PathBuf::from("workspace")
            .join("target")
            .join("llvm-cov-target");
        let exe = target.join("debug").join("deps").join("e2e-test");
        assert_eq!(
            target_dir_and_profile_from_exe(&exe),
            Some((target, "debug".to_string()))
        );
    }

    #[test]
    fn target_authority_preserves_non_debug_profile() {
        let target = PathBuf::from("workspace").join("target");
        let exe = target.join("release-lib").join("deps").join("e2e-test");
        assert_eq!(
            target_dir_and_profile_from_exe(&exe),
            Some((target, "release-lib".to_string()))
        );
    }

    #[test]
    fn target_authority_rejects_layout_without_deps_component() {
        let exe = PathBuf::from("workspace")
            .join("target")
            .join("debug")
            .join("e2e-test");
        assert_eq!(target_dir_and_profile_from_exe(&exe), None);
    }

    #[test]
    fn target_authority_rejects_layout_with_wrong_immediate_parent() {
        let exe = PathBuf::from("workspace")
            .join("target")
            .join("debug")
            .join("artifacts")
            .join("e2e-test");
        assert_eq!(target_dir_and_profile_from_exe(&exe), None);
    }

    #[test]
    fn malformed_exe_fallback_anchors_relative_target_for_all_artifacts() {
        const CHILD_MARKER: &str = "HEW_TESTUTIL_RELATIVE_TARGET_CHILD";
        const WORKSPACE_ROOT: &str = "HEW_TESTUTIL_WORKSPACE_ROOT";

        if std::env::var_os(CHILD_MARKER).is_some() {
            let workspace_root = PathBuf::from(
                std::env::var_os(WORKSPACE_ROOT).expect("child workspace root must be set"),
            );
            let alternate_cwd = std::env::current_dir().expect("read child working directory");
            let malformed_exe = alternate_cwd.join("bin").join("e2e-test");
            let configured_target = Path::new("coverage-target");

            let (target_dir, profile) = target_dir_and_profile_from_sources(
                Some(&malformed_exe),
                std::env::var_os("CARGO_TARGET_DIR").as_deref(),
                &workspace_root,
                false,
            );

            assert_eq!(target_dir, workspace_root.join(configured_target));
            assert_ne!(target_dir, alternate_cwd.join(configured_target));
            assert_eq!(profile, "release");
            assert_eq!(
                target_dir.join(&profile).join(hew_bin_name()),
                workspace_root
                    .join(configured_target)
                    .join("release")
                    .join(hew_bin_name())
            );
            assert_eq!(
                target_dir.join(&profile).join(hew_lib_name()),
                workspace_root
                    .join(configured_target)
                    .join("release")
                    .join(hew_lib_name())
            );
            return;
        }

        let temp = tempfile::tempdir().expect("create tempdir");
        let workspace_root = temp.path().join("workspace");
        let alternate_cwd = temp.path().join("alternate-caller");
        fs::create_dir_all(&workspace_root).expect("create workspace root");
        fs::create_dir_all(&alternate_cwd).expect("create alternate working directory");

        let output = Command::new(std::env::current_exe().expect("resolve test executable"))
            .args([
                "--exact",
                "hew_lib_bootstrap_tests::malformed_exe_fallback_anchors_relative_target_for_all_artifacts",
                "--nocapture",
            ])
            .current_dir(&alternate_cwd)
            .env(CHILD_MARKER, "1")
            .env(WORKSPACE_ROOT, &workspace_root)
            .env("CARGO_TARGET_DIR", "coverage-target")
            .output()
            .expect("run target-authority child from alternate working directory");
        assert!(
            output.status.success(),
            "alternate-CWD target-authority child failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[test]
    fn malformed_exe_fallback_preserves_absolute_target() {
        let temp = tempfile::tempdir().expect("create tempdir");
        let workspace_root = temp.path().join("workspace");
        let absolute_target = temp.path().join("absolute-target");
        let malformed_exe = temp.path().join("e2e-test");

        let (target_dir, profile) = target_dir_and_profile_from_sources(
            Some(&malformed_exe),
            Some(absolute_target.as_os_str()),
            &workspace_root,
            true,
        );

        assert_eq!(target_dir, absolute_target);
        assert_eq!(profile, "debug");
    }

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
                        ensure_built_serialized_inner(
                            target_dir,
                            "debug",
                            "stub",
                            artifact,
                            || true,
                            |_td, _prof| {
                                build_count.fetch_add(1, Ordering::SeqCst);
                                fs::write(artifact, b"stub archive")
                                    .map_err(|e| format!("write stub artifact: {e}"))
                            },
                            false,
                        )
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

    /// The `hew` binary bootstrap (hew-lang/hew#1887) shares the exact
    /// serialization spine as the library bootstrap: concurrent callers racing
    /// a bin-named artifact must still build exactly once, and every later
    /// caller must take the stamped fast path rather than re-running the build
    /// inside a per-test deadline. Guarding this with its own artifact name
    /// keeps the bin path from silently regressing to a per-call rebuild if the
    /// stamp/lock wiring is ever changed.
    #[test]
    fn concurrent_bin_callers_build_exactly_once() {
        let dir = tempfile::tempdir().expect("create tempdir");
        let target_dir = dir.path().to_path_buf();
        let artifact = target_dir.join(hew_bin_name());
        let build_count = AtomicUsize::new(0);

        std::thread::scope(|scope| {
            let handles: Vec<_> = (0..8)
                .map(|_| {
                    let target_dir = &target_dir;
                    let artifact = &artifact;
                    let build_count = &build_count;
                    scope.spawn(move || {
                        ensure_built_serialized_inner(
                            target_dir,
                            "debug",
                            "stub",
                            artifact,
                            || true,
                            |_td, _prof| {
                                build_count.fetch_add(1, Ordering::SeqCst);
                                fs::write(artifact, b"stub hew binary")
                                    .map_err(|e| format!("write stub binary: {e}"))
                            },
                            false,
                        )
                    })
                })
                .collect();
            for handle in handles {
                handle
                    .join()
                    .expect("bin bootstrap thread should not panic")
                    .expect("bin bootstrap thread should succeed");
            }
        });

        assert_eq!(
            build_count.load(Ordering::SeqCst),
            1,
            "bin build stub should run exactly once across concurrent callers"
        );
        assert!(artifact.is_file(), "stub bin artifact should be present");

        // A fresh caller after the winner stamped must not rebuild.
        ensure_built_serialized_inner(
            &target_dir,
            "debug",
            "stub",
            &artifact,
            || true,
            |_td, _prof| {
                build_count.fetch_add(1, Ordering::SeqCst);
                Ok(())
            },
            false,
        )
        .expect("post-stamp caller should short-circuit");
        assert_eq!(
            build_count.load(Ordering::SeqCst),
            1,
            "a caller after the stamp must take the fast path, not rebuild"
        );
    }

    /// Bootstrapping one artifact must not certify a different one. A single
    /// per-target-directory stamp meant `hew`'s bootstrap stamped the run and a
    /// stale-but-present `libhew.a` then read as fresh, so the archive under
    /// test was never rebuilt. Each artifact proves its own build.
    #[test]
    fn one_artifact_bootstrap_does_not_certify_another() {
        let dir = tempfile::tempdir().expect("create tempdir");
        let target_dir = dir.path().to_path_buf();
        let bin = target_dir.join(hew_bin_name());
        let lib = target_dir.join(hew_lib_name());
        // The library artifact exists but is stale, exactly as it would be
        // after a previous run left it behind.
        fs::write(&lib, b"stale archive").expect("seed stale artifact");

        ensure_built_serialized_inner(
            &target_dir,
            "debug",
            "hew-bin",
            &bin,
            || true,
            |_, _| fs::write(&bin, b"fresh binary").map_err(|e| format!("write stub binary: {e}")),
            false,
        )
        .expect("bin bootstrap should succeed");

        let rebuilt = AtomicUsize::new(0);
        ensure_built_serialized_inner(
            &target_dir,
            "debug",
            "hew-lib",
            &lib,
            || true,
            |_, _| {
                rebuilt.fetch_add(1, Ordering::SeqCst);
                fs::write(&lib, b"fresh archive").map_err(|e| format!("write stub archive: {e}"))
            },
            false,
        )
        .expect("lib bootstrap should succeed");

        assert_eq!(
            rebuilt.load(Ordering::SeqCst),
            1,
            "the library must build on its own stamp, not inherit the binary's"
        );
        assert_eq!(
            fs::read(&lib).expect("read artifact"),
            b"fresh archive",
            "the stale artifact must have been replaced"
        );
    }

    #[test]
    fn test_run_requires_a_prebuilt_fresh_artifact() {
        let dir = tempfile::tempdir().expect("create tempdir");
        let artifact = dir.path().join("libhew-stub.a");
        let build_count = AtomicUsize::new(0);

        let missing = ensure_built_serialized_inner(
            dir.path(),
            "debug",
            "stub",
            &artifact,
            || true,
            |_, _| {
                build_count.fetch_add(1, Ordering::SeqCst);
                Ok(())
            },
            true,
        )
        .expect_err("a test run must not build a missing artifact");
        assert!(missing.contains("run `make stdlib`"), "error: {missing}");
        assert_eq!(build_count.load(Ordering::SeqCst), 0);

        fs::write(&artifact, b"prebuilt").expect("seed prebuilt artifact");
        let stale = ensure_built_serialized_inner(
            dir.path(),
            "debug",
            "stub",
            &artifact,
            || false,
            |_, _| {
                build_count.fetch_add(1, Ordering::SeqCst);
                Ok(())
            },
            true,
        )
        .expect_err("a test run must not accept an uncertified artifact");
        assert!(stale.contains("run `make stdlib`"), "error: {stale}");
        assert_eq!(build_count.load(Ordering::SeqCst), 0);

        ensure_built_serialized_inner(
            dir.path(),
            "debug",
            "stub",
            &artifact,
            || true,
            |_, _| {
                build_count.fetch_add(1, Ordering::SeqCst);
                Ok(())
            },
            true,
        )
        .expect("a prebuilt certified artifact should be accepted");
        assert_eq!(build_count.load(Ordering::SeqCst), 0);
    }
}

#[cfg(test)]
#[cfg(unix)]
mod tests {
    use super::*;
    use std::cell::Cell;

    #[test]
    fn compiled_binary_path_uses_host_executable_suffix() {
        assert_eq!(
            compiled_binary_path(Path::new("output"), "program"),
            PathBuf::from(format!("output/program{}", std::env::consts::EXE_SUFFIX))
        );
    }

    #[test]
    fn transient_spawn_pressure_is_retried_before_child_execution() {
        let attempts = Cell::new(0);
        let mut command = Command::new("true");
        let mut child = spawn_with_retry(|| {
            let attempt = attempts.get();
            attempts.set(attempt + 1);
            if attempt < 2 {
                Err(std::io::Error::from(ErrorKind::WouldBlock))
            } else {
                command.spawn()
            }
        })
        .expect("transient spawn pressure should recover");
        assert!(child.wait().expect("reap true child").success());
        assert_eq!(attempts.get(), 3);
    }

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

    /// Lifecycle state for [`HeartbeatProbe`]'s direct child and process
    /// group, tracked explicitly so `Drop` never signals a pgid number
    /// after it could plausibly have been recycled by the kernel for an
    /// unrelated process group.
    ///
    /// A process group's numeric pgid cannot be reassigned to a new
    /// process while *any* process still references it as its own
    /// group -- so signaling `pgid` is always safe in `Running` (nothing
    /// has died) and in `LeaderKilledUnreaped` (the leader is a
    /// still-unreaped zombie, and/or the grandchild is still alive, both
    /// of which hold the reservation). It stops being safe the instant
    /// the group is fully empty: leader reaped *and* grandchild reaped,
    /// which is exactly the state `GroupTerminated` records -- from that
    /// point on the kernel is free to recycle the number for a completely
    /// unrelated process group, so `Drop` must not signal it again.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum ProbeState {
        /// Nothing has been signaled yet.
        Running,
        /// `terminate_group` confirmed the whole group gone (`killpg`
        /// itself succeeded, or failed with `ESRCH` -- already empty)
        /// *and* reaped the leader. The group is fully gone; its pgid
        /// number is no longer this probe's to signal.
        ///
        /// This is deliberately never reached by a partial ("leader-only")
        /// kill: unlike the general-purpose [`terminate_process_group`]
        /// helper, this probe has no child-only fallback, because a
        /// fallback kill leaves the grandchild's fate unconfirmed and
        /// `GroupTerminated` must mean the grandchild is provably gone
        /// too, not just "we gave up trying to kill the whole group".
        GroupTerminated,
        /// The negative control's setup: the leader was killed directly
        /// but deliberately left unreaped so its PID slot -- and thus the
        /// group's pgid, which the grandchild also still references --
        /// stays reserved while the test observes the grandchild's
        /// heartbeat.
        LeaderKilledUnreaped,
    }

    /// Whether `Drop` still needs to signal the group and reap the
    /// leader for a given lifecycle state. Pulled out as a pure function
    /// of the state alone so the invariant -- "signal in every state
    /// except a confirmed complete termination" -- is directly testable
    /// without spawning any process.
    fn probe_drop_needs_cleanup(state: ProbeState) -> bool {
        !matches!(state, ProbeState::GroupTerminated)
    }

    /// Classifies a raw `killpg` result against the one distinction that
    /// matters for [`HeartbeatProbe::terminate_group`]: did this call
    /// confirm the group is gone, or might it still be alive? Takes the
    /// syscall's return value and captured `errno` as plain parameters
    /// (rather than reading `io::Error::last_os_error()` itself) so the
    /// decision is a pure function directly testable against an injected
    /// failure, without depending on global errno state at the moment the
    /// test runs.
    ///
    /// `Ok(())` means confirmed gone (the call succeeded, or failed with
    /// `ESRCH` because the group was already empty). Any other failure is
    /// `Err` -- the group, and thus a still-heart-beating grandchild, may
    /// well be alive, so the caller must not treat this as termination.
    fn classify_killpg_result(result: i32, pgid: i32, os_error: Option<i32>) -> Result<(), String> {
        if result == 0 {
            return Ok(());
        }
        if os_error == Some(libc::ESRCH) {
            Ok(())
        } else {
            Err(format!(
                "cannot kill process group {pgid}: os error {os_error:?}"
            ))
        }
    }

    /// Owns the heartbeat probe's direct child (the process group leader)
    /// and process group from the moment it is spawned, so the whole
    /// group -- leader and the grandchild it backgrounds -- is always
    /// killed and reaped on `Drop`, including when a startup wait
    /// (`await_min_heartbeats`) or an assertion panics before either test
    /// method below runs its own explicit teardown. `Drop` consults
    /// [`ProbeState`] rather than unconditionally re-signaling, because
    /// once the group has been confirmed fully torn down its pgid number
    /// may already have been recycled by the kernel for an unrelated
    /// process.
    struct HeartbeatProbe {
        child: Child,
        pgid: i32,
        state: ProbeState,
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
                state: ProbeState::Running,
            }
        }

        /// Kills the whole process group (leader and grandchild) and
        /// reaps the leader -- the action the positive test proves.
        /// Deliberately does not call the crate's general-purpose
        /// [`terminate_process_group`] helper: that helper falls back to
        /// a leader-only kill (and still reaps the leader) when `killpg`
        /// fails for a real reason, which would let this probe declare
        /// `GroupTerminated` -- and so tell `Drop` to stand down -- while
        /// the grandchild might still be heart-beating. This probe only
        /// ever transitions to `GroupTerminated` after
        /// [`classify_killpg_result`] confirms the group itself is gone
        /// *and* the leader has been reaped; on any other outcome the
        /// leader is left unreaped and the state stays `Running`, so
        /// `Drop` still safely retries -- the group's pgid is still
        /// this probe's own until that confirmation happens.
        fn terminate_group(&mut self) -> Result<(), String> {
            // SAFETY: signal-only, no memory access. `own_process_group`
            // put the leader in a process group whose pgid is the
            // leader's own pid.
            let result = unsafe { libc::killpg(self.pgid, libc::SIGKILL) };
            let os_error = if result == 0 {
                None
            } else {
                std::io::Error::last_os_error().raw_os_error()
            };
            self.finish_terminate_group(result, os_error)
        }

        /// Finishes the real classification/state-transition path from a
        /// captured `killpg` outcome, which the permission-denied negative
        /// control injects without depending on the host's process table.
        fn finish_terminate_group(
            &mut self,
            result: i32,
            os_error: Option<i32>,
        ) -> Result<(), String> {
            classify_killpg_result(result, self.pgid, os_error)?;
            self.child.wait().map_err(|error| {
                format!("cannot reap heartbeat probe leader after kill: {error}")
            })?;
            self.state = ProbeState::GroupTerminated;
            Ok(())
        }

        /// Kills only the leader, deliberately *not* reaping it --
        /// the negative control's setup step. Leaving the leader a
        /// zombie (in addition to the grandchild remaining alive) keeps
        /// this pgid number reserved for the rest of the test; `Drop`
        /// signals the group and reaps the leader afterward.
        fn kill_leader_unreaped(&mut self) {
            self.child.kill().expect("kill leader only");
            self.state = ProbeState::LeaderKilledUnreaped;
        }
    }

    impl Drop for HeartbeatProbe {
        fn drop(&mut self) {
            if !probe_drop_needs_cleanup(self.state) {
                return;
            }
            // Safe to signal here: in `Running`, nothing has died yet, so
            // this pgid is still exclusively this probe's; in
            // `LeaderKilledUnreaped`, the unreaped leader zombie and/or
            // the still-alive grandchild keep the pgid number reserved,
            // so it cannot yet have been recycled for an unrelated group.
            // SAFETY: signal-only, no memory access.
            unsafe {
                libc::killpg(self.pgid, libc::SIGKILL);
            }
            let _ = self.child.wait();
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

    /// Negative control for the test above: killing only the leader (the
    /// pre-hardening pattern) must leave the grandchild's process group
    /// untouched, so its heartbeat keeps growing past the pre-kill count
    /// -- proving `terminate_process_group_stops_grandchild_heartbeat`
    /// actually discriminates rather than passing regardless of what the
    /// kill call does. Growth is proven with a bounded poll for that
    /// specific event, not a fixed sleep-then-single-check window, so it
    /// cannot false-fail under scheduler contention. The leader is left
    /// unreaped (see `ProbeState::LeaderKilledUnreaped`) for the whole
    /// growth check, then reclaimed by `Drop`.
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

        probe.kill_leader_unreaped();
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

        // `probe`'s Drop signals the group (still safe: the unreaped
        // leader zombie and the live grandchild both hold the pgid
        // reservation) and reaps the leader.
    }

    /// Lifecycle/PID-state invariant: `Drop` must re-signal the group in
    /// every state where the pgid could still plausibly be this probe's
    /// (nothing killed yet, or the leader/grandchild still hold the
    /// reservation as a zombie/live process), and must NOT re-signal once
    /// a `terminate_group` call has confirmed the group fully torn down
    /// -- past that point the pgid number may already belong to an
    /// unrelated process group recycled by the kernel, and signaling it
    /// would be a real, high-severity foot-gun. This is a pure function
    /// of the state alone, so the invariant is checked directly without
    /// spawning any process or depending on any timing.
    #[test]
    fn probe_drop_cleanup_gated_by_confirmed_full_termination() {
        assert!(
            probe_drop_needs_cleanup(ProbeState::Running),
            "nothing has been signaled yet -- Drop must still clean up"
        );
        assert!(
            probe_drop_needs_cleanup(ProbeState::LeaderKilledUnreaped),
            "the unreaped leader zombie and/or live grandchild still hold this pgid \
             reserved -- Drop must still signal and reap"
        );
        assert!(
            !probe_drop_needs_cleanup(ProbeState::GroupTerminated),
            "the group was already fully killed and reaped -- Drop must NOT re-signal \
             a pgid the kernel may have already recycled for an unrelated process group"
        );
    }

    /// `classify_killpg_result` on synthetic inputs: success and `ESRCH`
    /// both confirm the group gone, and no other `errno` value may be
    /// mistaken for that -- the exact decision `terminate_group` relies
    /// on to gate the `GroupTerminated` transition.
    #[test]
    fn classify_killpg_result_only_confirms_gone_on_success_or_esrch() {
        assert!(classify_killpg_result(0, 4242, None).is_ok());
        assert!(classify_killpg_result(-1, 4242, Some(libc::ESRCH)).is_ok());
        assert!(classify_killpg_result(-1, 4242, Some(libc::EPERM)).is_err());
        assert!(classify_killpg_result(-1, 4242, Some(libc::EINVAL)).is_err());
        assert!(classify_killpg_result(-1, 4242, None).is_err());
    }

    /// `classify_killpg_result` through the probe's real termination boundary,
    /// with a deterministic injected `EPERM` outcome. This avoids assuming that
    /// process group 1 exists or belongs to init: FreeBSD can validly return
    /// `ESRCH` for that ambient probe. The short-lived owned child bounds the
    /// test even if a regression accepts `EPERM` and waits for the group leader.
    #[test]
    fn classify_killpg_result_rejects_a_real_injected_permission_failure() {
        let mut command = Command::new("sh");
        command.args(["-c", "sleep 0.2"]);
        own_process_group(&mut command);
        let child = command.spawn().expect("spawn bounded permission probe");
        let pgid = child.id().cast_signed();
        let mut probe = HeartbeatProbe {
            child,
            pgid,
            state: ProbeState::Running,
        };
        let result = probe.finish_terminate_group(-1, Some(libc::EPERM));
        let error = result.expect_err("an injected EPERM must keep termination unconfirmed");
        assert!(
            error.contains(&format!("os error {:?}", Some(libc::EPERM))),
            "permission-denied classification should preserve the injected errno: {error}"
        );
        assert_eq!(
            probe.state,
            ProbeState::Running,
            "a denied group signal must not advance the probe to GroupTerminated"
        );
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
