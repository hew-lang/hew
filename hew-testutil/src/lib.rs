//! Shared integration-test helpers.

use std::fmt;
use std::io::{ErrorKind, Read, Write};
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

#[derive(Debug)]
struct BoundedChild {
    child: Child,
    #[cfg(windows)]
    job: Option<windows_job::WindowsJob>,
}

impl BoundedChild {
    #[cfg(unix)]
    fn spawn(command: &mut Command, label: &str) -> Result<Self, BoundedExecError> {
        use std::os::unix::process::CommandExt;

        // SAFETY: this runs in the child after fork and before exec. It only
        // moves the child into a fresh process group so timeout kills also
        // close pipe handles inherited by grandchildren.
        unsafe {
            command.pre_exec(|| {
                if libc::setpgid(0, 0) == 0 {
                    Ok(())
                } else {
                    Err(std::io::Error::last_os_error())
                }
            });
        }

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
        let process_group = self.child.id().cast_signed();
        // SAFETY: the helper put the child in a process group whose PGID is the
        // root PID. ESRCH means the group is already gone.
        let result = unsafe { libc::killpg(process_group, libc::SIGKILL) };
        if result != 0 {
            let group_error = std::io::Error::last_os_error();
            if group_error.raw_os_error() != Some(libc::ESRCH) {
                kill_child_only(&mut self.child, label).map_err(|kill_error| {
                    BoundedExecError::failed(
                        label,
                        format!(
                            "cannot kill process group {process_group}: {group_error}; fallback child kill failed: {kill_error}"
                        ),
                    )
                })?;
                self.child.wait().map_err(|error| {
                    BoundedExecError::failed(
                        label,
                        format!("cannot reap child after timeout: {error}"),
                    )
                })?;
                return Ok(false);
            }
        }
        self.child.wait().map_err(|error| {
            BoundedExecError::failed(label, format!("cannot reap child after timeout: {error}"))
        })?;
        Ok(true)
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
