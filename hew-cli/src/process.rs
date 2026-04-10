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

/// Child process wrapper that bundles platform-specific cleanup resources
/// required for reliable timeout termination.
///
/// On Unix the cleanup mechanism is `killpg(SIGKILL)` on the process group
/// created by `setpgid(0, 0)` in the child.  On Windows the child is spawned
/// suspended, assigned to a Job Object before any user code runs, then
/// resumed; `TerminateJobObject` therefore covers the entire process tree.
///
/// Use [`BoundedChild::spawn`] to create, then either:
/// - [`BoundedChild::wait_with_timeout`] for interactive runs (stdout/stderr
///   flow to the terminal; no pipe draining needed), or
/// - the [`run_binary_with_timeout`] helper for captured output.
pub(crate) struct BoundedChild {
    child: Child,
    /// Windows-only: Job Object that owns the child and all its descendants.
    /// `None` if job creation or assignment failed at spawn time.
    #[cfg(windows)]
    job: Option<windows_job::WindowsJob>,
}

impl BoundedChild {
    /// Spawn a process with the appropriate process-isolation setup for this
    /// platform and return it together with any cleanup resources.
    #[cfg(unix)]
    pub(crate) fn spawn(command: &mut Command) -> Result<Self, String> {
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

        let child = command
            .spawn()
            .map_err(|e| format!("cannot spawn child process: {e}"))?;
        Ok(Self { child })
    }

    /// Spawn the child suspended, assign it to a Job Object, then resume it.
    ///
    /// The child is created with `CREATE_SUSPENDED` so no user code runs — and
    /// therefore no grandchildren can be spawned — before `AssignProcessToJobObject`
    /// completes.  Once the child is in the job every process it subsequently
    /// creates is auto-enrolled, guaranteeing `TerminateJobObject` covers the
    /// whole tree even if the root exits before the timeout fires.
    ///
    /// Failure modes and fallbacks:
    /// - Job creation fails → spawn normally (no suspend), `job = None`,
    ///   timeout falls back to `taskkill /T /F`.
    /// - Spawn suspended fails → propagate error.
    /// - Assignment fails → resume (mandatory), `job = None`, taskkill fallback.
    /// - Resume fails → kill the orphaned suspended process, propagate error.
    #[cfg(windows)]
    pub(crate) fn spawn(command: &mut Command) -> Result<Self, String> {
        use std::os::windows::process::CommandExt;

        const CREATE_SUSPENDED: u32 = 0x0000_0004;

        match windows_job::WindowsJob::new() {
            Err(_) => {
                // Job creation failed: spawn normally and rely on taskkill fallback.
                let child = command
                    .spawn()
                    .map_err(|e| format!("cannot spawn child process: {e}"))?;
                return Ok(Self { child, job: None });
            }
            Ok(job) => {
                // Spawn suspended: the child holds no locks and has spawned no
                // descendants, so the assignment window is truly race-free.
                let mut child = command
                    .creation_flags(CREATE_SUSPENDED)
                    .spawn()
                    .map_err(|e| format!("cannot spawn child process: {e}"))?;

                // Assign while suspended — this is the race-free moment.
                let job = match job.assign(&child) {
                    Ok(()) => Some(job),
                    Err(_) => None, // assignment failed; taskkill fallback
                };

                // Resume. If this fails the process is permanently stuck.
                if let Err(e) = windows_job::resume_child_process(&child) {
                    let _ = child.kill();
                    let _ = child.wait();
                    return Err(format!("cannot resume suspended child process: {e}"));
                }

                Ok(Self { child, job })
            }
        }
    }

    #[cfg(not(any(unix, windows)))]
    pub(crate) fn spawn(command: &mut Command) -> Result<Self, String> {
        let child = command
            .spawn()
            .map_err(|e| format!("cannot spawn child process: {e}"))?;
        Ok(Self { child })
    }

    /// Kill the entire process tree and reap the root child.
    ///
    /// Returns `true` iff every process that could hold a pipe write-end has
    /// been provably terminated so that drain threads can be safely joined.
    #[cfg(unix)]
    fn terminate_process_group(&mut self) -> Result<bool, String> {
        let tree_killed = kill_timed_out_child(&mut self.child, TimeoutKillTarget::ProcessGroup)?;
        self.child
            .wait()
            .map_err(|e| format!("cannot reap timed-out child process: {e}"))?;
        Ok(tree_killed)
    }

    /// Kill via Job Object (primary) or `taskkill /T /F` (fallback).
    ///
    /// The Job Object path terminates the entire tree by ownership, not by
    /// PID lookup, so it is race-free with respect to the root child exiting.
    /// After `TerminateJobObject` returns the kernel has already closed the
    /// process handle tables for every process in the job, guaranteeing that
    /// all pipe write-end handles are released before we join drain threads.
    #[cfg(windows)]
    fn terminate_process_group(&mut self) -> Result<bool, String> {
        match self.job.as_ref() {
            Some(job) => {
                job.terminate()?;
                // Reap the root child. It may have already exited on its own
                // (that is the scenario where the Job Object path is essential
                // — the root is gone but descendants may still be alive).
                self.child
                    .wait()
                    .map_err(|e| format!("cannot reap timed-out child process: {e}"))?;
                Ok(true)
            }
            None => {
                // Job assignment failed at spawn time; fall back to taskkill.
                // Only exit 0 counts as proof of tree termination.
                let tree_killed = windows_kill_taskkill(&mut self.child)?;
                self.child
                    .wait()
                    .map_err(|e| format!("cannot reap timed-out child process: {e}"))?;
                Ok(tree_killed)
            }
        }
    }

    #[cfg(not(any(unix, windows)))]
    fn terminate_process_group(&mut self) -> Result<bool, String> {
        kill_child_only(&mut self.child)?;
        self.child
            .wait()
            .map_err(|e| format!("cannot reap timed-out child process: {e}"))?;
        Ok(false)
    }

    /// Wait up to `timeout` for the child to exit, terminating the process
    /// group if the deadline is reached.
    ///
    /// Unlike [`run_binary_with_timeout`], this method does not capture
    /// stdout/stderr — they flow to the terminal as-is.  Use this for
    /// interactive `hew run --timeout` where the user sees output directly.
    ///
    /// Returns [`ChildWaitOutcome::Exited`] if the child exits before the
    /// deadline, or [`ChildWaitOutcome::Timeout`] after killing the process
    /// group.
    pub(crate) fn wait_with_timeout(
        &mut self,
        timeout: Duration,
    ) -> Result<ChildWaitOutcome, String> {
        let start = Instant::now();
        loop {
            match self.child.try_wait() {
                Ok(Some(status)) => return Ok(ChildWaitOutcome::Exited(status)),
                Ok(None) => {
                    if start.elapsed() > timeout {
                        self.terminate_process_group()?;
                        return Ok(ChildWaitOutcome::Timeout);
                    }
                    std::thread::sleep(Duration::from_millis(10));
                }
                Err(e) => return Err(format!("cannot poll child process: {e}")),
            }
        }
    }

    /// Return the child's process ID.
    pub(crate) fn id(&self) -> u32 {
        self.child.id()
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

    let mut bounded = BoundedChild::spawn(&mut command)?;
    let drain = ConcurrentChildOutput::spawn(&mut bounded.child)?;
    let start = Instant::now();

    loop {
        match bounded.child.try_wait() {
            Ok(Some(status)) => {
                let (stdout, stderr) = drain.finish()?;
                if status.success() {
                    return Ok(BinaryRunOutcome::Success { stdout });
                }
                return Ok(BinaryRunOutcome::Failed { stdout, stderr });
            }
            Ok(None) => {
                if start.elapsed() > timeout {
                    let tree_killed = bounded.terminate_process_group()?;
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
/// Returns `true` only when `taskkill` exits 0, which means it positively
/// identified and terminated every process in the tree.  All other outcomes
/// return `false` so the caller detaches drain threads rather than joining:
///
/// - Exit 128 ("PID not found"): the root child exited on its own between
///   `try_wait()` and this call.  Its descendants may still be alive and
///   holding inherited pipe write-ends open, so we cannot claim the tree is
///   dead.
/// - Any other non-zero exit or spawn failure: partial or no kill; fall back
///   to `child.kill()` for the root and return `false`.
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
                Ok(s) => {
                    // Exit 128 means the root PID was not found — the child
                    // exited naturally, but its descendants may still be alive.
                    // Any other non-zero code means partial or no kill.
                    // In both cases fall back to a best-effort child kill and
                    // return false so drain threads are detached, not joined.
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

/// Kill the process tree rooted at `child` on Windows using `taskkill /T /F`.
///
/// Used as a fallback when `BoundedChild` could not assign a Job Object at
/// spawn time (e.g. Windows 7 nested-job limit).  Returns `true` only when
/// `taskkill` exits 0 (positively identified and terminated every process in
/// the tree).  All other outcomes return `false` so drain threads are detached
/// rather than joined.
///
/// Note: exit 128 ("PID not found") means the root exited on its own before
/// `taskkill` ran.  Descendants may still be alive so we return `false`.
#[cfg(windows)]
fn windows_kill_taskkill(child: &mut Child) -> Result<bool, String> {
    let pid = child.id();
    match Command::new("taskkill")
        .args(["/T", "/F", "/PID", &pid.to_string()])
        .status()
    {
        Ok(s) if s.success() => Ok(true),
        Ok(s) => {
            kill_child_only(child).map_err(|kill_error| {
                format!(
                    "taskkill exited with {s}; \
                     fallback child kill also failed: {kill_error}"
                )
            })?;
            Ok(false)
        }
        Err(spawn_error) => {
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

/// Windows Job Object RAII wrapper.
///
/// Creates a Job Object at spawn time and assigns the child to it so that
/// `TerminateJobObject` can kill the entire process tree — including
/// descendants and even when the root has already exited — without any
/// PID-lookup race.
#[cfg(windows)]
mod windows_job {
    use std::os::windows::io::AsRawHandle;
    use std::process::Child;

    type HANDLE = *mut core::ffi::c_void;
    type BOOL = i32;
    type DWORD = u32;

    const FALSE: BOOL = 0;

    extern "system" {
        fn CreateJobObjectW(
            lp_job_attributes: *mut core::ffi::c_void,
            lp_name: *const u16,
        ) -> HANDLE;
        fn AssignProcessToJobObject(h_job: HANDLE, h_process: HANDLE) -> BOOL;
        fn TerminateJobObject(h_job: HANDLE, u_exit_code: u32) -> BOOL;
        fn CloseHandle(h_object: HANDLE) -> BOOL;
        fn GetLastError() -> DWORD;
    }

    // NtResumeProcess is not in the Win32 SDK headers but is a stable
    // ntdll.dll export present on all Windows versions since NT 4.0.
    // It resumes all threads in a process atomically, which is exactly
    // what we need after a CREATE_SUSPENDED spawn.  We use it instead of
    // ResumeThread because std::process::Child does not expose the primary-
    // thread handle that CreateProcess returns.
    #[link(name = "ntdll")]
    extern "system" {
        fn NtResumeProcess(process_handle: HANDLE) -> i32; // NTSTATUS
    }

    /// Resume all threads in a child process that was created suspended.
    ///
    /// Calls `NtResumeProcess`, which decrements the suspend count on every
    /// thread.  For a freshly `CREATE_SUSPENDED` process this unblocks the
    /// primary thread and lets the process start executing.
    pub(super) fn resume_child_process(child: &Child) -> Result<(), String> {
        let handle = child.as_raw_handle() as HANDLE;
        // SAFETY: handle is valid for the lifetime of child.
        // NTSTATUS: values < 0 are errors; >= 0 are success codes.
        let status = unsafe { NtResumeProcess(handle) };
        if status < 0 {
            let code = unsafe { GetLastError() };
            Err(format!(
                "NtResumeProcess failed: NTSTATUS {status:#010x} (last error {code})"
            ))
        } else {
            Ok(())
        }
    }

    pub(super) struct WindowsJob(HANDLE);

    // SAFETY: The HANDLE is valid until `CloseHandle` is called in `Drop`.
    // We never share the HANDLE across threads without synchronisation.
    unsafe impl Send for WindowsJob {}
    unsafe impl Sync for WindowsJob {}

    impl WindowsJob {
        /// Create an anonymous Job Object.
        pub(super) fn new() -> Result<Self, String> {
            // SAFETY: null attributes and null name are documented valid args.
            let handle = unsafe { CreateJobObjectW(core::ptr::null_mut(), core::ptr::null()) };
            if handle.is_null() {
                // SAFETY: GetLastError is always safe to call.
                let code = unsafe { GetLastError() };
                Err(format!("CreateJobObjectW failed: error {code}"))
            } else {
                Ok(Self(handle))
            }
        }

        /// Assign the child process to this Job Object.
        ///
        /// Must be called immediately after spawn, before the child can itself
        /// spawn grandchildren (which would otherwise not be in the job).
        pub(super) fn assign(&self, child: &Child) -> Result<(), String> {
            let process_handle = child.as_raw_handle() as HANDLE;
            // SAFETY: both handles are valid at this point.
            let ok = unsafe { AssignProcessToJobObject(self.0, process_handle) };
            if ok == FALSE {
                // SAFETY: always safe.
                let code = unsafe { GetLastError() };
                Err(format!("AssignProcessToJobObject failed: error {code}"))
            } else {
                Ok(())
            }
        }

        /// Terminate every process currently assigned to this Job Object.
        ///
        /// This is race-free with respect to the root process exiting: the Job
        /// Object owns the tree by handle, not by PID.  Any process that was in
        /// the job when it exited is still counted; its descendants remain in the
        /// job and will be terminated here.
        pub(super) fn terminate(&self) -> Result<(), String> {
            // SAFETY: the handle is valid until Drop.
            let ok = unsafe { TerminateJobObject(self.0, 1) };
            if ok == FALSE {
                // SAFETY: always safe.
                let code = unsafe { GetLastError() };
                Err(format!("TerminateJobObject failed: error {code}"))
            } else {
                Ok(())
            }
        }
    }

    impl Drop for WindowsJob {
        fn drop(&mut self) {
            // SAFETY: self.0 is a valid handle obtained from CreateJobObjectW,
            // and Drop is called at most once.
            unsafe { CloseHandle(self.0) };
        }
    }
}

/// Tests for [`BoundedChild`] process-group lifecycle.
///
/// These are unit tests (no Hew compilation required) that directly exercise
/// the spawn + timeout + kill path to prove grandchildren are reaped.
#[cfg(test)]
#[cfg(unix)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    /// Verify that [`BoundedChild::wait_with_timeout`] kills the full process
    /// GROUP on timeout, not just the direct child process.
    ///
    /// The shell script spawns a grandchild `sleep 999`, writes its PID to a
    /// temp file, then spins forever.  After the timeout fires, both the shell
    /// script (direct child) and the grandchild sleep must be dead — proving
    /// that `killpg` is used rather than `kill(child_pid)`.
    #[test]
    fn bounded_child_timeout_kills_grandchild_process_group() {
        let dir = tempfile::tempdir().unwrap();
        let pid_file = dir.path().join("grandchild.pid");
        let pid_file_str = pid_file.to_str().unwrap();

        // Shell script: start a grandchild sleep, record its PID, then spin.
        let script = dir.path().join("tree_spinner.sh");
        std::fs::write(
            &script,
            format!("#!/bin/sh\nsleep 999 & echo $! > {pid_file_str}\nwhile true; do :; done\n"),
        )
        .unwrap();
        std::fs::set_permissions(&script, std::fs::Permissions::from_mode(0o755)).unwrap();

        let mut cmd = Command::new(&script);
        let mut bounded = BoundedChild::spawn(&mut cmd).expect("failed to spawn tree_spinner.sh");

        // Give the grandchild a moment to start and write its PID.
        std::thread::sleep(Duration::from_millis(200));

        let outcome = bounded
            .wait_with_timeout(Duration::from_secs(1))
            .expect("wait_with_timeout failed");
        assert!(
            matches!(outcome, ChildWaitOutcome::Timeout),
            "expected Timeout outcome, got: {outcome:?}"
        );

        // Allow the OS to finish reaping.
        std::thread::sleep(Duration::from_millis(200));

        let pid_str = std::fs::read_to_string(&pid_file)
            .expect("grandchild should have written its PID before the timeout fired");
        let grandchild_pid: u32 = pid_str
            .trim()
            .parse()
            .expect("grandchild PID file should contain a numeric PID");

        #[allow(
            clippy::cast_possible_wrap,
            reason = "PIDs fit in i32 on all supported Unix platforms"
        )]
        // SAFETY: `kill(pid, 0)` is a POSIX liveness probe — no signal is sent.
        let alive = unsafe { libc::kill(grandchild_pid as libc::pid_t, 0) } == 0;
        assert!(
            !alive,
            "grandchild PID {grandchild_pid} should be dead after process-group kill on timeout"
        );
    }
}
