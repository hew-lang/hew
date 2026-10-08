//! Hew runtime: child process spawning and management.
//!
//! Runs commands to completion with captured output, or starts a child with
//! inherited, null or piped stdio. A child's pipes become `Stream<bytes>` and
//! `Sink<bytes>` owners; on Unix they wait on the I/O reactor, so a child's
//! output is a select source. The child handle owns the process until it is
//! reaped: the PID cannot be reused while a signal can still name it, and the
//! reaped status is cached so a second wait returns it again. A wait parks on
//! a stream that ends when the child exits (see [`hew_process_exit_stream`]),
//! so it holds no thread. Stdout/stderr
//! strings in [`HewProcessResult`] are owned managed UTF-8 handles, preserving
//! embedded NUL. Command and argument inputs borrow managed handles and reject
//! interior NUL before crossing the OS boundary. Captured non-UTF-8 output is
//! decoded lossily.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::stream::HewStreamPair;
use crate::util::{cstr_to_str, MutexExt};
use crate::vec::{ElemKind, HewTypeOwnershipKind, HewVec};
use hew_cabi::string::{
    string_from_str, string_release, string_retain, string_to_cstring, HewString,
};
use std::process::{Command, ExitStatus, Stdio};

/// Result of a completed process.
#[derive(Debug)]
pub struct HewProcessResult {
    /// How the process ended.
    pub status: ExitStatus,
    /// Captured stdout, an owned managed UTF-8 string.
    pub stdout: *mut HewString,
    /// Captured stderr, an owned managed UTF-8 string.
    pub stderr: *mut HewString,
}

#[cfg(unix)]
mod exit_watch;
#[cfg(unix)]
pub(crate) use exit_watch::ExitWatch;

/// Handle to a started child process. It owns the process until it is
/// reaped and caches the status from then on. Concurrent waits share it, so
/// reaping, signalling and opening an exit watch all hold its lock: none can
/// name the process ID after the reap frees it.
pub struct HewProcess {
    state: std::sync::Mutex<ChildState>,
    /// The OS error that kept the last wait from watching the child.
    wait_error: std::sync::atomic::AtomicI32,
}

struct ChildState {
    child: std::process::Child,
    status: Option<ExitStatus>,
}

impl std::fmt::Debug for HewProcess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewProcess").finish_non_exhaustive()
    }
}

/// The signal that ended a process, if a signal did.
fn terminating_signal(status: ExitStatus) -> Option<i32> {
    #[cfg(unix)]
    {
        std::os::unix::process::ExitStatusExt::signal(&status)
    }
    #[cfg(not(unix))]
    {
        let _ = status;
        None
    }
}

/// The exit code, or the signal number for a signalled process.
fn status_number(status: ExitStatus) -> i64 {
    terminating_signal(status)
        .map_or_else(|| i64::from(status.code().unwrap_or_default()), i64::from)
}

/// Convert a byte slice to an owned managed UTF-8 string, replacing invalid UTF-8
/// with the replacement character.
fn bytes_to_string(bytes: &[u8]) -> *mut HewString {
    let s = String::from_utf8_lossy(bytes);
    string_from_str(&s)
}

/// Copy and validate a managed command or argument at the OS boundary.
unsafe fn process_input(value: *const HewString, context: &str) -> Option<String> {
    // SAFETY: the caller holds a live managed owner for this copy.
    let Ok(foreign) = (unsafe { string_to_cstring(value) }) else {
        crate::set_last_error(format!("{context}: input contains interior NUL"));
        return None;
    };
    // CString and String use Rust ownership; no raw allocation escapes.
    Some(foreign.into_string().expect("managed input is UTF-8"))
}

/// Build a [`HewProcessResult`] from an [`std::process::Output`].
#[expect(
    clippy::needless_pass_by_value,
    reason = "Output is consumed to extract owned fields"
)]
fn output_to_result(output: std::process::Output) -> *mut HewProcessResult {
    let stdout = bytes_to_string(&output.stdout);
    let stderr = bytes_to_string(&output.stderr);
    Box::into_raw(Box::new(HewProcessResult {
        status: output.status,
        stdout,
        stderr,
    }))
}

impl HewProcess {
    fn status(&self) -> Option<ExitStatus> {
        self.state.lock_or_recover().status
    }

    /// Reap the child if it has exited, without waiting.
    fn poll(&self) -> std::io::Result<Option<ExitStatus>> {
        let mut state = self.state.lock_or_recover();
        if state.status.is_none() {
            state.status = state.child.try_wait()?;
        }
        Ok(state.status)
    }

    /// Kill an unreaped child and reap it, so dropping a handle never leaves
    /// a running process or a zombie behind.
    fn kill_and_reap(&self) {
        let mut state = self.state.lock_or_recover();
        if state.status.is_some() {
            return;
        }
        let kill_error = state.child.kill().err();
        match state.child.wait() {
            Ok(status) => state.status = Some(status),
            Err(wait_error) => crate::set_last_error(match kill_error {
                Some(kill_error) => format!(
                    "hew_process_drop: kill failed: {kill_error}; wait failed: {wait_error}"
                ),
                None => format!("hew_process_drop: {wait_error}"),
            }),
        }
    }
}

/// Execute a prepared [`Command`] and convert its output into a Hew result.
fn command_output_to_result(
    command: &mut Command,
    context: &str,
    command_name: &str,
) -> *mut HewProcessResult {
    match command.output() {
        Ok(output) => {
            crate::hew_clear_error();
            output_to_result(output)
        }
        Err(error) => {
            crate::set_last_error(format!(
                "{context}: failed to execute '{command_name}': {error}"
            ));
            std::ptr::null_mut()
        }
    }
}

/// Convert a `Vec<String>`-backed [`HewVec`] into owned Rust strings.
///
/// # Safety
///
/// `args` must be either null (treated as an empty argument vector) or a valid
/// `HewVec` pointer containing string elements.
unsafe fn hewvec_string_args(arg_vec: *mut HewVec, context: &str) -> Option<Vec<String>> {
    if arg_vec.is_null() {
        return Some(Vec::new());
    }

    // SAFETY: caller guarantees arg_vec is a valid HewVec pointer.
    let args_ref = unsafe { &*arg_vec };
    let pointer_size = core::mem::size_of::<*const HewString>();
    let strings = if args_ref.layout.is_null() {
        args_ref.elem_kind == ElemKind::String
    } else {
        // SAFETY: a descriptor-backed vector owns its live layout storage.
        let layout = unsafe { &*args_ref.layout };
        layout.ownership_kind == HewTypeOwnershipKind::String
            && layout.size == pointer_size
            && layout.align == core::mem::align_of::<*const HewString>()
    };
    if !strings || args_ref.elem_size != pointer_size {
        crate::set_last_error(format!("{context}: args must be Vec<String>"));
        return None;
    }

    let mut owned_args = Vec::with_capacity(args_ref.len);
    for index in 0..args_ref.len {
        #[expect(
            clippy::cast_ptr_alignment,
            reason = "validated string vector storage preserves pointer alignment"
        )]
        // SAFETY: the exact string tag and geometry describe live managed
        // handle slots. This call borrows the vector throughout conversion;
        // no retain or release is needed to copy each string into Rust.
        let raw_arg = unsafe { args_ref.data.cast::<*const HewString>().add(index).read() };
        // SAFETY: raw_arg is borrowed from this live string vector.
        owned_args.push(unsafe { process_input(raw_arg, context) }?);
    }

    Some(owned_args)
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Build a [`Command`] that runs `cmd_str` through the platform's system shell:
/// `sh -c "cmd"` on Unix, `cmd /C "cmd"` on Windows.
fn shell_command(cmd_str: &str) -> Command {
    #[cfg(windows)]
    {
        let mut command = Command::new("cmd");
        command.arg("/C").arg(cmd_str);
        command
    }
    #[cfg(not(windows))]
    {
        let mut command = Command::new("sh");
        command.arg("-c").arg(cmd_str);
        command
    }
}

/// Run a command via the system shell (`sh -c "cmd"` on Unix, `cmd /C "cmd"` on
/// Windows) and wait for completion.
///
/// Returns a heap-allocated [`HewProcessResult`], or null on error.
/// The caller must free the result with [`hew_process_result_free`].
///
/// # Safety
///
/// `cmd` must be a live managed handle, or null (empty).
#[no_mangle]
pub unsafe extern "C" fn hew_process_run(cmd: *const HewString) -> *mut HewProcessResult {
    // SAFETY: cmd is a borrowed managed handle at this ABI boundary.
    let Some(cmd_str) = (unsafe { process_input(cmd, "hew_process_run") }) else {
        return std::ptr::null_mut();
    };
    let mut command = shell_command(&cmd_str);
    command_output_to_result(&mut command, "hew_process_run", &cmd_str)
}

/// Run a command with an explicit `Vec<String>` argv surface (no shell).
///
/// Returns a heap-allocated [`HewProcessResult`], or null on error.
/// The caller must free the result with [`hew_process_result_free`].
///
/// # Safety
///
/// `cmd` must be a live managed handle, or null (empty). `args` must be a
/// valid `Vec<String>` handle or null (treated as an empty argv).
#[no_mangle]
pub unsafe extern "C" fn hew_process_run_argv(
    cmd: *const HewString,
    argv_vec: *mut HewVec,
) -> *mut HewProcessResult {
    // SAFETY: cmd is a borrowed managed handle at this ABI boundary.
    let Some(cmd_str) = (unsafe { process_input(cmd, "hew_process_run_argv") }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: argv_vec is either null or a valid Vec<String>-backed HewVec.
    let Some(owned_args) = (unsafe { hewvec_string_args(argv_vec, "hew_process_run_argv") }) else {
        return std::ptr::null_mut();
    };

    let mut command = Command::new(&cmd_str);
    command.args(owned_args);
    command_output_to_result(&mut command, "hew_process_run_argv", &cmd_str)
}

/// `std.process.Stdio` in declaration order.
fn stdio(code: i32, context: &str) -> Option<Stdio> {
    match code {
        0 => Some(Stdio::inherit()),
        1 => Some(Stdio::null()),
        2 => Some(Stdio::piped()),
        _ => {
            crate::set_last_error(format!("{context}: unknown stdio mode {code}"));
            None
        }
    }
}

/// Start `program` with `args` (never through a shell) and the given stdio
/// modes: 0 inherit, 1 null, 2 piped.
///
/// Returns a heap-allocated [`HewProcess`], or null with the failure in the
/// last-error slot. The caller releases it with [`hew_process_drop`].
///
/// # Safety
///
/// `program` must be a live managed handle, or null (empty). `args` must be
/// null or a valid `Vec<String>`-backed [`HewVec`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_start(
    program: *const HewString,
    args: *mut HewVec,
    stdin: i32,
    stdout: i32,
    stderr: i32,
) -> *mut HewProcess {
    const CONTEXT: &str = "hew_process_start";
    // SAFETY: program is a borrowed managed handle at this ABI boundary.
    let Some(program) = (unsafe { process_input(program, CONTEXT) }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: args is either null or a valid Vec<String>-backed HewVec.
    let Some(args) = (unsafe { hewvec_string_args(args, CONTEXT) }) else {
        return std::ptr::null_mut();
    };
    let (Some(stdin), Some(stdout), Some(stderr)) = (
        stdio(stdin, CONTEXT),
        stdio(stdout, CONTEXT),
        stdio(stderr, CONTEXT),
    ) else {
        return std::ptr::null_mut();
    };
    let mut command = Command::new(&program);
    command
        .args(args)
        .stdin(stdin)
        .stdout(stdout)
        .stderr(stderr);
    // An inheriting child shares stdout and stderr: queued output goes first.
    crate::output::flush();
    match command.spawn() {
        Ok(child) => {
            crate::hew_clear_error();
            Box::into_raw(Box::new(HewProcess {
                state: std::sync::Mutex::new(ChildState {
                    child,
                    status: None,
                }),
                wait_error: std::sync::atomic::AtomicI32::new(0),
            }))
        }
        Err(error) => {
            crate::set_last_error(format!("{CONTEXT}: failed to execute '{program}': {error}"));
            std::ptr::null_mut()
        }
    }
}

/// Report whether `proc` is a live child handle; [`hew_process_start`]
/// returns null when the launch fails.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_process_is_valid(proc: *mut HewProcess) -> bool {
    !proc.is_null()
}

/// The child's operating-system process ID.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_id(proc: *mut HewProcess) -> i64 {
    // SAFETY: proc is a live HewProcess per caller contract.
    i64::from(unsafe { &*proc }.state.lock_or_recover().child.id())
}

/// The parent's end of a child pipe as a file.
#[cfg(unix)]
fn file(handle: impl Into<std::os::fd::OwnedFd>) -> std::fs::File {
    std::fs::File::from(handle.into())
}

/// The parent's end of a child pipe as a file.
#[cfg(windows)]
fn file(handle: impl Into<std::os::windows::io::OwnedHandle>) -> std::fs::File {
    std::fs::File::from(handle.into())
}

/// Take the parent's end of the child's stdin (0), stdout (1) or stderr (2)
/// pipe as a stream pair holding only that half. Returns null when the stream
/// was not piped or was already taken.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_take_pipe(
    proc: *mut HewProcess,
    which: i32,
) -> *mut HewStreamPair {
    // SAFETY: proc is a live HewProcess per caller contract.
    let mut state = unsafe { &*proc }.state.lock_or_recover();
    let child = &mut state.child;
    let pair = match which {
        0 => child
            .stdin
            .take()
            .map(|pipe| crate::stream::child_pipe_sink(file(pipe))),
        1 => child
            .stdout
            .take()
            .map(|pipe| crate::stream::child_pipe_stream(file(pipe))),
        2 => child
            .stderr
            .take()
            .map(|pipe| crate::stream::child_pipe_stream(file(pipe))),
        _ => None,
    };
    pair.unwrap_or(std::ptr::null_mut())
}

/// A stream with no items that ends once the child has exited, leaving it
/// unreaped: `Child.wait` reads it to park until the exit, holding no thread,
/// then reaps. Each wait takes its own stream; dropping it, as a cancelled
/// wait does, withdraws the watch. A child already reaped gets a stream that
/// has ended. Returns null when the OS refuses the watch;
/// [`hew_process_wait_error`] then has its error.
///
/// On Unix the stream waits on the I/O reactor for a pidfd (Linux) or a
/// kqueue `EVFILT_PROC` filter (FreeBSD, macOS). On Windows the system thread
/// pool's wait on the process handle ends it.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_exit_stream(proc: *mut HewProcess) -> *mut HewStreamPair {
    // SAFETY: proc is a live HewProcess per caller contract.
    let proc = unsafe { &*proc };
    // Held while the watch names the child, so no concurrent wait reaps it
    // and frees its ID first.
    let state = proc.state.lock_or_recover();
    if state.status.is_some() {
        return crate::stream::ended_stream();
    }
    #[cfg(unix)]
    let watched = ExitWatch::open(state.child.id()).map(crate::stream::child_exit_stream);
    #[cfg(windows)]
    let watched = {
        use std::os::windows::io::AsHandle;
        state
            .child
            .as_handle()
            .try_clone_to_owned()
            .and_then(crate::stream::child_exit_stream)
    };
    drop(state);
    watched.unwrap_or_else(|error| {
        proc.wait_error.store(
            error.raw_os_error().unwrap_or(libc::EIO),
            std::sync::atomic::Ordering::SeqCst,
        );
        std::ptr::null_mut()
    })
}

/// The OS error that kept the last [`hew_process_exit_stream`] from
/// watching the child.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_wait_error(proc: *mut HewProcess) -> i32 {
    // SAFETY: proc is a live HewProcess per caller contract.
    unsafe { &*proc }
        .wait_error
        .load(std::sync::atomic::Ordering::SeqCst)
}

/// Reap the child if it has exited, without waiting: 1 when it has (its
/// status is then cached), 0 while it runs, or the negated OS error.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_poll(proc: *mut HewProcess) -> i32 {
    // SAFETY: proc is a live HewProcess per caller contract.
    match unsafe { (*proc).poll() } {
        Ok(Some(_)) => 1,
        Ok(None) => 0,
        Err(error) => -error.raw_os_error().unwrap_or(libc::EIO),
    }
}

/// Whether the reaped child was ended by a signal. Valid after
/// [`hew_process_poll`] answered 1.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_signalled(proc: *mut HewProcess) -> bool {
    // SAFETY: proc is a live HewProcess per caller contract.
    unsafe { &*proc }
        .status()
        .is_some_and(|status| terminating_signal(status).is_some())
}

/// The reaped child's exit code, or its signal number when signalled. Valid
/// after [`hew_process_poll`] answered 1.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_status(proc: *mut HewProcess) -> i64 {
    // SAFETY: proc is a live HewProcess per caller contract.
    unsafe { &*proc }.status().map_or(0, status_number)
}

/// Ask the child to stop (`force` false: SIGTERM) or stop it (`force` true:
/// SIGKILL). Windows has one way to stop a process, so both terminate it. A
/// reaped child is not signalled. Returns 0, or the OS error.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_signal(proc: *mut HewProcess, force: bool) -> i32 {
    #[cfg_attr(
        unix,
        expect(unused_mut, reason = "only Windows kills through the Child")
    )]
    // SAFETY: proc is a live HewProcess per caller contract.
    let mut state = unsafe { &*proc }.state.lock_or_recover();
    if state.status.is_some() {
        return 0;
    }
    #[cfg(unix)]
    let result = {
        let Ok(pid) = libc::pid_t::try_from(state.child.id()) else {
            return libc::EINVAL;
        };
        let signal = if force { libc::SIGKILL } else { libc::SIGTERM };
        // SAFETY: the child is unreaped, so its ID still names it.
        if unsafe { libc::kill(pid, signal) } == 0 {
            Ok(())
        } else {
            Err(std::io::Error::last_os_error())
        }
    };
    #[cfg(not(unix))]
    let result = {
        let _ = force;
        state.child.kill()
    };
    match result {
        Ok(()) => 0,
        Err(error) => error.raw_os_error().unwrap_or(libc::EIO),
    }
}

/// Release a [`HewProcess`] at scope exit. A child still running is killed
/// and reaped first, so no process or zombie outlives its owner.
///
/// # Safety
///
/// `p` must be a pointer previously returned by [`hew_process_start`],
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_process_drop(p: *mut HewProcess) {
    if p.is_null() {
        return;
    }
    // SAFETY: p was allocated with Box::into_raw and has not been freed.
    let proc = unsafe { Box::from_raw(p) };
    let _ = proc.poll();
    proc.kill_and_reap();
}

/// Return an owned managed copy of the current thread's last process error.
#[no_mangle]
pub extern "C" fn hew_process_last_error() -> *mut HewString {
    let ptr = crate::hew_last_error();
    if ptr.is_null() {
        return string_from_str("");
    }
    // SAFETY: ptr comes from thread-local storage and remains valid until the
    // next error mutation; we duplicate it immediately.
    let Some(text) = (unsafe { cstr_to_str(&ptr, "hew_process_last_error") }) else {
        return std::ptr::null_mut();
    };
    string_from_str(text)
}

/// Return whether a process result pointer is non-null.
#[no_mangle]
pub extern "C" fn hew_process_result_is_valid(r: *const HewProcessResult) -> bool {
    !r.is_null()
}

/// Whether a completed process was ended by a signal.
///
/// # Safety
///
/// `r` must be a valid pointer returned by a `hew_process_run*` function.
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_signalled(r: *const HewProcessResult) -> bool {
    // SAFETY: r is valid per caller contract.
    terminating_signal(unsafe { (*r).status }).is_some()
}

/// A completed process's exit code, or its signal number when signalled.
///
/// # Safety
///
/// `r` must be a valid pointer returned by a `hew_process_run*` function.
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_status(r: *const HewProcessResult) -> i64 {
    // SAFETY: r is valid per caller contract.
    status_number(unsafe { (*r).status })
}

/// Retain an owned stdout handle that survives freeing the process result.
///
/// # Safety
///
/// `r` must be a valid pointer returned by a `hew_process_run*` function.
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_stdout(r: *const HewProcessResult) -> *mut HewString {
    cabi_guard!(r.is_null(), std::ptr::null_mut());
    crate::hew_clear_error();
    // SAFETY: r is valid per caller contract.
    unsafe { string_retain((*r).stdout) }
}

/// Retain an owned stderr handle that survives freeing the process result.
///
/// # Safety
///
/// `r` must be a valid pointer returned by a `hew_process_run*` function.
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_stderr(r: *const HewProcessResult) -> *mut HewString {
    cabi_guard!(r.is_null(), std::ptr::null_mut());
    crate::hew_clear_error();
    // SAFETY: r is valid per caller contract.
    unsafe { string_retain((*r).stderr) }
}

/// Free a [`HewProcessResult`] previously returned by a `hew_process_run*`
/// function, including its owned managed stdout and stderr strings.
///
/// # Safety
///
/// `r` must be a pointer previously returned by a `hew_process_run*` function,
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_free(r: *mut HewProcessResult) {
    if r.is_null() {
        return;
    }
    // SAFETY: r was allocated with Box::into_raw and has not been freed.
    let result = unsafe { Box::from_raw(r) };
    // SAFETY: the result owns one reference to each managed field. Accessors
    // retain independent owners, which remain valid after this result is freed.
    unsafe {
        string_release(result.stdout);
        string_release(result.stderr);
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_as_str;
    use std::ffi::CStr;

    /// Read a borrowed managed result, including the canonical empty value.
    unsafe fn read_string(ptr: *mut HewString) -> String {
        // SAFETY: the test keeps the result owner live for this read.
        unsafe { string_as_str(ptr) }.to_owned()
    }

    /// Helper: read the thread-local last error string.
    ///
    /// # Safety
    ///
    /// The runtime must have already populated `hew_last_error()` for this thread.
    unsafe fn read_last_error() -> String {
        let ptr = crate::hew_last_error();
        assert!(!ptr.is_null(), "expected hew_last_error to be populated");
        // SAFETY: hew_last_error returns a borrowed NUL-terminated TLS diagnostic,
        // valid until the next error update on this thread.
        unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned()
    }

    /// A managed `Vec<String>` holding `args`; the caller frees it.
    fn string_vec(items: &[&str]) -> *mut HewVec {
        // SAFETY: each pushed managed value is retained by the vector.
        unsafe {
            let strings = crate::vec::hew_vec_new_str();
            for item in items {
                let value = ManagedString::new(item);
                crate::vec::hew_vec_push_str(strings, value.as_ptr());
            }
            strings
        }
    }

    /// Start `program` with every stdio mode set to `mode`.
    fn start(program: &str, args: &[&str], mode: i32) -> *mut HewProcess {
        let program = ManagedString::new(program);
        let args = string_vec(args);
        // SAFETY: the program and argv stay live through the call.
        unsafe {
            let child = hew_process_start(program.as_ptr(), args, mode, mode, mode);
            crate::vec::hew_vec_free(args);
            child
        }
    }

    /// Block until the child exits and reap it, caching its status.
    fn wait(child: *mut HewProcess) -> (bool, i64) {
        // SAFETY: the test owns this live child.
        unsafe {
            {
                let mut state = (*child).state.lock_or_recover();
                let status = state.child.wait().unwrap();
                state.status = Some(status);
            }
            assert_eq!(hew_process_poll(child), 1);
            (hew_process_signalled(child), hew_process_status(child))
        }
    }

    #[test]
    fn managed_command_and_argv_reject_interior_nul() {
        let cmd = ManagedString::new("unused");
        let nul = ManagedString::new("prefix\0suffix");
        // SAFETY: managed values remain live; no OS call may run.
        unsafe {
            assert!(hew_process_run(nul.as_ptr()).is_null());
            assert!(read_last_error().contains("interior NUL"));
            let args = string_vec(&["prefix\0suffix"]);
            assert!(hew_process_run_argv(cmd.as_ptr(), args).is_null());
            assert!(read_last_error().contains("interior NUL"));
            assert!(hew_process_start(cmd.as_ptr(), args, 0, 0, 0).is_null());
            assert!(read_last_error().contains("interior NUL"));
            crate::vec::hew_vec_free(args);
            assert!(hew_process_start(nul.as_ptr(), std::ptr::null_mut(), 0, 0, 0).is_null());
            assert!(read_last_error().contains("interior NUL"));
        }
    }

    #[test]
    fn descriptor_argv_borrows_strings_and_rejects_interior_nul() {
        let spaced = ManagedString::new("hello world");
        let empty = ManagedString::new("");
        let nul = ManagedString::new("a\0b");
        let layout = crate::vec::HewTypeLayout {
            size: core::mem::size_of::<*const HewString>(),
            align: core::mem::align_of::<*const HewString>(),
            ownership_kind: HewTypeOwnershipKind::String,
        };
        // SAFETY: the String descriptor creates managed handle slots, and
        // each pushed value remains live while its vector owner is retained.
        unsafe {
            let argv = crate::vec::hew_vec_new_with_layout(&raw const layout);
            for value in [spaced.as_ptr(), empty.as_ptr()] {
                crate::vec::hew_vec_push_owned(argv, (&raw const value).cast());
            }
            assert_eq!(
                hewvec_string_args(argv, "test").unwrap(),
                ["hello world", ""]
            );
            let value = nul.as_ptr();
            crate::vec::hew_vec_push_owned(argv, (&raw const value).cast());
            assert!(hewvec_string_args(argv, "test").is_none());
            assert!(read_last_error().contains("interior NUL"));
            crate::vec::hew_vec_free_owned(argv);
            assert_eq!(string_as_str(spaced.as_ptr()), "hello world");
            assert_eq!(string_as_str(nul.as_ptr()), "a\0b");
            crate::hew_clear_error();
        }
    }

    #[test]
    #[cfg(unix)]
    fn captured_output_retains_nul_and_decodes_invalid_utf8_lossily() {
        let cmd = ManagedString::new("printf 'A\\000é中🙂\\377'; printf 'err\\000or' >&2");
        // SAFETY: the command is live; result and accessor owners are released once.
        unsafe {
            let result = hew_process_run(cmd.as_ptr());
            assert!(!result.is_null());
            drop(cmd);
            assert!(!hew_process_result_signalled(result));
            assert_eq!(hew_process_result_status(result), 0);
            let first = hew_process_result_stdout(result);
            let second = hew_process_result_stdout(result);
            let err = hew_process_result_stderr(result);
            hew_process_result_free(result);
            assert_eq!(read_string(first), "A\0é中🙂�");
            string_release(first);
            assert_eq!(read_string(second), "A\0é中🙂�");
            assert_eq!(read_string(err), "err\0or");
            string_release(second);
            string_release(err);
        }
    }

    #[test]
    fn run_reports_the_exit_code() {
        let cmd = ManagedString::new("exit 42");
        // SAFETY: cmd is live; the result is released once.
        unsafe {
            let result = hew_process_run(cmd.as_ptr());
            assert!(!result.is_null());
            assert!(!hew_process_result_signalled(result));
            assert_eq!(hew_process_result_status(result), 42);
            hew_process_result_free(result);
        }
    }

    #[test]
    #[cfg(unix)]
    fn run_reports_a_terminating_signal_apart_from_an_exit_code() {
        let cmd = ManagedString::new("kill -TERM $$");
        // SAFETY: cmd is live; the result is released once.
        unsafe {
            let result = hew_process_run(cmd.as_ptr());
            assert!(!result.is_null());
            assert!(hew_process_result_signalled(result));
            assert_eq!(hew_process_result_status(result), i64::from(libc::SIGTERM));
            hew_process_result_free(result);
        }
    }

    #[test]
    #[cfg(unix)]
    fn run_argv_preserves_spaced_and_empty_arguments() {
        let cmd = ManagedString::new("printf");
        let args = string_vec(&["<%s>|<%s>|<%s>", "hello world", "", "tail"]);
        // SAFETY: cmd and argv are valid handles, released once.
        unsafe {
            let result = hew_process_run_argv(cmd.as_ptr(), args);
            assert!(!result.is_null());
            assert_eq!(read_string((*result).stdout), "<hello world>|<>|<tail>");
            hew_process_result_free(result);
            crate::vec::hew_vec_free(args);
        }
    }

    #[test]
    fn run_argv_rejects_non_string_vec() {
        let cmd = ManagedString::new("printf");
        // SAFETY: an i32 vector is the wrong element kind on purpose.
        unsafe {
            let args = crate::vec::hew_vec_new();
            crate::vec::hew_vec_push_i32(args, 7);
            assert!(hew_process_run_argv(cmd.as_ptr(), args).is_null());
            assert!(read_last_error().contains("Vec<String>"));
            assert!(hew_process_start(cmd.as_ptr(), args, 0, 0, 0).is_null());
            assert!(read_last_error().contains("Vec<String>"));
            crate::vec::hew_vec_free(args);
            crate::hew_clear_error();
        }
    }

    #[test]
    fn missing_executable_is_a_launch_failure_with_detail() {
        let child = start("hew-process-executable-that-does-not-exist", &[], 0);
        assert!(child.is_null());
        // SAFETY: null is accepted by the predicate; the error is this thread's.
        unsafe {
            assert!(!hew_process_is_valid(child));
            let err = read_last_error();
            assert!(
                err.contains("hew-process-executable-that-does-not-exist")
                    && err.contains("failed to execute"),
                "unexpected last error: {err}"
            );
            crate::hew_clear_error();
        }
    }

    #[test]
    fn unknown_stdio_mode_is_refused_before_launch() {
        let program = ManagedString::new("unused");
        // SAFETY: the program is live; no OS call may run.
        unsafe {
            assert!(hew_process_start(program.as_ptr(), std::ptr::null_mut(), 0, 3, 0).is_null());
            assert!(read_last_error().contains("unknown stdio mode 3"));
            crate::hew_clear_error();
        }
    }

    #[test]
    #[cfg(unix)]
    fn wait_reports_the_exit_code_and_caches_the_status() {
        let child = start("sh", &["-c", "exit 7"], 1);
        assert!(!child.is_null());
        assert_eq!(wait(child), (false, 7));
        // SAFETY: the test owns this live child.
        unsafe {
            // A second poll answers from the cached status.
            assert_eq!(hew_process_poll(child), 1);
            assert_eq!(hew_process_status(child), 7);
            // A reaped child is never signalled.
            assert_eq!(hew_process_signal(child, true), 0);
            hew_process_drop(child);
        }
    }

    #[test]
    #[cfg(unix)]
    fn exit_watch_reports_the_exit_without_reaping() {
        let child = start("sleep", &["0.2"], 1);
        assert!(!child.is_null());
        // SAFETY: the test owns this live child.
        let pid = u32::try_from(unsafe { hew_process_id(child) }).unwrap();
        let watch = ExitWatch::open(pid).unwrap();
        assert!(!watch.exited());
        let mut ready = libc::pollfd {
            fd: watch.fd(),
            events: libc::POLLIN,
            revents: 0,
        };
        // SAFETY: one live descriptor; blocks until the child exits.
        assert_eq!(unsafe { libc::poll(&raw mut ready, 1, 10_000) }, 1);
        assert!(watch.exited());
        // SAFETY: the test owns this live child.
        unsafe {
            // The watch reaped nothing: the status is still there to take.
            assert_eq!(hew_process_poll(child), 1);
            assert_eq!(hew_process_status(child), 0);
            // Reaped: a later wait's stream has already ended.
            let pair = hew_process_exit_stream(child);
            assert!(!pair.is_null() && (*pair).sink.is_null());
            assert_eq!(crate::stream::hew_stream_is_closed((*pair).stream), 1);
            crate::stream::hew_stream_pair_free(pair);
            hew_process_drop(child);
        }
    }

    #[test]
    #[cfg(unix)]
    fn terminate_and_kill_report_their_signals() {
        for (force, signal) in [(false, libc::SIGTERM), (true, libc::SIGKILL)] {
            let child = start("sleep", &["60"], 1);
            assert!(!child.is_null());
            // SAFETY: the test owns this live child.
            unsafe {
                assert_eq!(hew_process_poll(child), 0);
                assert_eq!(hew_process_signal(child, force), 0);
            }
            assert_eq!(wait(child), (true, i64::from(signal)));
            // SAFETY: the test owns this live child.
            unsafe { hew_process_drop(child) };
        }
    }

    #[test]
    #[cfg(windows)]
    fn kill_ends_a_running_child() {
        let child = start("ping", &["-n", "61", "127.0.0.1"], 1);
        assert!(!child.is_null());
        // SAFETY: the test owns this live child.
        unsafe {
            assert_eq!(hew_process_signal(child, false), 0);
        }
        let (signalled, code) = wait(child);
        assert!(!signalled && code != 0);
        // SAFETY: the test owns this live child.
        unsafe { hew_process_drop(child) };
    }

    #[test]
    #[cfg(unix)]
    fn dropping_a_running_child_kills_and_reaps_it() {
        let child = start("sleep", &["60"], 1);
        assert!(!child.is_null());
        // SAFETY: the test owns this live child.
        let pid = unsafe { hew_process_id(child) };
        // SAFETY: as above; drop releases it once.
        unsafe { hew_process_drop(child) };
        // Reaped: the ID no longer names a child of this process.
        // SAFETY: waitpid with WNOHANG on a local status.
        let rc = unsafe {
            libc::waitpid(
                libc::pid_t::try_from(pid).unwrap(),
                std::ptr::null_mut(),
                libc::WNOHANG,
            )
        };
        assert_eq!(rc, -1);
        assert_eq!(
            std::io::Error::last_os_error().raw_os_error(),
            Some(libc::ECHILD)
        );
    }

    #[test]
    fn pipes_are_taken_once_and_only_when_piped() {
        #[cfg(windows)]
        let (program, args) = ("cmd", vec!["/C", "exit 0"]);
        #[cfg(not(windows))]
        let (program, args) = ("true", vec![]);
        let inherited = start(program, &args, 1);
        let piped = start(program, &args, 2);
        assert!(!inherited.is_null() && !piped.is_null());
        // SAFETY: the test owns both children and frees each taken pair.
        unsafe {
            for which in 0..3 {
                assert!(hew_process_take_pipe(inherited, which).is_null());
                let pair = hew_process_take_pipe(piped, which);
                assert!(!pair.is_null());
                assert_eq!((*pair).sink.is_null(), which != 0);
                assert_eq!((*pair).stream.is_null(), which == 0);
                drop(Box::from_raw(pair));
                assert!(hew_process_take_pipe(piped, which).is_null());
            }
            hew_process_drop(inherited);
            hew_process_drop(piped);
        }
    }

    #[test]
    fn null_release_is_a_side_effect_free_noop() {
        crate::set_last_error("sentinel process error".to_owned());
        // SAFETY: null is explicitly accepted by all process release functions.
        unsafe {
            hew_process_result_free(std::ptr::null_mut());
            assert_eq!(read_last_error(), "sentinel process error");
            hew_process_drop(std::ptr::null_mut());
            assert_eq!(read_last_error(), "sentinel process error");
        }
        crate::hew_clear_error();
    }
}
