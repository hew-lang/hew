//! Hew runtime: child process spawning and management.
//!
//! Provides process execution (with shell or explicit arguments), spawning,
//! waiting, and killing for compiled Hew programs. Stdout/stderr strings in
//! [`HewProcessResult`] are owned managed UTF-8 handles, preserving embedded NUL.
//! Command and argument inputs borrow managed handles and reject interior NUL
//! before crossing the OS boundary. Captured non-UTF-8 output is decoded lossily.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::util::cstr_to_str;
use crate::vec::{ElemKind, HewVec};
use hew_cabi::string::{
    string_from_str, string_release, string_retain, string_to_cstring, HewString,
};
use std::process::Command;

/// Result of a completed process.
#[derive(Debug)]
pub struct HewProcessResult {
    /// Exit code of the process (or -1 if the process was killed by a signal).
    pub exit_code: i32,
    /// Captured stdout, an owned managed UTF-8 string.
    pub stdout: *mut HewString,
    /// Captured stderr, an owned managed UTF-8 string.
    pub stderr: *mut HewString,
}

/// Handle to a running child process.
pub struct HewProcess {
    inner: std::process::Child,
    reaped: bool,
}

impl std::fmt::Debug for HewProcess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewProcess").finish_non_exhaustive()
    }
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
    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = bytes_to_string(&output.stdout);
    let stderr = bytes_to_string(&output.stderr);
    Box::into_raw(Box::new(HewProcessResult {
        exit_code,
        stdout,
        stderr,
    }))
}

fn mark_reaped(proc: &mut HewProcess) {
    proc.reaped = true;
}

fn reap_process_for_drop(proc: &mut HewProcess) {
    if proc.reaped {
        crate::hew_clear_error();
        return;
    }

    match proc.inner.try_wait() {
        Ok(Some(_status)) => {
            mark_reaped(proc);
            crate::hew_clear_error();
            return;
        }
        Ok(None) => {}
        Err(error) => {
            crate::set_last_error(format!("hew_process_drop: {error}"));
        }
    }

    let kill_error = proc.inner.kill().err();
    match proc.inner.wait() {
        Ok(_status) => {
            mark_reaped(proc);
            crate::hew_clear_error();
        }
        Err(wait_error) => {
            if let Some(kill_error) = kill_error {
                crate::set_last_error(format!(
                    "hew_process_drop: kill failed: {kill_error}; wait failed: {wait_error}"
                ));
            } else {
                crate::set_last_error(format!("hew_process_drop: {wait_error}"));
            }
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
    if args_ref.elem_kind != ElemKind::String {
        crate::set_last_error(format!("{context}: args must be Vec<String>"));
        return None;
    }

    let mut owned_args = Vec::with_capacity(args_ref.len);
    for index in 0..args_ref.len {
        let Ok(index_i64) = i64::try_from(index) else {
            crate::set_last_error(format!("{context}: args length exceeds Hew index range"));
            return None;
        };
        // SAFETY: index_i64 was derived from an in-bounds usize index; get_str
        // returns a retained header-aware String owner for this slot.
        let raw_arg = unsafe { crate::vec::hew_vec_get_str(arg_vec, index_i64) };
        // SAFETY: raw_arg is the retained managed owner returned by get_str.
        let arg = unsafe { process_input(raw_arg, context) };
        // SAFETY: raw_arg is a retained owner and must be released here.
        unsafe { string_release(raw_arg.cast_mut()) };
        owned_args.push(arg?);
    }

    Some(owned_args)
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Build a [`Command`] that runs `cmd_str` through the platform's system shell:
/// `sh -c "cmd"` on Unix, `cmd /C "cmd"` on Windows. Centralized so the
/// shell-based run/spawn entry points stay portable.
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

/// Run a command with an explicit argument array (no shell).
///
/// Returns a heap-allocated [`HewProcessResult`], or null on error.
/// The caller must free the result with [`hew_process_result_free`].
///
/// # Safety
///
/// `cmd` must be a live managed handle, or null (empty).
/// `args` must point to an array of `argc` managed string handle
/// pointers. `argc` must be >= 0.
#[expect(
    clippy::similar_names,
    reason = "argc/args and arg_ptr/arg_str are standard C conventions"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_process_run_args(
    cmd: *const HewString,
    args: *const *const HewString,
    argc: i32,
) -> *mut HewProcessResult {
    if argc < 0 {
        crate::set_last_error("hew_process_run_args: argc must be non-negative");
        return std::ptr::null_mut();
    }
    // SAFETY: cmd is a borrowed managed handle at this ABI boundary.
    let Some(cmd_str) = (unsafe { process_input(cmd, "hew_process_run_args") }) else {
        return std::ptr::null_mut();
    };

    let mut command = Command::new(&cmd_str);

    if argc > 0 {
        if args.is_null() {
            crate::set_last_error("hew_process_run_args: args is null while argc > 0");
            return std::ptr::null_mut();
        }
        #[expect(clippy::cast_sign_loss, reason = "guarded by argc >= 0 above")]
        let arg_count = argc as usize;
        for index in 0..arg_count {
            // SAFETY: args[i] is a valid pointer per caller contract, within the
            // bounds of the args array of length argc.
            let arg_ptr = unsafe { *args.add(index) };
            let arg_context = format!("hew_process_run_args: args[{index}]");
            // SAFETY: arg_ptr comes from the caller-provided args array.
            let Some(arg_str) = (unsafe { process_input(arg_ptr, &arg_context) }) else {
                return std::ptr::null_mut();
            };
            command.arg(arg_str);
        }
    }

    command_output_to_result(&mut command, "hew_process_run_args", &cmd_str)
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

/// Spawn a command via the system shell (`sh -c "cmd"` on Unix, `cmd /C "cmd"`
/// on Windows) without waiting.
///
/// Returns a heap-allocated [`HewProcess`] handle, or null on error.
/// The caller must free the handle with [`hew_process_free`].
///
/// # Safety
///
/// `cmd` must be a live managed handle, or null (empty).
#[no_mangle]
pub unsafe extern "C" fn hew_process_spawn(cmd: *const HewString) -> *mut HewProcess {
    // SAFETY: cmd is a borrowed managed handle at this ABI boundary.
    let Some(cmd_str) = (unsafe { process_input(cmd, "hew_process_spawn") }) else {
        return std::ptr::null_mut();
    };
    match shell_command(&cmd_str).spawn() {
        Ok(child) => {
            crate::hew_clear_error();
            Box::into_raw(Box::new(HewProcess {
                inner: child,
                reaped: false,
            }))
        }
        Err(error) => {
            crate::set_last_error(format!(
                "hew_process_spawn: failed to execute '{cmd_str}': {error}"
            ));
            std::ptr::null_mut()
        }
    }
}

/// Report whether `proc` is a live child handle.
///
/// [`hew_process_spawn`] and [`hew_process_spawn_argv`] return null when the
/// launch fails; this predicate is how a caller tells a launched child apart
/// from a failure before treating the handle as a resource.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_process_is_valid(proc: *mut HewProcess) -> bool {
    !proc.is_null()
}

/// Spawn a command directly (no shell) with an explicit argv vector, without
/// waiting.
///
/// Returns a heap-allocated [`HewProcess`] handle, or null on error with the
/// failure detail recorded in the runtime's last-error slot.  The caller must
/// free the handle with [`hew_process_drop`].
///
/// # Safety
///
/// `cmd` must be a live managed handle, or null (empty).  `argv_vec` must be
/// null or a valid `Vec<String>`-backed [`HewVec`].
#[no_mangle]
pub unsafe extern "C" fn hew_process_spawn_argv(
    cmd: *const HewString,
    argv_vec: *mut HewVec,
) -> *mut HewProcess {
    // SAFETY: cmd is a borrowed managed handle at this ABI boundary.
    let Some(cmd_str) = (unsafe { process_input(cmd, "hew_process_spawn_argv") }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: argv_vec is either null or a valid Vec<String>-backed HewVec.
    let Some(owned_args) = (unsafe { hewvec_string_args(argv_vec, "hew_process_spawn_argv") })
    else {
        return std::ptr::null_mut();
    };

    let mut command = Command::new(&cmd_str);
    command.args(owned_args);
    match command.spawn() {
        Ok(child) => {
            crate::hew_clear_error();
            Box::into_raw(Box::new(HewProcess {
                inner: child,
                reaped: false,
            }))
        }
        Err(error) => {
            crate::set_last_error(format!(
                "hew_process_spawn_argv: failed to execute '{cmd_str}': {error}"
            ));
            std::ptr::null_mut()
        }
    }
}

/// Wait for a spawned process to finish.
///
/// Returns the exit code, or `-1` on error.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_process_wait(proc: *mut HewProcess) -> i32 {
    cabi_guard!(proc.is_null(), -1);
    // SAFETY: proc is a valid HewProcess pointer per caller contract.
    let p = unsafe { &mut *proc };
    match p.inner.wait() {
        Ok(status) => {
            mark_reaped(p);
            crate::hew_clear_error();
            status.code().unwrap_or(-1)
        }
        Err(error) => {
            crate::set_last_error(format!("hew_process_wait: {error}"));
            -1
        }
    }
}

/// Reap a [`HewProcess`] handle at scope exit.
///
/// If the child is still running, this kills it and waits for exit before
/// releasing the owned handle. If it has already exited, this performs the
/// final reap without sending a signal.
///
/// # Safety
///
/// `p` must be a pointer previously returned by [`hew_process_spawn`],
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_process_drop(p: *mut HewProcess) {
    if p.is_null() {
        return;
    }
    // SAFETY: p was allocated with Box::into_raw and has not been freed.
    let mut proc = unsafe { Box::from_raw(p) };
    reap_process_for_drop(&mut proc);
}

/// Kill a spawned process.
///
/// Returns `0` on success, `-1` on error.
///
/// # Safety
///
/// `proc` must be a valid pointer to a [`HewProcess`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_process_kill(proc: *mut HewProcess) -> i32 {
    cabi_guard!(proc.is_null(), -1);
    // SAFETY: proc is a valid HewProcess pointer per caller contract.
    let p = unsafe { &mut *proc };
    match p.inner.kill() {
        Ok(()) => {
            crate::hew_clear_error();
            0
        }
        Err(error) => {
            crate::set_last_error(format!("hew_process_kill: {error}"));
            -1
        }
    }
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

/// Return the exit code from a completed process result.
///
/// # Safety
///
/// `r` must be a valid pointer returned by a `hew_process_run*` function.
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_exit_code(r: *const HewProcessResult) -> i32 {
    cabi_guard!(r.is_null(), -1);
    crate::hew_clear_error();
    // SAFETY: r is valid per caller contract.
    unsafe { (*r).exit_code }
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

/// Free a [`HewProcessResult`] previously returned by [`hew_process_run`]
/// or [`hew_process_run_args`], including its owned managed stdout and
/// stderr strings.
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

/// Free a [`HewProcess`] handle previously returned by [`hew_process_spawn`].
///
/// # Safety
///
/// `p` must be a pointer previously returned by [`hew_process_spawn`],
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_process_free(p: *mut HewProcess) {
    // SAFETY: same contract as `hew_process_drop`; preserve the legacy symbol as
    // the canonical release path for raw FFI callers.
    unsafe { hew_process_drop(p) };
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
        // SAFETY: ptr is a live managed string owned by the runtime.
        unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned()
    }

    #[test]
    fn managed_command_and_both_argv_forms_reject_interior_nul() {
        let cmd = ManagedString::new("unused");
        let nul = ManagedString::new("prefix\0suffix");
        // SAFETY: managed values and the array remain live; no OS call may run.
        unsafe {
            assert!(hew_process_run(nul.as_ptr()).is_null());
            assert!(read_last_error().contains("interior NUL"));
            assert!(hew_process_spawn(nul.as_ptr()).is_null());
            assert!(read_last_error().contains("interior NUL"));
            let raw_array = [nul.as_ptr()];
            assert!(hew_process_run_args(cmd.as_ptr(), raw_array.as_ptr(), 1).is_null());
            assert!(read_last_error().contains("args[0]: input contains interior NUL"));
            let argv = crate::vec::hew_vec_new_str();
            crate::vec::hew_vec_push_str(argv, nul.as_ptr());
            assert!(hew_process_run_argv(cmd.as_ptr(), argv).is_null());
            assert!(read_last_error().contains("interior NUL"));
            assert!(hew_process_spawn_argv(cmd.as_ptr(), argv).is_null());
            assert!(read_last_error().contains("interior NUL"));
            crate::vec::hew_vec_free(argv);
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
            assert_eq!(hew_process_result_exit_code(result), 0);
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
    fn run_echo_command() {
        let cmd = ManagedString::new("echo hello");
        // SAFETY: cmd is a live managed string.
        let result = unsafe { hew_process_run(cmd.as_ptr()) };
        assert!(!result.is_null());

        // SAFETY: result is a valid HewProcessResult.
        unsafe {
            let r = &*result;
            assert_eq!(r.exit_code, 0);
            let stdout = read_string(r.stdout);
            assert_eq!(stdout.trim(), "hello");
            hew_process_result_free(result);
        }
    }

    #[test]
    fn run_exit_code() {
        let cmd = ManagedString::new("exit 42");
        // SAFETY: cmd is a live managed string.
        let result = unsafe { hew_process_run(cmd.as_ptr()) };
        assert!(!result.is_null());

        // SAFETY: result is a valid HewProcessResult.
        unsafe {
            let r = &*result;
            assert_eq!(r.exit_code, 42);
            hew_process_result_free(result);
        }
    }

    #[test]
    #[cfg(unix)]
    fn run_args_echo() {
        let cmd = ManagedString::new("echo");
        let first_arg = ManagedString::new("hello");
        let second_arg = ManagedString::new("world");
        let args = [first_arg.as_ptr(), second_arg.as_ptr()];

        // SAFETY: cmd and args are live managed strings.
        let result = unsafe { hew_process_run_args(cmd.as_ptr(), args.as_ptr(), 2) };
        assert!(!result.is_null());

        // SAFETY: result is a valid HewProcessResult.
        unsafe {
            let r = &*result;
            assert_eq!(r.exit_code, 0);
            let stdout = read_string(r.stdout);
            assert_eq!(stdout.trim(), "hello world");
            hew_process_result_free(result);
        }
    }

    #[test]
    #[cfg(unix)]
    fn run_argv_preserves_spaced_and_empty_arguments() {
        let cmd = ManagedString::new("printf");
        let fmt = hew_cabi::string::string_from_str("<%s>|<%s>|<%s>");
        let spaced = hew_cabi::string::string_from_str("hello world");
        let empty = hew_cabi::string::string_from_str("");
        let tail = hew_cabi::string::string_from_str("tail");
        // SAFETY: hew_vec_new_str allocates a valid Vec<String> handle.
        let argv = unsafe { crate::vec::hew_vec_new_str() };

        // SAFETY: argv is a valid string vec and all managed handles are live.
        unsafe {
            crate::vec::hew_vec_push_str(argv, fmt);
            crate::vec::hew_vec_push_str(argv, spaced);
            crate::vec::hew_vec_push_str(argv, empty);
            crate::vec::hew_vec_push_str(argv, tail);
            hew_cabi::string::string_release(fmt);
            hew_cabi::string::string_release(spaced);
            hew_cabi::string::string_release(empty);
            hew_cabi::string::string_release(tail);
        }

        // SAFETY: cmd and argv are valid handles for the C ABI.
        let result = unsafe { hew_process_run_argv(cmd.as_ptr(), argv) };
        assert!(!result.is_null());

        // SAFETY: result is a valid HewProcessResult and argv must be released afterwards.
        unsafe {
            let r = &*result;
            assert_eq!(r.exit_code, 0);
            let stdout = read_string(r.stdout);
            assert_eq!(stdout, "<hello world>|<>|<tail>");
            hew_process_result_free(result);
            crate::vec::hew_vec_free(argv);
        }
    }

    #[test]
    fn run_argv_rejects_non_string_vec() {
        let cmd = ManagedString::new("printf");
        // SAFETY: hew_vec_new allocates a valid i32 vec.
        let argv = unsafe { crate::vec::hew_vec_new() };

        // SAFETY: argv is a valid vec and 7 is just placeholder data.
        unsafe {
            crate::vec::hew_vec_push_i32(argv, 7);
            crate::hew_clear_error();
        }

        // SAFETY: cmd is valid and argv is intentionally the wrong element kind.
        let result = unsafe { hew_process_run_argv(cmd.as_ptr(), argv) };
        assert!(result.is_null());

        // SAFETY: last error is set by hew_process_run_argv on this thread.
        unsafe {
            let err = read_last_error();
            assert!(err.contains("Vec<String>"), "unexpected last error: {err}");
            crate::vec::hew_vec_free(argv);
            crate::hew_clear_error();
        }
    }

    #[test]
    fn run_argv_missing_command_surfaces_error() {
        let cmd = ManagedString::new("hew-process-command-that-does-not-exist");
        // SAFETY: hew_vec_new_str allocates a valid empty Vec<String>.
        let argv = unsafe { crate::vec::hew_vec_new_str() };

        // SAFETY: cmd and argv are valid handles.
        crate::hew_clear_error();
        // SAFETY: cmd and argv are valid handles for the C ABI.
        let result = unsafe { hew_process_run_argv(cmd.as_ptr(), argv) };
        assert!(result.is_null());

        // SAFETY: last error is set by hew_process_run_argv on this thread.
        unsafe {
            let err = read_last_error();
            assert!(
                err.contains("hew-process-command-that-does-not-exist"),
                "unexpected last error: {err}"
            );
            crate::vec::hew_vec_free(argv);
            crate::hew_clear_error();
        }
    }

    #[test]
    fn spawn_and_wait() {
        let cmd = ManagedString::new("echo spawned");
        // SAFETY: cmd is a live managed string.
        let proc = unsafe { hew_process_spawn(cmd.as_ptr()) };
        assert!(!proc.is_null());

        // SAFETY: proc is a valid HewProcess.
        unsafe {
            let exit_code = hew_process_wait(proc);
            assert_eq!(exit_code, 0);
            hew_process_free(proc);
        }
    }

    #[test]
    fn spawn_and_kill() {
        // Target the long-lived child directly so killing it also closes the
        // inherited test output pipes; killing a shell can orphan its child.
        #[cfg(windows)]
        let (command, arguments) = ("ping", vec!["-n", "61", "127.0.0.1"]);
        #[cfg(not(windows))]
        let (command, arguments) = ("sleep", vec!["60"]);
        let cmd = ManagedString::new(command);
        // SAFETY: argv retains each live managed input until it is released.
        let proc = unsafe {
            let argv = crate::vec::hew_vec_new_str();
            for argument in arguments {
                let value = ManagedString::new(argument);
                crate::vec::hew_vec_push_str(argv, value.as_ptr());
            }
            let proc = hew_process_spawn_argv(cmd.as_ptr(), argv);
            crate::vec::hew_vec_free(argv);
            proc
        };
        assert!(!proc.is_null());

        // SAFETY: proc is a valid HewProcess.
        unsafe {
            let kill_rc = hew_process_kill(proc);
            assert_eq!(kill_rc, 0);
            // After killing, wait should return a non-zero/signal exit code.
            let exit_code = hew_process_wait(proc);
            assert_ne!(exit_code, 0);
            hew_process_free(proc);
        }
    }

    // #16: a failed launch produced a null handle that the Hew `Child`
    // resource wrapped and presented as a live child. `hew_process_is_valid`
    // is the authority that keeps a failed launch out of the Ok path.
    #[test]
    fn spawn_argv_missing_executable_is_invalid_with_detail() {
        let cmd = ManagedString::new("hew-process-executable-that-does-not-exist");
        // SAFETY: hew_vec_new_str allocates a valid empty Vec<String>.
        let argv = unsafe { crate::vec::hew_vec_new_str() };
        // SAFETY: cmd and argv are valid handles for the C ABI.
        let proc = unsafe { hew_process_spawn_argv(cmd.as_ptr(), argv) };
        assert!(proc.is_null());
        // SAFETY: null is explicitly allowed by hew_process_is_valid.
        assert!(!unsafe { hew_process_is_valid(proc) });
        // SAFETY: last error is set by hew_process_spawn_argv on this thread.
        unsafe {
            let err = read_last_error();
            assert!(
                err.contains("hew-process-executable-that-does-not-exist")
                    && err.contains("failed to execute"),
                "unexpected last error: {err}"
            );
            crate::vec::hew_vec_free(argv);
            crate::hew_clear_error();
        }
    }

    #[test]
    #[cfg(unix)]
    fn spawn_argv_launches_and_waits() {
        let cmd = ManagedString::new("echo");
        let arg = hew_cabi::string::string_from_str("spawned-argv");
        // SAFETY: hew_vec_new_str allocates a valid Vec<String> handle.
        let argv = unsafe { crate::vec::hew_vec_new_str() };
        // SAFETY: argv is a valid string vec and arg is a valid C string.
        unsafe {
            crate::vec::hew_vec_push_str(argv, arg);
            hew_cabi::string::string_release(arg);
        };
        // SAFETY: cmd and argv are valid handles for the C ABI.
        let proc = unsafe { hew_process_spawn_argv(cmd.as_ptr(), argv) };
        assert!(!proc.is_null());
        // SAFETY: proc is a live HewProcess and argv must be released afterwards.
        unsafe {
            assert!(hew_process_is_valid(proc));
            assert_eq!(hew_process_wait(proc), 0);
            hew_process_free(proc);
            crate::vec::hew_vec_free(argv);
        }
    }

    #[test]
    fn null_handling() {
        // SAFETY: null pointers are explicitly handled by all functions.
        unsafe {
            let result = hew_process_run(std::ptr::null());
            assert!(!result.is_null());
            assert_eq!(hew_process_result_exit_code(result), 0);
            hew_process_result_free(result);
            assert!(hew_process_run_args(std::ptr::null(), std::ptr::null(), 0).is_null());
            assert!(hew_process_run_argv(std::ptr::null(), std::ptr::null_mut()).is_null());
            let child = hew_process_spawn(std::ptr::null());
            assert!(!child.is_null());
            assert_eq!(hew_process_wait(child), 0);
            hew_process_free(child);
            assert!(hew_process_spawn_argv(std::ptr::null(), std::ptr::null_mut()).is_null());
            assert!(!hew_process_is_valid(std::ptr::null_mut()));
            assert!(!hew_process_result_is_valid(std::ptr::null()));
            assert_eq!(hew_process_wait(std::ptr::null_mut()), -1);
            assert_eq!(hew_process_kill(std::ptr::null_mut()), -1);
            hew_process_result_free(std::ptr::null_mut());
            hew_process_free(std::ptr::null_mut());
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
            hew_process_free(std::ptr::null_mut());
            assert_eq!(read_last_error(), "sentinel process error");
        }

        crate::hew_clear_error();
    }
}
