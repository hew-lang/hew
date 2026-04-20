//! Hew runtime: child process spawning and management.
//!
//! Provides process execution (with shell or explicit arguments), spawning,
//! waiting, and killing for compiled Hew programs. Stdout/stderr strings in
//! [`HewProcessResult`] are allocated with `libc::malloc` and NUL-terminated.

use crate::vec::{ElemKind, HewVec};
use crate::{cabi::str_to_malloc, util::cstr_to_str};
use std::os::raw::c_char;
use std::process::Command;

/// Result of a completed process.
#[derive(Debug)]
pub struct HewProcessResult {
    /// Exit code of the process (or -1 if the process was killed by a signal).
    pub exit_code: i32,
    /// Captured stdout, malloc-allocated and NUL-terminated.
    pub stdout: *mut c_char,
    /// Captured stderr, malloc-allocated and NUL-terminated.
    pub stderr: *mut c_char,
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

/// Convert a byte slice to a malloc-allocated C string, replacing invalid UTF-8
/// with the replacement character.
fn bytes_to_malloc(bytes: &[u8]) -> *mut c_char {
    let s = String::from_utf8_lossy(bytes);
    str_to_malloc(&s)
}

/// Free a C string allocated by `strdup`/`malloc` if present.
unsafe fn free_c_string(ptr: *const c_char) {
    if !ptr.is_null() {
        // SAFETY: ptr was allocated by strdup/malloc and is owned by the caller.
        unsafe { libc::free(ptr.cast_mut().cast()) };
    }
}

/// Build a [`HewProcessResult`] from an [`std::process::Output`].
#[expect(
    clippy::needless_pass_by_value,
    reason = "Output is consumed to extract owned fields"
)]
fn output_to_result(output: std::process::Output) -> *mut HewProcessResult {
    let exit_code = output.status.code().unwrap_or(-1);
    let stdout = bytes_to_malloc(&output.stdout);
    let stderr = bytes_to_malloc(&output.stderr);
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
        // SAFETY: index_i64 was derived from an in-bounds usize index and get_str
        // returns an owned strdup of the string element.
        let raw_arg = unsafe { crate::vec::hew_vec_get_str(arg_vec, index_i64) };
        let arg_context = format!("{context}: args[{index}]");
        // SAFETY: raw_arg is the strdup returned by hew_vec_get_str for this slot.
        let Some(arg_text) = (unsafe { cstr_to_str(&raw_arg, &arg_context) }) else {
            // SAFETY: raw_arg came from hew_vec_get_str and must be released here.
            unsafe { free_c_string(raw_arg) };
            return None;
        };
        owned_args.push(arg_text.to_owned());
        // SAFETY: raw_arg came from hew_vec_get_str and must be released here.
        unsafe { free_c_string(raw_arg) };
    }

    Some(owned_args)
}

/// Clone a result string field into a new malloc-owned C string.
unsafe fn clone_result_string(ptr: *const c_char, context: &str) -> *mut c_char {
    // SAFETY: ptr is expected to reference a valid NUL-terminated result field.
    let Some(text) = (unsafe { cstr_to_str(&ptr, context) }) else {
        return std::ptr::null_mut();
    };
    crate::hew_clear_error();
    str_to_malloc(text)
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Run a command via the system shell (`sh -c "cmd"`) and wait for completion.
///
/// Returns a heap-allocated [`HewProcessResult`], or null on error.
/// The caller must free the result with [`hew_process_result_free`].
///
/// # Safety
///
/// `cmd` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_process_run(cmd: *const c_char) -> *mut HewProcessResult {
    // SAFETY: cmd is a caller-provided C string at this ABI boundary.
    let Some(cmd_str) = (unsafe { cstr_to_str(&cmd, "hew_process_run") }) else {
        return std::ptr::null_mut();
    };
    let mut command = Command::new("sh");
    command.arg("-c").arg(cmd_str);
    command_output_to_result(&mut command, "hew_process_run", cmd_str)
}

/// Run a command with an explicit argument array (no shell).
///
/// Returns a heap-allocated [`HewProcessResult`], or null on error.
/// The caller must free the result with [`hew_process_result_free`].
///
/// # Safety
///
/// `cmd` must be a valid NUL-terminated C string, or null.
/// `args` must point to an array of `argc` valid NUL-terminated C string
/// pointers. `argc` must be >= 0.
#[expect(
    clippy::similar_names,
    reason = "argc/args and arg_ptr/arg_str are standard C conventions"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_process_run_args(
    cmd: *const c_char,
    args: *const *const c_char,
    argc: i32,
) -> *mut HewProcessResult {
    if argc < 0 {
        crate::set_last_error("hew_process_run_args: argc must be non-negative");
        return std::ptr::null_mut();
    }
    // SAFETY: cmd is a caller-provided C string at this ABI boundary.
    let Some(cmd_str) = (unsafe { cstr_to_str(&cmd, "hew_process_run_args") }) else {
        return std::ptr::null_mut();
    };

    let mut command = Command::new(cmd_str);

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
            let Some(arg_str) = (unsafe { cstr_to_str(&arg_ptr, &arg_context) }) else {
                return std::ptr::null_mut();
            };
            command.arg(arg_str);
        }
    }

    command_output_to_result(&mut command, "hew_process_run_args", cmd_str)
}

/// Run a command with an explicit `Vec<String>` argv surface (no shell).
///
/// Returns a heap-allocated [`HewProcessResult`], or null on error.
/// The caller must free the result with [`hew_process_result_free`].
///
/// # Safety
///
/// `cmd` must be a valid NUL-terminated C string, or null. `args` must be a
/// valid `Vec<String>` handle or null (treated as an empty argv).
#[no_mangle]
pub unsafe extern "C" fn hew_process_run_argv(
    cmd: *const c_char,
    argv_vec: *mut HewVec,
) -> *mut HewProcessResult {
    // SAFETY: cmd is a caller-provided C string at this ABI boundary.
    let Some(cmd_str) = (unsafe { cstr_to_str(&cmd, "hew_process_run_argv") }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: argv_vec is either null or a valid Vec<String>-backed HewVec.
    let Some(owned_args) = (unsafe { hewvec_string_args(argv_vec, "hew_process_run_argv") }) else {
        return std::ptr::null_mut();
    };

    let mut command = Command::new(cmd_str);
    command.args(owned_args);
    command_output_to_result(&mut command, "hew_process_run_argv", cmd_str)
}

/// Spawn a command via the system shell (`sh -c "cmd"`) without waiting.
///
/// Returns a heap-allocated [`HewProcess`] handle, or null on error.
/// The caller must free the handle with [`hew_process_free`].
///
/// # Safety
///
/// `cmd` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_process_spawn(cmd: *const c_char) -> *mut HewProcess {
    // SAFETY: cmd is a caller-provided C string at this ABI boundary.
    let Some(cmd_str) = (unsafe { cstr_to_str(&cmd, "hew_process_spawn") }) else {
        return std::ptr::null_mut();
    };
    match Command::new("sh").arg("-c").arg(cmd_str).spawn() {
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
    cabi_guard!(p.is_null());
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

/// Return a malloc-owned copy of the current thread's last process error.
#[no_mangle]
pub extern "C" fn hew_process_last_error() -> *mut c_char {
    let ptr = crate::hew_last_error();
    if ptr.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: ptr comes from thread-local storage and remains valid until the
    // next error mutation; we duplicate it immediately.
    let Some(text) = (unsafe { cstr_to_str(&ptr, "hew_process_last_error") }) else {
        return std::ptr::null_mut();
    };
    str_to_malloc(text)
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

/// Clone the stdout field from a completed process result.
///
/// # Safety
///
/// `r` must be a valid pointer returned by a `hew_process_run*` function.
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_stdout(r: *const HewProcessResult) -> *mut c_char {
    cabi_guard!(r.is_null(), std::ptr::null_mut());
    // SAFETY: r is valid per caller contract.
    unsafe { clone_result_string((*r).stdout.cast_const(), "hew_process_result_stdout") }
}

/// Clone the stderr field from a completed process result.
///
/// # Safety
///
/// `r` must be a valid pointer returned by a `hew_process_run*` function.
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_stderr(r: *const HewProcessResult) -> *mut c_char {
    cabi_guard!(r.is_null(), std::ptr::null_mut());
    // SAFETY: r is valid per caller contract.
    unsafe { clone_result_string((*r).stderr.cast_const(), "hew_process_result_stderr") }
}

/// Free a [`HewProcessResult`] previously returned by [`hew_process_run`]
/// or [`hew_process_run_args`], including the malloc-allocated stdout and
/// stderr strings.
///
/// # Safety
///
/// `r` must be a pointer previously returned by a `hew_process_run*` function,
/// and must not have been freed already. Null is accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_process_result_free(r: *mut HewProcessResult) {
    cabi_guard!(r.is_null());
    // SAFETY: r was allocated with Box::into_raw and has not been freed.
    let result = unsafe { Box::from_raw(r) };
    if !result.stdout.is_null() {
        // SAFETY: stdout was allocated with libc::malloc.
        unsafe { libc::free(result.stdout.cast()) };
    }
    if !result.stderr.is_null() {
        // SAFETY: stderr was allocated with libc::malloc.
        unsafe { libc::free(result.stderr.cast()) };
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
    use std::ffi::{CStr, CString};

    /// Helper: read a C string pointer without freeing it.
    ///
    /// # Safety
    ///
    /// `ptr` must be a non-null, NUL-terminated C string.
    unsafe fn read_cstr(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string.
        unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned()
    }

    /// Helper: read the thread-local last error string.
    ///
    /// # Safety
    ///
    /// The runtime must have already populated `hew_last_error()` for this thread.
    unsafe fn read_last_error() -> String {
        let ptr = crate::hew_last_error();
        assert!(!ptr.is_null(), "expected hew_last_error to be populated");
        // SAFETY: ptr is a valid NUL-terminated C string owned by the runtime.
        unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned()
    }

    #[test]
    fn run_echo_command() {
        let cmd = CString::new("echo hello").unwrap();
        // SAFETY: cmd is a valid NUL-terminated C string.
        let result = unsafe { hew_process_run(cmd.as_ptr()) };
        assert!(!result.is_null());

        // SAFETY: result is a valid HewProcessResult.
        unsafe {
            let r = &*result;
            assert_eq!(r.exit_code, 0);
            let stdout = read_cstr(r.stdout);
            assert_eq!(stdout.trim(), "hello");
            hew_process_result_free(result);
        }
    }

    #[test]
    fn run_exit_code() {
        let cmd = CString::new("exit 42").unwrap();
        // SAFETY: cmd is a valid NUL-terminated C string.
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
    fn run_args_echo() {
        let cmd = CString::new("echo").unwrap();
        let first_arg = CString::new("hello").unwrap();
        let second_arg = CString::new("world").unwrap();
        let args = [first_arg.as_ptr(), second_arg.as_ptr()];

        // SAFETY: cmd and args are valid NUL-terminated C strings.
        let result = unsafe { hew_process_run_args(cmd.as_ptr(), args.as_ptr(), 2) };
        assert!(!result.is_null());

        // SAFETY: result is a valid HewProcessResult.
        unsafe {
            let r = &*result;
            assert_eq!(r.exit_code, 0);
            let stdout = read_cstr(r.stdout);
            assert_eq!(stdout.trim(), "hello world");
            hew_process_result_free(result);
        }
    }

    #[test]
    fn run_argv_preserves_spaced_and_empty_arguments() {
        let cmd = CString::new("printf").unwrap();
        let fmt = CString::new("<%s>|<%s>|<%s>").unwrap();
        let spaced = CString::new("hello world").unwrap();
        let empty = CString::new("").unwrap();
        let tail = CString::new("tail").unwrap();
        // SAFETY: hew_vec_new_str allocates a valid Vec<String> handle.
        let argv = unsafe { crate::vec::hew_vec_new_str() };

        // SAFETY: argv is a valid string vec and all CString pointers are valid.
        unsafe {
            crate::vec::hew_vec_push_str(argv, fmt.as_ptr());
            crate::vec::hew_vec_push_str(argv, spaced.as_ptr());
            crate::vec::hew_vec_push_str(argv, empty.as_ptr());
            crate::vec::hew_vec_push_str(argv, tail.as_ptr());
        }

        // SAFETY: cmd and argv are valid handles for the C ABI.
        let result = unsafe { hew_process_run_argv(cmd.as_ptr(), argv) };
        assert!(!result.is_null());

        // SAFETY: result is a valid HewProcessResult and argv must be released afterwards.
        unsafe {
            let r = &*result;
            assert_eq!(r.exit_code, 0);
            let stdout = read_cstr(r.stdout);
            assert_eq!(stdout, "<hello world>|<>|<tail>");
            hew_process_result_free(result);
            crate::vec::hew_vec_free(argv);
        }
    }

    #[test]
    fn run_argv_rejects_non_string_vec() {
        let cmd = CString::new("printf").unwrap();
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
        let cmd = CString::new("hew-process-command-that-does-not-exist").unwrap();
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
        let cmd = CString::new("echo spawned").unwrap();
        // SAFETY: cmd is a valid NUL-terminated C string.
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
        let cmd = CString::new("sleep 60").unwrap();
        // SAFETY: cmd is a valid NUL-terminated C string.
        let proc = unsafe { hew_process_spawn(cmd.as_ptr()) };
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

    #[test]
    fn null_handling() {
        // SAFETY: null pointers are explicitly handled by all functions.
        unsafe {
            assert!(hew_process_run(std::ptr::null()).is_null());
            assert!(hew_process_run_args(std::ptr::null(), std::ptr::null(), 0).is_null());
            assert!(hew_process_run_argv(std::ptr::null(), std::ptr::null_mut()).is_null());
            assert!(hew_process_spawn(std::ptr::null()).is_null());
            assert!(!hew_process_result_is_valid(std::ptr::null()));
            assert_eq!(hew_process_wait(std::ptr::null_mut()), -1);
            assert_eq!(hew_process_kill(std::ptr::null_mut()), -1);
            hew_process_result_free(std::ptr::null_mut());
            hew_process_free(std::ptr::null_mut());
        }
    }
}
