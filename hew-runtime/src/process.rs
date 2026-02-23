//! Hew runtime: child process spawning and management.
//!
//! Provides process execution (with shell or explicit arguments), spawning,
//! waiting, and killing for compiled Hew programs. Stdout/stderr strings in
//! [`HewProcessResult`] are allocated with `libc::malloc` and NUL-terminated.

use crate::cabi::str_to_malloc;
use std::ffi::CStr;
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
    if cmd.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: cmd is a valid NUL-terminated C string per caller contract.
    let Ok(cmd_str) = unsafe { CStr::from_ptr(cmd) }.to_str() else {
        return std::ptr::null_mut();
    };
    match Command::new("sh").arg("-c").arg(cmd_str).output() {
        Ok(output) => output_to_result(output),
        Err(_) => std::ptr::null_mut(),
    }
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
    if cmd.is_null() || argc < 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: cmd is a valid NUL-terminated C string per caller contract.
    let Ok(cmd_str) = unsafe { CStr::from_ptr(cmd) }.to_str() else {
        return std::ptr::null_mut();
    };

    let mut command = Command::new(cmd_str);

    if argc > 0 {
        if args.is_null() {
            return std::ptr::null_mut();
        }
        #[expect(clippy::cast_sign_loss, reason = "guarded by argc >= 0 above")]
        let arg_count = argc as usize;
        for i in 0..arg_count {
            // SAFETY: args[i] is a valid pointer per caller contract, within the
            // bounds of the args array of length argc.
            let arg_ptr = unsafe { *args.add(i) };
            if arg_ptr.is_null() {
                return std::ptr::null_mut();
            }
            // SAFETY: arg_ptr is a valid NUL-terminated C string per caller contract.
            let Ok(arg_str) = unsafe { CStr::from_ptr(arg_ptr) }.to_str() else {
                return std::ptr::null_mut();
            };
            command.arg(arg_str);
        }
    }

    match command.output() {
        Ok(output) => output_to_result(output),
        Err(_) => std::ptr::null_mut(),
    }
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
    if cmd.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: cmd is a valid NUL-terminated C string per caller contract.
    let Ok(cmd_str) = unsafe { CStr::from_ptr(cmd) }.to_str() else {
        return std::ptr::null_mut();
    };
    match Command::new("sh").arg("-c").arg(cmd_str).spawn() {
        Ok(child) => Box::into_raw(Box::new(HewProcess { inner: child })),
        Err(_) => std::ptr::null_mut(),
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
    if proc.is_null() {
        return -1;
    }
    // SAFETY: proc is a valid HewProcess pointer per caller contract.
    let p = unsafe { &mut *proc };
    match p.inner.wait() {
        Ok(status) => status.code().unwrap_or(-1),
        Err(_) => -1,
    }
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
    if proc.is_null() {
        return -1;
    }
    // SAFETY: proc is a valid HewProcess pointer per caller contract.
    let p = unsafe { &mut *proc };
    match p.inner.kill() {
        Ok(()) => 0,
        Err(_) => -1,
    }
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
    if r.is_null() {
        return;
    }
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
    if p.is_null() {
        return;
    }
    // SAFETY: p was allocated with Box::into_raw and has not been freed.
    drop(unsafe { Box::from_raw(p) });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

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
        let arg1 = CString::new("hello").unwrap();
        let arg2 = CString::new("world").unwrap();
        let args = [arg1.as_ptr(), arg2.as_ptr()];

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
            assert!(hew_process_spawn(std::ptr::null()).is_null());
            assert_eq!(hew_process_wait(std::ptr::null_mut()), -1);
            assert_eq!(hew_process_kill(std::ptr::null_mut()), -1);
            hew_process_result_free(std::ptr::null_mut());
            hew_process_free(std::ptr::null_mut());
        }
    }
}
