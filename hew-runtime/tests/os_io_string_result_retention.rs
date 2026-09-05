//! Measured ownership of the OS / I/O `-> string` runtime exports.
//!
//! A `fresh` FFI row is not enough to mint a caller-side `hew_string_drop`:
//! these probes establish the separate retention fact for every symbol below.
//! R1 keeps two results live and requires distinct allocations; R2 reads the
//! header's owner count through `cstring_ensure_unique`; R3 releases both and
//! then reads again from the still-live producer/input state.  Together they
//! establish that the one owner at handoff is the caller's and no producer
//! retains a pointer into that allocation.

#![cfg(unix)]

use std::ffi::{c_char, CStr, CString};
use std::io::Write as _;
use std::process::{Command, Stdio};

use hew_cabi::cabi::{cstring_ensure_unique, free_cstring, str_to_malloc};
use hew_cabi::string::{string_as_bytes, string_release, HewString};
use hew_runtime::env::{
    hew_args_get, hew_cwd, hew_env_get, hew_env_remove, hew_env_set, hew_home_dir, hew_hostname,
    hew_temp_dir,
};
use hew_runtime::file_io::hew_file_read;
use hew_runtime::path::{
    hew_glob, hew_glob_count, hew_glob_error, hew_glob_free, hew_glob_get, hew_glob_is_valid,
    hew_path_absolute,
};
use hew_runtime::process::{
    hew_process_result_free, hew_process_result_stderr, hew_process_result_stdout, HewProcessResult,
};
use hew_runtime::stdio::{hew_io_read_all, hew_io_read_line};
use hew_runtime::stream::{
    hew_file_read_stream_collect_string, hew_stream_collect_string, hew_stream_from_bytes,
};

const STDIN_CHILD: &str = "HEW_OS_IO_RETENTION_STDIN_CHILD";

/// Establish R1/R2/R3 for one result producer whose source state remains live
/// across calls.  `call` deliberately runs three times: freeing a returned
/// string must never corrupt the source used for the third result.
fn assert_result_is_transferred(symbol: &str, mut call: impl FnMut() -> *mut c_char) {
    let first = call();
    let second = call();
    assert!(
        !first.is_null() && !second.is_null(),
        "{symbol}: expected non-null results"
    );

    // R1: a producer-held borrow could return one live address twice; these
    // two callers must instead own independent allocations simultaneously.
    assert_ne!(
        first, second,
        "{symbol}: two live results share an address rather than transferring fresh allocations"
    );

    // R2: `cstring_ensure_unique` is a non-destructive owner-count read when
    // rc == 1.  A copy would mean the producer retained another owner.
    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: each pointer is a live header-aware Hew string returned by
        // the symbol under test.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "{symbol}: {label} was not uniquely owned at handoff"
        );
    }

    // SAFETY: `first` is a live NUL-terminated result.
    let text = unsafe { CStr::from_ptr(first) }.to_bytes().to_vec();

    // R3: these are the balancing releases named by every audited row.
    // SAFETY: R2 established that both pointers are live sole owners.
    unsafe {
        free_cstring(first);
        free_cstring(second);
    }

    let third = call();
    assert!(
        !third.is_null(),
        "{symbol}: producer/input state did not survive releasing earlier results"
    );
    // SAFETY: `third` is a live NUL-terminated result.
    let after = unsafe { CStr::from_ptr(third) }.to_bytes();
    assert_eq!(
        after,
        text.as_slice(),
        "{symbol}: caller release changed the producer/input state"
    );
    // SAFETY: R2's same producer invariant applies to this fresh result.
    unsafe { free_cstring(third) };
}

fn assert_managed_result_is_transferred(symbol: &str, mut call: impl FnMut() -> *mut HewString) {
    let first = call();
    let second = call();
    assert_ne!(
        first, second,
        "{symbol}: two non-empty results must be distinct"
    );
    // SAFETY: both calls return live managed owners.
    let expected = unsafe { string_as_bytes(first) }.to_vec();
    // SAFETY: `first` and `second` are the two independent owners returned above.
    unsafe {
        string_release(first);
        string_release(second);
    }
    let third = call();
    // SAFETY: the third result remains live for the assertion and release.
    unsafe {
        assert_eq!(string_as_bytes(third), expected);
        string_release(third);
    }
}

#[test]
fn args_get_result_is_transferred() {
    assert_result_is_transferred("hew_args_get", || {
        // SAFETY: index zero is always a valid i32 input; null is handled by
        // the assertion in the shared retention probe.
        unsafe { hew_args_get(0) }
    });
}

struct EnvKey(CString);

impl Drop for EnvKey {
    fn drop(&mut self) {
        // SAFETY: the CString remains valid for this call; removal is the
        // matching cleanup for the test-only, process-unique name.
        unsafe { hew_env_remove(self.0.as_ptr()) };
    }
}

#[test]
fn env_get_result_is_transferred() {
    let key = EnvKey(
        CString::new(format!("HEW_RETENTION_{}_{}", std::process::id(), line!()))
            .expect("generated environment key contains no NUL"),
    );
    let value = CString::new("hew-os-io-retention").expect("literal contains no NUL");
    // SAFETY: both CStrings are valid for the duration of the call.
    assert_eq!(unsafe { hew_env_set(key.0.as_ptr(), value.as_ptr()) }, 0);
    assert_result_is_transferred("hew_env_get", || {
        // SAFETY: the test-owned CString remains valid for every probe call.
        unsafe { hew_env_get(key.0.as_ptr()) }
    });
}

#[test]
fn cwd_result_is_transferred() {
    assert_result_is_transferred("hew_cwd", || {
        // SAFETY: this no-argument export has no caller preconditions.
        unsafe { hew_cwd() }
    });
}

#[test]
fn home_dir_result_is_transferred() {
    assert_result_is_transferred("hew_home_dir", || {
        // SAFETY: this no-argument export has no caller preconditions.
        unsafe { hew_home_dir() }
    });
}

#[test]
fn hostname_result_is_transferred() {
    assert_result_is_transferred("hew_hostname", || {
        // SAFETY: this no-argument export has no caller preconditions.
        unsafe { hew_hostname() }
    });
}

#[test]
fn temp_dir_result_is_transferred() {
    assert_result_is_transferred("hew_temp_dir", || {
        // SAFETY: this no-argument export has no caller preconditions.
        unsafe { hew_temp_dir() }
    });
}

#[test]
fn file_read_result_is_transferred() {
    let dir = tempfile::tempdir().expect("temporary directory");
    let path = dir.path().join("retention.txt");
    std::fs::write(&path, "file retention witness").expect("write fixture");
    let path = CString::new(path.to_string_lossy().as_bytes()).expect("temporary path has no NUL");
    assert_result_is_transferred("hew_file_read", || {
        // SAFETY: the temporary path CString remains valid for every call.
        unsafe { hew_file_read(path.as_ptr()) }
    });
}

#[test]
fn path_absolute_result_is_transferred() {
    let path = CString::new(".").expect("literal contains no NUL");
    assert_result_is_transferred("hew_path_absolute", || {
        // SAFETY: the literal path CString remains valid for every call.
        unsafe { hew_path_absolute(path.as_ptr()) }
    });
}

#[test]
fn glob_error_result_is_transferred() {
    let pattern = CString::new("hew-retention-no-match-*-unlikely").expect("literal has no NUL");
    // SAFETY: the pattern is a valid C string and the returned handle remains
    // live for all three borrowed-result reads.
    let result = unsafe { hew_glob(pattern.as_ptr()) };
    assert!(!result.is_null(), "glob must return a result handle");
    // SAFETY: `result` is a live result handle from `hew_glob` above.
    let valid = unsafe { hew_glob_is_valid(result) };
    assert!(valid);
    assert_result_is_transferred("hew_glob_error", || {
        // SAFETY: the caller keeps `result` live for every borrowed lookup.
        unsafe { hew_glob_error(result) }
    });
    // SAFETY: `result` is the still-live glob result returned above.
    unsafe { hew_glob_free(result) };
}

#[test]
fn glob_get_result_is_transferred() {
    let dir = tempfile::tempdir().expect("temporary directory");
    let entry = dir.path().join("retention.txt");
    std::fs::write(&entry, "glob fixture").expect("write fixture");
    let pattern = CString::new(format!("{}/*.txt", dir.path().display()))
        .expect("temporary pattern has no NUL");
    // SAFETY: the pattern is valid and the result remains live through the
    // getter's three reads.
    let result = unsafe { hew_glob(pattern.as_ptr()) };
    // SAFETY: non-null `result` comes directly from `hew_glob` and is live.
    let valid = unsafe { hew_glob_is_valid(result) };
    assert!(!result.is_null() && valid);
    // SAFETY: the validated result handle remains live for this count read.
    let count = unsafe { hew_glob_count(result) };
    assert_eq!(count, 1, "fixture must match once");
    assert_result_is_transferred("hew_glob_get", || {
        // SAFETY: the caller keeps `result` live for every indexed lookup.
        unsafe { hew_glob_get(result, 0) }
    });
    // SAFETY: `result` is still live and has not been freed.
    unsafe { hew_glob_free(result) };
}

#[test]
fn process_result_stdout_and_stderr_are_transferred() {
    // Build the same retained source state `hew_process_run*` creates, without
    // shelling out: the getters' contract is cloning these two result fields.
    let stdout = str_to_malloc("stdout retention witness");
    let stderr = str_to_malloc("stderr retention witness");
    assert!(!stdout.is_null() && !stderr.is_null());
    let result = Box::into_raw(Box::new(HewProcessResult {
        exit_code: 0,
        stdout,
        stderr,
    }));
    assert_result_is_transferred("hew_process_result_stdout", || {
        // SAFETY: `result` remains live until the explicit final free below.
        unsafe { hew_process_result_stdout(result) }
    });
    assert_result_is_transferred("hew_process_result_stderr", || {
        // SAFETY: `result` remains live until the explicit final free below.
        unsafe { hew_process_result_stderr(result) }
    });
    // SAFETY: `result` owns the source fields and has not been released.
    unsafe { hew_process_result_free(result) };
}

#[test]
fn stream_collect_string_result_is_transferred() {
    let payload = b"stream retention witness";
    assert_result_is_transferred("hew_stream_collect_string", || {
        // `hew_stream_collect_string` consumes the stream, so each R1/R3
        // observation builds an independent but equivalent live input stream.
        // SAFETY: `payload` remains readable for its stated length, and the
        // returned stream is consumed exactly once by collect.
        unsafe {
            let stream = hew_stream_from_bytes(payload.as_ptr(), payload.len(), 0);
            hew_stream_collect_string(stream)
        }
    });
    assert_result_is_transferred("hew_file_read_stream_collect_string", || {
        // The nominal adapter has the same consuming representation. Each
        // observation still needs an independently owned input stream.
        // SAFETY: `payload` remains readable for its stated length, and the
        // returned stream is consumed exactly once by collect.
        unsafe {
            let stream = hew_stream_from_bytes(payload.as_ptr(), payload.len(), 0);
            hew_file_read_stream_collect_string(stream)
        }
    });
}

fn run_stdin_child(test_name: &str, input: &[u8]) {
    let mut child = Command::new(std::env::current_exe().expect("current test executable"))
        .args(["--exact", test_name, "--nocapture", "--test-threads=1"])
        .env(STDIN_CHILD, "1")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn isolated stdin retention child");
    child
        .stdin
        .as_mut()
        .expect("child stdin")
        .write_all(input)
        .expect("write child stdin");
    let output = child.wait_with_output().expect("wait for stdin child");
    assert!(
        output.status.success(),
        "{test_name} child failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn io_read_line_result_is_transferred() {
    if std::env::var_os(STDIN_CHILD).is_some() {
        // Three equal, independently queued lines keep the stdin source alive
        // for R3 while each `read_line` consumes exactly one record.
        assert_managed_result_is_transferred("hew_io_read_line", || hew_io_read_line());
    } else {
        run_stdin_child("io_read_line_result_is_transferred", b"line\nline\nline\n");
    }
}

unsafe fn read_all_from_replaced_stdin(input: &[u8]) -> *mut HewString {
    let mut pipe_fds = [0; 2];
    // SAFETY: `pipe_fds` has space for the read/write descriptors.
    // SAFETY: `pipe_fds` has space for the read/write descriptors.
    let pipe_status = unsafe { libc::pipe(pipe_fds.as_mut_ptr()) };
    assert_eq!(pipe_status, 0, "create stdin pipe");
    let mut written = 0;
    while written < input.len() {
        // SAFETY: the remaining slice is valid to read and the descriptor is
        // the pipe's writable end.
        let count = unsafe {
            libc::write(
                pipe_fds[1],
                input[written..].as_ptr().cast(),
                input.len() - written,
            )
        };
        assert!(count > 0, "write stdin fixture");
        written += usize::try_from(count).expect("positive write count fits usize");
    }
    // SAFETY: close the writer so `read_to_string` observes EOF.
    // SAFETY: the writable pipe descriptor is valid and owned by this helper.
    let close_writer = unsafe { libc::close(pipe_fds[1]) };
    assert_eq!(close_writer, 0, "close stdin pipe writer");
    // SAFETY: duplicate and replace descriptor zero inside this isolated child.
    let saved = unsafe { libc::dup(libc::STDIN_FILENO) };
    assert!(saved >= 0, "save child stdin");
    // SAFETY: both descriptors are live; dup2 atomically makes fd 0 refer to
    // the test pipe in this isolated child process.
    let replace_stdin = unsafe { libc::dup2(pipe_fds[0], libc::STDIN_FILENO) };
    assert_eq!(replace_stdin, libc::STDIN_FILENO, "replace child stdin");
    // SAFETY: fd zero now owns a duplicate of this pipe's read end.
    let close_read = unsafe { libc::close(pipe_fds[0]) };
    assert_eq!(close_read, 0, "close duplicate read descriptor");
    let result = hew_io_read_all();
    // SAFETY: restore the test runner's original descriptor before returning.
    // SAFETY: `saved` is the valid descriptor duplicated before replacement.
    let restore_stdin = unsafe { libc::dup2(saved, libc::STDIN_FILENO) };
    assert_eq!(restore_stdin, libc::STDIN_FILENO, "restore child stdin");
    // SAFETY: fd zero now owns the restored duplicate, so `saved` is surplus.
    let close_saved = unsafe { libc::close(saved) };
    assert_eq!(close_saved, 0, "close saved stdin");
    result
}

#[test]
fn io_read_all_result_is_transferred() {
    if std::env::var_os(STDIN_CHILD).is_some() {
        assert_managed_result_is_transferred("hew_io_read_all", || {
            // SAFETY: this test-only helper restores stdin before returning.
            unsafe { read_all_from_replaced_stdin(b"read-all retention witness") }
        });
    } else {
        run_stdin_child("io_read_all_result_is_transferred", b"");
    }
}
