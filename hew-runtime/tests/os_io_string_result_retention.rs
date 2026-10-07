//! Managed OS and I/O results remain valid independently of sibling results.
//! Each producer survives releasing its prior results. Empty values use the
//! canonical null handle; non-empty fresh results own distinct allocations.

#![cfg(unix)]

#[path = "../src/test_string.rs"]
mod test_string;
use test_string::ManagedString;

use hew_cabi::string::{string_as_bytes, string_from_str, string_release, HewString};
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
use hew_runtime::stream::{
    hew_file_read_stream_collect_string, hew_stream_collect_string, hew_stream_from_bytes,
};

/// Check independent result ownership for one result producer whose source state remains live
/// across calls.  `call` deliberately runs three times: freeing a returned
/// string must never corrupt the source used for the third result.
fn assert_managed_result_is_transferred(symbol: &str, mut call: impl FnMut() -> *mut HewString) {
    let first = call();
    let second = call();
    if !first.is_null() {
        assert_ne!(first, second, "{symbol}: distinct owners");
    }
    // SAFETY: both calls return live managed owners.
    let expected = unsafe { string_as_bytes(first) }.to_vec();
    // SAFETY: `first` and `second` are the two independent owners returned above.
    unsafe {
        string_release(first);
        assert_eq!(string_as_bytes(second), expected);
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
    assert_managed_result_is_transferred("hew_args_get", || {
        // SAFETY: index zero is always a valid i32 input; null is handled by
        // the assertion in the shared retention probe.
        unsafe { hew_args_get(0) }
    });
}

struct EnvKey(ManagedString);

impl Drop for EnvKey {
    fn drop(&mut self) {
        // SAFETY: the managed string remains valid for this call; removal is the
        // matching cleanup for the test-only, process-unique name.
        unsafe { hew_env_remove(self.0.as_ptr()) };
    }
}

#[test]
fn env_get_result_is_transferred() {
    let key = EnvKey(ManagedString::new(format!(
        "HEW_RETENTION_{}_{}",
        std::process::id(),
        line!()
    )));
    let value = ManagedString::new("hew-os-io-retention");
    // SAFETY: both managed values are valid for the duration of the call.
    assert_eq!(unsafe { hew_env_set(key.0.as_ptr(), value.as_ptr()) }, 0);
    assert_managed_result_is_transferred("hew_env_get", || {
        // SAFETY: the test-owned managed string remains valid for every probe call.
        unsafe { hew_env_get(key.0.as_ptr()) }
    });
}

#[test]
fn cwd_result_is_transferred() {
    assert_managed_result_is_transferred("hew_cwd", || {
        // SAFETY: this no-argument export has no caller preconditions.
        unsafe { hew_cwd() }
    });
}

#[test]
fn home_dir_result_is_transferred() {
    assert_managed_result_is_transferred("hew_home_dir", || {
        // SAFETY: this no-argument export has no caller preconditions.
        unsafe { hew_home_dir() }
    });
}

#[test]
fn hostname_result_is_transferred() {
    assert_managed_result_is_transferred("hew_hostname", || {
        // SAFETY: this no-argument export has no caller preconditions.
        unsafe { hew_hostname() }
    });
}

#[test]
fn temp_dir_result_is_transferred() {
    assert_managed_result_is_transferred("hew_temp_dir", || {
        // SAFETY: this no-argument export has no caller preconditions.
        unsafe { hew_temp_dir() }
    });
}

#[test]
fn file_read_result_is_transferred() {
    let dir = tempfile::tempdir().expect("temporary directory");
    let path = dir.path().join("retention.txt");
    std::fs::write(&path, "file\0retention é中🙂").expect("write fixture");
    let path = string_from_str(path.to_str().unwrap());
    assert_managed_result_is_transferred("hew_file_read", || {
        // SAFETY: the managed path remains live throughout these reads.
        unsafe { hew_file_read(path) }
    });
    // SAFETY: the fixture owns this path reference.
    unsafe { string_release(path) };
}

#[test]
fn path_absolute_result_is_transferred() {
    let path = ManagedString::new(".");
    assert_managed_result_is_transferred("hew_path_absolute", || {
        // SAFETY: the literal path managed string remains valid for every call.
        unsafe { hew_path_absolute(path.as_ptr()) }
    });
}

#[test]
fn glob_error_result_is_transferred() {
    let pattern = ManagedString::new("hew-retention-no-match-*-unlikely");
    // SAFETY: the pattern is a valid managed string and the returned handle remains
    // live for all three borrowed-result reads.
    let result = unsafe { hew_glob(pattern.as_ptr()) };
    assert!(!result.is_null(), "glob must return a result handle");
    // SAFETY: `result` is a live result handle from `hew_glob` above.
    let valid = unsafe { hew_glob_is_valid(result) };
    assert!(valid);
    assert_managed_result_is_transferred("hew_glob_error", || {
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
    let pattern = ManagedString::new(format!("{}/*.txt", dir.path().display()));
    // SAFETY: the pattern is valid and the result remains live through the
    // getter's three reads.
    let result = unsafe { hew_glob(pattern.as_ptr()) };
    // SAFETY: non-null `result` comes directly from `hew_glob` and is live.
    let valid = unsafe { hew_glob_is_valid(result) };
    assert!(!result.is_null() && valid);
    // SAFETY: the validated result handle remains live for this count read.
    let count = unsafe { hew_glob_count(result) };
    assert_eq!(count, 1, "fixture must match once");
    assert_managed_result_is_transferred("hew_glob_get", || {
        // SAFETY: the caller keeps `result` live for every indexed lookup.
        unsafe { hew_glob_get(result, 0) }
    });
    // SAFETY: `result` is still live and has not been freed.
    unsafe { hew_glob_free(result) };
}

#[test]
fn process_result_stdout_and_stderr_are_retained() {
    for release_result_first in [false, true] {
        let result = Box::into_raw(Box::new(HewProcessResult {
            status: std::process::ExitStatus::default(),
            stdout: string_from_str("stdout\0é中🙂"),
            stderr: string_from_str("stderr\0é中🙂"),
        }));
        // SAFETY: result is live, and every accessor transfers a retained owner.
        unsafe {
            let out1 = hew_process_result_stdout(result);
            let out2 = hew_process_result_stdout(result);
            let err = hew_process_result_stderr(result);
            assert_eq!(out1, (*result).stdout);
            assert_eq!(out2, out1);
            assert_eq!(err, (*result).stderr);
            if release_result_first {
                hew_process_result_free(result);
            }
            assert_eq!(string_as_bytes(out1), "stdout\0é中🙂".as_bytes());
            string_release(out1);
            assert_eq!(string_as_bytes(out2), "stdout\0é中🙂".as_bytes());
            assert_eq!(string_as_bytes(err), "stderr\0é中🙂".as_bytes());
            string_release(out2);
            string_release(err);
            if !release_result_first {
                let again = hew_process_result_stdout(result);
                assert_eq!(string_as_bytes(again), "stdout\0é中🙂".as_bytes());
                string_release(again);
                hew_process_result_free(result);
            }
        }
    }
}

#[test]
fn stream_collect_string_result_is_transferred() {
    let payload = b"stream retention witness";
    assert_managed_result_is_transferred("hew_stream_collect_string", || {
        // `hew_stream_collect_string` consumes the stream, so each R1/R3
        // observation builds an independent but equivalent live input stream.
        // SAFETY: `payload` remains readable for its stated length, and the
        // returned stream is consumed exactly once by collect.
        unsafe {
            let stream = hew_stream_from_bytes(payload.as_ptr(), payload.len(), 0);
            hew_stream_collect_string(stream)
        }
    });
    assert_managed_result_is_transferred("hew_file_read_stream_collect_string", || {
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
