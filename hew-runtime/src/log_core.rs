//! Core logging primitives — always compiled (no feature gate).
//!
//! These are the functions that back `std::log` in Hew programs.
//! They use only the Rust standard library (atomics, stderr, `SystemTime`)
//! so they can be unconditionally linked without pulling in external crates.

use crate::cabi::cstr_to_str;
use std::os::raw::c_char;
use std::sync::atomic::{AtomicI32, Ordering};

/// Global log level filter. 0=ERROR, 1=WARN, 2=INFO, 3=DEBUG, 4=TRACE.
/// Default: −1 (uninitialized — all output suppressed until `setup` is called).
static LOG_LEVEL: AtomicI32 = AtomicI32::new(-1);

/// Global log output format. 0=TEXT (colored), 1=JSON (single-line).
/// Default: TEXT (0).
static LOG_FORMAT: AtomicI32 = AtomicI32::new(0);

/// Get the current global log level filter.
#[no_mangle]
pub extern "C" fn hew_log_get_level() -> i32 {
    LOG_LEVEL.load(Ordering::Relaxed)
}

/// Set the global log level filter.
///
/// Level mapping: 0=ERROR, 1=WARN, 2=INFO, 3=DEBUG, 4=TRACE.
/// Values outside 0..=4 are clamped.
#[no_mangle]
pub extern "C" fn hew_log_set_level(level: i32) {
    LOG_LEVEL.store(level.clamp(0, 4), Ordering::Relaxed);
}

/// Get the current log output format.
///
/// Returns 0 for TEXT (colored), 1 for JSON (single-line).
#[no_mangle]
pub extern "C" fn hew_log_get_format() -> i32 {
    LOG_FORMAT.load(Ordering::Relaxed)
}

/// Set the log output format.
///
/// Format mapping: 0=TEXT (colored), 1=JSON (single-line NDJSON).
/// Values outside 0..=1 are clamped.
#[no_mangle]
pub extern "C" fn hew_log_set_format(format: i32) {
    LOG_FORMAT.store(format.clamp(0, 1), Ordering::Relaxed);
}

/// Emit a log line if the message level passes the global filter.
///
/// If the global format is TEXT (0), outputs colored text to stderr.
/// If the global format is JSON (1), outputs single-line NDJSON to stderr.
///
/// # Safety
///
/// `msg` must be a valid NUL-terminated C string (or null, which is a no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_log_emit(level: i32, msg: *const c_char) {
    // Filter: lower numeric level = higher severity.  Emit only when
    // message level ≤ the configured threshold.
    if level > LOG_LEVEL.load(Ordering::Relaxed) {
        return;
    }

    // SAFETY: msg is a valid NUL-terminated C string per caller contract.
    let Some(text) = (unsafe { cstr_to_str(msg) }) else {
        return;
    };

    if LOG_FORMAT.load(Ordering::Relaxed) == 1 {
        emit_json(level, text);
    } else {
        emit_text(level, text);
    }
}

/// Emit a colored text log line to stderr.
fn emit_text(level: i32, text: &str) {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = now.as_secs() % 86400;
    let hours = secs / 3600;
    let minutes = (secs % 3600) / 60;
    let seconds = secs % 60;
    let millis = now.subsec_millis();

    let (level_str, color_code) = match level {
        0 => ("ERROR", "\x1b[31m"),
        1 => ("WARN ", "\x1b[33m"),
        2 => ("INFO ", "\x1b[32m"),
        3 => ("DEBUG", "\x1b[34m"),
        4 => ("TRACE", "\x1b[2m"),
        _ => ("INFO ", "\x1b[32m"),
    };

    let reset = "\x1b[0m";
    let dim = "\x1b[2m";
    eprintln!(
        "{dim}{hours:02}:{minutes:02}:{seconds:02}.{millis:03}{reset} {color_code}{level_str}{reset} {text}"
    );
}

/// Emit a single-line JSON (NDJSON) log record to stderr.
fn emit_json(level: i32, text: &str) {
    let level_str = match level {
        0 => "ERROR",
        1 => "WARN",
        2 => "INFO",
        3 => "DEBUG",
        4 => "TRACE",
        _ => "UNKNOWN",
    };

    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = now.as_secs();
    let millis = now.subsec_millis();

    // Split text into base message and key=value pairs.
    // Named args are appended as " key=value" by the codegen.
    let (msg, fields) = split_msg_fields(text);

    eprintln!(
        "{{\"ts\":{secs}.{millis:03},\"level\":\"{level_str}\",\"msg\":\"{msg_escaped}\"{fields}}}",
        msg_escaped = json_escape(msg),
    );
}

/// Split a log message into the base message and serialized JSON fields.
///
/// The codegen appends named arguments as ` key=value` pairs after the
/// human-readable message.  We scan backwards from the end to find where
/// the key=value suffix begins so that multi-word messages are preserved.
fn split_msg_fields(text: &str) -> (&str, String) {
    // Walk backwards through space-delimited tokens.  As long as a token
    // contains '=' it's a field; the rest is the base message.
    let mut boundary = text.len();
    for segment in text.rsplit(' ') {
        if segment.contains('=') {
            boundary = segment.as_ptr() as usize - text.as_ptr() as usize;
        } else {
            break;
        }
    }

    if boundary == text.len() {
        return (text, String::new());
    }

    let msg = text[..boundary].trim_end();
    let rest = &text[boundary..];
    let mut fields = String::new();
    for pair in rest.split(' ') {
        if let Some((k, v)) = pair.split_once('=') {
            use std::fmt::Write as _;
            let _ = write!(fields, ",\"{}\":\"{}\"", json_escape(k), json_escape(v));
        }
    }
    (msg, fields)
}

/// Escape a string for inclusion in a JSON value.
fn json_escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

/// Write a string directly to stderr.
///
/// # Safety
///
/// `s` must be a valid NUL-terminated C string (or null, which is a no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_stderr_write(s: *const c_char) {
    // SAFETY: s is a valid NUL-terminated C string per caller contract.
    if let Some(msg) = unsafe { cstr_to_str(s) } {
        eprint!("{msg}");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn level_default_is_uninitialized() {
        // Reset for test isolation (other tests may have changed it).
        LOG_LEVEL.store(-1, Ordering::Relaxed);
        assert_eq!(hew_log_get_level(), -1);
    }

    #[test]
    fn level_set_and_get() {
        hew_log_set_level(4); // TRACE
        assert_eq!(hew_log_get_level(), 4);
        hew_log_set_level(0); // ERROR
        assert_eq!(hew_log_get_level(), 0);
        hew_log_set_level(2); // reset to INFO
    }

    #[test]
    fn level_clamps_out_of_range() {
        hew_log_set_level(99);
        assert_eq!(hew_log_get_level(), 4);
        hew_log_set_level(-10);
        assert_eq!(hew_log_get_level(), 0);
        hew_log_set_level(2); // reset
    }

    #[test]
    fn format_default_is_text() {
        assert_eq!(hew_log_get_format(), 0);
    }

    #[test]
    fn format_set_and_get() {
        hew_log_set_format(1); // JSON
        assert_eq!(hew_log_get_format(), 1);
        hew_log_set_format(0); // reset
        assert_eq!(hew_log_get_format(), 0);
    }

    #[test]
    fn format_clamps_out_of_range() {
        hew_log_set_format(99);
        assert_eq!(hew_log_get_format(), 1);
        hew_log_set_format(-5);
        assert_eq!(hew_log_get_format(), 0);
    }

    #[test]
    fn emit_filters_by_level() {
        hew_log_set_level(2); // INFO
        let msg = CString::new("should appear").unwrap();
        // INFO (2) ≤ INFO (2): emitted (no crash)
        unsafe { hew_log_emit(2, msg.as_ptr()) };
        let dbg = CString::new("should be filtered").unwrap();
        // DEBUG (3) > INFO (2): filtered out
        unsafe { hew_log_emit(3, dbg.as_ptr()) };
    }

    #[test]
    fn emit_null_is_noop() {
        hew_log_set_level(4);
        // SAFETY: Passing null is the case under test; the function handles it.
        unsafe { hew_log_emit(2, std::ptr::null()) };
    }

    #[test]
    fn emit_suppressed_when_uninitialized() {
        LOG_LEVEL.store(-1, Ordering::Relaxed);
        let msg = CString::new("hidden").unwrap();
        // Level 0 (ERROR) > -1: filtered out
        unsafe { hew_log_emit(0, msg.as_ptr()) };
        hew_log_set_level(2); // reset
    }

    #[test]
    fn stderr_write_null_is_noop() {
        // SAFETY: Passing null is the case under test; the function handles it.
        unsafe { hew_stderr_write(std::ptr::null()) };
    }

    #[test]
    fn stderr_write_valid_string() {
        let msg = CString::new("test output\n").unwrap();
        // SAFETY: msg is a valid CString.
        unsafe { hew_stderr_write(msg.as_ptr()) };
    }

    #[test]
    fn json_escape_basics() {
        assert_eq!(json_escape("hello"), "hello");
        assert_eq!(json_escape("he\"llo"), "he\\\"llo");
        assert_eq!(json_escape("a\\b"), "a\\\\b");
        assert_eq!(json_escape("line\nnew"), "line\\nnew");
    }

    #[test]
    fn emit_json_does_not_crash() {
        emit_json(2, "test message");
        emit_json(0, "error key=val");
        emit_json(3, "debug k1=v1 k2=v2");
    }

    #[test]
    fn emit_json_mode() {
        hew_log_set_level(4);
        hew_log_set_format(1);
        let msg = CString::new("hello key=value").unwrap();
        // SAFETY: msg is a valid CString.
        unsafe { hew_log_emit(2, msg.as_ptr()) };
        hew_log_set_format(0); // reset
        hew_log_set_level(2);
    }

    #[test]
    fn split_msg_fields_no_fields() {
        let (msg, fields) = split_msg_fields("hello world");
        assert_eq!(msg, "hello world");
        assert!(fields.is_empty());
    }

    #[test]
    fn split_msg_fields_with_fields() {
        let (msg, fields) = split_msg_fields("request handled status=200 path=/api");
        assert_eq!(msg, "request handled");
        assert!(fields.contains("\"status\":\"200\""));
        assert!(fields.contains("\"path\":\"/api\""));
    }

    #[test]
    fn split_msg_fields_single_word_msg() {
        let (msg, fields) = split_msg_fields("started port=8080");
        assert_eq!(msg, "started");
        assert!(fields.contains("\"port\":\"8080\""));
    }

    #[test]
    fn split_msg_fields_all_fields() {
        let (msg, fields) = split_msg_fields("k=v");
        assert_eq!(msg, "");
        assert!(fields.contains("\"k\":\"v\""));
    }
}
