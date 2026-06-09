//! Core logging primitives — always compiled (no feature gate).
//!
//! These are the functions that back `std::log` in Hew programs.
//! They use only the Rust standard library (atomics, stderr, `SystemTime`)
//! so they can be unconditionally linked without pulling in external crates.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::cabi::{cstr_to_str, malloc_cstring};
use std::os::raw::c_char;
use std::sync::atomic::{AtomicI32, Ordering};

/// Global log level filter. 0=ERROR, 1=WARN, 2=INFO, 3=DEBUG, 4=TRACE.
/// Default: −1 (uninitialized — all output suppressed until `setup` is called).
static LOG_LEVEL: AtomicI32 = AtomicI32::new(-1);

/// Global log output format. 0=TEXT (coloured), 1=JSON (single-line).
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
/// Returns 0 for TEXT (coloured), 1 for JSON (single-line).
#[no_mangle]
pub extern "C" fn hew_log_get_format() -> i32 {
    LOG_FORMAT.load(Ordering::Relaxed)
}

/// Set the log output format.
///
/// Format mapping: 0=TEXT (coloured), 1=JSON (single-line NDJSON).
/// Values outside 0..=1 are clamped.
#[no_mangle]
pub extern "C" fn hew_log_set_format(format: i32) {
    LOG_FORMAT.store(format.clamp(0, 1), Ordering::Relaxed);
}

/// Emit a log line if the message level passes the global filter.
///
/// If the global format is TEXT (0), outputs coloured text to stderr.
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

/// Emit a log line with explicit level threshold and format, bypassing globals.
///
/// `level`       — message severity (0=ERROR … 4=TRACE).
/// `min_level`   — threshold: emit only when `level ≤ min_level`. Pass −1 to
///                 suppress all output; pass 4 to emit everything.
/// `format`      — 0=TEXT, 1=JSON. Values outside 0..=1 fall back to TEXT.
/// `msg`         — NUL-terminated C string (null is a no-op).
///
/// This is the structured-logging back-end used by Logger values in Hew.
/// Unlike `hew_log_emit`, it does not consult the global `LOG_LEVEL` /
/// `LOG_FORMAT` atomics, so Logger-scoped settings are thread-safe.
///
/// # Safety
///
/// `msg` must be a valid NUL-terminated C string (or null, which is a no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_log_emit_ex(
    level: i32,
    min_level: i32,
    format: i32,
    msg: *const c_char,
) {
    if level > min_level {
        return;
    }

    // SAFETY: msg is a valid NUL-terminated C string per caller contract.
    let Some(text) = (unsafe { cstr_to_str(msg) }) else {
        return;
    };

    if format == 1 {
        emit_json(level, text);
    } else {
        emit_text(level, text);
    }
}

/// Emit a coloured text log line to stderr.
///
/// Field values that were quoted in the wire string (to survive space-splitting)
/// are decoded and rendered as `key=value` with the original value bytes, so
/// text output remains human-readable even when values contain spaces.
fn emit_text(level: i32, text: &str) {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = now.as_secs() % 86400;
    let hours = secs / 3600;
    let minutes = (secs % 3600) / 60;
    let seconds = secs % 60;
    let millis = now.subsec_millis();

    let (level_str, colour_code) = match level {
        0 => ("ERROR", "\x1b[31m"),
        1 => ("WARN ", "\x1b[33m"),
        3 => ("DEBUG", "\x1b[34m"),
        4 => ("TRACE", "\x1b[2m"),
        _ => ("INFO ", "\x1b[32m"),
    };

    let reset = "\x1b[0m";
    let dim = "\x1b[2m";

    // Decode the wire representation into human-readable form: `msg key=val …`
    // where values with spaces are shown decoded (without surrounding quotes).
    let display = decode_text_line(text);
    eprintln!(
        "{dim}{hours:02}:{minutes:02}:{seconds:02}.{millis:03}{reset} {colour_code}{level_str}{reset} {display}"
    );
}

/// Rebuild a human-readable `msg key=val …` string from the wire representation.
///
/// The wire representation uses quoted values (`key="val with spaces"`) to
/// survive space-based splitting.  This function decodes those quoted values
/// back to their original form so TEXT output stays readable.
///
/// Input:  `"login user=\"John Doe\" status=200"`
/// Output: `"login user=John Doe status=200"`
fn decode_text_line(text: &str) -> String {
    let (msg, _) = split_msg_fields(text); // we only need msg for the first part
                                           // Re-parse fields to get decoded key+value pairs for human output.
    let fields_start_byte = {
        // Find the same boundary split_msg_fields uses.
        let trimmed_msg = msg;
        if trimmed_msg.len() < text.len() {
            trimmed_msg.len()
        } else {
            text.len()
        }
    };
    // Fast path: no fields — return as-is.
    if fields_start_byte == text.len() {
        return text.to_owned();
    }

    let rest = text[fields_start_byte..].trim_start();
    let mut out = String::with_capacity(text.len());
    out.push_str(msg);

    let rest_bytes = rest.as_bytes();
    let mut rp = 0usize;
    while rp < rest.len() {
        // Skip spaces between fields.
        while rp < rest.len() && rest_bytes[rp] == b' ' {
            rp += 1;
        }
        if rp >= rest.len() {
            break;
        }

        // Read key.
        let key_start = rp;
        while rp < rest.len() && rest_bytes[rp] != b'=' {
            rp += 1;
        }
        if rp >= rest.len() {
            break;
        }
        let key = &rest[key_start..rp];
        rp += 1; // skip `=`

        // Read value (quoted or bare) and decode it.
        let value: String = if rp < rest.len() && rest_bytes[rp] == b'"' {
            rp += 1;
            let mut val = String::new();
            while rp < rest.len() {
                let b = rest_bytes[rp];
                if b == b'\\' && rp + 1 < rest.len() {
                    rp += 1;
                    match rest_bytes[rp] {
                        b'"' => val.push('"'),
                        b'\\' => val.push('\\'),
                        b'n' => val.push('\n'),
                        b'r' => val.push('\r'),
                        other => {
                            val.push('\\');
                            val.push(other as char);
                        }
                    }
                    rp += 1;
                } else if b == b'"' {
                    rp += 1;
                    break;
                } else {
                    val.push(b as char);
                    rp += 1;
                }
            }
            val
        } else {
            let val_start = rp;
            while rp < rest.len() && rest_bytes[rp] != b' ' {
                rp += 1;
            }
            rest[val_start..rp].to_owned()
        };

        out.push(' ');
        out.push_str(key);
        out.push('=');
        out.push_str(&value);
    }
    out
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
/// The stdlib field builders append named arguments as ` key=value` or
/// `key="quoted value"` pairs after the human-readable message.  Values
/// that contain spaces, double-quotes, backslashes, or newlines are wrapped
/// in double quotes with backslash-escape sequences so the tokenizer can
/// round-trip them correctly.
///
/// Tokenization rules:
///   - Fields are separated by a single space.
///   - A field is `key=value` where value is either a bare token (no spaces)
///     or a double-quoted string using `\"` and `\\` escapes.
///   - Walking backwards from the end: a token is a field token if it
///     contains `=`; otherwise it is part of the message.  For
///     quoted-value tokens the full `key="..."` span is walked as a unit.
#[allow(
    clippy::too_many_lines,
    reason = "tokeniser state machine; extraction would harm readability"
)]
fn split_msg_fields(text: &str) -> (&str, String) {
    // ── Phase 1: find where the field block starts ──────────────────────
    // We tokenise backwards, respecting quoted values, until we see a token
    // without `=` (which must be a word of the message).
    let bytes = text.as_bytes();
    let mut pos = text.len(); // exclusive end of un-examined prefix
    let mut fields_start = text.len(); // will track the earliest field byte

    loop {
        if pos == 0 {
            break;
        }
        // Every field except the first is preceded by a space.
        // Skip the space separator if present.
        if pos < text.len() {
            if bytes[pos - 1] == b' ' {
                pos -= 1;
            } else {
                break;
            }
        }
        if pos == 0 {
            break;
        }

        // Determine whether the preceding token is a field or a message word.
        // A quoted-value field ends with `"`.
        let (token_start, has_eq) = if bytes[pos - 1] == b'"' {
            // Quoted value: scan back to find the matching un-escaped opening `"`.
            let mut q = pos - 1;
            let mut found_open = false;
            while q > 0 {
                q -= 1;
                // A `"` that is not preceded by `\` is the opener.
                if bytes[q] == b'"' {
                    // Count preceding backslashes to determine if escaped.
                    let mut nb = 0usize;
                    let mut p2 = q;
                    while p2 > 0 && bytes[p2 - 1] == b'\\' {
                        p2 -= 1;
                        nb += 1;
                    }
                    if nb.is_multiple_of(2) {
                        found_open = true;
                        break;
                    }
                }
            }
            // q now points at the opening `"`.  Before it must be `=` then key.
            if found_open && q > 0 && bytes[q - 1] == b'=' {
                let before_eq = q - 1; // index of `=`
                let tok_start = if let Some(sp) = text[..before_eq].rfind(' ') {
                    sp + 1
                } else {
                    0
                };
                (tok_start, true)
            } else {
                // Malformed — stop and treat the rest as message.
                break;
            }
        } else {
            // Bare-value token: scan back to the previous space.
            let tok_start = if let Some(sp) = text[..pos].rfind(' ') {
                sp + 1
            } else {
                0
            };
            let tok = &text[tok_start..pos];
            (tok_start, tok.contains('='))
        };

        if !has_eq {
            break;
        }

        fields_start = token_start;
        pos = token_start;
        if pos == 0 {
            break;
        }
    }

    if fields_start == text.len() {
        return (text, String::new());
    }

    // ── Phase 2: decode the field block into JSON key:value pairs ───────
    let msg = text[..fields_start].trim_end();
    let rest = &text[fields_start..];
    let mut fields = String::new();

    let rest_bytes = rest.as_bytes();
    let mut rp = 0usize; // read position in `rest`
    while rp < rest.len() {
        // Skip inter-field spaces.
        while rp < rest.len() && rest_bytes[rp] == b' ' {
            rp += 1;
        }
        if rp >= rest.len() {
            break;
        }

        // Read key (up to `=`).
        let key_start = rp;
        while rp < rest.len() && rest_bytes[rp] != b'=' {
            rp += 1;
        }
        if rp >= rest.len() {
            break;
        }
        let key = &rest[key_start..rp];
        rp += 1; // skip `=`

        // Read value: quoted or bare.
        let value: String = if rp < rest.len() && rest_bytes[rp] == b'"' {
            rp += 1; // skip opening `"`
            let mut val = String::new();
            while rp < rest.len() {
                let b = rest_bytes[rp];
                if b == b'\\' && rp + 1 < rest.len() {
                    rp += 1;
                    match rest_bytes[rp] {
                        b'"' => val.push('"'),
                        b'\\' => val.push('\\'),
                        b'n' => val.push('\n'),
                        b'r' => val.push('\r'),
                        other => {
                            val.push('\\');
                            val.push(other as char);
                        }
                    }
                    rp += 1;
                } else if b == b'"' {
                    rp += 1; // skip closing `"`
                    break;
                } else {
                    val.push(b as char);
                    rp += 1;
                }
            }
            val
        } else {
            // Bare value — ends at the next space (or end of string).
            let val_start = rp;
            while rp < rest.len() && rest_bytes[rp] != b' ' {
                rp += 1;
            }
            rest[val_start..rp].to_owned()
        };

        if !key.is_empty() {
            use std::fmt::Write as _;
            let _ = write!(
                fields,
                ",\"{}\":\"{}\"",
                json_escape(key),
                json_escape(&value)
            );
        }
    }

    (msg, fields)
}

/// Encode a field value for the wire format exchanged between the Hew stdlib
/// field builders (`log.field` etc.) and `split_msg_fields`.
///
/// Values that are safe as bare tokens (no space, `"`, `\`, `\n`, `\r`) are
/// returned unchanged.  All others are wrapped in double quotes with
/// backslash escaping so the tokenizer can recover the original string.
///
/// WHY: the wire string is a space-separated list of `key=value` tokens
/// appended to a human-readable message.  A bare value with spaces would be
/// indistinguishable from message words.  Quoting is the minimal change that
/// preserves the existing text-mode output for simple values while making
/// compound values unambiguous.
/// WHEN: becomes unnecessary if the wire format changes to length-prefix or
///       a dedicated separator byte.
/// REAL: a length-prefix format would be more robust but requires a larger
///       protocol change touching both the Hew stdlib and the runtime.
#[must_use]
pub fn encode_field_value(v: &str) -> String {
    // Fast path: values that need no quoting.
    if !v
        .chars()
        .any(|c| matches!(c, ' ' | '"' | '\\' | '\n' | '\r'))
    {
        return v.to_owned();
    }
    let mut out = String::with_capacity(v.len() + 2);
    out.push('"');
    for ch in v.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            other => out.push(other),
        }
    }
    out.push('"');
    out
}

/// Escape a string for inclusion in a JSON string value.
///
/// This is intentionally small and std-only (log core is always linked).
fn json_escape(s: &str) -> String {
    use std::fmt::Write as _;

    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0C}' => out.push_str("\\f"),
            ch if ch.is_control() => {
                let _ = write!(out, "\\u{:04x}", ch as u32);
            }
            ch => out.push(ch),
        }
    }
    out
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

/// Encode a log field value for the wire format used by the slog logger.
///
/// Called from the Hew stdlib (`log.field`, `log.field_int`, etc.) before
/// appending a field to the log wire string.  Values that are safe as bare
/// tokens (no space, `"`, `\`, `\n`, `\r`) are returned unchanged.  All
/// others are wrapped in double quotes with backslash escaping so
/// `split_msg_fields` can recover the original bytes.
///
/// The caller must release the returned pointer with `hew_string_drop`.
///
/// # Safety
///
/// `v` must be a valid NUL-terminated C string (or null, which returns an
/// empty string).
#[no_mangle]
pub unsafe extern "C" fn hew_log_encode_field_value(v: *const c_char) -> *mut c_char {
    // SAFETY: v is a valid NUL-terminated C string per caller contract.
    let encoded = if let Some(s) = unsafe { cstr_to_str(v) } {
        encode_field_value(s)
    } else {
        String::new()
    };
    let bytes = encoded.as_bytes();
    // SAFETY: bytes is a valid UTF-8 slice from a Rust String.
    unsafe { malloc_cstring(bytes.as_ptr(), bytes.len()) }
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
        hew_log_set_format(0); // ensure reset for test-ordering safety
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
        // SAFETY: msg is a valid NUL-terminated C string.
        unsafe { hew_log_emit(2, msg.as_ptr()) };
        let dbg = CString::new("should be filtered").unwrap();
        // DEBUG (3) > INFO (2): filtered out
        // SAFETY: dbg is a valid NUL-terminated C string.
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
        // SAFETY: msg is a valid NUL-terminated C string.
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
    fn json_escape_controls() {
        assert_eq!(json_escape("\u{08}"), "\\b");
        assert_eq!(json_escape("\u{0C}"), "\\f");
        assert_eq!(json_escape("\u{001f}"), "\\u001f");
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

    // ── encode_field_value ───────────────────────────────────────────────

    #[test]
    fn encode_field_value_simple_value_passes_through() {
        assert_eq!(encode_field_value("prod"), "prod");
        assert_eq!(encode_field_value("200"), "200");
        assert_eq!(encode_field_value("/api/v1"), "/api/v1");
        assert_eq!(encode_field_value(""), "");
    }

    #[test]
    fn encode_field_value_space_produces_quoted_form() {
        assert_eq!(encode_field_value("John Doe"), "\"John Doe\"");
    }

    #[test]
    fn encode_field_value_double_quote_is_escaped() {
        assert_eq!(encode_field_value("say \"hi\""), "\"say \\\"hi\\\"\"");
    }

    #[test]
    fn encode_field_value_backslash_is_escaped() {
        assert_eq!(encode_field_value("C:\\Users"), "\"C:\\\\Users\"");
    }

    #[test]
    fn encode_field_value_newline_is_escaped() {
        assert_eq!(encode_field_value("line1\nline2"), "\"line1\\nline2\"");
    }

    #[test]
    fn encode_field_value_cr_is_escaped() {
        assert_eq!(encode_field_value("a\rb"), "\"a\\rb\"");
    }

    // ── split_msg_fields roundtrip for special values ────────────────────

    /// Regression: before this fix, `user=John Doe` split on the space and
    /// produced `user=John` with a stray `Doe`.  Prove the bug is gone.
    #[test]
    fn split_msg_fields_value_with_space_roundtrips() {
        // Wire string as produced by encode_field_value("John Doe") → `user="John Doe"`
        let wire = "login user=\"John Doe\"";
        let (msg, fields) = split_msg_fields(wire);
        assert_eq!(msg, "login");
        // JSON output must contain the full name, not a truncated token.
        assert!(
            fields.contains("\"user\":\"John Doe\""),
            "fields={fields:?}"
        );
    }

    #[test]
    fn split_msg_fields_value_with_equals_roundtrips() {
        // `=` in a value is encoded as a bare char (no special escaping needed);
        // only the key=value delimiter `=` matters structurally.
        let wire = "req path=/a=b";
        let (msg, fields) = split_msg_fields(wire);
        assert_eq!(msg, "req");
        assert!(fields.contains("\"path\":\"/a=b\""), "fields={fields:?}");
    }

    #[test]
    fn split_msg_fields_value_with_embedded_quote_roundtrips() {
        let encoded = encode_field_value("say \"hi\"");
        let wire = format!("msg key={encoded}");
        let (msg, fields) = split_msg_fields(&wire);
        assert_eq!(msg, "msg");
        // json_escape will double-escape, so JSON output has \" for the literal "
        assert!(
            fields.contains("\"key\":\"say \\\"hi\\\"\""),
            "fields={fields:?}"
        );
    }

    #[test]
    fn split_msg_fields_value_with_newline_roundtrips() {
        let encoded = encode_field_value("line1\nline2");
        let wire = format!("event desc={encoded}");
        let (msg, fields) = split_msg_fields(&wire);
        assert_eq!(msg, "event");
        // The decoded value is "line1\nline2"; json_escape turns \n → \\n in JSON.
        assert!(
            fields.contains("\"desc\":\"line1\\nline2\""),
            "fields={fields:?}"
        );
    }

    #[test]
    fn split_msg_fields_multi_fields_with_spaces_roundtrip() {
        let u = encode_field_value("John Doe");
        let p = encode_field_value("/home/john doe");
        let wire = format!("login user={u} path={p} status=200");
        let (msg, fields) = split_msg_fields(&wire);
        assert_eq!(msg, "login");
        assert!(
            fields.contains("\"user\":\"John Doe\""),
            "fields={fields:?}"
        );
        assert!(
            fields.contains("\"path\":\"/home/john doe\""),
            "fields={fields:?}"
        );
        assert!(fields.contains("\"status\":\"200\""), "fields={fields:?}");
    }

    #[test]
    fn split_msg_fields_mixed_bare_and_quoted_fields() {
        // Bare field followed by a quoted field.
        let encoded = encode_field_value("John Doe");
        let wire = format!("msg status=200 user={encoded}");
        let (msg, fields) = split_msg_fields(&wire);
        assert_eq!(msg, "msg");
        assert!(fields.contains("\"status\":\"200\""), "fields={fields:?}");
        assert!(
            fields.contains("\"user\":\"John Doe\""),
            "fields={fields:?}"
        );
    }

    #[test]
    fn decode_text_line_quoted_value_renders_without_quotes() {
        let encoded = encode_field_value("John Doe");
        let wire = format!("login user={encoded}");
        let decoded = decode_text_line(&wire);
        // Human-readable output: no surrounding quotes around the value.
        assert_eq!(decoded, "login user=John Doe");
    }

    #[test]
    fn decode_text_line_bare_value_unchanged() {
        let decoded = decode_text_line("started port=8080");
        assert_eq!(decoded, "started port=8080");
    }

    #[test]
    fn decode_text_line_no_fields_unchanged() {
        let decoded = decode_text_line("hello world");
        assert_eq!(decoded, "hello world");
    }

    #[test]
    fn emit_ex_filters_by_min_level() {
        // min_level=2 (INFO): ERROR(0) passes, DEBUG(3) is filtered
        let msg = CString::new("ex_filter_test").unwrap();
        // SAFETY: msg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(0, 2, 0, msg.as_ptr()) }; // ERROR ≤ INFO: emitted
        let dbg = CString::new("ex_filtered").unwrap();
        // SAFETY: dbg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(3, 2, 0, dbg.as_ptr()) }; // DEBUG > INFO: filtered
    }

    #[test]
    fn emit_ex_respects_explicit_format() {
        let msg = CString::new("ex_json_test key=val").unwrap();
        // SAFETY: msg is a valid NUL-terminated CString.
        // format=1 (JSON) regardless of global LOG_FORMAT
        unsafe { hew_log_emit_ex(2, 4, 1, msg.as_ptr()) };
    }

    #[test]
    fn emit_ex_null_is_noop() {
        // SAFETY: Passing null is the case under test; the function handles it.
        unsafe { hew_log_emit_ex(2, 4, 0, std::ptr::null()) };
    }

    #[test]
    fn emit_ex_suppressed_by_minus_one_threshold() {
        let msg = CString::new("should_not_appear").unwrap();
        // min_level=-1 means nothing passes (even ERROR level 0 > -1)
        // SAFETY: msg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(0, -1, 0, msg.as_ptr()) };
    }

    #[test]
    fn emit_ex_invalid_params_do_not_crash_or_ub() {
        let msg = CString::new("invalid_params k=v").unwrap();

        // Invalid level values (negative/huge) must not panic.
        // SAFETY: msg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(-10, 4, 0, msg.as_ptr()) };
        // SAFETY: msg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(i32::MAX, i32::MAX, 0, msg.as_ptr()) };

        // Invalid min_level values must not panic.
        // SAFETY: msg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(0, i32::MIN, 0, msg.as_ptr()) };
        // SAFETY: msg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(0, i32::MAX, 0, msg.as_ptr()) };

        // Invalid format values must fall back to TEXT branch (no crash).
        // SAFETY: msg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(2, 4, 99, msg.as_ptr()) };
        // SAFETY: msg is a valid NUL-terminated CString.
        unsafe { hew_log_emit_ex(2, 4, -5, msg.as_ptr()) };

        // Null msg must be a safe no-op even when the filter would pass.
        // SAFETY: Passing null is the case under test; the function handles it.
        unsafe { hew_log_emit_ex(0, 4, 999, std::ptr::null()) };

        // And also safe when the function returns early without touching msg.
        // SAFETY: Passing null is the case under test; the function handles it.
        unsafe { hew_log_emit_ex(i32::MAX, 0, 1, std::ptr::null()) };
    }
}
