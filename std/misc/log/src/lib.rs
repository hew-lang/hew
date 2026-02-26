//! Hew runtime: `logging` module (legacy).
//!
//! Provides structured logging via the `log` + `env_logger` crates.
//! All functions use C ABI so they can be called from LLVM-compiled Hew
//! binaries.
//!
//! The core logging primitives (`hew_log_emit`, `hew_log_set_level`, etc.)
//! live in `log_core` and are always compiled.  This module adds
//! the `log`-crate-based legacy helpers that are feature-gated.
//!
//! Initialization is guarded by [`std::sync::Once`] so it is safe to call
//! `hew_log_init` or `hew_log_init_level` multiple times — only the first
//! call takes effect.

#[cfg(feature = "export-meta")]
pub mod export_meta;

use std::os::raw::c_char;
use std::sync::Once;

use hew_cabi::cabi::cstr_to_str;

static INIT: Once = Once::new();

/// Initialize the logger using the `RUST_LOG` environment variable (or
/// default to `info`).
///
/// Safe to call multiple times; only the first invocation has an effect.
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(
        module = "std::misc::log",
        name = "setup",
        doc = "Initialize the default logger at info level"
    )
)]
#[no_mangle]
pub extern "C" fn hew_log_init() {
    INIT.call_once(|| {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();
    });
}

/// Initialize the logger with a specific maximum level.
///
/// Level mapping: 0 = error, 1 = warn, 2 = info, 3 = debug, 4 = trace.
/// Any other value defaults to `info`.
///
/// Safe to call multiple times; only the first invocation has an effect.
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(
        module = "std::misc::log",
        name = "setup_level",
        doc = "Initialize the logger at the given severity level (0=error...4=trace)"
    )
)]
#[no_mangle]
pub extern "C" fn hew_log_init_level(level: i32) {
    INIT.call_once(|| {
        let filter = match level {
            0 => "error",
            1 => "warn",
            3 => "debug",
            4 => "trace",
            _ => "info",
        };
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or(filter)).init();
    });
}

/// Log a message at **error** level.
///
/// # Safety
///
/// `msg` must be a valid NUL-terminated C string (or null, which is a no-op).
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(module = "std::misc::log", doc = "Log a message at error level")
)]
#[no_mangle]
pub unsafe extern "C" fn hew_log_error(msg: *const c_char) {
    // SAFETY: msg is a valid NUL-terminated C string per caller contract.
    if let Some(s) = unsafe { cstr_to_str(msg) } {
        log::error!("{s}");
    }
}

/// Log a message at **warn** level.
///
/// # Safety
///
/// `msg` must be a valid NUL-terminated C string (or null, which is a no-op).
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(module = "std::misc::log", doc = "Log a message at warn level")
)]
#[no_mangle]
pub unsafe extern "C" fn hew_log_warn(msg: *const c_char) {
    // SAFETY: msg is a valid NUL-terminated C string per caller contract.
    if let Some(s) = unsafe { cstr_to_str(msg) } {
        log::warn!("{s}");
    }
}

/// Log a message at **info** level.
///
/// # Safety
///
/// `msg` must be a valid NUL-terminated C string (or null, which is a no-op).
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(module = "std::misc::log", doc = "Log a message at info level")
)]
#[no_mangle]
pub unsafe extern "C" fn hew_log_info(msg: *const c_char) {
    // SAFETY: msg is a valid NUL-terminated C string per caller contract.
    if let Some(s) = unsafe { cstr_to_str(msg) } {
        log::info!("{s}");
    }
}

/// Log a message at **debug** level.
///
/// # Safety
///
/// `msg` must be a valid NUL-terminated C string (or null, which is a no-op).
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(module = "std::misc::log", doc = "Log a message at debug level")
)]
#[no_mangle]
pub unsafe extern "C" fn hew_log_debug(msg: *const c_char) {
    // SAFETY: msg is a valid NUL-terminated C string per caller contract.
    if let Some(s) = unsafe { cstr_to_str(msg) } {
        log::debug!("{s}");
    }
}

/// Log a message at **trace** level.
///
/// # Safety
///
/// `msg` must be a valid NUL-terminated C string (or null, which is a no-op).
#[cfg_attr(
    feature = "export-meta",
    hew_export_macro::hew_export(module = "std::misc::log", doc = "Log a message at trace level")
)]
#[no_mangle]
pub unsafe extern "C" fn hew_log_trace(msg: *const c_char) {
    // SAFETY: msg is a valid NUL-terminated C string per caller contract.
    if let Some(s) = unsafe { cstr_to_str(msg) } {
        log::trace!("{s}");
    }
}

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_init_is_idempotent() {
        // Calling init multiple times must not panic.
        hew_log_init();
        hew_log_init();
    }

    #[test]
    fn test_log_null_pointer_is_noop() {
        hew_log_init();
        // Null pointers must not crash.
        // SAFETY: Passing null is the case under test; the function handles it.
        unsafe {
            hew_log_error(std::ptr::null());
            hew_log_warn(std::ptr::null());
            hew_log_info(std::ptr::null());
            hew_log_debug(std::ptr::null());
            hew_log_trace(std::ptr::null());
        }
    }

    #[test]
    fn test_log_valid_message() {
        hew_log_init();
        let msg = CString::new("hello from test").expect("CString::new failed");
        // SAFETY: msg is a valid CString.
        unsafe {
            hew_log_info(msg.as_ptr());
        }
    }
}
