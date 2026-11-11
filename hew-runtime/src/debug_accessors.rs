//! C ABI a debugger calls to read the runtime's failure and actor state.
//!
//! Always linked, release builds included. A live process answers through
//! these functions (`call hew_debug_last_fault()` in gdb, `expr` in lldb). A
//! core file cannot run code, so the last failure is also kept at the
//! documented symbol `hew_last_fault_record`
//! ([`crate::failure_record::hew_last_fault_record`]), which a script reads
//! directly.

use std::ffi::{c_char, CString};
use std::sync::Mutex;

use crate::failure_record::FailureRecord;

/// Keep the accessors in every image that can fail or run actors: the linker
/// drops unreferenced functions, and nothing in generated code calls these.
pub(crate) fn retain() {
    std::hint::black_box((
        hew_debug_last_fault as extern "C" fn() -> *const FailureRecord,
        hew_debug_current_actor as extern "C" fn() -> *mut crate::actor::HewActor,
        hew_debug_dump_actors as extern "C" fn() -> *const c_char,
    ));
}

/// The most recent failure raised in this process, or null if none was.
/// Inspect the returned storage only while failure publishers are stopped.
#[no_mangle]
pub extern "C" fn hew_debug_last_fault() -> *const FailureRecord {
    crate::failure_record::last_fault_ptr()
}

/// The actor running on the calling thread, or null outside an actor turn.
#[no_mangle]
pub extern "C" fn hew_debug_current_actor() -> *mut crate::actor::HewActor {
    crate::actor::hew_actor_self()
}

/// Every live actor as JSON, the same document `/api/actors` serves.
///
/// The text stays valid until the next call. Returns null when another call
/// is in progress, so a debugger stopping a thread inside this function
/// cannot deadlock a second caller.
#[no_mangle]
pub extern "C" fn hew_debug_dump_actors() -> *const c_char {
    static DUMP: Mutex<Option<CString>> = Mutex::new(None);
    let Ok(mut slot) = DUMP.try_lock() else {
        return std::ptr::null();
    };
    let json = actors_json();
    let text = CString::new(json).unwrap_or_default();
    slot.insert(text).as_ptr()
}

/// The `/api/actors` document.
#[must_use]
pub fn actors_json() -> String {
    #[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
    {
        crate::profiler::actors_json(&crate::profiler::actor_registry::snapshot_all())
    }
    #[cfg(any(not(feature = "profiler"), target_arch = "wasm32"))]
    {
        "[]".to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dump_is_a_json_array() {
        let text = hew_debug_dump_actors();
        assert!(!text.is_null());
        // SAFETY: the dump stays valid until the next call.
        let json = unsafe { std::ffi::CStr::from_ptr(text) }.to_str().unwrap();
        assert!(serde_json::from_str::<serde_json::Value>(json)
            .unwrap()
            .is_array());
    }

    #[test]
    fn no_actor_outside_a_turn() {
        assert!(hew_debug_current_actor().is_null());
    }
}
