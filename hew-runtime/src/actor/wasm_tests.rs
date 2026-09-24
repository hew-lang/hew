//! Actor unit tests (wasm32 target).
use super::*;

unsafe extern "C-unwind" fn self_stop_without_reply_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    hew_actor_self_stop();

    std::ptr::null_mut()
}

// ── WASM ask error discrimination tests ─────────────────────────────

// ── MailboxFull / NoRunnableWork discrimination (WASM) ───────────────

/// Dispatch that does nothing: receives the message but does not reply and
/// does not self-stop. Used to drive `MailboxFull` and `NoRunnableWork` tests.
unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    std::ptr::null_mut()
}
