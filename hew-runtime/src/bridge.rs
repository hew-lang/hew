//! WASM host bridge: tick-driven scheduler, message passing, and type metadata.
//!
//! This module provides the boundary between a WASM host (browser JS, embedded
//! application) and the Hew actor runtime running inside a WASM module.
//!
//! # Host API
//!
//! - [`hew_wasm_tick`] — drive the scheduler forward (run N activations)
//! - [`hew_wasm_send`] — deliver a message to a named actor
//! - [`hew_wasm_recv`] — poll for outbound messages from actors
//! - [`hew_wasm_query_meta`] — query actor type metadata (JSON)
//!
//! # Type Metadata
//!
//! The codegen emits calls to [`hew_wasm_register_actor_meta`] during actor
//! registration, populating a metadata registry that the host can query via
//! [`hew_wasm_query_meta`]. This lets JS hosts construct typed message payloads
//! and deserialize responses without hardcoded knowledge of actor schemas.
//!
//! Metadata format (JSON):
//! ```json
//! {
//!   "actors": {
//!     "Counter": {
//!       "handlers": [
//!         {
//!           "name": "increment",
//!           "msg_type": 0,
//!           "params": [
//!             { "name": "amount", "type": "i64", "offset": 0, "size": 8 }
//!           ],
//!           "return_type": null
//!         }
//!       ]
//!     }
//!   }
//! }
//! ```

use std::collections::VecDeque;
use std::ffi::c_void;

// ── Outbound message queue ──────────────────────────────────────────────

/// An outbound message emitted by an actor for the host.
struct OutboundMsg {
    /// Source actor name (empty string if unnamed).
    source: String,
    /// Message type tag.
    msg_type: i32,
    /// Payload bytes (deep-copied from actor dispatch).
    data: Vec<u8>,
}

/// Outbound message queue. Actors call `hew_wasm_emit` during dispatch,
/// which appends here. The host drains via `hew_wasm_recv`.
static mut OUTBOUND: Option<VecDeque<OutboundMsg>> = None;

fn outbound_queue() -> &'static mut VecDeque<OutboundMsg> {
    // SAFETY: WASM is single-threaded.
    unsafe { OUTBOUND.get_or_insert_with(VecDeque::new) }
}

// ── Type metadata registry ──────────────────────────────────────────────

/// Describes a single parameter in a receive handler.
#[repr(C)]
pub struct HewParamMeta {
    /// Parameter name (NUL-terminated C string, static lifetime from codegen).
    pub name: *const u8,
    /// Type name (e.g., "i32", "i64", "f64", "bool", "string", "ptr").
    pub type_name: *const u8,
    /// Byte offset within the packed message struct.
    pub offset: u32,
    /// Size in bytes.
    pub size: u32,
}

/// Describes a single receive handler on an actor.
#[repr(C)]
pub struct HewHandlerMeta {
    /// Handler name (NUL-terminated C string).
    pub name: *const u8,
    /// Message type index (0-based).
    pub msg_type: i32,
    /// Number of parameters.
    pub param_count: u32,
    /// Pointer to array of `param_count` [`HewParamMeta`] entries.
    pub params: *const HewParamMeta,
    /// Return type name (NUL-terminated), or null if void.
    pub return_type: *const u8,
    /// Return type size in bytes (0 if void).
    pub return_size: u32,
}

/// Describes an actor's complete message interface.
#[repr(C)]
pub struct HewActorMeta {
    /// Actor type name (NUL-terminated C string).
    pub name: *const u8,
    /// Number of receive handlers.
    pub handler_count: u32,
    /// Pointer to array of `handler_count` [`HewHandlerMeta`] entries.
    pub handlers: *const HewHandlerMeta,
}

/// Registered actor metadata. Populated by codegen via
/// [`hew_wasm_register_actor_meta`].
static mut META_REGISTRY: Option<Vec<ActorMetaEntry>> = None;

/// Owned metadata entry (strings are heap-allocated copies).
struct ActorMetaEntry {
    name: String,
    handlers: Vec<HandlerMetaEntry>,
}

struct HandlerMetaEntry {
    name: String,
    msg_type: i32,
    params: Vec<ParamMetaEntry>,
    return_type: Option<String>,
    return_size: u32,
}

struct ParamMetaEntry {
    name: String,
    type_name: String,
    offset: u32,
    size: u32,
}

fn meta_registry() -> &'static mut Vec<ActorMetaEntry> {
    // SAFETY: WASM is single-threaded.
    unsafe { META_REGISTRY.get_or_insert_with(Vec::new) }
}

/// Cached JSON output from [`hew_wasm_query_meta`]. Invalidated when new
/// metadata is registered.
static mut META_JSON_CACHE: Option<String> = None;

// ── Host → WASM: send a message to a named actor ───────────────────────

/// Send a message to a named actor inside the WASM module.
///
/// The host provides the actor name, message type index, and a data payload
/// that must match the packed struct layout for that handler.
///
/// Returns 0 on success, -1 if the actor name is not found, -2 if the
/// mailbox send fails.
///
/// # Safety
///
/// - `name_ptr` must point to `name_len` valid UTF-8 bytes.
/// - `data_ptr` must point to `data_len` readable bytes (or be null if
///   `data_len` is 0).
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_send(
    name_ptr: *const u8,
    name_len: usize,
    msg_type: i32,
    data_ptr: *const c_void,
    data_len: usize,
) -> i32 {
    if name_ptr.is_null() || name_len == 0 {
        return -1;
    }

    // SAFETY: Caller guarantees name_ptr/name_len validity.
    let name_bytes = unsafe { std::slice::from_raw_parts(name_ptr, name_len) };
    let name = match std::str::from_utf8(name_bytes) {
        Ok(s) => s,
        Err(_) => return -1,
    };

    // Look up the actor in the registry.
    let name_cstr = match std::ffi::CString::new(name) {
        Ok(s) => s,
        Err(_) => return -1,
    };

    extern "C" {
        fn hew_registry_lookup(name: *const std::ffi::c_char) -> *mut c_void;
        fn hew_mailbox_send(mb: *mut c_void, msg_type: i32, data: *mut c_void, size: usize) -> i32;
    }

    // SAFETY: name_cstr is a valid NUL-terminated string.
    let actor_ptr = unsafe { hew_registry_lookup(name_cstr.as_ptr()) };
    if actor_ptr.is_null() {
        return -1;
    }

    // Read the actor's mailbox pointer. The mailbox field is at a known
    // offset in HewActor (field index 6, after sched_link_next, id, pid,
    // state, state_size, dispatch).
    //
    // We use the same repr(C) layout as the native HewActor. Rather than
    // importing the full struct (which has native-only dependencies), we
    // compute the offset manually.
    //
    // HewActor layout (64-bit):
    //   0: sched_link_next (8 bytes, AtomicPtr)
    //   8: id (8 bytes, u64)
    //  16: pid (8 bytes, u64)
    //  24: state (8 bytes, *mut c_void)
    //  32: state_size (8 bytes, usize)
    //  40: dispatch (8 bytes, Option<fn>)
    //  48: mailbox (8 bytes, *mut c_void)
    const MAILBOX_OFFSET: usize = 48;

    // SAFETY: actor_ptr is a valid HewActor pointer from the registry.
    let mailbox_ptr = unsafe {
        *(actor_ptr
            .cast::<u8>()
            .add(MAILBOX_OFFSET)
            .cast::<*mut c_void>())
    };
    if mailbox_ptr.is_null() {
        return -2;
    }

    // SAFETY: mailbox_ptr is valid, data_ptr/data_len are caller-guaranteed.
    let rc = unsafe { hew_mailbox_send(mailbox_ptr, msg_type, data_ptr as *mut c_void, data_len) };

    // After sending, wake the actor: transition IDLE → RUNNABLE and enqueue.
    // This mirrors what the native scheduler does in hew_actor_send.
    //
    // actor_state is at offset 56 (after mailbox at 48):
    //  48: mailbox (8 bytes)
    //  56: actor_state (4 bytes, AtomicI32)
    const ACTOR_STATE_OFFSET: usize = 56;
    const IDLE: i32 = 0; // HewActorState::Idle
    const RUNNABLE: i32 = 1; // HewActorState::Runnable

    // SAFETY: actor_ptr + offset points to AtomicI32 actor_state.
    let state_ptr = unsafe {
        &*(actor_ptr
            .cast::<u8>()
            .add(ACTOR_STATE_OFFSET)
            .cast::<std::sync::atomic::AtomicI32>())
    };

    // Single-threaded: if idle, make runnable and enqueue.
    if state_ptr.load(std::sync::atomic::Ordering::Relaxed) == IDLE {
        state_ptr.store(RUNNABLE, std::sync::atomic::Ordering::Relaxed);
        // Enqueue into the WASM scheduler's run queue.
        // SAFETY: actor_ptr is a valid actor.
        unsafe { crate::scheduler_wasm::hew_wasm_sched_enqueue(actor_ptr) };
    }

    rc
}

// ── WASM → Host: actor emits a message ──────────────────────────────────

/// Emit a message from an actor to the host.
///
/// Called from within an actor's dispatch function. The message is queued
/// and the host retrieves it via [`hew_wasm_recv`].
///
/// # Safety
///
/// - `data_ptr` must point to `data_len` readable bytes (or be null if
///   `data_len` is 0).
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_emit(msg_type: i32, data_ptr: *const c_void, data_len: usize) {
    let data = if data_len > 0 && !data_ptr.is_null() {
        // SAFETY: Caller guarantees data_ptr/data_len validity.
        unsafe { std::slice::from_raw_parts(data_ptr.cast::<u8>(), data_len) }.to_vec()
    } else {
        Vec::new()
    };

    // Try to get the current actor's name from the registry for source info.
    // For now, leave source empty — the host can identify by msg_type.
    let source = String::new();

    outbound_queue().push_back(OutboundMsg {
        source,
        msg_type,
        data,
    });
}

// ── Host polls for outbound messages ────────────────────────────────────

/// Header prepended to each message in the recv buffer.
///
/// Layout (little-endian):
///   0..4:  msg_type (i32)
///   4..8:  data_len (u32)
///   8..:   data bytes (data_len bytes)
const RECV_HEADER_SIZE: usize = 8;

/// Poll for outbound messages from actors.
///
/// Drains the outbound queue into `out_buf`, packing each message as:
///   `[msg_type: i32-le][data_len: u32-le][data: data_len bytes]`
///
/// Returns the number of bytes written. Returns 0 if no messages are
/// pending. Messages that don't fit in the remaining buffer are left in
/// the queue for the next call.
///
/// # Safety
///
/// `out_buf` must point to `buf_len` writable bytes.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_recv(out_buf: *mut u8, buf_len: usize) -> i32 {
    if out_buf.is_null() || buf_len == 0 {
        return 0;
    }

    let queue = outbound_queue();
    let mut offset = 0usize;

    while let Some(front) = queue.front() {
        let needed = RECV_HEADER_SIZE + front.data.len();
        if offset + needed > buf_len {
            break;
        }

        let msg = queue.pop_front().unwrap();

        // SAFETY: out_buf + offset is within bounds (checked above).
        unsafe {
            // Write msg_type as i32 little-endian.
            let dest = out_buf.add(offset);
            std::ptr::copy_nonoverlapping(msg.msg_type.to_le_bytes().as_ptr(), dest, 4);
            // Write data_len as u32 little-endian.
            #[expect(
                clippy::cast_possible_truncation,
                reason = "message data will not exceed u32::MAX"
            )]
            let data_len_u32 = msg.data.len() as u32;
            std::ptr::copy_nonoverlapping(data_len_u32.to_le_bytes().as_ptr(), dest.add(4), 4);
            // Write data bytes.
            if !msg.data.is_empty() {
                std::ptr::copy_nonoverlapping(
                    msg.data.as_ptr(),
                    dest.add(RECV_HEADER_SIZE),
                    msg.data.len(),
                );
            }
        }

        offset += needed;
    }

    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "recv buffer size will not exceed i32::MAX"
    )]
    {
        offset as i32
    }
}

/// Return the number of pending outbound messages.
#[cfg_attr(not(test), no_mangle)]
pub extern "C" fn hew_wasm_outbound_len() -> i32 {
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "outbound queue length will not exceed i32::MAX"
    )]
    {
        outbound_queue().len() as i32
    }
}

// ── Host drives the scheduler ───────────────────────────────────────────

/// Drive the WASM scheduler forward.
///
/// Runs up to `max_activations` actor activations, then returns control to
/// the host. Returns the number of actors still in the run queue.
///
/// This is the primary scheduling API for both browser and embedded hosts:
/// - Browser: call from `requestAnimationFrame` or `setTimeout`
/// - Embedded: interleave with host work
///
/// # Safety
///
/// The scheduler must have been initialized with `hew_sched_init`.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_tick(max_activations: i32) -> i32 {
    // SAFETY: Scheduler must be initialized.
    unsafe { crate::scheduler_wasm::hew_wasm_sched_tick(max_activations) }
}

// ── Type metadata registration (called by codegen) ──────────────────────

/// Register an actor's message type metadata.
///
/// Called by codegen during actor type initialization. The metadata
/// describes all receive handlers, their parameters, and return types,
/// enabling the host to construct and parse typed messages.
///
/// # Safety
///
/// `meta` must point to a valid [`HewActorMeta`] struct. All embedded
/// pointers (name strings, handler arrays, param arrays) must be valid
/// for the static lifetime of the WASM module.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_register_actor_meta(meta: *const HewActorMeta) {
    if meta.is_null() {
        return;
    }

    // SAFETY: Caller guarantees meta validity.
    let meta = unsafe { &*meta };

    let name = unsafe { cstr_to_string(meta.name) };

    let mut handlers = Vec::with_capacity(meta.handler_count as usize);
    for i in 0..meta.handler_count as usize {
        // SAFETY: handlers array has handler_count entries.
        let h = unsafe { &*meta.handlers.add(i) };
        let handler_name = unsafe { cstr_to_string(h.name) };

        let mut params = Vec::with_capacity(h.param_count as usize);
        for j in 0..h.param_count as usize {
            // SAFETY: params array has param_count entries.
            let p = unsafe { &*h.params.add(j) };
            params.push(ParamMetaEntry {
                name: unsafe { cstr_to_string(p.name) },
                type_name: unsafe { cstr_to_string(p.type_name) },
                offset: p.offset,
                size: p.size,
            });
        }

        handlers.push(HandlerMetaEntry {
            name: handler_name,
            msg_type: h.msg_type,
            params,
            return_type: if h.return_type.is_null() {
                None
            } else {
                Some(unsafe { cstr_to_string(h.return_type) })
            },
            return_size: h.return_size,
        });
    }

    let reg = meta_registry();

    // Replace existing entry for the same actor name.
    if let Some(existing) = reg.iter_mut().find(|e| e.name == name) {
        existing.handlers = handlers;
    } else {
        reg.push(ActorMetaEntry { name, handlers });
    }

    // Invalidate cache.
    // SAFETY: single-threaded WASM.
    unsafe {
        META_JSON_CACHE = None;
    }
}

/// Read a NUL-terminated C string into an owned `String`.
///
/// # Safety
///
/// `ptr` must point to a valid NUL-terminated UTF-8 string.
unsafe fn cstr_to_string(ptr: *const u8) -> String {
    if ptr.is_null() {
        return String::new();
    }
    // SAFETY: Caller guarantees ptr is a valid NUL-terminated string.
    unsafe { std::ffi::CStr::from_ptr(ptr.cast()) }
        .to_string_lossy()
        .into_owned()
}

// ── Type metadata query (called by host) ────────────────────────────────

/// Query actor type metadata as a JSON string.
///
/// If `name_ptr` is null, returns metadata for ALL registered actors.
/// If `name_ptr` is non-null (pointing to `name_len` UTF-8 bytes), returns
/// metadata for that specific actor (or `{"actors":{}}` if not found).
///
/// Returns a pointer to a NUL-terminated JSON string. The pointer is valid
/// until the next call to `hew_wasm_register_actor_meta` or
/// `hew_wasm_query_meta`.
///
/// The returned `*out_len` is set to the string length (excluding NUL).
///
/// # Safety
///
/// - If non-null, `name_ptr` must point to `name_len` valid UTF-8 bytes.
/// - `out_len` must be a valid pointer to a `usize`.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_query_meta(
    name_ptr: *const u8,
    name_len: usize,
    out_len: *mut usize,
) -> *const u8 {
    let filter_name = if name_ptr.is_null() || name_len == 0 {
        None
    } else {
        // SAFETY: Caller guarantees name_ptr/name_len validity.
        let bytes = unsafe { std::slice::from_raw_parts(name_ptr, name_len) };
        std::str::from_utf8(bytes).ok()
    };

    // Build JSON (or use cache for the all-actors case).
    let json = if filter_name.is_none() {
        // SAFETY: single-threaded.
        unsafe {
            if META_JSON_CACHE.is_none() {
                META_JSON_CACHE = Some(build_meta_json(None));
            }
            META_JSON_CACHE.as_ref().unwrap()
        }
    } else {
        // Per-actor query: always rebuild (cheap for one actor).
        // SAFETY: single-threaded.
        unsafe {
            META_JSON_CACHE = Some(build_meta_json(filter_name));
            META_JSON_CACHE.as_ref().unwrap()
        }
    };

    if !out_len.is_null() {
        // SAFETY: Caller guarantees out_len is valid.
        unsafe { *out_len = json.len() };
    }

    json.as_ptr()
}

/// Build a JSON string describing actor metadata.
fn build_meta_json(filter_name: Option<&str>) -> String {
    let reg = meta_registry();

    let mut json = String::from("{\"actors\":{");
    let mut first_actor = true;

    for entry in reg.iter() {
        if let Some(name) = filter_name {
            if entry.name != name {
                continue;
            }
        }

        if !first_actor {
            json.push(',');
        }
        first_actor = false;

        json.push('"');
        json_escape_into(&mut json, &entry.name);
        json.push_str("\":{\"handlers\":[");

        let mut first_handler = true;
        for h in &entry.handlers {
            if !first_handler {
                json.push(',');
            }
            first_handler = false;

            json.push_str("{\"name\":\"");
            json_escape_into(&mut json, &h.name);
            json.push_str("\",\"msg_type\":");
            json.push_str(&h.msg_type.to_string());
            json.push_str(",\"params\":[");

            let mut first_param = true;
            for p in &h.params {
                if !first_param {
                    json.push(',');
                }
                first_param = false;

                json.push_str("{\"name\":\"");
                json_escape_into(&mut json, &p.name);
                json.push_str("\",\"type\":\"");
                json_escape_into(&mut json, &p.type_name);
                json.push_str("\",\"offset\":");
                json.push_str(&p.offset.to_string());
                json.push_str(",\"size\":");
                json.push_str(&p.size.to_string());
                json.push('}');
            }

            json.push_str("],\"return_type\":");
            match &h.return_type {
                Some(rt) => {
                    json.push_str("{\"type\":\"");
                    json_escape_into(&mut json, rt);
                    json.push_str("\",\"size\":");
                    json.push_str(&h.return_size.to_string());
                    json.push('}');
                }
                None => json.push_str("null"),
            }
            json.push('}');
        }

        json.push_str("]}");
    }

    json.push_str("}}");
    json
}

/// Escape a string for JSON output (handles `"`, `\`, and control chars).
fn json_escape_into(out: &mut String, s: &str) {
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if c.is_control() => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
}

// ── Initialization / cleanup ────────────────────────────────────────────

/// Initialize the bridge (called from `hew_sched_init` on WASM).
///
/// Sets up the outbound queue and metadata registry.
pub fn bridge_init() {
    // SAFETY: single-threaded WASM.
    unsafe {
        if OUTBOUND.is_none() {
            OUTBOUND = Some(VecDeque::new());
        }
        if META_REGISTRY.is_none() {
            META_REGISTRY = Some(Vec::new());
        }
    }
}

/// Shut down the bridge, draining queues.
pub fn bridge_shutdown() {
    // SAFETY: single-threaded WASM.
    unsafe {
        if let Some(ref mut q) = OUTBOUND {
            q.clear();
        }
        META_JSON_CACHE = None;
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    // Serialize tests to avoid data races on static mut globals.
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    fn reset_bridge() {
        unsafe {
            OUTBOUND = Some(VecDeque::new());
            META_REGISTRY = Some(Vec::new());
            META_JSON_CACHE = None;
        }
    }

    #[test]
    fn emit_and_recv_roundtrip() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let data: [u8; 4] = [0x01, 0x02, 0x03, 0x04];
        unsafe {
            hew_wasm_emit(42, data.as_ptr().cast(), data.len());
        }

        assert_eq!(hew_wasm_outbound_len(), 1);

        let mut buf = [0u8; 64];
        let written = unsafe { hew_wasm_recv(buf.as_mut_ptr(), buf.len()) };

        // Header: 4 bytes msg_type + 4 bytes data_len + 4 bytes data = 12
        assert_eq!(written, 12);

        // msg_type = 42 (little-endian i32)
        assert_eq!(i32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]), 42);
        // data_len = 4
        assert_eq!(u32::from_le_bytes([buf[4], buf[5], buf[6], buf[7]]), 4);
        // data
        assert_eq!(&buf[8..12], &data);

        // Queue should be empty now.
        assert_eq!(hew_wasm_outbound_len(), 0);
    }

    #[test]
    fn recv_empty_returns_zero() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let mut buf = [0u8; 64];
        let written = unsafe { hew_wasm_recv(buf.as_mut_ptr(), buf.len()) };
        assert_eq!(written, 0);
    }

    #[test]
    fn recv_null_buf_returns_zero() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        unsafe { hew_wasm_emit(1, std::ptr::null(), 0) };
        let written = unsafe { hew_wasm_recv(std::ptr::null_mut(), 0) };
        assert_eq!(written, 0);
    }

    #[test]
    fn recv_partial_drain() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // Emit two messages.
        let d1: [u8; 4] = [1, 2, 3, 4];
        let d2: [u8; 4] = [5, 6, 7, 8];
        unsafe {
            hew_wasm_emit(10, d1.as_ptr().cast(), d1.len());
            hew_wasm_emit(20, d2.as_ptr().cast(), d2.len());
        }
        assert_eq!(hew_wasm_outbound_len(), 2);

        // Buffer only big enough for one message (12 bytes).
        let mut buf = [0u8; 12];
        let written = unsafe { hew_wasm_recv(buf.as_mut_ptr(), buf.len()) };
        assert_eq!(written, 12);
        assert_eq!(i32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]), 10);

        // Second message still pending.
        assert_eq!(hew_wasm_outbound_len(), 1);

        // Drain the rest.
        let written = unsafe { hew_wasm_recv(buf.as_mut_ptr(), buf.len()) };
        assert_eq!(written, 12);
        assert_eq!(i32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]), 20);

        assert_eq!(hew_wasm_outbound_len(), 0);
    }

    #[test]
    fn emit_null_data() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        unsafe { hew_wasm_emit(99, std::ptr::null(), 0) };
        assert_eq!(hew_wasm_outbound_len(), 1);

        let mut buf = [0u8; 64];
        let written = unsafe { hew_wasm_recv(buf.as_mut_ptr(), buf.len()) };
        // Header only: 4 + 4 = 8 bytes, data_len = 0.
        assert_eq!(written, 8);
        assert_eq!(i32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]), 99);
        assert_eq!(u32::from_le_bytes([buf[4], buf[5], buf[6], buf[7]]), 0);
    }

    #[test]
    fn send_to_unknown_actor_returns_error() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let name = b"nonexistent";
        let rc = unsafe { hew_wasm_send(name.as_ptr(), name.len(), 0, std::ptr::null(), 0) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn send_null_name_returns_error() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let rc = unsafe { hew_wasm_send(std::ptr::null(), 0, 0, std::ptr::null(), 0) };
        assert_eq!(rc, -1);
    }

    // ── Type metadata tests ─────────────────────────────────────────────

    #[test]
    fn query_meta_empty() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let mut len: usize = 0;
        let ptr = unsafe { hew_wasm_query_meta(std::ptr::null(), 0, &mut len) };
        assert!(len > 0);

        let json = unsafe { std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap() };
        assert_eq!(json, "{\"actors\":{}}");
    }

    #[test]
    fn register_and_query_meta() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // Build metadata for a "Counter" actor with one handler.
        let param = HewParamMeta {
            name: b"amount\0".as_ptr(),
            type_name: b"i64\0".as_ptr(),
            offset: 0,
            size: 8,
        };
        let handler = HewHandlerMeta {
            name: b"increment\0".as_ptr(),
            msg_type: 0,
            param_count: 1,
            params: &param,
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let actor_meta = HewActorMeta {
            name: b"Counter\0".as_ptr(),
            handler_count: 1,
            handlers: &handler,
        };

        unsafe { hew_wasm_register_actor_meta(&actor_meta) };

        let mut len: usize = 0;
        let ptr = unsafe { hew_wasm_query_meta(std::ptr::null(), 0, &mut len) };
        let json = unsafe { std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap() };

        // Verify the JSON contains expected structure.
        assert!(json.contains("\"Counter\""));
        assert!(json.contains("\"increment\""));
        assert!(json.contains("\"amount\""));
        assert!(json.contains("\"i64\""));
        assert!(json.contains("\"offset\":0"));
        assert!(json.contains("\"size\":8"));
        assert!(json.contains("\"return_type\":null"));
    }

    #[test]
    fn query_meta_by_name() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // Register two actors.
        let h1 = HewHandlerMeta {
            name: b"ping\0".as_ptr(),
            msg_type: 0,
            param_count: 0,
            params: std::ptr::null(),
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let m1 = HewActorMeta {
            name: b"Pinger\0".as_ptr(),
            handler_count: 1,
            handlers: &h1,
        };

        let h2 = HewHandlerMeta {
            name: b"pong\0".as_ptr(),
            msg_type: 0,
            param_count: 0,
            params: std::ptr::null(),
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let m2 = HewActorMeta {
            name: b"Ponger\0".as_ptr(),
            handler_count: 1,
            handlers: &h2,
        };

        unsafe {
            hew_wasm_register_actor_meta(&m1);
            hew_wasm_register_actor_meta(&m2);
        }

        // Query just "Pinger".
        let name = b"Pinger";
        let mut len: usize = 0;
        let ptr = unsafe { hew_wasm_query_meta(name.as_ptr(), name.len(), &mut len) };
        let json = unsafe { std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap() };

        assert!(json.contains("\"Pinger\""));
        assert!(json.contains("\"ping\""));
        // Should NOT contain the other actor.
        assert!(!json.contains("\"Ponger\""));
    }

    #[test]
    fn register_meta_with_return_type() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let handler = HewHandlerMeta {
            name: b"get_total\0".as_ptr(),
            msg_type: 0,
            param_count: 0,
            params: std::ptr::null(),
            return_type: b"i64\0".as_ptr(),
            return_size: 8,
        };
        let actor_meta = HewActorMeta {
            name: b"Accumulator\0".as_ptr(),
            handler_count: 1,
            handlers: &handler,
        };

        unsafe { hew_wasm_register_actor_meta(&actor_meta) };

        let mut len: usize = 0;
        let ptr = unsafe { hew_wasm_query_meta(std::ptr::null(), 0, &mut len) };
        let json = unsafe { std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap() };

        assert!(json.contains("\"return_type\":{\"type\":\"i64\",\"size\":8}"));
    }

    #[test]
    fn bridge_init_and_shutdown() {
        let _guard = TEST_LOCK.lock().unwrap();
        bridge_init();

        unsafe {
            hew_wasm_emit(1, std::ptr::null(), 0);
        }
        assert_eq!(hew_wasm_outbound_len(), 1);

        bridge_shutdown();
        // Queue still exists but is cleared.
        assert_eq!(hew_wasm_outbound_len(), 0);
    }
}
