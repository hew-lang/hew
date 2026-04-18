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

use std::collections::{HashMap, VecDeque};
use std::ffi::{c_void, CString};
use std::sync::{Mutex, MutexGuard, OnceLock};

use hew_cabi::cabi::cstr_to_string_lossy;

use crate::actor::HewActor;

// ── Outbound message queue ──────────────────────────────────────────────

/// An outbound message emitted by an actor for the host.
struct OutboundMsg {
    /// Message type tag.
    msg_type: i32,
    /// Payload bytes (deep-copied from actor dispatch).
    data: Vec<u8>,
}

/// Outbound message queue. Actors call `hew_wasm_emit` during dispatch,
/// which appends here. The host drains via `hew_wasm_recv`.
static OUTBOUND: OnceLock<Mutex<VecDeque<OutboundMsg>>> = OnceLock::new();

fn outbound_queue() -> MutexGuard<'static, VecDeque<OutboundMsg>> {
    OUTBOUND
        .get_or_init(|| Mutex::new(VecDeque::new()))
        .lock()
        .unwrap()
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
impl std::fmt::Debug for HewParamMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewParamMeta")
            .field("name", &self.name)
            .field("type_name", &self.type_name)
            .field("offset", &self.offset)
            .field("size", &self.size)
            .finish()
    }
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
impl std::fmt::Debug for HewHandlerMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewHandlerMeta")
            .field("name", &self.name)
            .field("msg_type", &self.msg_type)
            .field("param_count", &self.param_count)
            .field("params", &self.params)
            .field("return_type", &self.return_type)
            .field("return_size", &self.return_size)
            .finish()
    }
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
impl std::fmt::Debug for HewActorMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewActorMeta")
            .field("name", &self.name)
            .field("handler_count", &self.handler_count)
            .field("handlers", &self.handlers)
            .finish()
    }
}

/// Combined metadata registry + cache state.
struct MetaState {
    registry: HashMap<String, ActorMetaEntry>,
    /// `msg_type → "ActorName::handler_name"` side table for trace attribution.
    ///
    /// Populated at registration time by [`hew_wasm_register_actor_meta`].
    ///
    /// NOTE: if two actor types register the same `msg_type` integer, the
    /// last-registered wins.  This is a pre-existing ambiguity in the bridge
    /// model for AOT programs; collision is a codegen concern tracked
    /// separately.  See [`resolve_handler_name`] for the lookup path.
    handler_names: HashMap<i32, String>,
    cache_all: Option<String>,
}

/// Registered actor metadata and cache state. Populated by codegen via
/// [`hew_wasm_register_actor_meta`].
static META_STATE: OnceLock<Mutex<MetaState>> = OnceLock::new();

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

fn meta_state() -> MutexGuard<'static, MetaState> {
    META_STATE
        .get_or_init(|| {
            Mutex::new(MetaState {
                registry: HashMap::new(),
                handler_names: HashMap::new(),
                cache_all: None,
            })
        })
        .lock()
        .unwrap()
}

/// Resolve a `msg_type` integer to its fully-qualified handler name.
///
/// Returns `Some("ActorName::handler_name")` when the `msg_type` was
/// previously registered via [`hew_wasm_register_actor_meta`], or `None`
/// if registration has not yet occurred for this `msg_type` (e.g. a trace
/// event arrived before registration completed).
///
/// NOTE: last-registered wins on `msg_type` collision across actor types.
/// This is a pre-existing ambiguity in the AOT model — see the `handler_names`
/// field documentation on [`MetaState`] for details.
pub(crate) fn resolve_handler_name(msg_type: i32) -> Option<String> {
    meta_state().handler_names.get(&msg_type).cloned()
}

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
#[must_use]
pub unsafe extern "C" fn hew_wasm_send(
    name_ptr: *const u8,
    name_len: usize,
    msg_type: i32,
    data_ptr: *const c_void,
    data_len: usize,
) -> i32 {
    use crate::mailbox_wasm::{hew_mailbox_send, HewMailboxWasm};
    use crate::registry::hew_registry_lookup;

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
    const MAILBOX_OFFSET: usize = std::mem::offset_of!(HewActor, mailbox);

    // Verify offsets match expectations (checked at compile time).
    #[cfg(target_pointer_width = "64")]
    const _: () = assert!(MAILBOX_OFFSET == 48);
    #[cfg(target_pointer_width = "32")]
    const _: () = assert!(MAILBOX_OFFSET == 36);

    // After sending, wake the actor: transition IDLE → RUNNABLE and enqueue.
    // This mirrors what the native scheduler does in hew_actor_send.
    //
    // actor_state is at offset 56 (after mailbox at 48):
    //  48: mailbox (8 bytes)
    //  56: actor_state (4 bytes, AtomicI32)
    const ACTOR_STATE_OFFSET: usize = std::mem::offset_of!(HewActor, actor_state);

    // Verify offsets match expectations (checked at compile time).
    #[cfg(target_pointer_width = "64")]
    const _: () = assert!(ACTOR_STATE_OFFSET == 56);
    #[cfg(target_pointer_width = "32")]
    const _: () = assert!(ACTOR_STATE_OFFSET == 40);

    const IDLE: i32 = 0; // HewActorState::Idle
    const RUNNABLE: i32 = 1; // HewActorState::Runnable

    if name_ptr.is_null() || name_len == 0 {
        return -1;
    }

    // SAFETY: Caller guarantees name_ptr/name_len validity.
    let name_bytes = unsafe { std::slice::from_raw_parts(name_ptr, name_len) };
    let Ok(name) = std::str::from_utf8(name_bytes) else {
        return -1;
    };

    // Look up the actor in the registry.
    let Ok(name_cstr) = std::ffi::CString::new(name) else {
        return -1;
    };

    // SAFETY: name_cstr is a valid NUL-terminated string.
    let actor_ptr = unsafe { hew_registry_lookup(name_cstr.as_ptr()) };
    if actor_ptr.is_null() {
        return -1;
    }

    #[expect(
        clippy::cast_ptr_alignment,
        reason = "pointer from registry has correct alignment"
    )]
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
    let rc = unsafe {
        hew_mailbox_send(
            mailbox_ptr.cast::<HewMailboxWasm>(),
            msg_type,
            data_ptr.cast_mut(),
            data_len,
        )
    };

    #[expect(
        clippy::cast_ptr_alignment,
        reason = "actor_ptr is aligned to HewActor which contains AtomicI32"
    )]
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

    outbound_queue().push_back(OutboundMsg { msg_type, data });
}

// ── Host polls for outbound messages ────────────────────────────────────

/// Header prepended to each message in the recv buffer.
///
/// Layout (little-endian):
///   0..4:  `msg_type` (i32)
///   4..8:  `data_len` (u32)
///   8..:   data bytes (`data_len` bytes)
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
/// # Panics
///
/// Panics if the outbound queue mutex is poisoned.
///
/// # Safety
///
/// `out_buf` must point to `buf_len` writable bytes.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_recv(out_buf: *mut u8, buf_len: usize) -> i32 {
    if out_buf.is_null() || buf_len == 0 {
        return 0;
    }

    let mut queue = outbound_queue();
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
#[must_use]
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
#[must_use]
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

    // SAFETY: Caller guarantees meta.name is a valid NUL-terminated C string.
    let name = unsafe { cstr_to_string_lossy(meta.name.cast()) };

    let mut handlers = Vec::with_capacity(meta.handler_count as usize);
    for i in 0..meta.handler_count as usize {
        // SAFETY: handlers array has handler_count entries.
        let h = unsafe { &*meta.handlers.add(i) };
        // SAFETY: Handler name is a valid NUL-terminated C string per caller contract.
        let handler_name = unsafe { cstr_to_string_lossy(h.name.cast()) };

        let mut params = Vec::with_capacity(h.param_count as usize);
        for j in 0..h.param_count as usize {
            // SAFETY: params array has param_count entries.
            let p = unsafe { &*h.params.add(j) };
            params.push(ParamMetaEntry {
                // SAFETY: Param name is a valid NUL-terminated C string per caller contract.
                name: unsafe { cstr_to_string_lossy(p.name.cast()) },
                // SAFETY: Param type_name is a valid NUL-terminated C string per caller contract.
                type_name: unsafe { cstr_to_string_lossy(p.type_name.cast()) },
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
                // SAFETY: Non-null return_type is a valid NUL-terminated C string.
                Some(unsafe { cstr_to_string_lossy(h.return_type.cast()) })
            },
            return_size: h.return_size,
        });
    }

    let mut state = meta_state();
    // Populate the msg_type → "ActorName::handler_name" side table used by
    // drain_events_json for span-level trace attribution.
    for h in &handlers {
        state
            .handler_names
            .insert(h.msg_type, format!("{}::{}", name, h.name));
    }
    state
        .registry
        .insert(name.clone(), ActorMetaEntry { name, handlers });
    state.cache_all = None;
}

// ── Type metadata query (called by host) ────────────────────────────────

/// Query actor type metadata as a JSON string.
///
/// If `name_ptr` is null, returns metadata for ALL registered actors.
/// If `name_ptr` is non-null (pointing to `name_len` UTF-8 bytes), returns
/// metadata for that specific actor (or `{"actors":{}}` if not found).
///
/// Returns an owned pointer to a NUL-terminated JSON string. The caller owns
/// the allocation and must free it with [`hew_wasm_free_meta_json`].
///
/// The returned `*out_len` is set to the string length (excluding NUL).
///
/// # Panics
///
/// Panics if the metadata registry mutex is poisoned.
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

    let mut state = meta_state();
    let json_owned = if let Some(filter_name) = filter_name {
        if let Some(entry) = state.registry.get(filter_name) {
            build_meta_json(std::iter::once(entry))
        } else {
            build_meta_json(std::iter::empty())
        }
    } else {
        if state.cache_all.is_none() {
            state.cache_all = Some(build_meta_json(state.registry.values()));
        }
        state
            .cache_all
            .as_ref()
            .expect("metadata JSON cache should always contain a value")
            .clone()
    };
    drop(state);

    let Ok(c_json) = CString::new(json_owned) else {
        if !out_len.is_null() {
            // SAFETY: Caller guarantees out_len is valid.
            unsafe { *out_len = 0 };
        }
        return std::ptr::null();
    };
    let json_len = c_json.as_bytes().len();
    let json_ptr = c_json.into_raw().cast::<u8>();

    if !out_len.is_null() {
        // SAFETY: Caller guarantees out_len is valid.
        unsafe { *out_len = json_len };
    }

    json_ptr
}

/// Free a JSON string returned by [`hew_wasm_query_meta`].
///
/// # Safety
///
/// `ptr` must be a pointer returned by [`hew_wasm_query_meta`] that has not
/// already been freed.
#[cfg_attr(not(test), no_mangle)]
pub unsafe extern "C" fn hew_wasm_free_meta_json(ptr: *mut u8) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: Caller guarantees ptr came from CString::into_raw.
    let _ = unsafe { CString::from_raw(ptr.cast()) };
}

/// Build a JSON string describing actor metadata.
fn build_meta_json<'a, I>(entries: I) -> String
where
    I: IntoIterator<Item = &'a ActorMetaEntry>,
{
    let mut json = String::from("{\"actors\":{");
    let mut first_actor = true;

    for entry in entries {
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
                use std::fmt::Write;
                let _ = write!(out, "\\u{:04x}", c as u32);
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
    let _ = OUTBOUND.get_or_init(|| Mutex::new(VecDeque::new()));
    let _ = META_STATE.get_or_init(|| {
        Mutex::new(MetaState {
            registry: HashMap::new(),
            handler_names: HashMap::new(),
            cache_all: None,
        })
    });
}

/// Shut down the bridge, draining queues and clearing metadata.
///
/// After this call the outbound queue, metadata registry, and JSON cache are
/// all empty.  A subsequent call to [`hew_wasm_query_meta`] will return an
/// empty result, preventing stale actor metadata from leaking across sessions.
pub fn bridge_shutdown() {
    outbound_queue().clear();
    let mut state = meta_state();
    state.registry.clear();
    // SHIM: JIT reload must also clear `handler_names`; see #1226 M2.
    // For AOT the map is populated once at startup and outlives the session.
    // When #1226 M2 lands, add `state.handler_names.clear()` here.
    state.cache_all = None;
}

// ── WASM stubs for actor type registration (imported via env namespace) ──

/// WASM-exported no-op stub for `hew_actor_register_type`.
///
/// The generated WASM code emits calls to this function at startup. In WASM,
/// there is no profiler registry, so this is always a no-op. The symbol is
/// exported so it can be satisfied as an import from the `env` namespace.
///
/// Signature: `hew_actor_register_type(dispatch: *const c_void, name: *const c_char)`
///
/// # Safety
///
/// This is a no-op and does not dereference its parameters, so any pointer
/// values are safe.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_register_type(
    _dispatch: *const c_void,
    _name: *const std::ffi::c_char,
) {
}

/// WASM-exported no-op stub for `hew_register_handler_name`.
///
/// The generated WASM code emits calls to this function at startup. In WASM,
/// there is no profiler registry, so this is always a no-op. The symbol is
/// exported so it can be satisfied as an import from the `env` namespace.
///
/// Signature: `hew_register_handler_name(dispatch: *const c_void, msg_type: i32, name: *const c_char)`
///
/// # Safety
///
/// This is a no-op and does not dereference its parameters, so any pointer
/// values are safe.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_register_handler_name(
    _dispatch: *const c_void,
    _msg_type: i32,
    _name: *const std::ffi::c_char,
) {
}

// ── Test helpers (pub(crate) so tracing.rs tests can share the lock) ───

/// Serialisation lock for bridge tests; also acquired by `tracing.rs` tests
/// that call into bridge globals so all bridge-touching tests run serially.
///
/// Only compiled under `#[cfg(test)]`.
#[cfg(test)]
pub(crate) static BRIDGE_TEST_LOCK: Mutex<()> = Mutex::new(());

/// Clear all bridge state: outbound queue, metadata registry, handler-name
/// side table, and JSON cache.  Full reset including `handler_names`, which
/// [`bridge_shutdown`] intentionally skips for AOT semantics.
///
/// Only compiled under `#[cfg(test)]`.
#[cfg(test)]
pub(crate) fn reset_bridge_full() {
    outbound_queue().clear();
    let mut state = meta_state();
    state.registry.clear();
    state.handler_names.clear();
    state.cache_all = None;
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    // Tests within this module use BRIDGE_TEST_LOCK (pub(crate) above);
    // tracing.rs tests that touch bridge globals acquire the same lock,
    // so all bridge-global-touching tests run serially regardless of module.
    use super::BRIDGE_TEST_LOCK as TEST_LOCK;

    fn reset_bridge() {
        reset_bridge_full();
    }

    #[test]
    fn emit_and_recv_roundtrip() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let data: [u8; 4] = [0x01, 0x02, 0x03, 0x04];
        // SAFETY: data is a valid byte slice; len matches the array size.
        unsafe {
            hew_wasm_emit(42, data.as_ptr().cast(), data.len());
        }

        assert_eq!(hew_wasm_outbound_len(), 1);

        let mut buf = [0u8; 64];
        // SAFETY: buf is a valid mutable byte buffer of the given length.
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
        // SAFETY: buf is a valid mutable byte buffer.
        let written = unsafe { hew_wasm_recv(buf.as_mut_ptr(), buf.len()) };
        assert_eq!(written, 0);
    }

    #[test]
    fn recv_null_buf_returns_zero() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // SAFETY: Emitting with null data and zero length is explicitly supported.
        unsafe { hew_wasm_emit(1, std::ptr::null(), 0) };
        // SAFETY: Testing null buffer handling; function should return 0.
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
        // SAFETY: Both data slices are valid byte arrays with matching lengths.
        unsafe {
            hew_wasm_emit(10, d1.as_ptr().cast(), d1.len());
            hew_wasm_emit(20, d2.as_ptr().cast(), d2.len());
        }
        assert_eq!(hew_wasm_outbound_len(), 2);

        // Buffer only big enough for one message (12 bytes).
        let mut buf = [0u8; 12];
        // SAFETY: buf is a valid 12-byte mutable buffer.
        let written = unsafe { hew_wasm_recv(buf.as_mut_ptr(), buf.len()) };
        assert_eq!(written, 12);
        assert_eq!(i32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]), 10);

        // Second message still pending.
        assert_eq!(hew_wasm_outbound_len(), 1);

        // Drain the rest.
        // SAFETY: buf is a valid 12-byte mutable buffer.
        let written = unsafe { hew_wasm_recv(buf.as_mut_ptr(), buf.len()) };
        assert_eq!(written, 12);
        assert_eq!(i32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]), 20);

        assert_eq!(hew_wasm_outbound_len(), 0);
    }

    #[test]
    fn emit_null_data() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // SAFETY: Emitting with null data and zero length is explicitly supported.
        unsafe { hew_wasm_emit(99, std::ptr::null(), 0) };
        assert_eq!(hew_wasm_outbound_len(), 1);

        let mut buf = [0u8; 64];
        // SAFETY: buf is a valid mutable byte buffer of the given length.
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
        // SAFETY: name is a valid byte slice with matching length; null data with zero len is valid.
        let rc = unsafe { hew_wasm_send(name.as_ptr(), name.len(), 0, std::ptr::null(), 0) };
        assert_eq!(rc, -1);
    }

    #[test]
    fn send_null_name_returns_error() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // SAFETY: Testing null name handling; function should return -1.
        let rc = unsafe { hew_wasm_send(std::ptr::null(), 0, 0, std::ptr::null(), 0) };
        assert_eq!(rc, -1);
    }

    // ── Type metadata tests ─────────────────────────────────────────────

    #[test]
    fn query_meta_empty() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let mut len: usize = 0;
        // SAFETY: Null name with zero len queries all actors; len is a valid out-pointer.
        let ptr = unsafe { hew_wasm_query_meta(std::ptr::null(), 0, &raw mut len) };
        assert!(len > 0);

        // SAFETY: ptr/len are valid as returned by hew_wasm_query_meta.
        let json = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap_or_default()
        };
        assert_eq!(json, "{\"actors\":{}}");
        // SAFETY: ptr was allocated by hew_wasm_query_meta.
        unsafe { hew_wasm_free_meta_json(ptr.cast_mut()) };
    }

    #[test]
    fn register_and_query_meta() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // Build metadata for a "Counter" actor with one handler.
        let param = HewParamMeta {
            name: c"amount".as_ptr().cast(),
            type_name: c"i64".as_ptr().cast(),
            offset: 0,
            size: 8,
        };
        let handler = HewHandlerMeta {
            name: c"increment".as_ptr().cast(),
            msg_type: 0,
            param_count: 1,
            params: &raw const param,
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let actor_meta = HewActorMeta {
            name: c"Counter".as_ptr().cast(),
            handler_count: 1,
            handlers: &raw const handler,
        };

        // SAFETY: actor_meta is a valid stack-allocated struct with valid C strings.
        unsafe { hew_wasm_register_actor_meta(&raw const actor_meta) };

        let mut len: usize = 0;
        // SAFETY: Null name with zero len queries all actors; len is a valid out-pointer.
        let ptr = unsafe { hew_wasm_query_meta(std::ptr::null(), 0, &raw mut len) };
        // SAFETY: ptr/len are valid as returned by hew_wasm_query_meta.
        let json = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap_or_default()
        };

        // Verify the JSON contains expected structure.
        assert!(json.contains("\"Counter\""));
        assert!(json.contains("\"increment\""));
        assert!(json.contains("\"amount\""));
        assert!(json.contains("\"i64\""));
        assert!(json.contains("\"offset\":0"));
        assert!(json.contains("\"size\":8"));
        assert!(json.contains("\"return_type\":null"));
        // SAFETY: ptr was allocated by hew_wasm_query_meta.
        unsafe { hew_wasm_free_meta_json(ptr.cast_mut()) };
    }

    #[test]
    fn query_meta_by_name() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // Register two actors.
        let h1 = HewHandlerMeta {
            name: c"ping".as_ptr().cast(),
            msg_type: 0,
            param_count: 0,
            params: std::ptr::null(),
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let m1 = HewActorMeta {
            name: c"Pinger".as_ptr().cast(),
            handler_count: 1,
            handlers: &raw const h1,
        };

        let h2 = HewHandlerMeta {
            name: c"pong".as_ptr().cast(),
            msg_type: 0,
            param_count: 0,
            params: std::ptr::null(),
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let m2 = HewActorMeta {
            name: c"Ponger".as_ptr().cast(),
            handler_count: 1,
            handlers: &raw const h2,
        };

        // SAFETY: m1 and m2 are valid stack-allocated structs with valid C strings.
        unsafe {
            hew_wasm_register_actor_meta(&raw const m1);
            hew_wasm_register_actor_meta(&raw const m2);
        }

        // Query just "Pinger".
        let name = b"Pinger";
        let mut len: usize = 0;
        // SAFETY: name is a valid byte slice; len is a valid out-pointer.
        let ptr = unsafe { hew_wasm_query_meta(name.as_ptr(), name.len(), &raw mut len) };
        // SAFETY: ptr/len are valid as returned by hew_wasm_query_meta.
        let json = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap_or_default()
        };

        assert!(json.contains("\"Pinger\""));
        assert!(json.contains("\"ping\""));
        // Should NOT contain the other actor.
        assert!(!json.contains("\"Ponger\""));
        // SAFETY: ptr was allocated by hew_wasm_query_meta.
        unsafe { hew_wasm_free_meta_json(ptr.cast_mut()) };
    }

    #[test]
    fn filtered_query_does_not_poison_unfiltered_cache() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let h1 = HewHandlerMeta {
            name: c"ping".as_ptr().cast(),
            msg_type: 0,
            param_count: 0,
            params: std::ptr::null(),
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let m1 = HewActorMeta {
            name: c"Pinger".as_ptr().cast(),
            handler_count: 1,
            handlers: &raw const h1,
        };

        let h2 = HewHandlerMeta {
            name: c"pong".as_ptr().cast(),
            msg_type: 0,
            param_count: 0,
            params: std::ptr::null(),
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let m2 = HewActorMeta {
            name: c"Ponger".as_ptr().cast(),
            handler_count: 1,
            handlers: &raw const h2,
        };

        // SAFETY: m1 and m2 are valid stack-allocated structs with valid C strings.
        unsafe {
            hew_wasm_register_actor_meta(&raw const m1);
            hew_wasm_register_actor_meta(&raw const m2);
        }

        let name = b"Pinger";
        let mut filtered_len: usize = 0;
        // SAFETY: name is a valid byte slice; filtered_len is a valid out-pointer.
        let filtered_ptr =
            unsafe { hew_wasm_query_meta(name.as_ptr(), name.len(), &raw mut filtered_len) };
        // SAFETY: filtered_ptr was allocated by hew_wasm_query_meta.
        unsafe { hew_wasm_free_meta_json(filtered_ptr.cast_mut()) };

        let mut all_len: usize = 0;
        // SAFETY: Null name queries all actors; all_len is a valid out-pointer.
        let all_ptr = unsafe { hew_wasm_query_meta(std::ptr::null(), 0, &raw mut all_len) };
        // SAFETY: all_ptr/all_len are valid as returned by hew_wasm_query_meta.
        let json = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(all_ptr, all_len)).unwrap_or_default()
        };

        assert!(json.contains("\"Pinger\""));
        assert!(json.contains("\"Ponger\""));
        // SAFETY: all_ptr was allocated by hew_wasm_query_meta.
        unsafe { hew_wasm_free_meta_json(all_ptr.cast_mut()) };
    }

    #[test]
    fn register_meta_with_return_type() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        let handler = HewHandlerMeta {
            name: c"get_total".as_ptr().cast(),
            msg_type: 0,
            param_count: 0,
            params: std::ptr::null(),
            return_type: c"i64".as_ptr().cast(),
            return_size: 8,
        };
        let actor_meta = HewActorMeta {
            name: c"Accumulator".as_ptr().cast(),
            handler_count: 1,
            handlers: &raw const handler,
        };

        // SAFETY: actor_meta is a valid stack-allocated struct with valid C strings.
        unsafe { hew_wasm_register_actor_meta(&raw const actor_meta) };

        let mut len: usize = 0;
        // SAFETY: Null name queries all actors; len is a valid out-pointer.
        let ptr = unsafe { hew_wasm_query_meta(std::ptr::null(), 0, &raw mut len) };
        // SAFETY: ptr/len are valid as returned by hew_wasm_query_meta.
        let json = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap_or_default()
        };

        assert!(json.contains("\"return_type\":{\"type\":\"i64\",\"size\":8}"));
        // SAFETY: ptr was allocated by hew_wasm_query_meta.
        unsafe { hew_wasm_free_meta_json(ptr.cast_mut()) };
    }

    #[test]
    fn bridge_init_and_shutdown() {
        let _guard = TEST_LOCK.lock().unwrap();
        bridge_init();

        // SAFETY: Emitting with null data and zero length is explicitly supported.
        unsafe {
            hew_wasm_emit(1, std::ptr::null(), 0);
        }
        assert_eq!(hew_wasm_outbound_len(), 1);

        bridge_shutdown();
        // Queue still exists but is cleared.
        assert_eq!(hew_wasm_outbound_len(), 0);
    }

    /// Regression: `bridge_shutdown()` must clear the metadata registry so that
    /// `hew_wasm_query_meta()` does not return stale actor metadata from a prior
    /// session after the bridge has been shut down and reinitialised.
    #[test]
    fn bridge_shutdown_clears_meta_registry() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // Register an actor so the registry is non-empty.
        let handler = HewHandlerMeta {
            name: c"ping".as_ptr().cast(),
            msg_type: 1,
            param_count: 0,
            params: std::ptr::null(),
            return_type: c"void".as_ptr().cast(),
            return_size: 0,
        };
        let actor_meta = HewActorMeta {
            name: c"PingActor".as_ptr().cast(),
            handler_count: 1,
            handlers: &raw const handler,
        };
        // SAFETY: actor_meta is a valid stack-allocated struct with valid C strings.
        unsafe { hew_wasm_register_actor_meta(&raw const actor_meta) };

        // Confirm the entry is visible before shutdown.
        {
            let state = meta_state();
            assert!(
                state.registry.contains_key("PingActor"),
                "registry must contain actor before shutdown"
            );
        }

        // Shutdown must clear the registry.
        bridge_shutdown();

        {
            let state = meta_state();
            assert!(
                state.registry.is_empty(),
                "registry must be empty after bridge_shutdown"
            );
        }

        // hew_wasm_query_meta must return an empty result (no actors).
        bridge_init();
        let mut len: usize = 0;
        // SAFETY: Null name queries all actors; len is a valid out-pointer.
        let ptr = unsafe { hew_wasm_query_meta(std::ptr::null(), 0, &raw mut len) };
        // SAFETY: ptr/len are valid as returned by hew_wasm_query_meta.
        let json = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).unwrap_or_default()
        };
        assert_eq!(
            json, "{\"actors\":{}}",
            "query_meta must return empty actors after shutdown"
        );
        // SAFETY: ptr was allocated by hew_wasm_query_meta.
        unsafe { hew_wasm_free_meta_json(ptr.cast_mut()) };
    }

    #[test]
    fn handler_name_side_table_populated_on_registration() {
        let _guard = TEST_LOCK.lock().unwrap();
        reset_bridge();

        // Before registration, no mapping exists.
        assert_eq!(resolve_handler_name(42), None);

        // Build a minimal HewActorMeta with one handler.
        let handler_name = b"handle_bar\0";
        let actor_name = b"Foo\0";
        let handler = HewHandlerMeta {
            name: handler_name.as_ptr().cast(),
            msg_type: 42,
            params: std::ptr::null(),
            param_count: 0,
            return_type: std::ptr::null(),
            return_size: 0,
        };
        let actor_meta = HewActorMeta {
            name: actor_name.as_ptr().cast(),
            handlers: &raw const handler,
            handler_count: 1,
        };

        // SAFETY: all pointers are valid for this stack frame's lifetime;
        // hew_wasm_register_actor_meta copies the strings.
        unsafe {
            hew_wasm_register_actor_meta(&raw const actor_meta);
        }

        assert_eq!(resolve_handler_name(42), Some("Foo::handle_bar".to_owned()));
        // Unknown msg_type still returns None.
        assert_eq!(resolve_handler_name(99), None);
    }
}
