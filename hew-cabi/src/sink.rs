//! Sink ABI types shared between the Hew runtime and native packages.
//!
//! Native packages that produce `Sink` handles (e.g. HTTP streaming responses)
//! call [`into_sink_ptr`] or [`into_write_sink_ptr`] to create heap-allocated
//! opaque handles compatible with the runtime's C ABI.

use std::io::Write;
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::{Mutex, MutexGuard};

// ── Stream/sink error channel (single owner: hew-runtime) ─────────────────────
//
// The thread-local error state is OWNED by hew-runtime (linked into libhew.a)
// and exposed through the four `hew_stream_*` C ABI symbols declared below.
// hew-cabi declares them as imports and forwards through thin wrappers so that:
//
//   * native packages that statically link hew-cabi do NOT each emit their own
//     `#[no_mangle]` copy of these symbols — duplicate `#[no_mangle]`
//     definitions in two archives make the final link fail (the macOS linker
//     rejects them unconditionally); and
//   * a producer inside a separately-compiled package and the consumer in the
//     main program share ONE thread-local, so an error raised inside a package
//     is visible to `hew_stream_last_error` rather than silently lost.
//
// This mirrors the `hew_layout_*` / `hew_vec_*` idiom already used in this crate
// (see `map.rs`): the symbol lives in the runtime, hew-cabi only declares it.
// hew-cabi's own unit tests have no hew-runtime to link against, so a
// `#[cfg(test)]` stub at the bottom of this file defines these four symbols.

extern "C" {
    #[link_name = "hew_stream_set_last_error"]
    fn rt_set_last_error(ptr: *const u8, len: usize);
    #[link_name = "hew_stream_set_last_error_with_errno"]
    fn rt_set_last_error_with_errno(ptr: *const u8, len: usize, errno: i32);
    #[link_name = "hew_stream_last_error"]
    fn rt_last_error() -> *mut crate::string::HewString;
    #[link_name = "hew_stream_last_errno"]
    fn rt_last_errno() -> i32;
}

/// Store an error message for the current thread. Retrievable via
/// [`hew_stream_last_error`]. The associated errno is cleared to 0.
///
/// Forwards to the runtime-owned thread-local through the `hew_stream_*` C ABI.
#[allow(
    clippy::needless_pass_by_value,
    reason = "stable hew_cabi::sink API: callers (std/net/http) hand off an owned \
              error message; the forwarder serialises it across the C ABI"
)]
pub fn set_last_error(msg: String) {
    let bytes = msg.as_bytes();
    // SAFETY: ptr/len describe `msg`'s valid UTF-8 bytes; the runtime copies them
    // into its thread-local and does not retain the pointer past the call.
    unsafe { rt_set_last_error(bytes.as_ptr(), bytes.len()) };
}

/// Store an error message together with its OS errno for the current thread.
/// Both are retrievable via [`hew_stream_last_error`] and [`hew_stream_last_errno`].
///
/// Forwards to the runtime-owned thread-local through the `hew_stream_*` C ABI.
#[allow(
    clippy::needless_pass_by_value,
    reason = "stable hew_cabi::sink API: callers hand off an owned error message; \
              the forwarder serialises it across the C ABI"
)]
pub fn set_last_error_with_errno(msg: String, errno: i32) {
    let bytes = msg.as_bytes();
    // SAFETY: ptr/len describe `msg`'s valid UTF-8 bytes; the runtime copies them
    // into its thread-local and does not retain the pointer past the call.
    unsafe { rt_set_last_error_with_errno(bytes.as_ptr(), bytes.len(), errno) };
}

/// Take the last stream/sink error as an owned managed string.
///
/// The runtime clears the message and errno. Null represents no error or an
/// empty message. Release the result with [`crate::string::string_release`].
#[must_use]
pub fn hew_stream_last_error() -> *mut crate::string::HewString {
    // SAFETY: the runtime getter transfers a managed owner with no preconditions.
    unsafe { rt_last_error() }
}

/// Return the OS errno associated with the last stream/sink error, or 0 if none.
///
/// Clears the errno after reading. Safe wrapper over the runtime-owned
/// `hew_stream_last_errno` C symbol — independent of [`hew_stream_last_error`],
/// so callers may read errno without consuming the message, or vice-versa.
#[must_use]
pub fn hew_stream_last_errno() -> i32 {
    // SAFETY: the runtime export reads and clears a thread-local i32.
    unsafe { rt_last_errno() }
}

/// Take a non-empty stream/sink error into a Rust `String`.
///
/// Consumes the runtime's managed result and clears the message and errno.
/// No error and an empty message both have the canonical null representation
/// and return `None`.
#[must_use]
pub fn take_last_error() -> Option<String> {
    let ptr = hew_stream_last_error();
    if ptr.is_null() {
        return None;
    }
    // SAFETY: ptr is a managed owner transferred by the runtime getter.
    let msg = unsafe { crate::string::string_as_str(ptr) }.to_owned();
    // SAFETY: the runtime transferred this managed owner; its borrow has ended.
    unsafe { crate::string::string_release(ptr) };
    Some(msg)
}

/// Take the OS errno associated with the last stream/sink error, or 0 if none.
///
/// Compatibility wrapper preserving the pre-single-owner `hew_cabi::sink` API.
/// Forwards to the runtime-owned `hew_stream_last_errno` C symbol and clears the
/// stored errno. Independent of [`take_last_error`].
#[must_use]
pub fn take_last_errno() -> i32 {
    hew_stream_last_errno()
}

// ── Sink handle ───────────────────────────────────────────────────────────────

/// Result of a non-blocking sink write attempt.
#[must_use]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(i32)]
pub enum TrySendResult {
    /// The item was accepted by the sink.
    Accepted = 0,
    /// The sink or its receiving peer is closed.
    Closed = 1,
    /// The sink is open, but its bounded buffer has no capacity.
    Full = 2,
    /// The write failed; the runtime error channel carries the diagnostic.
    Failed = 3,
}

impl TrySendResult {
    /// Return the stable C ABI status code for this result.
    #[must_use]
    pub const fn into_abi_code(self) -> i32 {
        self as i32
    }
}

trait SinkOps: Send {
    fn retire(&mut self) -> bool {
        true
    }
    fn take_owners(&mut self) -> Option<SinkOwners> {
        None
    }
    fn write_item(&mut self, data: &[u8]);
    fn try_write_item(&mut self, _data: &[u8]) -> TrySendResult {
        set_last_error("this sink does not support nonblocking writes".into());
        TrySendResult::Failed
    }
    fn take_pending(&mut self) -> Vec<u8> {
        Vec::new()
    }
    fn pending_content(&self) -> &[u8] {
        &[]
    }
    fn flush(&mut self);
    fn close(&mut self);
}

/// Exact value slots pinned until the owning runtime cursor finishes them.
#[derive(Debug)]
pub struct SinkOwners {
    pub values: Vec<(*mut core::ffi::c_void, crate::value::HewValueLayout)>,
    pub storage: *mut core::ffi::c_void,
    pub free: unsafe fn(*mut core::ffi::c_void),
    /// Canonical owned field storage retained until every typed value finishes cleanup.
    pub managed_backing: Option<(*const core::ffi::c_void, u64)>,
}

struct OwnedSink<T> {
    callbacks: CallbackSink<T>,
    owners: fn(&mut T) -> Option<SinkOwners>,
    retire: fn(&mut T) -> bool,
}

impl<T: Send> SinkOps for OwnedSink<T> {
    fn retire(&mut self) -> bool {
        (self.retire)(&mut self.callbacks.backing)
    }
    fn take_owners(&mut self) -> Option<SinkOwners> {
        (self.owners)(&mut self.callbacks.backing)
    }
    fn write_item(&mut self, data: &[u8]) {
        self.callbacks.write_item(data);
    }
    fn flush(&mut self) {
        self.callbacks.flush();
    }
    fn close(&mut self) {
        self.callbacks.close();
    }
}

struct CallbackSink<T> {
    backing: T,
    write_item: fn(&mut T, &[u8]),
    flush: fn(&mut T),
    close: fn(&mut T),
}

impl<T: Send> SinkOps for CallbackSink<T> {
    fn write_item(&mut self, data: &[u8]) {
        (self.write_item)(&mut self.backing, data);
    }

    fn flush(&mut self) {
        (self.flush)(&mut self.backing);
    }

    fn close(&mut self) {
        (self.close)(&mut self.backing);
    }
}

struct NonblockingSink<T> {
    backing: T,
    write: fn(&mut T, &[u8]) -> bool,
    try_write: fn(&mut T, &[u8]) -> std::io::Result<usize>,
    flush: fn(&mut T),
    close: fn(&mut T),
    pending: Vec<u8>,
    offset: usize,
}

fn try_write_error(error: &std::io::Error) -> TrySendResult {
    use std::io::ErrorKind;
    match error.kind() {
        ErrorKind::WouldBlock | ErrorKind::Interrupted => TrySendResult::Full,
        ErrorKind::BrokenPipe
        | ErrorKind::ConnectionReset
        | ErrorKind::ConnectionAborted
        | ErrorKind::NotConnected => TrySendResult::Closed,
        _ => {
            set_last_error_with_errno(
                format!("sink try_send: {error}"),
                error.raw_os_error().unwrap_or(0),
            );
            TrySendResult::Failed
        }
    }
}

impl<T: Send> NonblockingSink<T> {
    fn drain_pending(&mut self) -> bool {
        let success = self.pending.is_empty()
            || (self.write)(&mut self.backing, &self.pending[self.offset..]);
        self.pending.clear();
        self.offset = 0;
        success
    }
}

impl<T: Send> SinkOps for NonblockingSink<T> {
    fn write_item(&mut self, data: &[u8]) {
        if self.drain_pending() {
            (self.write)(&mut self.backing, data);
        }
    }

    fn try_write_item(&mut self, data: &[u8]) -> TrySendResult {
        if !self.pending.is_empty() {
            match (self.try_write)(&mut self.backing, &self.pending[self.offset..]) {
                Ok(count) => {
                    self.offset += count;
                    if self.offset < self.pending.len() {
                        return TrySendResult::Full;
                    }
                    self.pending.clear();
                    self.offset = 0;
                }
                Err(error) => return try_write_error(&error),
            }
        }
        if data.is_empty() {
            return TrySendResult::Accepted;
        }
        match (self.try_write)(&mut self.backing, data) {
            Ok(0) => TrySendResult::Full,
            Ok(count) => {
                if count < data.len() {
                    self.pending.extend_from_slice(&data[count..]);
                }
                TrySendResult::Accepted
            }
            Err(error) => try_write_error(&error),
        }
    }

    fn take_pending(&mut self) -> Vec<u8> {
        let pending = std::mem::take(&mut self.pending);
        let offset = std::mem::take(&mut self.offset);
        pending.into_iter().skip(offset).collect()
    }

    fn pending_content(&self) -> &[u8] {
        &self.pending[self.offset..]
    }

    fn flush(&mut self) {
        if self.drain_pending() {
            (self.flush)(&mut self.backing);
        }
    }

    fn close(&mut self) {
        self.drain_pending();
        (self.close)(&mut self.backing);
    }
}

/// Opaque writable sink handle.
pub struct HewSink {
    state: Mutex<SinkState>,
    runtime_adapter: AtomicPtr<core::ffi::c_void>,
}

/// Backing retained while an admitted finish waits for native readiness.
pub struct PendingFinish {
    sink: *const HewSink,
    inner: Option<Box<dyn SinkOps>>,
    adapter: *const core::ffi::c_void,
    close: bool,
    connection: Option<i32>,
    #[cfg(windows)]
    pipe_handle: Option<usize>,
}

impl std::fmt::Debug for PendingFinish {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PendingFinish")
            .field("sink", &self.sink)
            .field("pending", &self.inner.is_some())
            .finish_non_exhaustive()
    }
}

// SAFETY: construction requires the endpoint loan to survive producer quiescence.
unsafe impl Send for PendingFinish {}

impl PendingFinish {
    /// Retained TCP identity for the existing reactor finish operation.
    #[must_use]
    pub fn native_connection(&self) -> Option<i32> {
        self.connection
    }

    /// Transfer the accepted remainder to its native finish operation.
    pub fn take_pending(&mut self) -> Vec<u8> {
        self.inner
            .as_mut()
            .map_or_else(Vec::new, |inner| inner.take_pending())
    }

    /// Retained pipe identity for native cancellation.
    #[cfg(windows)]
    #[must_use]
    pub fn native_pipe_handle(&self) -> Option<usize> {
        self.pipe_handle
    }
    /// Whether retirement selected the final backing close.
    #[must_use]
    pub fn needs_close(&self) -> bool {
        self.close
    }
    /// The adapter remains pinned by this pending backing.
    #[must_use]
    pub fn adapter(&self) -> *const core::ffi::c_void {
        self.adapter
    }

    /// Complete the retired backing outside its endpoint lock.
    pub fn finish(mut self) {
        if let Some(mut inner) = self.inner.take() {
            if self.close {
                inner.close();
            }
            if !self.adapter.is_null() {
                // SAFETY: the construction loan retains the endpoint through completion.
                unsafe { (*self.sink).state().inner = Some(inner) };
            }
        }
    }
}

impl Drop for PendingFinish {
    fn drop(&mut self) {
        if self.inner.is_some() {
            std::process::abort();
        }
    }
}

struct SinkState {
    inner: Option<Box<dyn SinkOps>>,
    finished: bool,
    /// Opaque borrow of the suspending channel core (`ChannelCore`) when this
    /// sink is the write half of an in-memory pipe (NEW-7). Null for every
    /// other sink kind. The runtime sets this so a suspending write can
    /// reach the shared queue + the parked-consumer wake without downcasting
    /// the `dyn SinkOps` backing. The pointer borrows the `Arc<ChannelCore>`
    /// owned by the backing, so it stays valid for the lifetime of the sink.
    channel_core: *const std::ffi::c_void,
    /// Borrowed transport handle owned by a TCP backing, cleared on close.
    native_connection: Option<i32>,
    #[cfg(windows)]
    native_pipe_handle: Option<usize>,
}

// SAFETY: SinkOps is Send; opaque identities only borrow that owned backing.
unsafe impl Send for SinkState {}

impl SinkState {
    fn empty() -> Self {
        Self {
            inner: None,
            finished: true,
            channel_core: core::ptr::null(),
            native_connection: None,
            #[cfg(windows)]
            native_pipe_handle: None,
        }
    }
}

impl std::fmt::Debug for HewSink {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewSink").finish_non_exhaustive()
    }
}

impl HewSink {
    fn new(state: SinkState, adapter: *const core::ffi::c_void) -> Self {
        Self {
            state: Mutex::new(state),
            runtime_adapter: AtomicPtr::new(adapter.cast_mut()),
        }
    }

    fn state(&self) -> MutexGuard<'_, SinkState> {
        self.state.lock().unwrap_or_else(|_| std::process::abort())
    }

    fn state_mut(&mut self) -> &mut SinkState {
        self.state
            .get_mut()
            .unwrap_or_else(|_| std::process::abort())
    }

    /// Borrow an adapter identity pinned by this endpoint's backing.
    #[must_use]
    pub fn runtime_adapter(&self) -> *const core::ffi::c_void {
        self.runtime_adapter.load(Ordering::Acquire).cast_const()
    }

    /// Transfer hidden owners after the endpoint's I/O has drained.
    pub fn take_owners(&mut self) -> Option<SinkOwners> {
        self.runtime_adapter
            .store(core::ptr::null_mut(), Ordering::Release);
        let state = self.state_mut();
        state.inner.as_mut().and_then(|inner| inner.take_owners())
    }

    /// Install and borrow one shared backing without moving this endpoint.
    ///
    /// # Safety
    /// `install` must return an endpoint retaining its non-null adapter identity.
    /// Neither callback may re-enter this endpoint or execute authored code.
    pub unsafe fn with_shared<R>(
        &self,
        install: impl FnOnce(Self) -> Self,
        borrow: impl FnOnce(*const core::ffi::c_void, bool) -> R,
    ) -> R {
        let mut state = self.state();
        if self.runtime_adapter.load(Ordering::Acquire).is_null() {
            let original = Self::new(
                std::mem::replace(&mut *state, SinkState::empty()),
                core::ptr::null(),
            );
            let mut replacement = install(original);
            *state = std::mem::replace(replacement.state_mut(), SinkState::empty());
            self.runtime_adapter.store(
                replacement
                    .runtime_adapter
                    .swap(core::ptr::null_mut(), Ordering::AcqRel),
                Ordering::Release,
            );
            if self.runtime_adapter.load(Ordering::Acquire).is_null() {
                std::process::abort();
            }
        }
        borrow(self.runtime_adapter(), state.finished)
    }

    /// Borrow the pipe backing's identity through its exclusive native I/O loan.
    ///
    /// # Safety
    /// The backing owns this live handle until close. Native I/O lends it
    /// exclusively until its producer is quiescent.
    #[cfg(windows)]
    pub unsafe fn set_native_pipe_handle(&mut self, handle: usize) {
        let state = self.state_mut();
        state.native_pipe_handle = Some(handle);
    }

    /// Borrow the pipe identity while retaining the sink's backing loan.
    #[cfg(windows)]
    #[must_use]
    pub fn native_pipe_handle(&self) -> Option<usize> {
        let state = self.state();
        state.native_pipe_handle
    }

    /// Associate the TCP backing's live transport handle with native I/O.
    /// The backing remains the sole resource owner.
    pub fn set_native_connection(&mut self, connection: i32) {
        let state = self.state_mut();
        state.native_connection = Some(connection);
    }

    /// Borrow the TCP backing's transport identity until this sink closes.
    #[must_use]
    pub fn native_connection(&self) -> Option<i32> {
        let state = self.state();
        state.native_connection
    }

    /// Copy pending content for transactional native write preparation.
    #[must_use]
    pub fn pending_content(&self) -> Vec<u8> {
        self.state()
            .inner
            .as_ref()
            .map_or_else(Vec::new, |inner| inner.pending_content().to_vec())
    }

    /// Transfer buffered bytes to the sink's exclusive native write operation.
    pub fn take_pending(&self) -> Vec<u8> {
        let mut state = self.state();
        state
            .inner
            .as_mut()
            .map_or_else(Vec::new, |inner| inner.take_pending())
    }

    /// Attach an opaque suspending-channel-core borrow (NEW-7). Called by the
    /// runtime pipe constructor after building the channel sink backing.
    pub fn set_channel_core(&mut self, core: *const std::ffi::c_void) {
        let state = self.state_mut();
        state.channel_core = core;
    }

    /// The opaque suspending-channel-core borrow, or null when this sink is not
    /// the write half of an in-memory pipe.
    #[must_use]
    pub fn channel_core_ptr(&self) -> *const std::ffi::c_void {
        let state = self.state();
        state.channel_core
    }

    /// Whether this endpoint has published its end of the stream.
    /// Shared owners remain retained until the endpoint's release cursor drains.
    #[must_use]
    pub fn is_closed(&self) -> bool {
        let state = self.state();
        state.finished
    }

    /// Write one item (exact bytes). Blocks if the backing buffer is full.
    pub fn write_item(&self, data: &[u8]) {
        let mut state = self.state();
        if state.finished {
            return;
        }
        let Some(inner) = state.inner.as_mut() else {
            return;
        };
        inner.write_item(data);
    }

    /// Attempt to write one item without blocking.
    ///
    /// Returns whether the item was accepted, the buffer was full, or the sink
    /// was closed, or a write failed. Unsupported backings report failure.
    pub fn try_write_item(&self, data: &[u8]) -> TrySendResult {
        let mut state = self.state();
        if state.finished {
            return TrySendResult::Closed;
        }
        let Some(inner) = state.inner.as_mut() else {
            return TrySendResult::Closed;
        };
        inner.try_write_item(data)
    }

    /// Flush any buffered writes (file sinks).
    pub fn flush(&self) {
        let mut state = self.state();
        if state.finished {
            return;
        }
        let Some(inner) = state.inner.as_mut() else {
            return;
        };
        inner.flush();
    }

    /// Retire this producer, draining its backing outside the endpoint lock.
    pub fn finish(&self) {
        // SAFETY: this synchronous borrow retains the endpoint through completion.
        if let Some(pending) = unsafe { self.begin_finish() } {
            pending.finish();
        }
    }

    /// Retire admission and transfer the backing to its native finish operation.
    ///
    /// # Safety
    /// The endpoint must survive until the returned backing is finished. The
    /// caller must drain that finish even when its invocation is cancelled.
    pub unsafe fn begin_finish(&self) -> Option<PendingFinish> {
        let mut pending = {
            let mut state = self.state();
            if state.finished {
                return None;
            }
            state.finished = true;
            state.channel_core = std::ptr::null();
            PendingFinish {
                sink: self,
                inner: Some(state.inner.take()?),
                adapter: self.runtime_adapter(),
                close: true,
                connection: state.native_connection.take(),
                #[cfg(windows)]
                pipe_handle: state.native_pipe_handle.take(),
            }
        };
        pending.close = pending.inner.as_mut().is_some_and(|inner| inner.retire());
        Some(pending)
    }

    /// Signal EOF to the reader and release the backing resource.
    pub fn close(&self) {
        self.finish();
    }
}

/// Create an endpoint whose backing retains compiler-described value owners.
/// The runtime consumes those owners after native I/O quiescence.
///
/// # Safety
/// `context` must identify the runtime adapter pinned by `backing` until its
/// owners are transferred. Callbacks must implement that adapter's contract.
/// `retire` publishes producer retirement and selects the final close; it must
/// neither wait nor execute authored code.
#[allow(
    clippy::too_many_arguments,
    reason = "the owned endpoint carries its identity and complete lifecycle callbacks"
)]
pub unsafe fn into_owned_sink_ptr<T: Send + 'static>(
    backing: T,
    context: *const core::ffi::c_void,
    finished: bool,
    write_item: fn(&mut T, &[u8]),
    flush: fn(&mut T),
    retire: fn(&mut T) -> bool,
    close: fn(&mut T),
    owners: fn(&mut T) -> Option<SinkOwners>,
) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink::new(
        SinkState {
            inner: Some(Box::new(OwnedSink {
                retire,
                callbacks: CallbackSink {
                    backing,
                    write_item,
                    flush,
                    close,
                },
                owners,
            })),
            finished,
            channel_core: core::ptr::null(),
            native_connection: None,
            #[cfg(windows)]
            native_pipe_handle: None,
        },
        context,
    )))
}

impl Drop for HewSink {
    fn drop(&mut self) {
        self.close();
    }
}

/// Create a heap-allocated [`HewSink`] from custom sink callbacks.
pub fn into_sink_ptr<T: Send + 'static>(
    backing: T,
    write_item: fn(&mut T, &[u8]),
    flush: fn(&mut T),
    close: fn(&mut T),
) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink::new(
        SinkState {
            // ALLOCATOR-PAIRING: GlobalAlloc
            inner: Some(Box::new(CallbackSink {
                backing,
                write_item,
                flush,
                close,
            })),
            finished: false,
            channel_core: std::ptr::null(),
            native_connection: None,
            #[cfg(windows)]
            native_pipe_handle: None,
        },
        core::ptr::null(),
    )))
}

/// Create a sink that retains an accepted item's unwritten bytes in order.
pub fn into_nonblocking_sink_ptr<T: Send + 'static>(
    backing: T,
    write: fn(&mut T, &[u8]) -> bool,
    try_write: fn(&mut T, &[u8]) -> std::io::Result<usize>,
    flush: fn(&mut T),
    close: fn(&mut T),
) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink::new(
        SinkState {
            inner: Some(Box::new(NonblockingSink {
                backing,
                write,
                try_write,
                flush,
                close,
                pending: Vec::new(),
                offset: 0,
            })),
            finished: false,
            channel_core: std::ptr::null(),
            native_connection: None,
            #[cfg(windows)]
            native_pipe_handle: None,
        },
        core::ptr::null(),
    )))
}

fn write_via_write<W: Write>(backing: &mut W, data: &[u8]) {
    let result = if data.is_empty() {
        backing.write(data).map(|_| ())
    } else {
        backing.write_all(data)
    };
    if let Err(err) = result {
        set_last_error(format!("hew_sink_write: {err}"));
    }
}

fn flush_via_write<W: Write>(backing: &mut W) {
    if let Err(err) = backing.flush() {
        set_last_error(format!("hew_sink_flush: {err}"));
    }
}

fn close_via_write<W: Write>(backing: &mut W) {
    if let Err(err) = backing.flush() {
        set_last_error(format!("hew_sink_close: {err}"));
    }
}

/// Create a heap-allocated [`HewSink`] from a [`Write`] implementation.
pub fn into_write_sink_ptr(backing: impl Write + Send + 'static) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink::new(
        SinkState {
            // ALLOCATOR-PAIRING: GlobalAlloc
            inner: Some(Box::new(CallbackSink {
                backing,
                write_item: write_via_write::<_>,
                flush: flush_via_write::<_>,
                close: close_via_write::<_>,
            })),
            finished: false,
            channel_core: std::ptr::null(),
            native_connection: None,
            #[cfg(windows)]
            native_pipe_handle: None,
        },
        core::ptr::null(),
    )))
}

// ── Channel-backed sink ───────────────────────────────────────────────────────

/// Channel sink: wraps an `mpsc::SyncSender` and provides genuine non-blocking
/// `try_write_item` semantics.
struct ChannelSinkBacking {
    tx: std::sync::mpsc::SyncSender<Vec<u8>>,
}

// SAFETY: mpsc::SyncSender<Vec<u8>> is Send.
unsafe impl Send for ChannelSinkBacking {}

impl SinkOps for ChannelSinkBacking {
    /// Blocking send. Waits until space is available in the bounded channel.
    fn write_item(&mut self, data: &[u8]) {
        // Errors mean the receiver is gone; treat as a silent discard so that
        // pipeline tear-down does not propagate spurious errors.
        let _ = self.tx.send(data.to_vec());
    }

    /// Non-blocking send that distinguishes acceptance, backpressure, and closure.
    fn try_write_item(&mut self, data: &[u8]) -> TrySendResult {
        use std::sync::mpsc::TrySendError;
        match self.tx.try_send(data.to_vec()) {
            Ok(()) => TrySendResult::Accepted,
            Err(TrySendError::Disconnected(_)) => TrySendResult::Closed,
            Err(TrySendError::Full(_)) => TrySendResult::Full,
        }
    }

    fn flush(&mut self) {
        // mpsc channels are not buffered on the send side; flush is a no-op.
    }

    fn close(&mut self) {
        // Dropping the SyncSender signals EOF to the receiver; nothing explicit needed.
    }
}

/// Create a heap-allocated [`HewSink`] backed by a bounded `mpsc` channel sender.
///
/// Unlike [`into_write_sink_ptr`], the returned sink supports genuine non-blocking
/// writes via [`HewSink::try_write_item`], distinguishing an accepted item from a
/// full queue or a disconnected receiver.
///
/// # Ownership
///
/// The caller transfers ownership of `tx`. The sink owns the sender until it is
/// closed or dropped, at which point the sender is released and the paired receiver
/// observes EOF.
#[must_use]
pub fn into_channel_sink_ptr(tx: std::sync::mpsc::SyncSender<Vec<u8>>) -> *mut HewSink {
    Box::into_raw(Box::new(HewSink::new(
        SinkState {
            // ALLOCATOR-PAIRING: GlobalAlloc
            inner: Some(Box::new(ChannelSinkBacking { tx })),
            finished: false,
            channel_core: std::ptr::null(),
            native_connection: None,
            #[cfg(windows)]
            native_pipe_handle: None,
        },
        core::ptr::null(),
    )))
}

/// Standalone definitions of hew-runtime's stream-error C ABI, for hew-cabi's
/// own unit tests only.
///
/// In a real link the four `hew_stream_*` symbols come from hew-runtime
/// (libhew.a) and hew-cabi merely declares them (see the `extern "C"` block near
/// the top of this file). But hew-cabi's test binary has no hew-runtime to link
/// against, so these `#[no_mangle]` definitions stand in. They mirror the runtime
/// implementation — a thread-local message + errno with header-aware cstring
/// allocation — so any test exercising the forwarders observes identical
/// behaviour. Compiled ONLY under `cfg(test)`; never present in shipped archives.
#[cfg(test)]
mod runtime_abi_stub {
    use std::cell::RefCell;

    thread_local! {
        static LAST_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
        static LAST_ERRNO: RefCell<i32> = const { RefCell::new(0) };
    }

    fn store(ptr: *const u8, len: usize, errno: i32) {
        let msg = if ptr.is_null() || len == 0 {
            String::new()
        } else {
            // SAFETY: the forwarders pass a valid ptr/len pair borrowed from a
            // live `String` for the duration of the call.
            let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
            String::from_utf8_lossy(slice).into_owned()
        };
        LAST_ERROR.with(|e| *e.borrow_mut() = Some(msg));
        LAST_ERRNO.with(|e| *e.borrow_mut() = errno);
    }

    /// # Safety
    /// `ptr`/`len` must describe a valid initialized byte range, or `ptr` may be
    /// null with `len` 0.
    #[no_mangle]
    unsafe extern "C" fn hew_stream_set_last_error(ptr: *const u8, len: usize) {
        store(ptr, len, 0);
    }

    /// # Safety
    /// As [`hew_stream_set_last_error`]; `errno` is stored verbatim.
    #[no_mangle]
    unsafe extern "C" fn hew_stream_set_last_error_with_errno(
        ptr: *const u8,
        len: usize,
        errno: i32,
    ) {
        store(ptr, len, errno);
    }

    #[no_mangle]
    extern "C" fn hew_stream_last_error() -> *mut crate::string::HewString {
        LAST_ERRNO.with(|e| *e.borrow_mut() = 0);
        LAST_ERROR
            .with(|e| e.borrow_mut().take())
            .map_or(std::ptr::null_mut(), |message| {
                crate::string::string_from_str(&message)
            })
    }

    #[no_mangle]
    extern "C" fn hew_stream_last_errno() -> i32 {
        LAST_ERRNO.with(|e| {
            let v = *e.borrow();
            *e.borrow_mut() = 0;
            v
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    /// Proves a pointer lacking `CStringHeader`'s magic sentinel — what the OLD
    /// raw-`libc::malloc` `hew_stream_last_error` allocator produced — causes
    /// `free_cstring` to abort. Run in a subprocess so the SIGABRT does not
    /// take down the test runner.
    ///
    /// This is the falsification probe: it demonstrates that the fix is not
    /// redundant and that the previous raw-malloc path was the actual hazard.
    #[test]
    fn free_cstring_aborts_on_raw_malloc_provenance() {
        if std::env::var("HEW_SINK_RUN_ABORT_PROBE").is_ok() {
            // Simulate the OLD hew_stream_last_error allocator's output: a
            // pointer with no CStringHeader sentinel, as free_cstring expects.
            // Allocated via the sized-block allocator here (raw libc calls
            // aren't used anymore) — the hazard is the missing CStringHeader,
            // not which allocator produced the pointer.
            let msg = b"simulated old-path error\0";
            // SAFETY: allocating len bytes via the sized-block allocator;
            // buf_try_alloc returns a valid pointer or null.
            let ptr = crate::mem::buf_try_alloc(msg.len()).cast::<std::ffi::c_char>();
            assert!(!ptr.is_null());
            // SAFETY: ptr is freshly allocated with msg.len() bytes.
            unsafe { std::ptr::copy_nonoverlapping(msg.as_ptr(), ptr.cast::<u8>(), msg.len()) };
            // free_cstring will read base = ptr - CSTRING_HEADER_SIZE, find no magic
            // sentinel, print a diagnostic, and call libc::abort. This must not return.
            // SAFETY: ptr was just allocated via the sized-block allocator; we are intentionally
            // passing a pointer without CStringHeader's header to prove
            // free_cstring detects the mismatch and aborts.
            unsafe { crate::cabi::free_cstring(ptr) };
            // If we reach here the abort did NOT fire — exit cleanly so the parent fails.
            std::process::exit(0);
        }

        let exe = std::env::current_exe().expect("current_exe");
        let status = std::process::Command::new(exe)
            .args([
                "--exact",
                "sink::tests::free_cstring_aborts_on_raw_malloc_provenance",
            ])
            .env("HEW_SINK_RUN_ABORT_PROBE", "1")
            .env("RUST_BACKTRACE", "0")
            .output()
            .expect("spawn abort probe");
        assert!(
            !status.status.success(),
            "free_cstring on a pointer without CStringHeader's sentinel must abort \
             (subprocess should not exit 0); stdout={:?} stderr={:?}",
            String::from_utf8_lossy(&status.stdout),
            String::from_utf8_lossy(&status.stderr),
        );
    }

    // ── Stream/sink error channel forwarders ────────────────────────────

    /// `take_last_error` returns `None` when no error has been recorded.
    #[test]
    fn take_last_error_none_when_unset() {
        let _ = take_last_error(); // clear any residual thread-local state
        assert_eq!(take_last_error(), None);
    }

    /// A message stored through `set_last_error` round-trips back through the
    /// runtime-owned C getter and the compat `take_last_error`, then clears.
    #[test]
    fn set_then_take_last_error_round_trips_and_clears() {
        let _ = take_last_error();
        set_last_error("disk offline".to_string());
        assert_eq!(take_last_error().as_deref(), Some("disk offline"));
        assert_eq!(take_last_error(), None, "second take must be None");
    }

    #[test]
    fn take_managed_error_preserves_nul_and_unicode() {
        set_last_error("failed\0é中🙂".to_owned());
        assert_eq!(take_last_error().as_deref(), Some("failed\0é中🙂"));
        assert_eq!(take_last_error(), None);
        set_last_error_with_errno(String::new(), 42);
        assert_eq!(take_last_error(), None, "empty is the canonical null value");
        assert_eq!(
            take_last_errno(),
            0,
            "reading still consumes error metadata"
        );
    }

    /// `set_last_error_with_errno` stores both fields; the errno is readable via
    /// `take_last_errno` independently, and `take_last_error` yields the message.
    #[test]
    fn set_with_errno_then_take_both() {
        let _ = take_last_error();
        set_last_error_with_errno("connection reset".to_string(), 54);
        assert_eq!(take_last_errno(), 54);
        assert_eq!(take_last_error().as_deref(), Some("connection reset"));
    }

    /// The compat `take_last_error` frees the header-aware C string handed back by
    /// the getter through `free_cstring`; an allocator-pairing mismatch would
    /// abort the process here rather than return.
    #[test]
    fn take_last_error_frees_cstring_without_abort() {
        let _ = take_last_error();
        set_last_error("freed cleanly".to_string());
        assert_eq!(take_last_error().as_deref(), Some("freed cleanly"));
    }

    // ── HewSink / into_sink_ptr / into_write_sink_ptr ───────────────────

    /// Test double that records calls to `write_item`, flush, and close.
    #[derive(Debug)]
    struct MockSink {
        written: Arc<std::sync::Mutex<Vec<Vec<u8>>>>,
        flush_count: Arc<AtomicUsize>,
        close_count: Arc<AtomicUsize>,
    }

    #[derive(Debug)]
    struct RawSink {
        written: Arc<std::sync::Mutex<Vec<Vec<u8>>>>,
        close_count: Arc<AtomicUsize>,
    }

    impl MockSink {
        #[expect(
            clippy::type_complexity,
            reason = "Test helper return type; splitting would reduce clarity"
        )]
        fn new() -> (
            Self,
            Arc<std::sync::Mutex<Vec<Vec<u8>>>>,
            Arc<AtomicUsize>,
            Arc<AtomicUsize>,
        ) {
            let written = Arc::new(std::sync::Mutex::new(Vec::new()));
            let flush_count = Arc::new(AtomicUsize::new(0));
            let close_count = Arc::new(AtomicUsize::new(0));
            (
                Self {
                    written: Arc::clone(&written),
                    flush_count: Arc::clone(&flush_count),
                    close_count: Arc::clone(&close_count),
                },
                written,
                flush_count,
                close_count,
            )
        }
    }

    impl Write for MockSink {
        fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
            self.written.lock().unwrap().push(data.to_vec());
            Ok(data.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            self.flush_count.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }
    }

    impl Drop for MockSink {
        fn drop(&mut self) {
            self.close_count.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn hew_sink_write_flush_close() {
        let (mock, written, flush_count, close_count) = MockSink::new();
        // SAFETY: into_write_sink_ptr returns a Box::into_raw allocation owned by this test.
        let mut sink = unsafe { Box::from_raw(into_write_sink_ptr(mock)) }; // ALLOCATOR-PAIRING: GlobalAlloc
        sink.write_item(b"hello");
        sink.write_item(b"world");
        sink.flush();
        sink.close();

        let items = written.lock().unwrap();
        assert_eq!(items.len(), 2);
        assert_eq!(items[0], b"hello");
        assert_eq!(items[1], b"world");
        assert_eq!(flush_count.load(Ordering::SeqCst), 2);
        assert_eq!(close_count.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn hew_sink_drop_calls_close() {
        let (mock, _written, _flush_count, close_count) = MockSink::new();
        {
            // SAFETY: into_write_sink_ptr returns a Box::into_raw allocation owned by this test.
            let _sink = unsafe { Box::from_raw(into_write_sink_ptr(mock)) }; // ALLOCATOR-PAIRING: GlobalAlloc
            assert_eq!(close_count.load(Ordering::SeqCst), 0);
            // _sink is dropped here.
        }
        assert_eq!(
            close_count.load(Ordering::SeqCst),
            1,
            "Drop must call close"
        );
    }

    #[test]
    fn into_sink_ptr_returns_valid_heap_allocation() {
        let (mock, written, _flush_count, close_count) = MockSink::new();
        let ptr = into_write_sink_ptr(mock);
        assert!(!ptr.is_null());

        // SAFETY: ptr was just created by into_sink_ptr via Box::into_raw.
        unsafe {
            (*ptr).write_item(b"via pointer");
            // Reclaim and drop to free memory and trigger close.
            let _ = Box::from_raw(ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        }

        let items = written.lock().unwrap();
        assert_eq!(items.len(), 1);
        assert_eq!(items[0], b"via pointer");
        assert_eq!(
            close_count.load(Ordering::SeqCst),
            1,
            "Drop via Box::from_raw should call close"
        );
    }

    #[test]
    fn into_sink_ptr_write_empty_data() {
        let (mock, written, _flush_count, _close_count) = MockSink::new();
        let ptr = into_write_sink_ptr(mock);
        // SAFETY: ptr was just created by into_sink_ptr.
        unsafe {
            (*ptr).write_item(b"");
            let _ = Box::from_raw(ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        }
        let items = written.lock().unwrap();
        assert_eq!(items.len(), 1);
        assert!(items[0].is_empty(), "empty write should produce empty vec");
    }

    #[test]
    fn close_clears_channel_core_ptr_to_remove_stale_deref_window() {
        let (mock, _written, _flush_count, _close_count) = MockSink::new();
        let ptr = into_write_sink_ptr(mock);
        // SAFETY: ptr was just created by into_write_sink_ptr.
        unsafe {
            // Simulate the pipe constructor attaching a channel-core borrow.
            let fake_core = 0xdead_beef_usize as *const std::ffi::c_void;
            (*ptr).set_channel_core(fake_core);
            assert_eq!((*ptr).channel_core_ptr(), fake_core);
            // close() must null the borrow so a later suspending send cannot
            // dereference a (now possibly stale) channel core.
            (*ptr).close();
            assert!(
                (*ptr).channel_core_ptr().is_null(),
                "close() must clear the channel_core borrow (no stale deref)"
            );
            let _ = Box::from_raw(ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    #[test]
    fn into_sink_ptr_custom_callbacks_preserve_item_boundaries() {
        let written = Arc::new(std::sync::Mutex::new(Vec::new()));
        let close_count = Arc::new(AtomicUsize::new(0));

        let ptr = into_sink_ptr(
            RawSink {
                written: Arc::clone(&written),
                close_count: Arc::clone(&close_count),
            },
            |sink, data| sink.written.lock().unwrap().push(data.to_vec()),
            |_sink| {},
            |sink| {
                sink.close_count.fetch_add(1, Ordering::SeqCst);
            },
        );

        // SAFETY: ptr was created by into_sink_ptr above.
        unsafe {
            (*ptr).write_item(b"abc");
            let _ = Box::from_raw(ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        }

        let items = written.lock().unwrap();
        assert_eq!(items.as_slice(), &[b"abc".to_vec()]);
        assert_eq!(close_count.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn channel_sink_try_write_distinguishes_accepted_full_and_closed() {
        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let ptr = into_channel_sink_ptr(tx);
        // SAFETY: ptr was created by into_channel_sink_ptr and is reclaimed once.
        let mut sink = unsafe { Box::from_raw(ptr) }; // ALLOCATOR-PAIRING: GlobalAlloc

        assert_eq!(sink.try_write_item(b"accepted"), TrySendResult::Accepted);
        assert_eq!(sink.try_write_item(b"full"), TrySendResult::Full);

        drop(rx);
        assert_eq!(sink.try_write_item(b"closed"), TrySendResult::Closed);
    }

    #[test]
    fn closed_sink_try_write_reports_closed() {
        let (mock, written, _flush_count, _close_count) = MockSink::new();
        // SAFETY: into_write_sink_ptr returns a Box::into_raw allocation owned by this test.
        let mut sink = unsafe { Box::from_raw(into_write_sink_ptr(mock)) }; // ALLOCATOR-PAIRING: GlobalAlloc
        sink.close();

        assert_eq!(sink.try_write_item(b"discarded"), TrySendResult::Closed);
        assert!(written.lock().unwrap().is_empty());
    }
}
