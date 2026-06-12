//! Machine emit queue: synchronous FIFO drain at the outermost `step()` frame.
//!
//! The queue is intentionally single-threaded. Generated code either receives an
//! explicit `*mut EmitQueue` from codegen or passes null to use the calling
//! thread's owner. There is no scheduler, executor, or cross-thread sharing in
//! this substrate.

use std::collections::VecDeque;
use std::marker::PhantomData;
use std::panic;

/// Default maximum nested `step()`/drain depth.
pub const DEFAULT_REENTRANCY_CAP: usize = 64;

/// Compile-time override for [`DEFAULT_REENTRANCY_CAP`].
const CAP_ENV: Option<&str> = option_env!("HEW_MACHINE_EMIT_REENTRANCY_CAP");

fn configured_reentrancy_cap() -> usize {
    CAP_ENV
        .and_then(|value| value.parse::<usize>().ok())
        .filter(|value| *value > 0)
        .unwrap_or(DEFAULT_REENTRANCY_CAP)
}

/// Typed runtime panic payload for an indirect machine emit cycle.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MachineEmitReentrancyExceeded {
    /// Depth reached by the attempted nested entry.
    pub depth: usize,
    /// Configured maximum depth.
    pub cap: usize,
}

impl std::fmt::Display for MachineEmitReentrancyExceeded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "MachineEmitReentrancyExceeded: depth {} exceeded cap {}",
            self.depth, self.cap
        )
    }
}

impl std::error::Error for MachineEmitReentrancyExceeded {}

fn panic_reentrancy_exceeded(depth: usize, cap: usize) -> ! {
    panic::panic_any(MachineEmitReentrancyExceeded { depth, cap });
}

/// One pending machine emit.
///
/// `payload` is an opaque borrowed pointer. Slice 7 only lowers unit events and
/// passes null. Future payload serialization must either point at storage that
/// remains valid until the outermost step drain completes, or extend the ABI
/// with ownership/length so this substrate can copy bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EmitEvent {
    /// Zero-based event tag from the machine event declaration order.
    pub tag: u32,
    /// Back-compat alias for earlier tests and callers.
    pub event_idx: usize,
    /// Opaque borrowed payload pointer; null for unit events in this slice.
    pub payload: *const u8,
}

/// Append-only view used by drain handlers to enqueue nested emits.
#[derive(Debug)]
pub struct EmitQueueAppend<'a> {
    queue: *mut EmitQueue,
    _borrow: PhantomData<&'a mut EmitQueue>,
}

impl EmitQueueAppend<'_> {
    /// Append a nested emit to the same FIFO drain.
    pub fn push(&mut self, tag: u32, payload: *const u8) {
        // SAFETY: `EmitQueueAppend` is only constructed by `EmitQueue::drain`
        // for the queue currently being drained on this thread.
        unsafe {
            (*self.queue).push(tag, payload);
        }
    }
}

/// Error returned by [`EmitQueue::drain`] when the handler fails.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DrainError<E> {
    /// The per-event handler returned an error.
    Handler(E),
}

impl<E: std::fmt::Display> std::fmt::Display for DrainError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Handler(e) => write!(f, "machine emit drain handler error: {e}"),
        }
    }
}

impl<E: std::error::Error + 'static> std::error::Error for DrainError<E> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Handler(e) => Some(e),
        }
    }
}

/// Single-threaded machine emit queue.
#[derive(Debug)]
#[repr(C)]
pub struct EmitQueue {
    queue: VecDeque<EmitEvent>,
    reentrancy_depth: usize,
    drain_depth: usize,
    cap: usize,
    draining: bool,
    _not_send_sync: PhantomData<*const ()>,
}

impl EmitQueue {
    /// Create a queue using the default or build-env configured cap.
    #[must_use]
    pub fn new() -> Self {
        Self::with_cap(configured_reentrancy_cap())
    }

    /// Create a queue with a caller-specified reentrancy cap.
    #[must_use]
    pub fn with_cap(cap: usize) -> Self {
        Self {
            queue: VecDeque::new(),
            reentrancy_depth: 0,
            drain_depth: 0,
            cap,
            draining: false,
            _not_send_sync: PhantomData,
        }
    }

    /// Push an event onto the FIFO tail.
    pub fn push(&mut self, tag: u32, payload: *const u8) {
        self.queue.push_back(EmitEvent {
            tag,
            event_idx: tag as usize,
            payload,
        });
    }

    /// Enter a generated `step()` frame.
    ///
    /// Exceeding the cap raises a typed [`MachineEmitReentrancyExceeded`] panic.
    pub fn enter_step(&mut self) {
        self.reentrancy_depth = self.reentrancy_depth.saturating_add(1);
        if self.reentrancy_depth > self.cap {
            let depth = self.reentrancy_depth;
            self.reentrancy_depth = self.reentrancy_depth.saturating_sub(1);
            panic_reentrancy_exceeded(depth, self.cap);
        }
    }

    /// Exit a generated `step()` frame and drain only if it was outermost.
    ///
    /// # Errors
    ///
    /// Returns [`DrainError::Handler`] if the supplied drain handler rejects an
    /// event during the outermost-frame drain.
    pub fn exit_step<E>(
        &mut self,
        handler: impl FnMut(&EmitEvent, &mut EmitQueueAppend<'_>) -> Result<(), E>,
    ) -> Result<(), DrainError<E>> {
        if self.reentrancy_depth == 0 {
            return Ok(());
        }

        let was_outermost = self.reentrancy_depth == 1;
        let was_draining = self.draining;
        self.reentrancy_depth -= 1;
        if was_outermost && !was_draining {
            self.drain(handler)
        } else {
            Ok(())
        }
    }

    /// Drain all pending events in FIFO order.
    ///
    /// `Drop` on the internal guard clears remaining events if the handler
    /// unwinds, so a caught panic cannot leave stale queue state behind.
    ///
    /// # Errors
    ///
    /// Returns [`DrainError::Handler`] if the supplied handler rejects an event.
    pub fn drain<E>(
        &mut self,
        mut handler: impl FnMut(&EmitEvent, &mut EmitQueueAppend<'_>) -> Result<(), E>,
    ) -> Result<(), DrainError<E>> {
        self.drain_depth = self.drain_depth.saturating_add(1);
        if self.drain_depth > self.cap {
            let depth = self.drain_depth;
            self.drain_depth = self.drain_depth.saturating_sub(1);
            panic_reentrancy_exceeded(depth, self.cap);
        }

        self.draining = true;
        let queue_ptr = self as *mut EmitQueue;
        let _guard = DrainGuard { queue: queue_ptr };

        while let Some(event) = self.queue.pop_front() {
            let mut append = EmitQueueAppend {
                queue: queue_ptr,
                _borrow: PhantomData,
            };
            if let Err(error) = handler(&event, &mut append) {
                return Err(DrainError::Handler(error));
            }
        }

        Ok(())
    }

    /// Remove every pending event and reset active counters.
    pub fn clear(&mut self) {
        self.queue.clear();
        self.reentrancy_depth = 0;
        self.drain_depth = 0;
        self.draining = false;
    }

    /// Number of events waiting to drain.
    #[must_use]
    pub fn pending(&self) -> usize {
        self.queue.len()
    }

    /// Current nested `step()` depth.
    #[must_use]
    pub fn reentrancy_depth(&self) -> usize {
        self.reentrancy_depth
    }

    /// Configured cap.
    #[must_use]
    pub fn cap(&self) -> usize {
        self.cap
    }
}

impl Default for EmitQueue {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for EmitQueue {
    fn drop(&mut self) {
        self.clear();
    }
}

struct DrainGuard {
    queue: *mut EmitQueue,
}

impl Drop for DrainGuard {
    fn drop(&mut self) {
        // SAFETY: the guard is created from a live `&mut EmitQueue` and never
        // outlives the drain call that owns that borrow.
        let queue = unsafe { &mut *self.queue };
        queue.drain_depth = queue.drain_depth.saturating_sub(1);
        queue.draining = queue.drain_depth != 0;
        if std::thread::panicking() {
            queue.queue.clear();
            queue.draining = false;
        }
    }
}

struct ThreadEmitOwner {
    queue: *mut EmitQueue,
}

impl ThreadEmitOwner {
    fn new() -> Self {
        Self {
            queue: Box::into_raw(Box::new(EmitQueue::new())), // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }
}

impl Drop for ThreadEmitOwner {
    fn drop(&mut self) {
        if !self.queue.is_null() {
            // SAFETY: `queue` was allocated by `ThreadEmitOwner::new` and is
            // owned by this thread-local owner until thread teardown.
            unsafe {
                drop(Box::from_raw(self.queue)); // ALLOCATOR-PAIRING: GlobalAlloc
            }
            self.queue = std::ptr::null_mut();
        }
    }
}

std::thread_local! {
    static THREAD_EMIT_OWNER: ThreadEmitOwner = ThreadEmitOwner::new();
}

fn thread_queue_ptr() -> *mut EmitQueue {
    THREAD_EMIT_OWNER.with(|owner| owner.queue)
}

fn resolve_queue(queue: *mut EmitQueue) -> *mut EmitQueue {
    if queue.is_null() {
        thread_queue_ptr()
    } else {
        queue
    }
}

/// Push one machine emit.
///
/// # ABI
///
/// ```text
/// hew_machine_emit_push(queue: *mut EmitQueue, tag: u32, payload: *const u8) -> i32
/// ```
///
/// `queue == null` selects the calling thread's queue. `tag` is the event
/// declaration index. `payload` is borrowed and never dereferenced by this
/// function; it must remain valid until the outermost `step()` drain completes
/// if a downstream handler dereferences it. Slice 7 codegen passes null.
///
/// # Safety
///
/// A non-null `queue` must point to a live [`EmitQueue`] owned by the current
/// thread. The pointer must not be shared across threads.
#[no_mangle]
pub unsafe extern "C" fn hew_machine_emit_push(
    queue: *mut EmitQueue,
    tag: u32,
    payload: *const u8,
) -> i32 {
    let queue = resolve_queue(queue);
    if queue.is_null() {
        return -1;
    }
    // SAFETY: caller supplied a valid queue pointer or we resolved the current
    // thread's owner.
    unsafe {
        (*queue).push(tag, payload);
    }
    0
}

/// Mark entry to a generated `step()` frame.
///
/// # Safety
///
/// Same pointer contract as [`hew_machine_emit_push`].
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_machine_emit_step_enter(queue: *mut EmitQueue) -> i32 {
    let queue = resolve_queue(queue);
    if queue.is_null() {
        return -1;
    }
    // SAFETY: pointer contract documented above.
    unsafe {
        (*queue).enter_step();
    }
    0
}

/// Mark exit from a generated `step()` frame.
///
/// The C ABI drain sink is intentionally scheduler-free in Slice 7: events are
/// drained and discarded. Rust tests and future stdlib lifecycle dispatch use
/// [`thread_emit_drain`] / [`EmitQueue::exit_step`] with an explicit handler.
///
/// # Safety
///
/// Same pointer contract as [`hew_machine_emit_push`].
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_machine_emit_step_exit(queue: *mut EmitQueue) -> i32 {
    let queue = resolve_queue(queue);
    if queue.is_null() {
        return -1;
    }
    // SAFETY: pointer contract documented above.
    unsafe {
        let result = (*queue).exit_step(|_, _| Ok::<(), ()>(()));
        if result.is_err() {
            return -1;
        }
    }
    0
}

/// Drain the calling thread's queue with a Rust handler.
///
/// # Errors
///
/// Returns [`DrainError::Handler`] if the supplied handler rejects an event.
pub fn thread_emit_drain<E>(
    handler: impl FnMut(&EmitEvent, &mut EmitQueueAppend<'_>) -> Result<(), E>,
) -> Result<(), DrainError<E>> {
    let queue = thread_queue_ptr();
    // SAFETY: returned pointer is owned by the current thread.
    unsafe { (*queue).drain(handler) }
}

/// Number of pending events on the calling thread's queue.
#[must_use]
pub fn thread_emit_pending() -> usize {
    let queue = thread_queue_ptr();
    // SAFETY: returned pointer is owned by the current thread.
    unsafe { (*queue).pending() }
}

/// Clear the calling thread's queue and active counters.
pub fn thread_emit_clear() {
    let queue = thread_queue_ptr();
    // SAFETY: returned pointer is owned by the current thread.
    unsafe {
        (*queue).clear();
    }
}
