//! Shared helpers for poison-recovery on `std::sync` primitives.
//!
//! The Hew runtime recovers from poisoned locks rather than panicking,
//! because a panicked thread should not cascade-crash independent actors.

use std::io::Write as _;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Condvar, Mutex, MutexGuard, PoisonError};
use std::time::Duration;

/// Aligns `T` to a cache line (128 bytes on aarch64/Apple Silicon where the L2
/// prefetcher pairs 64-byte lines, 64 bytes elsewhere) to prevent false sharing.
#[cfg_attr(target_arch = "aarch64", repr(align(128)))]
#[cfg_attr(not(target_arch = "aarch64"), repr(align(64)))]
#[derive(Debug, Default)]
pub(crate) struct CachePadded<T>(pub(crate) T);

impl<T> std::ops::Deref for CachePadded<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> std::ops::DerefMut for CachePadded<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

/// Cache-line-sharded monotonic counter for hot-path runtime metrics.
///
/// Eliminates cross-core cache-line bouncing on global counters (`MESSAGES_SENT`,
/// `MESSAGES_RECEIVED`, `TASKS_SPAWNED`, `STEALS_TOTAL`) by hashing each thread
/// onto its own cache-line-aligned shard on native targets, while collapsing to
/// a single `AtomicU64` on single-threaded `wasm32`.
#[cfg(not(target_arch = "wasm32"))]
const COUNTER_SHARDS: usize = 64;

#[cfg(not(target_arch = "wasm32"))]
static NEXT_COUNTER_SHARD: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

#[cfg(not(target_arch = "wasm32"))]
std::thread_local! {
    static THREAD_COUNTER_SHARD: usize =
        NEXT_COUNTER_SHARD.fetch_add(1, Ordering::Relaxed) & (COUNTER_SHARDS - 1);
}

#[derive(Debug)]
pub(crate) struct ShardedCounter {
    #[cfg(not(target_arch = "wasm32"))]
    shards: [CachePadded<AtomicU64>; COUNTER_SHARDS],
    #[cfg(target_arch = "wasm32")]
    value: AtomicU64,
}

impl ShardedCounter {
    pub(crate) const fn new() -> Self {
        #[cfg(not(target_arch = "wasm32"))]
        {
            Self {
                shards: [const { CachePadded(AtomicU64::new(0)) }; COUNTER_SHARDS],
            }
        }
        #[cfg(target_arch = "wasm32")]
        {
            Self {
                value: AtomicU64::new(0),
            }
        }
    }

    #[inline]
    pub(crate) fn fetch_add(&self, val: u64, order: Ordering) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let idx = THREAD_COUNTER_SHARD.with(|&slot| slot);
            self.shards[idx].fetch_add(val, order);
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.value.fetch_add(val, order);
        }
    }

    pub(crate) fn load(&self, order: Ordering) -> u64 {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let mut sum = 0u64;
            for shard in &self.shards {
                sum = sum.wrapping_add(shard.load(order));
            }
            sum
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.value.load(order)
        }
    }

    pub(crate) fn store(&self, val: u64, order: Ordering) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.shards[0].store(val, order);
            for shard in &self.shards[1..] {
                shard.store(0, order);
            }
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.value.store(val, order);
        }
    }
}

/// Cache-line-sharded signed gauge for hot-path up/down runtime counters.
///
/// Supports increments on one worker thread and decrements on another without
/// cross-core cache-line bouncing. `load()` sums all shards and clamps at 0.
#[derive(Debug)]
pub(crate) struct ShardedGauge {
    #[cfg(not(target_arch = "wasm32"))]
    shards: [CachePadded<std::sync::atomic::AtomicI64>; COUNTER_SHARDS],
    #[cfg(target_arch = "wasm32")]
    value: std::sync::atomic::AtomicI64,
}

impl ShardedGauge {
    pub(crate) const fn new() -> Self {
        #[cfg(not(target_arch = "wasm32"))]
        {
            Self {
                shards: [const { CachePadded(std::sync::atomic::AtomicI64::new(0)) };
                    COUNTER_SHARDS],
            }
        }
        #[cfg(target_arch = "wasm32")]
        {
            Self {
                value: std::sync::atomic::AtomicI64::new(0),
            }
        }
    }

    #[inline]
    #[expect(
        clippy::cast_possible_wrap,
        reason = "runtime gauge deltas fit comfortably in i64"
    )]
    pub(crate) fn fetch_add(&self, val: u64, order: Ordering) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let idx = THREAD_COUNTER_SHARD.with(|&slot| slot);
            self.shards[idx].fetch_add(val as i64, order);
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.value.fetch_add(val as i64, order);
        }
    }

    #[inline]
    #[expect(
        clippy::cast_possible_wrap,
        reason = "runtime gauge deltas fit comfortably in i64"
    )]
    pub(crate) fn fetch_sub(&self, val: u64, order: Ordering) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let idx = THREAD_COUNTER_SHARD.with(|&slot| slot);
            self.shards[idx].fetch_sub(val as i64, order);
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.value.fetch_sub(val as i64, order);
        }
    }

    #[expect(
        clippy::cast_sign_loss,
        reason = "clamped non-negative with .max(0) before casting to u64"
    )]
    pub(crate) fn load(&self, order: Ordering) -> u64 {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let mut sum = 0i64;
            for shard in &self.shards {
                sum = sum.wrapping_add(shard.load(order));
            }
            sum.max(0) as u64
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.value.load(order).max(0) as u64
        }
    }

    #[expect(
        clippy::cast_possible_wrap,
        reason = "runtime gauge values fit comfortably in i64"
    )]
    pub(crate) fn store(&self, val: u64, order: Ordering) {
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.shards[0].store(val as i64, order);
            for shard in &self.shards[1..] {
                shard.store(0, order);
            }
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.value.store(val as i64, order);
        }
    }

    #[cfg(test)]
    pub(crate) fn swap(&self, val: u64, order: Ordering) -> u64 {
        let load_order = match order {
            Ordering::AcqRel => Ordering::Acquire,
            Ordering::Release => Ordering::Relaxed,
            other => other,
        };
        let store_order = match order {
            Ordering::AcqRel => Ordering::Release,
            Ordering::Acquire => Ordering::Relaxed,
            other => other,
        };
        let prev = self.load(load_order);
        self.store(val, store_order);
        prev
    }
}

/// Maximum recycled allocations retained per thread in a hot-path TLS pool.
const TLS_POOL_CAP: usize = 128;

/// Thread-local freelist for `Box`-backed runtime objects (`HewReplyChannel`,
/// `HewActorCall`, `HewNativeReply`).
///
/// `free` runs `ptr::drop_in_place` immediately so pooled slots never retain
/// references, then caches the raw allocation locally to bypass global
/// allocator locks on steady-state actor ask/reply paths. Remaining slots are
/// deallocated cleanly when the thread exits.
pub(crate) struct TlsBoxPool<T> {
    slots: std::cell::RefCell<Vec<*mut T>>,
}

impl<T> TlsBoxPool<T> {
    pub(crate) const fn new() -> Self {
        Self {
            slots: std::cell::RefCell::new(Vec::new()),
        }
    }

    #[inline]
    pub(crate) fn alloc(&self, val: T) -> *mut T {
        debug_assert!(std::mem::size_of::<T>() > 0);
        if let Ok(mut slots) = self.slots.try_borrow_mut() {
            if let Some(ptr) = slots.pop() {
                // SAFETY: `ptr` is a valid, properly aligned, uninitialized
                // `Layout::new::<T>()` allocation previously returned to this pool.
                unsafe { std::ptr::write(ptr, val) };
                return ptr;
            }
        }
        Box::into_raw(Box::new(val))
    }

    /// Destroy the value at `ptr` and return its raw backing allocation to the
    /// thread-local pool (or global allocator if full).
    ///
    /// # Safety
    ///
    /// `ptr` must be a valid, uniquely owned pointer allocated by `Self::alloc`
    /// (or `Box::into_raw(Box::new(_))`).
    #[inline]
    pub(crate) unsafe fn free(&self, ptr: *mut T) {
        debug_assert!(std::mem::size_of::<T>() > 0);
        // SAFETY: caller transfers unique ownership of the initialized `T`.
        unsafe { std::ptr::drop_in_place(ptr) };
        if let Ok(mut slots) = self.slots.try_borrow_mut() {
            if slots.len() < TLS_POOL_CAP {
                slots.push(ptr);
                return;
            }
        }
        // SAFETY: `ptr` was allocated with `Layout::new::<T>()` via GlobalAlloc
        // and its value has already been dropped above.
        unsafe { std::alloc::dealloc(ptr.cast::<u8>(), std::alloc::Layout::new::<T>()) };
    }
}

impl<T> Drop for TlsBoxPool<T> {
    fn drop(&mut self) {
        let layout = std::alloc::Layout::new::<T>();
        for ptr in self.slots.get_mut().drain(..) {
            // SAFETY: each pooled slot was dropped in `Self::free` and owns
            // one raw `Layout::new::<T>()` allocation from GlobalAlloc.
            unsafe { std::alloc::dealloc(ptr.cast::<u8>(), layout) };
        }
    }
}

/// Thread-local freelist for `crate::mem::buf_try_alloc` fixed-size blocks
/// (`HewMsgNode`, `HewMsgEnvelope`).
pub(crate) struct TlsBufPool {
    slots: std::cell::RefCell<Vec<*mut std::ffi::c_void>>,
}

impl TlsBufPool {
    pub(crate) const fn new() -> Self {
        Self {
            slots: std::cell::RefCell::new(Vec::new()),
        }
    }

    #[inline]
    pub(crate) fn pop(&self) -> Option<*mut std::ffi::c_void> {
        self.slots.try_borrow_mut().ok()?.pop()
    }

    /// Return a `buf_try_alloc` block to the thread-local pool (or free it).
    ///
    /// # Safety
    ///
    /// `ptr` must be a uniquely owned pointer from `crate::mem::buf_try_alloc`
    /// matching this pool's block size, with any contained Rust values already
    /// dropped.
    #[inline]
    pub(crate) unsafe fn push(&self, ptr: *mut std::ffi::c_void) {
        if let Ok(mut slots) = self.slots.try_borrow_mut() {
            if slots.len() < TLS_POOL_CAP {
                slots.push(ptr);
                return;
            }
        }
        // SAFETY: `ptr` came from `crate::mem::buf_try_alloc`.
        unsafe { crate::mem::buf_free(ptr) };
    }
}

impl Drop for TlsBufPool {
    fn drop(&mut self) {
        for ptr in self.slots.get_mut().drain(..) {
            // SAFETY: each pooled block came from `crate::mem::buf_try_alloc`.
            unsafe { crate::mem::buf_free(ptr) };
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::quarantine_panic_payload;
    use std::sync::{PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

    /// Build a JSON array by writing each element directly into the output string.
    ///
    /// Consumed only by the profiler's JSON snapshot endpoints; gated to the
    /// `profiler` feature (plus `test`) so non-profiler builds don't carry it dead.
    #[cfg(any(feature = "profiler", test))]
    pub(crate) fn json_array<I, F>(items: I, mut write_item: F) -> String
    where
        I: IntoIterator,
        F: FnMut(&mut String, I::Item),
    {
        let mut json = String::from("[");
        let mut first = true;
        for item in items {
            if first {
                first = false;
            } else {
                json.push(',');
            }
            write_item(&mut json, item);
        }
        json.push(']');
        json
    }

    /// Push a JSON string value into `out`, escaping control characters as needed.
    ///
    /// Consumed only by the profiler's JSON snapshot endpoints; gated to the
    /// `profiler` feature (plus `test`) so non-profiler builds don't carry it dead.
    #[cfg(any(feature = "profiler", test))]
    pub(crate) fn push_json_string(out: &mut String, s: &str) {
        use std::fmt::Write as _;

        out.push('"');
        for ch in s.chars() {
            match ch {
                '"' => out.push_str("\\\""),
                '\\' => out.push_str("\\\\"),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                ch if ch.is_control() => {
                    let _ = write!(out, "\\u{:04x}", ch as u32);
                }
                ch => out.push(ch),
            }
        }
        out.push('"');
    }

    /// Extract a readable message from a panic payload.
    pub(crate) fn panic_payload_message(panic_payload: &(dyn std::any::Any + Send)) -> String {
        if let Some(s) = panic_payload.downcast_ref::<&str>() {
            (*s).to_string()
        } else if let Some(s) = panic_payload.downcast_ref::<String>() {
            s.clone()
        } else {
            "non-string panic payload".to_string()
        }
    }

    /// Extract a diagnostic string, then release the owned payload through the
    /// runtime's containment authority.
    pub(crate) fn take_panic_payload_message(payload: Box<dyn std::any::Any + Send>) -> String {
        let message = panic_payload_message(payload.as_ref());
        quarantine_panic_payload(payload);
        message
    }

    /// Report a background-thread panic observed while joining during teardown.
    pub(crate) fn report_join_panic(context: &str, join_result: std::thread::Result<()>) {
        if let Err(panic_payload) = join_result {
            let message = format!(
                "{context} panicked during teardown: {}",
                take_panic_payload_message(panic_payload)
            );
            crate::set_last_error(message.clone());
            eprintln!("hew: {message}");
        }
    }

    /// Extension trait for [`RwLock`] that recovers from poisoned locks.
    pub(crate) trait RwLockExt<T> {
        fn read_or_recover(&self) -> RwLockReadGuard<'_, T>;
        fn write_or_recover(&self) -> RwLockWriteGuard<'_, T>;
    }

    impl<T> RwLockExt<T> for RwLock<T> {
        fn read_or_recover(&self) -> RwLockReadGuard<'_, T> {
            self.read().unwrap_or_else(PoisonError::into_inner)
        }

        fn write_or_recover(&self) -> RwLockWriteGuard<'_, T> {
            self.write().unwrap_or_else(PoisonError::into_inner)
        }
    }

    /// Convert a C string pointer to a Rust `&str`, setting last error on failure.
    ///
    /// # Safety
    ///
    /// `ptr` must be a valid, null-terminated C string or null.
    pub(crate) unsafe fn cstr_to_str<'a>(
        ptr: &'a *const std::ffi::c_char,
        context: &str,
    ) -> Option<&'a str> {
        if ptr.is_null() {
            crate::set_last_error(format!("{context}: null pointer"));
            return None;
        }
        // SAFETY: Caller guarantees ptr is a valid NUL-terminated C string.
        if let Ok(s) = unsafe { std::ffi::CStr::from_ptr(*ptr) }.to_str() {
            Some(s)
        } else {
            crate::set_last_error(format!("{context}: invalid UTF-8"));
            None
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) use native::{cstr_to_str, report_join_panic, take_panic_payload_message, RwLockExt};
#[cfg(all(any(feature = "profiler", test), not(target_arch = "wasm32")))]
pub(crate) use native::{json_array, push_json_string};

/// Dispose a caught panic payload without allowing hostile payload drop glue
/// to escape the runtime containment boundary.
///
/// `catch_unwind` transfers ownership of the payload allocation to its `Err`
/// value. Dropping that value is arbitrary Rust code: a custom panic payload
/// may itself panic from `Drop`. One secondary panic is quarantined and its
/// replacement payload is disposed as well. A recursively hostile chain is
/// not recoverable without either leaking or permitting an unwind through the
/// boundary, so the process terminates deterministically after two attempts.
pub(crate) fn quarantine_panic_payload(mut payload: Box<dyn std::any::Any + Send>) {
    for _ in 0..2 {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| drop(payload))) {
            Ok(()) => return,
            Err(next_payload) => payload = next_payload,
        }
    }

    // Do not use `eprintln!` on this terminal path: its internal write is
    // unwrapped and can panic when stderr is unavailable, which would let a
    // third unwind escape the containment boundary instead of aborting.
    let _ = std::io::stderr()
        .write_all(b"fatal: recursively panicking runtime panic-payload destructor\n");
    std::process::abort();
}

/// Extension trait for [`Mutex`] that recovers from poisoned locks.
pub(crate) trait MutexExt<T> {
    fn lock_or_recover(&self) -> MutexGuard<'_, T>;
}

impl<T> MutexExt<T> for Mutex<T> {
    fn lock_or_recover(&self) -> MutexGuard<'_, T> {
        self.lock().unwrap_or_else(PoisonError::into_inner)
    }
}

/// Extension trait for [`Condvar`] that recovers from poisoned waits.
pub(crate) trait CondvarExt {
    fn wait_or_recover<'a, T>(&self, guard: MutexGuard<'a, T>) -> MutexGuard<'a, T>;

    fn wait_timeout_or_recover<'a, T>(
        &self,
        guard: MutexGuard<'a, T>,
        dur: Duration,
    ) -> (MutexGuard<'a, T>, std::sync::WaitTimeoutResult);
}

impl CondvarExt for Condvar {
    fn wait_or_recover<'a, T>(&self, guard: MutexGuard<'a, T>) -> MutexGuard<'a, T> {
        self.wait(guard).unwrap_or_else(PoisonError::into_inner)
    }

    fn wait_timeout_or_recover<'a, T>(
        &self,
        guard: MutexGuard<'a, T>,
        dur: Duration,
    ) -> (MutexGuard<'a, T>, std::sync::WaitTimeoutResult) {
        self.wait_timeout(guard, dur)
            .unwrap_or_else(PoisonError::into_inner)
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    #[test]
    fn json_array_empty_returns_empty_array() {
        let json = json_array(std::iter::empty::<i32>(), |_, _| unreachable!());
        assert_eq!(json, "[]");
    }

    #[test]
    fn json_array_multiple_items_inserts_commas_between_items() {
        use std::fmt::Write as _;

        let json = json_array([1, 2, 3], |out, value| {
            let _ = write!(out, "{value}");
        });

        assert_eq!(json, "[1,2,3]");
    }

    #[test]
    fn push_json_string_escapes_json_metacharacters_and_controls() {
        let mut json = String::new();
        push_json_string(&mut json, "quo\"te\\slash\nline\rreturn\t\x1f");
        assert_eq!(json, r#""quo\"te\\slash\nline\rreturn\t\u001f""#);
    }

    #[test]
    fn cstr_to_str_borrows_through_pointer_binding() {
        let text = std::ffi::CString::new("hew").unwrap();
        let ptr = text.as_ptr();
        // SAFETY: ptr originates from the live CString above.
        let parsed = unsafe { cstr_to_str(&ptr, "test cstr_to_str") }.unwrap();
        assert_eq!(parsed, "hew");
    }
}
