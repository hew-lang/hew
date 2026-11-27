//! Shared helpers for poison-recovery on `std::sync` primitives and
//! zero-overhead word-0 intrusive thread-local freelists.
//!
//! The Hew runtime recovers from poisoned locks rather than panicking,
//! because a panicked thread should not cascade-crash independent actors.

use std::cell::Cell;
use std::io::Write as _;
use std::marker::PhantomData;
use std::ptr;
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

#[inline]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "pooled blocks satisfy size_of >= size_of::<*mut u8>() and align_of >= align_of::<*mut u8>()"
)]
unsafe fn read_word0_link(block: *mut u8) -> *mut u8 {
    // SAFETY: caller guarantees `block` is aligned for `*mut u8` and owns word 0.
    unsafe { block.cast::<*mut u8>().read() }
}

#[inline]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "pooled blocks satisfy size_of >= size_of::<*mut u8>() and align_of >= align_of::<*mut u8>()"
)]
unsafe fn write_word0_link(block: *mut u8, next: *mut u8) {
    // SAFETY: caller guarantees `block` is aligned for `*mut u8` and owns word 0.
    unsafe { block.cast::<*mut u8>().write(next) };
}

/// Intrusive word-0 thread-local freelist for `Box`-backed runtime objects
/// (`HewReplyChannel`, `HewActorCall`, `HewNativeReply`, `HewActorWaitEdge`).
///
/// `free` runs `ptr::drop_in_place` immediately so pooled slots never retain
/// references, then links the raw allocation through word 0 via `Cell<*mut u8>`
/// (eliminating `RefCell` borrow flags and heap `Vec` storage). Remaining slots
/// are deallocated cleanly when the thread exits; sanitizer builds bypass
/// caching so use-after-free bugs fault immediately.
pub(crate) struct TlsBoxPool<T> {
    head: Cell<*mut u8>,
    count: Cell<usize>,
    _marker: PhantomData<*mut T>,
}

impl<T> TlsBoxPool<T> {
    const LAYOUT_OK: () = {
        assert!(
            std::mem::size_of::<T>() >= std::mem::size_of::<*mut u8>(),
            "TlsBoxPool<T> requires size_of::<T>() >= size_of::<*mut u8>()"
        );
        assert!(
            std::mem::align_of::<T>() >= std::mem::align_of::<*mut u8>(),
            "TlsBoxPool<T> requires align_of::<T>() >= align_of::<*mut u8>()"
        );
    };

    pub(crate) const fn new() -> Self {
        Self {
            head: Cell::new(ptr::null_mut()),
            count: Cell::new(0),
            _marker: PhantomData,
        }
    }

    #[inline]
    pub(crate) fn alloc(&self, val: T) -> *mut T {
        let () = Self::LAYOUT_OK;
        let head = self.head.get();
        if !head.is_null() {
            // SAFETY: `head` is an uninitialized `Layout::new::<T>()` allocation
            // whose first word links the next cached block.
            let next = unsafe { read_word0_link(head) };
            self.head.set(next);
            self.count.set(self.count.get() - 1);
            let typed = head.cast::<T>();
            // SAFETY: `typed` is valid, properly aligned, and uniquely owned.
            unsafe { ptr::write(typed, val) };
            return typed;
        }
        Box::into_raw(Box::new(val))
    }

    /// Destroy the value at `ptr` and return its raw backing allocation to the
    /// thread-local pool (or global allocator if full or sanitized).
    ///
    /// # Safety
    ///
    /// `ptr` must be a valid, uniquely owned pointer allocated by `Self::alloc`
    /// (or `Box::into_raw(Box::new(_))`).
    #[inline]
    pub(crate) unsafe fn free(&self, ptr: *mut T) {
        let () = Self::LAYOUT_OK;
        // SAFETY: caller transfers unique ownership of the initialized `T`.
        // Dropping before touching `head`/`count` makes re-entrant `alloc`/`free`
        // inside `T::drop` completely safe without `RefCell` borrow flags.
        unsafe { ptr::drop_in_place(ptr) };
        let raw = ptr.cast::<u8>();
        if !cfg!(hew_sanitized) && self.count.get() < TLS_POOL_CAP {
            // SAFETY: `raw` owns a dropped `Layout::new::<T>()` allocation with
            // size and alignment >= `*mut u8`.
            unsafe { write_word0_link(raw, self.head.get()) };
            self.head.set(raw);
            self.count.set(self.count.get() + 1);
            return;
        }
        // SAFETY: `raw` was allocated with `Layout::new::<T>()` via GlobalAlloc
        // and its value has already been dropped above.
        unsafe { std::alloc::dealloc(raw, std::alloc::Layout::new::<T>()) };
    }
}

impl<T> Drop for TlsBoxPool<T> {
    fn drop(&mut self) {
        let () = Self::LAYOUT_OK;
        let layout = std::alloc::Layout::new::<T>();
        let mut cur = self.head.replace(ptr::null_mut());
        self.count.set(0);
        while !cur.is_null() {
            // SAFETY: each pooled block was dropped in `Self::free`, stores the
            // next pointer in word 0, and owns one `Layout::new::<T>()` block.
            let next = unsafe { read_word0_link(cur) };
            // SAFETY: `cur` was allocated with `layout` via GlobalAlloc.
            unsafe { std::alloc::dealloc(cur, layout) };
            cur = next;
        }
    }
}

/// Intrusive word-0 thread-local freelist for `crate::mem::buf_try_alloc`
/// fixed-size blocks (`HewMsgNode`, `HewMsgEnvelope`).
///
/// Links cached blocks through word 0 of the 16-byte-aligned payload region
/// (`ptr = base + HEADER`), leaving the preceding size header intact for
/// `crate::mem::buf_free` on eviction or thread exit.
pub(crate) struct TlsBufPool {
    head: Cell<*mut u8>,
    count: Cell<usize>,
}

impl TlsBufPool {
    pub(crate) const fn new() -> Self {
        Self {
            head: Cell::new(ptr::null_mut()),
            count: Cell::new(0),
        }
    }

    #[inline]
    pub(crate) fn pop(&self) -> Option<*mut std::ffi::c_void> {
        let head = self.head.get();
        if head.is_null() {
            return None;
        }
        // SAFETY: every cached block came from `crate::mem::buf_try_alloc`
        // (16-byte aligned, this pool's fixed size of at least one pointer)
        // and stores the next cached payload pointer in word 0.
        let next = unsafe { read_word0_link(head) };
        self.head.set(next);
        self.count.set(self.count.get() - 1);
        Some(head.cast::<std::ffi::c_void>())
    }

    /// Return a `buf_try_alloc` block to the thread-local pool (or free it).
    ///
    /// # Safety
    ///
    /// `ptr` must be a uniquely owned pointer from `crate::mem::buf_try_alloc`
    /// matching this pool's block size (at least `size_of::<*mut u8>()` bytes),
    /// with any contained Rust values already dropped.
    #[inline]
    pub(crate) unsafe fn push(&self, ptr: *mut std::ffi::c_void) {
        if ptr.is_null() {
            return;
        }
        if !cfg!(hew_sanitized) && self.count.get() < TLS_POOL_CAP {
            let raw = ptr.cast::<u8>();
            // SAFETY: `raw` is a uniquely owned `buf_try_alloc` payload region
            // aligned to `BUF_ALIGN` (16 >= align_of::<*mut u8>()).
            unsafe { write_word0_link(raw, self.head.get()) };
            self.head.set(raw);
            self.count.set(self.count.get() + 1);
            return;
        }
        // SAFETY: `ptr` came from `crate::mem::buf_try_alloc`.
        unsafe { crate::mem::buf_free(ptr) };
    }
}

impl Drop for TlsBufPool {
    fn drop(&mut self) {
        let mut cur = self.head.replace(ptr::null_mut());
        self.count.set(0);
        while !cur.is_null() {
            // SAFETY: read the word-0 link before `buf_free` reclaims `cur`.
            let next = unsafe { read_word0_link(cur) };
            // SAFETY: each pooled block came from `crate::mem::buf_try_alloc`.
            unsafe { crate::mem::buf_free(cur.cast::<std::ffi::c_void>()) };
            cur = next;
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
    use std::sync::atomic::AtomicUsize;

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

    #[test]
    fn tls_box_pool_recycles_lifo_caps_depth_and_handles_reentrant_drop() {
        static DROPS: AtomicUsize = AtomicUsize::new(0);

        struct Item {
            word0: usize,
            nested: Option<*mut Item>,
        }

        std::thread_local! {
            static POOL: TlsBoxPool<Item> = const { TlsBoxPool::new() };
        }

        impl Drop for Item {
            fn drop(&mut self) {
                DROPS.fetch_add(1, Ordering::Relaxed);
                if let Some(child) = self.nested.take() {
                    // Re-enter the same TLS pool from inside `T::drop`.
                    POOL.with(|pool| {
                        // SAFETY: `child` was uniquely allocated from `POOL`.
                        unsafe { pool.free(child) };
                    });
                }
            }
        }

        DROPS.store(0, Ordering::Relaxed);
        POOL.with(|pool| {
            let leaf = pool.alloc(Item {
                word0: 0xDEAD_BEEF,
                nested: None,
            });
            let root = pool.alloc(Item {
                word0: 0xCAFE_BABE,
                nested: Some(leaf),
            });
            // SAFETY: `root` owns `leaf` and both came from `pool.alloc`.
            unsafe { pool.free(root) };
            assert_eq!(DROPS.load(Ordering::Relaxed), 2);

            if !cfg!(hew_sanitized) {
                assert_eq!(pool.count.get(), 2);
                // `free(root)` runs `root.drop()` first (pushing `leaf`), then pushes
                // `root`, so LIFO order returns `root` first and `leaf` second.
                let reused_root = pool.alloc(Item {
                    word0: 42,
                    nested: None,
                });
                assert_eq!(reused_root, root);
                let reused_leaf = pool.alloc(Item {
                    word0: 43,
                    nested: None,
                });
                assert_eq!(reused_leaf, leaf);
                // SAFETY: `reused_root` and `reused_leaf` are uniquely owned.
                unsafe {
                    (*reused_root).word0 = 99;
                    assert_eq!((*reused_root).word0, 99);
                    pool.free(reused_leaf);
                    pool.free(reused_root);
                }
            }

            // Overflow beyond TLS_POOL_CAP deallocates cleanly.
            let mut ptrs = Vec::with_capacity(TLS_POOL_CAP + 16);
            for i in 0..(TLS_POOL_CAP + 16) {
                ptrs.push(pool.alloc(Item {
                    word0: i,
                    nested: None,
                }));
            }
            for p in ptrs {
                // SAFETY: each pointer came from `pool.alloc`.
                unsafe { pool.free(p) };
            }
            if !cfg!(hew_sanitized) {
                assert_eq!(pool.count.get(), TLS_POOL_CAP);
            }
        });
    }

    #[test]
    fn tls_buf_pool_recycles_and_preserves_header_for_drop() {
        let pool = TlsBufPool::new();
        let mut blocks = Vec::with_capacity(TLS_POOL_CAP + 8);
        for _ in 0..(TLS_POOL_CAP + 8) {
            let ptr = crate::mem::buf_try_alloc(128);
            assert!(!ptr.is_null());
            blocks.push(ptr);
        }
        for ptr in blocks {
            // SAFETY: each `ptr` is a uniquely owned 128B `buf_try_alloc` block.
            unsafe { pool.push(ptr) };
        }
        if !cfg!(hew_sanitized) {
            assert_eq!(pool.count.get(), TLS_POOL_CAP);
            let popped = pool.pop().expect("pooled block");
            // SAFETY: `popped` owns 128 writable bytes and is returned once.
            unsafe {
                popped.cast::<usize>().write(0x1234_5678);
                assert_eq!(popped.cast::<usize>().read(), 0x1234_5678);
                crate::mem::buf_free(popped);
            }
        }
        drop(pool);
    }
}
