//! Atomically reference-counted smart pointer (`Arc<T>`).
//!
//! Thread-safe reference counting for cross-actor sharing. Requires
//! `T: Frozen` (immutable data only). For single-actor use, prefer
//! `Rc<T>` which avoids atomic overhead.
//!
//! Layout: `[HewArcInner header | data bytes...]`
//! Same pointer convention as Rc: returned pointer is to the data region.

use std::alloc::{alloc, dealloc, Layout};
use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Header preceding the user data in an Arc allocation.
#[repr(C)]
#[derive(Debug)]
struct HewArcInner {
    strong: AtomicUsize,
    weak: AtomicUsize,
    drop_fn: Option<unsafe extern "C" fn(*mut u8)>,
    data_size: usize,
}

/// Recover header pointer from data pointer.
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_arc_new`].
unsafe fn header_from_data(data_ptr: *mut u8) -> *mut HewArcInner {
    // SAFETY: data sits immediately after HewArcInner.
    unsafe { data_ptr.sub(size_of::<HewArcInner>()) }.cast()
}

/// Compute allocation layout for header + data.
fn alloc_layout(data_size: usize) -> Layout {
    let total = size_of::<HewArcInner>() + data_size;
    let align = align_of::<HewArcInner>();
    Layout::from_size_align(total, align).expect("Arc layout overflow")
}

// ── Public C ABI ───────────────────────────────────────────────────────

/// Create a new `Arc<T>`. Copies `size` bytes from `data` into a
/// heap-allocated block with atomic reference count header.
///
/// # Safety
///
/// - `data` must be valid for `size` bytes (may be null if `size == 0`).
/// - `drop_fn`, if provided, will be called with a pointer to the data
///   region when the strong count reaches zero.
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "ptr was allocated with align_of::<HewArcInner>() via alloc_layout"
)]
pub unsafe extern "C" fn hew_arc_new(
    data: *const u8,
    size: usize,
    drop_fn: Option<unsafe extern "C" fn(*mut u8)>,
) -> *mut u8 {
    let layout = alloc_layout(size);
    // SAFETY: layout is valid (non-zero size due to header).
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        std::alloc::handle_alloc_error(layout);
    }

    // SAFETY: ptr was allocated with align_of::<HewArcInner>(), so the cast is aligned.
    let header = ptr.cast::<HewArcInner>();
    // SAFETY: ptr is freshly allocated with correct alignment.
    unsafe {
        header.write(HewArcInner {
            strong: AtomicUsize::new(1),
            weak: AtomicUsize::new(1), // +1 implicit weak ref held by strong refs
            drop_fn,
            data_size: size,
        });
    }

    // SAFETY: ptr + sizeof(header) is within the allocation of total bytes.
    let data_ptr = unsafe { ptr.add(size_of::<HewArcInner>()) };
    if !data.is_null() && size > 0 {
        // SAFETY: data is valid for size bytes, data_ptr is valid for size.
        unsafe { ptr::copy_nonoverlapping(data, data_ptr, size) };
    }
    data_ptr
}

/// Atomically increment the strong reference count. Returns the same
/// data pointer. Thread-safe.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_arc_new`] or [`hew_arc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_arc_clone(ptr: *mut u8) -> *mut u8 {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees ptr is a valid Arc data pointer.
    let header = unsafe { header_from_data(ptr) };
    // Relaxed is fine for increment — we just need atomicity.
    // SAFETY: header is valid.
    unsafe { (*header).strong.fetch_add(1, Ordering::Relaxed) };
    ptr
}

/// Atomically decrement the strong reference count. If it reaches zero,
/// calls the drop function (if any), then frees if weak is also zero.
///
/// Uses Release ordering on the decrement and Acquire fence before drop
/// (same pattern as `std::sync::Arc`).
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_arc_new`] or [`hew_arc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_arc_drop(ptr: *mut u8) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    let inner = unsafe { &*header };

    // Release: ensure all writes before this drop are visible to the
    // thread that observes strong == 0.
    let old = inner.strong.fetch_sub(1, Ordering::Release);
    debug_assert!(old > 0, "Arc double-free");

    if old != 1 {
        return; // Not the last strong reference.
    }

    // Acquire fence: synchronize with all Release decrements to ensure
    // we see all prior writes before running the destructor.
    std::sync::atomic::fence(Ordering::Acquire);

    // Call destructor on the data.
    if let Some(drop_fn) = inner.drop_fn {
        // SAFETY: drop_fn contract per hew_arc_new.
        unsafe { drop_fn(ptr) };
    }

    // Release the implicit weak ref held by the strong count.
    // Release ordering ensures the destructor above is visible before
    // any thread deallocates.
    if inner.weak.fetch_sub(1, Ordering::Release) == 1 {
        // We were the last weak ref (implicit). Deallocate.
        std::sync::atomic::fence(Ordering::Acquire);
        let layout = alloc_layout(inner.data_size);
        // SAFETY: header was allocated with this layout, no other refs remain.
        unsafe { dealloc(header.cast(), layout) };
    }
    // If weak > 0, external weak refs still exist; they will deallocate.
}

/// Get the current strong reference count (atomic load).
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_arc_new`] or [`hew_arc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_arc_strong_count(ptr: *mut u8) -> usize {
    if ptr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).strong.load(Ordering::Relaxed) }
}

/// Get the data pointer from an `Arc`. In the current layout the handle
/// itself *is* the data pointer, so this is an identity function — but
/// callers should use it for API clarity.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_arc_new`] or [`hew_arc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_arc_get(ptr: *mut u8) -> *mut u8 {
    ptr
}

/// Get the strong reference count as `u32`. Convenience wrapper around
/// [`hew_arc_strong_count`].
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_arc_new`] or [`hew_arc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_arc_count(ptr: *mut u8) -> u32 {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "C ABI convenience — counts should never exceed u32"
    )]
    // SAFETY: forwarded to hew_arc_strong_count with same preconditions.
    let count = unsafe { hew_arc_strong_count(ptr) } as u32;
    count
}

/// Create a `Weak` reference from an `Arc` data pointer. Atomically
/// increments the weak count. Returns a pointer to the *header*.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_arc_new`] or [`hew_arc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_arc_downgrade(ptr: *mut u8) -> *mut u8 {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).weak.fetch_add(1, Ordering::Relaxed) };
    header.cast()
}

/// Attempt to upgrade a `Weak<Arc>` reference back to a strong `Arc`.
/// Uses a CAS loop to atomically increment strong if > 0.
///
/// Returns the data pointer on success, null on failure.
///
/// # Safety
///
/// `weak_ptr` must have been returned by [`hew_arc_downgrade`].
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "weak_ptr is a header pointer allocated with align_of::<HewArcInner>()"
)]
pub unsafe extern "C" fn hew_weak_upgrade_arc(weak_ptr: *mut u8) -> *mut u8 {
    if weak_ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: weak_ptr was returned by hew_arc_downgrade which returns a header pointer
    // allocated with align_of::<HewArcInner>().
    let header = weak_ptr.cast::<HewArcInner>();
    // SAFETY: weak_ptr is a header pointer.
    let inner = unsafe { &*header };

    // CAS loop: increment strong only if currently > 0.
    loop {
        let current = inner.strong.load(Ordering::Relaxed);
        if current == 0 {
            return ptr::null_mut();
        }
        if inner
            .strong
            .compare_exchange_weak(current, current + 1, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            // SAFETY: data pointer is immediately after header, within the allocation.
            return unsafe { weak_ptr.add(size_of::<HewArcInner>()) };
        }
    }
}

/// Drop a `Weak<Arc>` reference. Atomically decrements the weak count.
/// If both strong and weak counts reach zero, frees the allocation.
///
/// # Safety
///
/// `weak_ptr` must have been returned by [`hew_arc_downgrade`].
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "weak_ptr is a header pointer allocated with align_of::<HewArcInner>()"
)]
pub unsafe extern "C" fn hew_weak_drop_arc(weak_ptr: *mut u8) {
    if weak_ptr.is_null() {
        return;
    }
    // SAFETY: weak_ptr was returned by hew_arc_downgrade which returns a header pointer
    // allocated with align_of::<HewArcInner>().
    let header = weak_ptr.cast::<HewArcInner>();
    // SAFETY: weak_ptr is a header pointer.
    let inner = unsafe { &*header };

    let old = inner.weak.fetch_sub(1, Ordering::Release);
    debug_assert!(old > 0, "Weak<Arc> double-free");

    if old != 1 {
        return;
    }

    // Last weak ref dropped. Strong count must already be 0 (otherwise
    // the implicit +1 weak ref would still be held). Deallocate.
    std::sync::atomic::fence(Ordering::Acquire);

    let layout = alloc_layout(inner.data_size);
    // SAFETY: header was allocated with this layout, strong=0 and weak=0.
    unsafe { dealloc(header.cast(), layout) };
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arc_basic_lifecycle() {
        unsafe {
            let val: i32 = 42;
            let arc = hew_arc_new((&raw const val).cast(), size_of::<i32>(), None);
            assert!(!arc.is_null());

            let read_val = arc.cast::<i32>().read();
            assert_eq!(read_val, 42);
            assert_eq!(hew_arc_strong_count(arc), 1);

            let arc2 = hew_arc_clone(arc);
            assert_eq!(arc2, arc);
            assert_eq!(hew_arc_strong_count(arc), 2);

            hew_arc_drop(arc2);
            assert_eq!(hew_arc_strong_count(arc), 1);

            hew_arc_drop(arc);
        }
    }

    #[test]
    fn arc_cross_thread() {
        use std::sync::atomic::AtomicI32;
        static DROP_COUNT: AtomicI32 = AtomicI32::new(0);

        unsafe extern "C" fn drop_counter(_data: *mut u8) {
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }

        unsafe {
            DROP_COUNT.store(0, Ordering::SeqCst);
            let val: i32 = 100;
            let arc = hew_arc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                Some(drop_counter),
            );

            let arc_copy = arc as usize;
            let handle = std::thread::spawn(move || {
                let arc = arc_copy as *mut u8;
                let clone = hew_arc_clone(arc);
                assert_eq!(clone.cast::<i32>().read(), 100);
                hew_arc_drop(clone);
            });
            handle.join().unwrap();

            assert_eq!(hew_arc_strong_count(arc), 1);
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 0);

            hew_arc_drop(arc);
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn arc_weak_upgrade() {
        unsafe {
            let val: i32 = 77;
            let arc = hew_arc_new((&raw const val).cast(), size_of::<i32>(), None);

            let weak = hew_arc_downgrade(arc);
            assert!(!weak.is_null());

            let upgraded = hew_weak_upgrade_arc(weak);
            assert!(!upgraded.is_null());
            assert_eq!(upgraded.cast::<i32>().read(), 77);
            assert_eq!(hew_arc_strong_count(arc), 2);

            hew_arc_drop(arc);
            hew_arc_drop(upgraded);

            let failed = hew_weak_upgrade_arc(weak);
            assert!(failed.is_null());

            hew_weak_drop_arc(weak);
        }
    }
}
