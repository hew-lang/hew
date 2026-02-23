//! Reference-counted smart pointer (`Rc<T>`).
//!
//! Non-atomic reference counting for single-actor use. NOT `Send` — cannot
//! cross actor boundaries. For cross-actor sharing, use `Arc<T>`.
//!
//! Layout: `[HewRcInner header | data bytes...]`
//! The returned pointer from `hew_rc_new` points to the **data region**
//! (immediately after the header). All functions accept/return data pointers.

use std::alloc::{alloc, dealloc, Layout};
use std::ptr;

/// Header preceding the user data in an Rc allocation.
#[repr(C)]
#[derive(Debug)]
struct HewRcInner {
    strong: usize,
    weak: usize,
    drop_fn: Option<unsafe extern "C" fn(*mut u8)>,
    data_size: usize,
}

/// Recover header pointer from data pointer.
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_rc_new`].
unsafe fn header_from_data(data_ptr: *mut u8) -> *mut HewRcInner {
    // SAFETY: data sits immediately after HewRcInner.
    unsafe { data_ptr.sub(size_of::<HewRcInner>()) }.cast()
}

/// Compute allocation layout for header + data. Returns `None` on overflow.
fn alloc_layout(data_size: usize) -> Option<Layout> {
    let total = size_of::<HewRcInner>().checked_add(data_size)?;
    let align = align_of::<HewRcInner>();
    Layout::from_size_align(total, align).ok()
}

// ── Public C ABI ───────────────────────────────────────────────────────

/// Create a new `Rc<T>`. Copies `size` bytes from `data` into a
/// heap-allocated block with a reference count header. Returns a pointer
/// to the data region (caller uses this as the Rc handle).
///
/// Returns null if the layout computation overflows.
///
/// # Safety
///
/// - `data` must be valid for `size` bytes (may be null if `size == 0`).
/// - `drop_fn`, if provided, will be called with a pointer to the data
///   region when the strong count reaches zero.
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "ptr was allocated with align_of::<HewRcInner>() via alloc_layout"
)]
pub unsafe extern "C" fn hew_rc_new(
    data: *const u8,
    size: usize,
    drop_fn: Option<unsafe extern "C" fn(*mut u8)>,
) -> *mut u8 {
    let Some(layout) = alloc_layout(size) else {
        return ptr::null_mut();
    };
    // SAFETY: layout is valid (non-zero size due to header).
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        std::alloc::handle_alloc_error(layout);
    }

    // SAFETY: ptr was allocated with align_of::<HewRcInner>(), so the cast is aligned.
    let header = ptr.cast::<HewRcInner>();
    // SAFETY: ptr is freshly allocated with correct alignment.
    unsafe {
        header.write(HewRcInner {
            strong: 1,
            weak: 0,
            drop_fn,
            data_size: size,
        });
    }

    // SAFETY: ptr + sizeof(header) is within the allocation of total bytes.
    let data_ptr = unsafe { ptr.add(size_of::<HewRcInner>()) };
    if !data.is_null() && size > 0 {
        // SAFETY: data is valid for size bytes, data_ptr is valid for size.
        unsafe { ptr::copy_nonoverlapping(data, data_ptr, size) };
    }
    data_ptr
}

/// Increment the strong reference count. Returns the same data pointer.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_clone(ptr: *mut u8) -> *mut u8 {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees ptr is a valid Rc data pointer.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).strong += 1 };
    ptr
}

/// Decrement the strong reference count. If it reaches zero, calls the
/// drop function (if any) on the data region, then frees the allocation
/// if the weak count is also zero.
///
/// # Panics
///
/// Panics if the layout computation overflows (should never happen since
/// the layout was valid at construction time).
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
/// Must not be used after this call (use-after-free).
#[no_mangle]
pub unsafe extern "C" fn hew_rc_drop(ptr: *mut u8) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: caller guarantees ptr is a valid Rc data pointer.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    let inner = unsafe { &mut *header };

    debug_assert!(inner.strong > 0, "Rc double-free");
    inner.strong -= 1;

    if inner.strong == 0 {
        // Call destructor on the data.
        if let Some(drop_fn) = inner.drop_fn {
            // SAFETY: drop_fn contract per hew_rc_new.
            unsafe { drop_fn(ptr) };
        }

        if inner.weak == 0 {
            // SAFETY: data_size was validated at construction time.
            let layout = alloc_layout(inner.data_size).expect("layout was valid at construction");
            // SAFETY: header was allocated with this layout.
            unsafe { dealloc(header.cast(), layout) };
        }
        // If weak > 0, the allocation stays alive for weak references
        // but the data is considered dead (strong == 0).
    }
}

/// Get the current strong reference count.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_strong_count(ptr: *mut u8) -> usize {
    if ptr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).strong }
}

/// Get the current weak reference count.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_weak_count(ptr: *mut u8) -> usize {
    if ptr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).weak }
}

/// Get the data pointer from an `Rc`. In the current layout the handle
/// itself *is* the data pointer, so this is an identity function — but
/// callers should use it for API clarity.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_get(ptr: *mut u8) -> *mut u8 {
    ptr
}

/// Get the strong reference count as `u32`. Convenience wrapper around
/// [`hew_rc_strong_count`].
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_count(ptr: *mut u8) -> u32 {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "C ABI convenience — counts should never exceed u32"
    )]
    // SAFETY: forwarded to hew_rc_strong_count with same preconditions.
    let count = unsafe { hew_rc_strong_count(ptr) } as u32;
    count
}

/// Returns 1 if this `Rc` is the only strong reference (refcount == 1),
/// 0 otherwise.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_is_unique(ptr: *mut u8) -> i32 {
    // SAFETY: forwarded to hew_rc_strong_count with same preconditions.
    i32::from(unsafe { hew_rc_strong_count(ptr) } == 1)
}

/// Create a `Weak` reference from an `Rc` data pointer. Increments the
/// weak count. Returns a pointer to the *header* (not the data) — weak
/// refs need the header to check if the strong count is still > 0.
///
/// # Safety
///
/// `ptr` must have been returned by [`hew_rc_new`] or [`hew_rc_clone`].
#[no_mangle]
pub unsafe extern "C" fn hew_rc_downgrade(ptr: *mut u8) -> *mut u8 {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees ptr is valid.
    let header = unsafe { header_from_data(ptr) };
    // SAFETY: header is valid.
    unsafe { (*header).weak += 1 };
    header.cast()
}

/// Attempt to upgrade a `Weak` reference back to a strong `Rc`.
/// Returns the data pointer if the value is still alive (strong > 0),
/// or null if the value has been dropped.
///
/// # Safety
///
/// `weak_ptr` must have been returned by [`hew_rc_downgrade`].
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "weak_ptr is a header pointer allocated with align_of::<HewRcInner>()"
)]
pub unsafe extern "C" fn hew_weak_upgrade_rc(weak_ptr: *mut u8) -> *mut u8 {
    if weak_ptr.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: weak_ptr was returned by hew_rc_downgrade which returns a header pointer
    // allocated with align_of::<HewRcInner>().
    let header = weak_ptr.cast::<HewRcInner>();
    // SAFETY: weak_ptr is a header pointer.
    let inner = unsafe { &mut *header };

    if inner.strong == 0 {
        return ptr::null_mut();
    }

    inner.strong += 1;
    // SAFETY: data pointer is immediately after header, within the allocation.
    unsafe { weak_ptr.add(size_of::<HewRcInner>()) }
}

/// Drop a `Weak` reference. Decrements the weak count. If both strong
/// and weak counts reach zero, frees the allocation.
///
/// # Panics
///
/// Panics if the layout computation overflows (should never happen since
/// the layout was valid at construction time).
///
/// # Safety
///
/// `weak_ptr` must have been returned by [`hew_rc_downgrade`].
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "weak_ptr is a header pointer allocated with align_of::<HewRcInner>()"
)]
pub unsafe extern "C" fn hew_weak_drop_rc(weak_ptr: *mut u8) {
    if weak_ptr.is_null() {
        return;
    }
    // SAFETY: weak_ptr was returned by hew_rc_downgrade which returns a header pointer
    // allocated with align_of::<HewRcInner>().
    let header = weak_ptr.cast::<HewRcInner>();
    // SAFETY: weak_ptr is a header pointer.
    let inner = unsafe { &mut *header };

    debug_assert!(inner.weak > 0, "Weak double-free");
    inner.weak -= 1;

    if inner.weak == 0 && inner.strong == 0 {
        // SAFETY: data_size was validated at construction time.
        let layout = alloc_layout(inner.data_size).expect("layout was valid at construction");
        // SAFETY: header was allocated with this layout.
        unsafe { dealloc(header.cast(), layout) };
    }
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rc_basic_lifecycle() {
        unsafe {
            let val: i32 = 42;
            let rc = hew_rc_new((&raw const val).cast(), size_of::<i32>(), None);
            assert!(!rc.is_null());

            // Read value through Rc
            let read_val = rc.cast::<i32>().read();
            assert_eq!(read_val, 42);
            assert_eq!(hew_rc_strong_count(rc), 1);

            // Clone
            let rc2 = hew_rc_clone(rc);
            assert_eq!(rc2, rc); // same pointer
            assert_eq!(hew_rc_strong_count(rc), 2);

            // Drop one
            hew_rc_drop(rc2);
            assert_eq!(hew_rc_strong_count(rc), 1);

            // Drop last — frees
            hew_rc_drop(rc);
        }
    }

    #[test]
    fn rc_with_drop_fn() {
        use std::sync::atomic::{AtomicI32, Ordering};
        static DROP_COUNT: AtomicI32 = AtomicI32::new(0);

        unsafe extern "C" fn drop_counter(_data: *mut u8) {
            DROP_COUNT.fetch_add(1, Ordering::SeqCst);
        }

        unsafe {
            DROP_COUNT.store(0, Ordering::SeqCst);
            let val: i32 = 99;
            let rc = hew_rc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                Some(drop_counter),
            );

            let rc2 = hew_rc_clone(rc);
            hew_rc_drop(rc2);
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 0); // not dropped yet

            hew_rc_drop(rc);
            assert_eq!(DROP_COUNT.load(Ordering::SeqCst), 1); // dropped!
        }
    }

    #[test]
    fn rc_weak_upgrade() {
        unsafe {
            let val: i32 = 77;
            let rc = hew_rc_new((&raw const val).cast(), size_of::<i32>(), None);

            // Downgrade to weak
            let weak = hew_rc_downgrade(rc);
            assert!(!weak.is_null());
            assert_eq!(hew_rc_weak_count(rc), 1);
            assert_eq!(hew_rc_strong_count(rc), 1);

            // Upgrade while strong > 0
            let upgraded = hew_weak_upgrade_rc(weak);
            assert!(!upgraded.is_null());
            assert_eq!(upgraded.cast::<i32>().read(), 77);
            assert_eq!(hew_rc_strong_count(rc), 2);

            // Drop all strong refs
            hew_rc_drop(rc);
            hew_rc_drop(upgraded);

            // Upgrade fails — strong == 0
            let failed = hew_weak_upgrade_rc(weak);
            assert!(failed.is_null());

            // Drop weak — frees allocation
            hew_weak_drop_rc(weak);
        }
    }
}
