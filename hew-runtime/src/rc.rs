//! Reference-counted smart pointer (`Rc<T>`).
//!
//! Non-atomic reference counting for single-actor use. NOT `Send` — cannot
//! cross actor boundaries. For cross-actor sharing, use `Arc<T>`.
//!
//! Layout: `[HewRcInner header | data bytes...]`
//! The returned pointer from `hew_rc_new` points to the **data region**
//! (immediately after the header). All functions accept/return data pointers.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

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
    data_align: usize,
    data_offset: usize,
}

/// Recover header pointer from data pointer.
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_rc_new`].
unsafe fn header_from_data(data_ptr: *mut u8) -> *mut HewRcInner {
    // SAFETY: hew_rc_new stores the offset in the usize immediately preceding
    // the returned data pointer.
    let offset = unsafe {
        data_ptr
            .sub(size_of::<usize>())
            .cast::<usize>()
            .read_unaligned()
    };
    // SAFETY: the stored offset points back to the start of the allocation.
    unsafe { data_ptr.sub(offset) }.cast()
}

/// Normalise the caller-supplied payload alignment into a valid `Layout`
/// alignment (a non-zero power of two).
///
/// Codegen knows the payload type's ABI alignment and threads it in via
/// `hew_rc_new`'s `align` argument. A value of `0` means "no explicit
/// alignment" (e.g. a zero-sized payload); we fall back to `1`, which is
/// the natural alignment of an empty allocation. A non-power-of-two value
/// would be a codegen bug, so we round up to the next power of two rather
/// than under-aligning the allocation.
fn normalise_align(align: usize) -> usize {
    if align <= 1 {
        1
    } else {
        align.next_power_of_two()
    }
}

/// Compute allocation layout for header + offset slot + aligned data.
/// Returns `None` on overflow.
fn alloc_layout(data_size: usize, data_align: usize) -> Option<(Layout, usize)> {
    let (prefix, _) = Layout::new::<HewRcInner>()
        .extend(Layout::new::<usize>())
        .ok()?;
    let data = Layout::from_size_align(data_size, data_align).ok()?;
    let (layout, data_offset) = prefix.extend(data).ok()?;
    Some((layout.pad_to_align(), data_offset))
}

// ── Public C ABI ───────────────────────────────────────────────────────

/// Create a new `Rc<T>`. Copies `size` bytes from `data` into a
/// heap-allocated block with a reference count header. Returns a pointer
/// to the data region (caller uses this as the Rc handle).
///
/// `align` is the payload type's ABI alignment, which codegen knows from
/// the LLVM layout and threads in explicitly. The allocation is aligned to
/// (at least) this value so that over-aligned payloads (SIMD vectors,
/// cache-line-padded structs) are not under-aligned. Pass `0` only for a
/// zero-sized / null payload.
///
/// Returns null if the layout computation overflows.
///
/// # Safety
///
/// - `data` must be valid for `size` bytes (may be null if `size == 0`).
/// - `align` must be the payload's true ABI alignment (or `0` when there
///   is no payload). A value smaller than the payload's real alignment
///   produces an under-aligned allocation.
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
    align: usize,
    drop_fn: Option<unsafe extern "C" fn(*mut u8)>,
) -> *mut u8 {
    let data_align = normalise_align(align);
    let Some((layout, data_offset)) = alloc_layout(size, data_align) else {
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
            data_align,
            data_offset,
        });
    }

    // SAFETY: the offset slot lies within the allocated prefix and records how
    // to recover the header from the returned data pointer.
    unsafe {
        ptr.add(data_offset - size_of::<usize>())
            .cast::<usize>()
            .write_unaligned(data_offset);
    }

    // SAFETY: data_offset was computed by Layout::extend for this allocation.
    let data_ptr = unsafe { ptr.add(data_offset) };
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

    // Decrement the strong count and snapshot every field the post-drop path
    // needs (`drop_fn`, `weak`, `data_size`, `data_align`) into locals while
    // the `&mut` borrow is alive, then END the borrow before calling
    // `drop_fn`. `drop_fn` is an opaque caller-supplied callback that may
    // reach this same allocation; holding a live `&mut *header` across that
    // call is aliasing UB (FND-01). After this block no reference into the
    // allocation is live — only the raw `header` pointer and the locals.
    let (strong_now, drop_fn, weak, data_size, data_align) = {
        // SAFETY: header is valid; the borrow is confined to this block and
        // dropped before any opaque call or deallocation below.
        let inner = unsafe { &mut *header };
        // Refcount invariant: strong must be > 0 before decrement. In release
        // mode a zero-or-underflowed strong count indicates a double-free or
        // use-after-free — both are memory-safety violations that must not
        // pass silently. `assert!` aborts in release; `debug_assert!` would
        // let the decrement wrap and corrupt the header in production builds.
        assert!(
            inner.strong > 0,
            "Rc double-free: strong refcount is already 0"
        );
        inner.strong -= 1;
        (
            inner.strong,
            inner.drop_fn,
            inner.weak,
            inner.data_size,
            inner.data_align,
        )
    };

    if strong_now == 0 {
        // Call destructor on the data. No reference into the allocation is
        // live here, so `drop_fn` may freely reach back into it.
        if let Some(drop_fn) = drop_fn {
            // SAFETY: drop_fn contract per hew_rc_new.
            unsafe { drop_fn(ptr) };
        }

        if weak == 0 {
            // SAFETY: data_size was validated at construction time.
            let (layout, _) =
                alloc_layout(data_size, data_align).expect("layout was valid at construction");
            // SAFETY: header was allocated with this layout; no borrow of the
            // allocation is live (the `&mut *header` ended above).
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
    unsafe { weak_ptr.add(inner.data_offset) }
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

    // Same rationale as the strong-count assert in hew_rc_drop: a zero weak
    // count before decrement is a double-free and must abort in release.
    assert!(
        inner.weak > 0,
        "Weak<Rc> double-free: weak refcount is already 0"
    );
    inner.weak -= 1;

    if inner.weak == 0 && inner.strong == 0 {
        // SAFETY: data_size was validated at construction time.
        let (layout, _) = alloc_layout(inner.data_size, inner.data_align)
            .expect("layout was valid at construction");
        // SAFETY: header was allocated with this layout.
        unsafe { dealloc(header.cast(), layout) };
    }
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "test data is allocated with proper alignment by hew_rc_new"
)]
mod tests {
    use super::*;

    #[test]
    fn rc_basic_lifecycle() {
        // SAFETY: Test exercises the Rc FFI lifecycle with valid pointers.
        unsafe {
            let val: i32 = 42;
            let rc = hew_rc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                None,
            );
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

        // SAFETY: Test exercises Rc FFI with a custom drop function.
        unsafe {
            DROP_COUNT.store(0, Ordering::SeqCst);
            let val: i32 = 99;
            let rc = hew_rc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
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
    fn rc_drop_fn_may_reach_allocation() {
        // FND-01 regression: the destructor is an opaque callback that may
        // read the very allocation being dropped. `hew_rc_drop` must NOT hold
        // a live `&mut *header` across `drop_fn(ptr)` — doing so is aliasing
        // UB that Miri (Stacked/Tree Borrows) flags. This drop_fn reads its
        // own data region AND reaches the header via the public FFI, so a live
        // mutable borrow across the call would be detected as a conflict.
        use std::sync::atomic::{AtomicI32, Ordering};
        static OBSERVED: AtomicI32 = AtomicI32::new(0);

        unsafe extern "C" fn drop_reads_self(data: *mut u8) {
            // SAFETY: `data` is this Rc's data region; it is still valid while
            // the destructor runs (dealloc happens strictly after drop_fn).
            let v = unsafe { data.cast::<i32>().read() };
            OBSERVED.store(v, Ordering::SeqCst);
            // Reach the header from the same allocation and WRITE to it. If
            // `hew_rc_drop` still held a live `&mut *header` across this call,
            // this aliasing write pops the Unique tag (Stacked Borrows) /
            // disables the mutable borrow (Tree Borrows), so the drop's own
            // post-call read of `weak`/`data_size` would be flagged as UB by
            // Miri. This is the exact window FND-01 closes.
            // SAFETY: `data` is a valid Rc data pointer for this allocation.
            let header = unsafe { header_from_data(data) };
            // SAFETY: header is valid; strong is 0 here (decremented before the
            // call), so the allocation is still live and writable. Bump weak up
            // and back so the value is preserved but the WRITE actually occurs.
            unsafe {
                (*header).weak += 1;
                (*header).weak -= 1;
            }
        }

        // SAFETY: exercises Rc FFI with a destructor that reads the allocation.
        unsafe {
            OBSERVED.store(0, Ordering::SeqCst);
            let val: i32 = 1234;
            let rc = hew_rc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                Some(drop_reads_self),
            );
            // The destructor stores the payload value (1234) into OBSERVED.
            hew_rc_drop(rc);
            assert_eq!(OBSERVED.load(Ordering::SeqCst), 1234);
        }
    }

    #[test]
    fn rc_weak_upgrade() {
        // SAFETY: Test exercises weak reference upgrade with valid Rc pointers.
        unsafe {
            let val: i32 = 77;
            let rc = hew_rc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                None,
            );

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

    #[test]
    fn rc_honours_overaligned_data_align() {
        // A 64-byte cache-line-aligned payload. Its true alignment (64)
        // exceeds align_of::<u128>() (16) — the bound the old trailing-zeros
        // heuristic capped at — and is NOT recoverable from the byte size.
        // The authoritative alignment must come from the caller.
        #[repr(align(64))]
        struct Over {
            _x: [u8; 64],
        }
        assert!(align_of::<Over>() > size_of::<u128>());

        // SAFETY: hew_rc_new copies from a valid Over pointer and returns an
        // Rc allocation that is dropped before the test exits.
        unsafe {
            let value = Over { _x: [1; 64] };
            let rc = hew_rc_new(
                (&raw const value).cast(),
                size_of::<Over>(),
                align_of::<Over>(),
                None,
            );
            assert!(!rc.is_null());
            let header = header_from_data(rc);
            // The header records the caller's authoritative alignment, and the
            // returned data pointer actually satisfies it. Under the old
            // heuristic data_align would have been capped at 16 and this
            // allocation could land on a 16-aligned (not 64-aligned) address.
            assert_eq!((*header).data_align, align_of::<Over>());
            assert_eq!(rc as usize % align_of::<Over>(), 0);
            hew_rc_drop(rc);
        }
    }

    #[test]
    fn rc_normalises_zero_and_non_power_of_two_align() {
        assert_eq!(normalise_align(0), 1);
        assert_eq!(normalise_align(1), 1);
        assert_eq!(normalise_align(8), 8);
        assert_eq!(normalise_align(3), 4);
        assert_eq!(normalise_align(48), 64);
    }
}
