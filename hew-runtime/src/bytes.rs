//! Hew runtime: `bytes` module.
//!
//! Reference-counted byte buffer with copy-on-write semantics.
//!
//! ## Heap layout
//!
//! ```text
//! [refcount:u32(atomic) | capacity:u32 | data[0..cap]]
//!  \___________ HEADER (8 bytes) ___________/
//! ```
//!
//! The pointer returned by [`hew_bytes_new`] points to `data[0]`;
//! the header lives at `ptr - HEADER_SIZE`.
//!
//! ## Value representation
//!
//! A `Bytes` value in Hew is represented as a [`BytesTriple`]: a data pointer,
//! an offset into the buffer, and a length. Multiple triples can share the same
//! underlying allocation (slicing is O(1)). Mutations (`push`, `append`) use
//! copy-on-write: if the refcount is > 1 the active region is copied to a fresh
//! buffer before mutating.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::sync::atomic::{AtomicU32, Ordering};

/// Size of the header preceding the data region, in bytes.
const HEADER_SIZE: usize = 8;

/// Minimum capacity for new or grown buffers.
const MIN_CAPACITY: u32 = 16;

// ---------------------------------------------------------------------------
// BytesTriple — the C-ABI value type
// ---------------------------------------------------------------------------

/// Fat representation of a `Bytes` value at the C ABI boundary.
///
/// - `ptr`    — pointer to `data[0]` of the heap allocation (or null for empty).
/// - `offset` — byte offset into the buffer where the active region starts.
/// - `len`    — number of active bytes starting from `offset`.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct BytesTriple {
    pub ptr: *mut u8,
    pub offset: u32,
    pub len: u32,
}

// ---------------------------------------------------------------------------
// Header accessors
// ---------------------------------------------------------------------------

/// Read the atomic refcount from the header preceding `data_ptr`.
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_bytes_new`] (non-null).
#[inline]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "header is 8-byte aligned from malloc, AtomicU32 needs 4"
)]
unsafe fn refcount(data_ptr: *mut u8) -> &'static AtomicU32 {
    // SAFETY: The header is at data_ptr - HEADER_SIZE. The first 4 bytes are
    // the AtomicU32 refcount. Caller guarantees data_ptr is valid.
    unsafe { &*data_ptr.sub(HEADER_SIZE).cast::<AtomicU32>() }
}

/// Read the capacity (u32) stored in the header preceding `data_ptr`.
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_bytes_new`] (non-null).
#[inline]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "header is 8-byte aligned from malloc, u32 needs 4"
)]
unsafe fn capacity(data_ptr: *mut u8) -> u32 {
    // SAFETY: Capacity is at offset 4 within the header (data_ptr - 4).
    // Caller guarantees data_ptr is valid.
    unsafe { data_ptr.sub(4).cast::<u32>().read() }
}

/// Write the capacity (u32) into the header preceding `data_ptr`.
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_bytes_new`] (non-null).
#[inline]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "header is 8-byte aligned from malloc, u32 needs 4"
)]
unsafe fn set_capacity(data_ptr: *mut u8, cap: u32) {
    // SAFETY: Capacity field is at data_ptr - 4. Caller guarantees data_ptr is
    // a valid bytes allocation.
    unsafe { data_ptr.sub(4).cast::<u32>().write(cap) };
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Allocate a new buffer with the given capacity. Returns a pointer to `data[0]`.
/// The refcount is initialised to 1.
///
/// # Safety
///
/// `cap` must be > 0.
#[expect(
    clippy::cast_ptr_alignment,
    reason = "header is 8-byte aligned from malloc, u32 needs 4"
)]
unsafe fn alloc_buf(cap: u32) -> *mut u8 {
    let alloc_size = HEADER_SIZE + cap as usize;
    // SAFETY: alloc_size > 0 (cap > 0 plus header).
    let base = unsafe { libc::malloc(alloc_size) }.cast::<u8>();
    if base.is_null() {
        // SAFETY: abort is always safe.
        unsafe { libc::abort() };
    }
    // Write refcount = 1
    // SAFETY: base is freshly allocated with at least HEADER_SIZE bytes.
    unsafe { base.cast::<u32>().write(1) };
    // Write capacity
    // SAFETY: base + 4 is within the allocation.
    unsafe { base.add(4).cast::<u32>().write(cap) };
    // Return pointer to data region
    // SAFETY: base + HEADER_SIZE is within the allocation.
    unsafe { base.add(HEADER_SIZE) }
}

/// Copy-on-write: if the buffer at `ptr` has refcount > 1, allocate a new
/// buffer containing only the active region `[offset..offset+len]`, decrement
/// the old refcount, and return the new data pointer (with offset reset to 0).
///
/// If the buffer is uniquely owned (refcount == 1), returns `ptr` unchanged.
///
/// # Safety
///
/// `ptr` must be a valid bytes data pointer (non-null).
unsafe fn ensure_unique(ptr: *mut u8, offset: u32, len: u32) -> *mut u8 {
    // SAFETY: Caller guarantees ptr is valid.
    let rc = unsafe { refcount(ptr) };
    if rc.load(Ordering::Acquire) == 1 {
        return ptr;
    }

    // Shared — need to clone the active region.
    let new_cap = if len < MIN_CAPACITY {
        MIN_CAPACITY
    } else {
        len
    };
    // SAFETY: new_cap > 0.
    let new_ptr = unsafe { alloc_buf(new_cap) };

    if len > 0 {
        // SAFETY: ptr + offset is valid for len bytes; new_ptr is freshly
        // allocated with at least new_cap >= len bytes.
        unsafe {
            std::ptr::copy_nonoverlapping(ptr.add(offset as usize), new_ptr, len as usize);
        }
    }

    // Drop old ref.
    // SAFETY: ptr is valid per caller contract.
    unsafe { hew_bytes_drop(ptr) };

    new_ptr
}

/// Compute the grown capacity: double the current, but at least `min_needed`,
/// and at least `MIN_CAPACITY`.
fn grow_capacity(current_cap: u32, min_needed: u32) -> u32 {
    let doubled = current_cap.saturating_mul(2);
    doubled.max(min_needed).max(MIN_CAPACITY)
}

/// Reallocate a uniquely-owned buffer to a new capacity, preserving `used`
/// bytes of data from the start. Returns the new data pointer.
///
/// # Safety
///
/// - `ptr` must be a valid bytes data pointer with refcount == 1.
/// - `used` must be <= current capacity.
/// - `new_cap` must be >= `used`.
unsafe fn realloc_buf(ptr: *mut u8, _used: u32, new_cap: u32) -> *mut u8 {
    // SAFETY: ptr - HEADER_SIZE is the base of the allocation.
    let base = unsafe { ptr.sub(HEADER_SIZE) };
    let alloc_size = HEADER_SIZE + new_cap as usize;
    // SAFETY: base was allocated by alloc_buf (via libc::malloc). alloc_size > 0.
    let new_base = unsafe { libc::realloc(base.cast(), alloc_size) }.cast::<u8>();
    if new_base.is_null() {
        // SAFETY: abort is always safe.
        unsafe { libc::abort() };
    }
    // SAFETY: new_base is valid for at least HEADER_SIZE + new_cap bytes.
    let new_data = unsafe { new_base.add(HEADER_SIZE) };
    // SAFETY: new_base is valid for at least HEADER_SIZE + new_cap bytes.
    unsafe { set_capacity(new_data, new_cap) };
    new_data
}

// ---------------------------------------------------------------------------
// Public C ABI
// ---------------------------------------------------------------------------

/// Allocate a new byte buffer with the given capacity. The refcount is set to 1.
/// Returns a pointer to `data[0]`.
///
/// If `capacity` is 0, a minimum capacity of [`MIN_CAPACITY`] is used.
///
/// # Safety
///
/// Caller must eventually call [`hew_bytes_drop`] to free the allocation.
#[no_mangle]
pub extern "C" fn hew_bytes_new(capacity: u32) -> *mut u8 {
    let cap = if capacity == 0 {
        MIN_CAPACITY
    } else {
        capacity
    };
    // SAFETY: cap > 0.
    unsafe { alloc_buf(cap) }
}

/// Refcount saturation threshold, mirroring `std::sync::Arc`'s `MAX_REFCOUNT`
/// discipline and `hew-cabi`'s `CSTRING_RC_MAX`. A retain that observes an old
/// count above this is one step from wrapping the `u32`; a later drop would
/// then free a still-aliased buffer (use-after-free), so we abort first.
const BYTES_RC_MAX: u32 = u32::MAX / 2;

/// Pure predicate: would retaining a buffer whose current (pre-increment)
/// refcount is `old` push the count past [`BYTES_RC_MAX`]? Extracted so the
/// abort condition is unit-testable without `2^31` real retains. Mirrors
/// `hew-cabi`'s `cstring_rc_would_overflow`.
#[inline]
#[must_use]
fn bytes_rc_would_overflow(old: u32) -> bool {
    old > BYTES_RC_MAX
}

/// Atomically increment the refcount of a byte buffer. No-op if `data_ptr` is null.
///
/// Overflow is fail-closed: if the pre-increment count is already past
/// [`BYTES_RC_MAX`] the refcount is about to wrap, so we abort rather than let
/// a future drop free a buffer that still has live aliases (the
/// `std::sync::Arc` `MAX_REFCOUNT` discipline).
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_bytes_new`] or be null.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_clone_ref(data_ptr: *mut u8) {
    if data_ptr.is_null() {
        return;
    }
    // SAFETY: Caller guarantees data_ptr is a valid bytes allocation.
    let rc = unsafe { refcount(data_ptr) };
    let old = rc.fetch_add(1, Ordering::Relaxed);
    if bytes_rc_would_overflow(old) {
        eprintln!(
            "hew-runtime: hew_bytes_clone_ref refcount overflow (old={old} > {BYTES_RC_MAX}); \
             aborting to avoid a use-after-free when the count wraps."
        );
        // SAFETY: abort is always safe; it does not return.
        unsafe { libc::abort() };
    }
}

/// Atomically decrement the refcount. If it reaches zero, free the allocation.
/// No-op if `data_ptr` is null.
///
/// # Safety
///
/// `data_ptr` must have been returned by [`hew_bytes_new`] or be null.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_drop(data_ptr: *mut u8) {
    if data_ptr.is_null() {
        return;
    }
    // SAFETY: Caller guarantees data_ptr is a valid bytes allocation.
    let rc = unsafe { refcount(data_ptr) };
    if rc.fetch_sub(1, Ordering::Release) == 1 {
        // Acquire fence before deallocation — same pattern as std::sync::Arc.
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: Refcount reached zero; we have exclusive access.
        let base = unsafe { data_ptr.sub(HEADER_SIZE) };
        // SAFETY: base was allocated by libc::malloc in alloc_buf.
        unsafe { libc::free(base.cast()) };
    }
}

/// Push a single byte onto the buffer, using copy-on-write if shared.
///
/// # Safety
///
/// `triple` must point to a valid `BytesTriple`. `triple.ptr` may be null (empty
/// bytes), in which case a new buffer is allocated.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_push(triple: &mut BytesTriple, byte: u8) {
    if triple.ptr.is_null() {
        // Allocate a fresh buffer.
        // SAFETY: MIN_CAPACITY > 0.
        let ptr = unsafe { alloc_buf(MIN_CAPACITY) };
        // SAFETY: ptr is freshly allocated with MIN_CAPACITY bytes.
        unsafe { *ptr = byte };
        triple.ptr = ptr;
        triple.offset = 0;
        triple.len = 1;
        return;
    }

    // Ensure unique ownership (CoW).
    // SAFETY: triple.ptr is non-null and valid per caller contract.
    let ptr = unsafe { ensure_unique(triple.ptr, triple.offset, triple.len) };
    if ptr != triple.ptr {
        // CoW happened — offset is now 0.
        triple.ptr = ptr;
        triple.offset = 0;
    }

    let end = triple.offset + triple.len;
    // SAFETY: ptr is valid per ensure_unique.
    let cap = unsafe { capacity(ptr) };

    if end >= cap {
        // Need to grow.
        let needed = end + 1;
        let new_cap = grow_capacity(cap, needed);
        // If offset > 0, compact first by moving data to start.
        if triple.offset > 0 {
            // SAFETY: ptr is uniquely owned; src and dst may overlap.
            unsafe {
                std::ptr::copy(ptr.add(triple.offset as usize), ptr, triple.len as usize);
            }
            triple.offset = 0;
        }
        // SAFETY: ptr is uniquely owned, triple.len <= cap, new_cap >= triple.len + 1.
        let ptr = unsafe { realloc_buf(ptr, triple.len, new_cap) };
        triple.ptr = ptr;
        // SAFETY: ptr has capacity new_cap >= triple.len + 1.
        unsafe { *ptr.add(triple.len as usize) = byte };
        triple.len += 1;
    } else {
        // Room available.
        // SAFETY: end < cap, so ptr + end is within the allocation.
        unsafe { *triple.ptr.add(end as usize) = byte };
        triple.len += 1;
    }
}

/// Append `src_len` bytes from `src_ptr + src_offset` onto the destination buffer.
/// Uses copy-on-write if the destination is shared.
///
/// # Safety
///
/// - `dst` must point to a valid `BytesTriple` (ptr may be null for empty).
/// - If `src_len > 0`, `src_ptr + src_offset` must be valid for `src_len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_append(
    dst: &mut BytesTriple,
    src_ptr: *const u8,
    src_offset: u32,
    src_len: u32,
) {
    if src_len == 0 {
        return;
    }

    if dst.ptr.is_null() {
        // Allocate fresh.
        let cap = if src_len < MIN_CAPACITY {
            MIN_CAPACITY
        } else {
            src_len
        };
        // SAFETY: cap > 0.
        let ptr = unsafe { alloc_buf(cap) };
        // SAFETY: src_ptr + src_offset is valid for src_len bytes; ptr is freshly
        // allocated with cap >= src_len bytes.
        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr.add(src_offset as usize), ptr, src_len as usize);
        }
        dst.ptr = ptr;
        dst.offset = 0;
        dst.len = src_len;
        return;
    }

    // CoW.
    // SAFETY: dst.ptr is non-null and valid.
    let ptr = unsafe { ensure_unique(dst.ptr, dst.offset, dst.len) };
    if ptr != dst.ptr {
        dst.ptr = ptr;
        dst.offset = 0;
    }

    let end = dst.offset + dst.len;
    let needed = end + src_len;
    // SAFETY: ptr is valid.
    let cap = unsafe { capacity(ptr) };

    let ptr = if needed > cap {
        let new_cap = grow_capacity(cap, needed);
        // Compact if offset > 0.
        if dst.offset > 0 {
            // SAFETY: ptr is uniquely owned; regions may overlap.
            unsafe {
                std::ptr::copy(ptr.add(dst.offset as usize), ptr, dst.len as usize);
            }
            dst.offset = 0;
        }
        // SAFETY: ptr is uniquely owned, dst.len <= cap, new_cap >= needed.
        let ptr = unsafe { realloc_buf(ptr, dst.len, new_cap) };
        dst.ptr = ptr;
        ptr
    } else {
        ptr
    };

    let write_offset = (dst.offset + dst.len) as usize;
    // SAFETY: write_offset + src_len <= capacity (ensured above).
    unsafe {
        std::ptr::copy_nonoverlapping(
            src_ptr.add(src_offset as usize),
            ptr.add(write_offset),
            src_len as usize,
        );
    }
    dst.len += src_len;
}

/// Concatenate two byte regions into a fresh allocation. Returns a new
/// `BytesTriple` with offset 0.
///
/// # Safety
///
/// If `a_len > 0`, `a_ptr + a_offset` must be valid for `a_len` bytes.
/// If `b_len > 0`, `b_ptr + b_offset` must be valid for `b_len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_concat(
    a_ptr: *const u8,
    a_offset: u32,
    a_len: u32,
    b_ptr: *const u8,
    b_offset: u32,
    b_len: u32,
) -> BytesTriple {
    let total = a_len.saturating_add(b_len);
    if total == 0 {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }

    let cap = if total < MIN_CAPACITY {
        MIN_CAPACITY
    } else {
        total
    };
    // SAFETY: cap > 0.
    let ptr = unsafe { alloc_buf(cap) };

    if a_len > 0 {
        // SAFETY: a_ptr + a_offset valid for a_len bytes; ptr is fresh.
        unsafe {
            std::ptr::copy_nonoverlapping(a_ptr.add(a_offset as usize), ptr, a_len as usize);
        }
    }
    if b_len > 0 {
        // SAFETY: b_ptr + b_offset valid for b_len bytes; ptr + a_len is within cap.
        unsafe {
            std::ptr::copy_nonoverlapping(
                b_ptr.add(b_offset as usize),
                ptr.add(a_len as usize),
                b_len as usize,
            );
        }
    }

    BytesTriple {
        ptr,
        offset: 0,
        len: total,
    }
}

/// Out-pointer variant of [`hew_bytes_concat`] for Windows x64 MSVC sret fix.
///
/// # Safety
///
/// Same as [`hew_bytes_concat`]. `out` must point to a valid, writable `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_concat_raw(
    a_ptr: *const u8,
    a_offset: u32,
    a_len: u32,
    b_ptr: *const u8,
    b_offset: u32,
    b_len: u32,
    out: *mut BytesTriple,
) {
    // SAFETY: preconditions forwarded from caller contract above.
    let triple = unsafe { hew_bytes_concat(a_ptr, a_offset, a_len, b_ptr, b_offset, b_len) };
    // SAFETY: caller guarantees `out` is a valid BytesTriple slot.
    unsafe { out.write(triple) };
}

/// Create a `BytesTriple` by copying `len` bytes from a static (or stack) pointer.
///
/// # Safety
///
/// `data` must be valid for `len` bytes (or null if `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_from_static(data: *const u8, len: u32) -> BytesTriple {
    if len == 0 || data.is_null() {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }

    let cap = if len < MIN_CAPACITY {
        MIN_CAPACITY
    } else {
        len
    };
    // SAFETY: cap > 0.
    let ptr = unsafe { alloc_buf(cap) };
    // SAFETY: data is valid for len bytes; ptr is freshly allocated with cap >= len.
    unsafe { std::ptr::copy_nonoverlapping(data, ptr, len as usize) };

    BytesTriple {
        ptr,
        offset: 0,
        len,
    }
}

/// Out-pointer variant of [`hew_bytes_from_static`] for Windows x64 MSVC sret fix.
///
/// # Safety
///
/// Same as [`hew_bytes_from_static`]. `out` must point to a valid, writable `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_from_static_raw(
    data: *const u8,
    len: u32,
    out: *mut BytesTriple,
) {
    // SAFETY: preconditions forwarded from caller contract above.
    let triple = unsafe { hew_bytes_from_static(data, len) };
    // SAFETY: caller guarantees `out` is a valid BytesTriple slot.
    unsafe { out.write(triple) };
}

/// Compare two byte regions for equality.
///
/// # Safety
///
/// If `a_len > 0`, `a_ptr + a_off` must be valid for `a_len` bytes.
/// If `b_len > 0`, `b_ptr + b_off` must be valid for `b_len` bytes.
/// Null pointers with len == 0 are valid (empty bytes).
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_eq(
    a_ptr: *const u8,
    a_off: u32,
    a_len: u32,
    b_ptr: *const u8,
    b_off: u32,
    b_len: u32,
) -> bool {
    if a_len != b_len {
        return false;
    }
    if a_len == 0 {
        return true;
    }
    // SAFETY: a_ptr + a_off is valid for a_len bytes per caller contract.
    let a_slice = unsafe { std::slice::from_raw_parts(a_ptr.add(a_off as usize), a_len as usize) };
    // SAFETY: b_ptr + b_off is valid for b_len bytes per caller contract.
    let b_slice = unsafe { std::slice::from_raw_parts(b_ptr.add(b_off as usize), b_len as usize) };
    a_slice == b_slice
}

/// Convert a `Bytes` value to a NUL-terminated UTF-8 C string (lossy).
///
/// This is the canonical `Bytes -> String` runtime conversion. It takes a
/// POINTER to the caller's `BytesTriple` (the address of the `bytes` value's
/// stack slot). By-pointer (not by-value): a `{ptr,i32,i32}` passed by value is
/// not reliably ABI-portable at the LLVM↔Rust C boundary (LLVM's three-register
/// small-struct classification vs Rust's repr(C) two-register pair), so codegen
/// passes the triple's address (`is_bytes_by_pointer_consumer`).
///
/// Invalid UTF-8 sequences are replaced with U+FFFD. The returned pointer is
/// allocated via `libc::malloc`; the caller (typically the Hew string GC)
/// must `libc::free` it.
///
/// # Safety
///
/// `triple` must point to a valid `BytesTriple`. If its `len > 0`,
/// `ptr + offset` must be valid for `len` bytes. A null `ptr` (or a null
/// `triple` pointer) is treated as the empty byte region.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_to_string(triple: *const BytesTriple) -> *mut std::ffi::c_char {
    let triple = if triple.is_null() {
        BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        }
    } else {
        // SAFETY: `triple` is non-null and points to the caller's valid
        // BytesTriple slot per the fn contract; BytesTriple is Copy.
        unsafe { *triple }
    };
    if triple.len == 0 || triple.ptr.is_null() {
        // Return an empty NUL-terminated, header-aware string.
        let out = crate::cabi::alloc_cstring_from_str(""); // CSTRING-ALLOC: str-open (hew_bytes_to_string empty path — header-aware String result; reaches hew_string_drop)
        if out.is_null() {
            // SAFETY: abort is always safe.
            unsafe { libc::abort() };
        }
        return out;
    }

    // SAFETY: ptr + offset is valid for len bytes per caller contract.
    let data = unsafe {
        std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
    };
    let s = String::from_utf8_lossy(data);
    // Header-aware (S1): the result reaches hew_string_drop / free_cstring.
    let out = crate::cabi::alloc_cstring_from_str(&s); // CSTRING-ALLOC: str-open (hew_bytes_to_string — header-aware String result; reaches hew_string_drop)
    if out.is_null() {
        // SAFETY: abort is always safe.
        unsafe { libc::abort() };
    }
    out
}

/// Create a `BytesTriple` from a NUL-terminated C string.
///
/// The bytes of the string (excluding the NUL terminator) are copied into a
/// new allocation.
///
/// # Safety
///
/// `str_ptr` must be a valid NUL-terminated C string or null.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_from_str(str_ptr: *const u8) -> BytesTriple {
    if str_ptr.is_null() {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }

    // SAFETY: str_ptr is a valid NUL-terminated C string per caller contract.
    let len = unsafe { libc::strlen(str_ptr.cast()) };

    #[expect(
        clippy::cast_possible_truncation,
        reason = "String lengths in Hew are bounded by u32::MAX"
    )]
    let len32 = len as u32;

    if len32 == 0 {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }

    let cap = if len32 < MIN_CAPACITY {
        MIN_CAPACITY
    } else {
        len32
    };
    // SAFETY: cap > 0.
    let ptr = unsafe { alloc_buf(cap) };
    // SAFETY: str_ptr is valid for len bytes; ptr is freshly allocated with cap >= len32.
    unsafe { std::ptr::copy_nonoverlapping(str_ptr, ptr, len) };

    BytesTriple {
        ptr,
        offset: 0,
        len: len32,
    }
}

/// Out-pointer variant of [`hew_bytes_from_str`] for Windows x64 MSVC sret fix.
///
/// # Safety
///
/// Same as [`hew_bytes_from_str`]. `out` must point to a valid, writable `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_from_str_raw(str_ptr: *const u8, out: *mut BytesTriple) {
    // SAFETY: preconditions forwarded from caller contract above.
    let triple = unsafe { hew_bytes_from_str(str_ptr) };
    // SAFETY: caller guarantees `out` is a valid BytesTriple slot.
    unsafe { out.write(triple) };
}

/// Return the active length of a `bytes` value.
///
/// This is the canonical `bytes.len()` runtime entry. It takes a POINTER to the
/// caller's `BytesTriple` (the address of the `bytes` value's stack slot) and
/// reads the `len` field. By-pointer (not by-value): a `{ptr,i32,i32}` passed by
/// value is not reliably ABI-portable at the LLVM↔Rust C boundary (LLVM's
/// three-register small-struct classification vs Rust's repr(C) two-register
/// pair), so codegen passes the triple's address (`is_bytes_by_pointer_consumer`).
///
/// # Safety
///
/// `triple` must point to a valid `BytesTriple` (either its `ptr` null with
/// `len == 0`, or `ptr` pointing to a `hew_bytes_*` allocation). The buffer
/// itself is not dereferenced — only the `len` field is read.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_len(triple: *const BytesTriple) -> i64 {
    if triple.is_null() {
        return 0;
    }
    // SAFETY: `triple` points to the caller's valid BytesTriple slot.
    i64::from(unsafe { (*triple).len })
}

// W4.039 — the wasm-gated Vec-backed `hew_bytes_to_string` was deleted
// when `hew_bytes_to_string` was canonicalised onto the `BytesTriple`
// ABI (single `#[repr(C)] BytesTriple` argument). The canonical
// definition above is target-agnostic; no wasm-specific override is
// needed.

// ---------------------------------------------------------------------------
// W3 collections-sugar S2 — fail-closed byte indexing / slicing
// ---------------------------------------------------------------------------
//
// These two runtime entries back the compiler-emitted `b[i]` and `b[a..b]`
// sugar (Q-CS2 locked semantics):
//
// - `b[i]`    -> `u8` at byte offset, O(1), abort on OOB.
// - `b[a..b]` -> `bytes` slice (refcount bump on shared buffer), O(1),
//   abort on invalid bounds.
//
// LESSONS: boundary-fail-closed (P0) — no sentinel byte, no empty-slice
// clamp. Codegen will pass the receiver as a `BytesTriple` (ptr/offset/len)
// and the integer endpoints as i64.

/// Abort with a bytes-indexing panic message (OOB / invalid bounds).
///
/// # Safety
///
/// Always aborts — safe to call from any context.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_abort_index_oob() -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: bytes index/slice out of bounds\n";
        #[cfg(not(target_os = "windows"))]
        libc::write(2, msg.as_ptr().cast(), msg.len());
        #[cfg(target_os = "windows")]
        libc::write(2, msg.as_ptr().cast(), msg.len() as core::ffi::c_uint);
        libc::abort();
    }
}

/// Abort with a bytes offset-arithmetic-overflow panic message.
///
/// Used by `hew_bytes_index` / `hew_bytes_slice` when `offset + N`
/// would wrap past `u32::MAX`. A wrapped offset would silently shift
/// the active region pointer (`offset` -> `ptr + offset_usize`) to a
/// wrong place inside the underlying allocation — a memory-safety
/// hazard. Fail closed (LESSONS row P0:49 boundary-fail-closed).
///
/// # Safety
///
/// Always aborts — safe to call from any context.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_abort_offset_overflow() -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: bytes slice/index offset arithmetic overflow (u32)\n";
        #[cfg(not(target_os = "windows"))]
        libc::write(2, msg.as_ptr().cast(), msg.len());
        #[cfg(target_os = "windows")]
        libc::write(2, msg.as_ptr().cast(), msg.len() as core::ffi::c_uint);
        libc::abort();
    }
}

/// Return the byte at byte offset `index` in the bytes value
/// `(ptr, offset, len)`.
///
/// Semantics (Q-CS2):
/// - O(1) load.
/// - Aborts if `index < 0` or `index >= len`. No sentinel return value.
/// - A null `ptr` is only valid when `len == 0`, in which case any index
///   is OOB and aborts.
///
/// # Safety
///
/// `(ptr, offset, len)` must be a valid `BytesTriple` representation: either
/// `ptr` is null and `len == 0`, or `ptr` points to a `hew_bytes_*`
/// allocation whose active region `[offset, offset+len)` is in bounds.
#[expect(
    clippy::cast_sign_loss,
    reason = "index is bounds-checked >= 0 before cast to u32"
)]
#[expect(
    clippy::cast_possible_truncation,
    reason = "index is bounds-checked < len (u32) before cast"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_index(ptr: *mut u8, offset: u32, len: u32, index: i64) -> u8 {
    if index < 0 || index >= i64::from(len) || ptr.is_null() {
        // SAFETY: abort is always safe.
        unsafe { hew_bytes_abort_index_oob() };
    }
    let idx = index as u32;
    // Checked `offset + idx`: a triple constructed via `hew_bytes_slice`
    // on a buffer near `u32::MAX` capacity (or a malformed externally-
    // supplied triple) could overflow here. In `--release` Rust does
    // NOT panic on `+` overflow — it wraps — which would silently shift
    // the read pointer to a wrong place in the allocation. Abort
    // explicitly (boundary-fail-closed, LESSONS P0:49).
    let Some(byte_off) = offset.checked_add(idx) else {
        // SAFETY: abort is always safe.
        unsafe { hew_bytes_abort_offset_overflow() };
    };
    // SAFETY: ptr is non-null and byte_off < offset+len <= allocation
    // capacity per caller contract; the checked_add above proved the
    // u32 add did not wrap.
    let read_at = unsafe { ptr.add(byte_off as usize) };
    // SAFETY: read_at is within the buffer per the bounds check above.
    unsafe { *read_at }
}

/// Slice the bytes value `(ptr, offset, len)` by byte range `[start, end)`.
///
/// Semantics (Q-CS2):
/// - O(1): no copy. The returned `BytesTriple` shares the same allocation,
///   with `offset` advanced by `start` and `len` set to `end - start`.
/// - Increments the underlying refcount when the result is non-empty so the
///   shared allocation survives independently of the input handle (drop
///   safety: caller of the slice releases their reference via
///   `hew_bytes_drop` on `result.ptr`).
/// - Aborts if `start < 0`, `end < 0`, `start > end`, or `end > len`.
///
/// # Safety
///
/// `(ptr, offset, len)` must be a valid `BytesTriple` representation as for
/// [`hew_bytes_index`].
#[expect(
    clippy::cast_sign_loss,
    reason = "start and end are bounds-checked >= 0 before cast to u32"
)]
#[expect(
    clippy::cast_possible_truncation,
    reason = "start and end are bounds-checked <= len (u32) before cast"
)]
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_slice(
    ptr: *mut u8,
    offset: u32,
    len: u32,
    start: i64,
    end: i64,
) -> BytesTriple {
    if start < 0 || end < 0 || start > end || end > i64::from(len) {
        // SAFETY: abort is always safe.
        unsafe { hew_bytes_abort_index_oob() };
    }
    let s = start as u32;
    let e = end as u32;
    let new_len = e - s;
    if new_len == 0 {
        // Empty slice — represent as a null/0/0 triple so drop is a no-op
        // and we do not retain a reference to the underlying buffer.
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }
    // Non-empty slice shares the underlying allocation.
    // Compute the new offset FIRST so a checked-add failure aborts
    // BEFORE the refcount bump (no leak on the overflow path; release
    // mode would otherwise wrap silently and produce a triple pointing
    // at the wrong place in the allocation — boundary-fail-closed
    // LESSONS P0:49).
    let Some(new_offset) = offset.checked_add(s) else {
        // SAFETY: abort is always safe.
        unsafe { hew_bytes_abort_offset_overflow() };
    };
    // Bump the refcount so the slice owner can independently drop
    // their handle (drop-safety triad: sync/async-cancel/actor-
    // shutdown all release exactly once).
    // SAFETY: ptr is non-null (len > 0 above means ptr was non-null per
    // the bounds check `end > i64::from(len)` plus `new_len > 0`).
    unsafe { hew_bytes_clone_ref(ptr) };
    BytesTriple {
        ptr,
        offset: new_offset,
        len: new_len,
    }
}

/// Out-pointer variant of [`hew_bytes_slice`] for Windows x64 MSVC sret fix.
///
/// # Safety
///
/// Same as [`hew_bytes_slice`]. `out` must point to a valid, writable `BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_slice_raw(
    ptr: *mut u8,
    offset: u32,
    len: u32,
    start: i64,
    end: i64,
    out: *mut BytesTriple,
) {
    // SAFETY: preconditions forwarded from caller contract above.
    let triple = unsafe { hew_bytes_slice(ptr, offset, len, start, end) };
    // SAFETY: caller guarantees `out` is a valid BytesTriple slot.
    unsafe { out.write(triple) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
#[expect(
    clippy::cast_possible_truncation,
    reason = "test data: byte buffer sizes are small enough to fit in u32"
)]
mod tests {
    use super::*;

    #[test]
    fn new_and_drop() {
        let ptr = hew_bytes_new(32);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid bytes allocation.
        unsafe {
            assert_eq!(refcount(ptr).load(Ordering::Relaxed), 1);
            assert_eq!(capacity(ptr), 32);
            hew_bytes_drop(ptr);
        }
    }

    #[test]
    fn clone_ref_and_drop() {
        let ptr = hew_bytes_new(16);
        // SAFETY: ptr is valid.
        unsafe {
            assert_eq!(refcount(ptr).load(Ordering::Relaxed), 1);

            hew_bytes_clone_ref(ptr);
            assert_eq!(refcount(ptr).load(Ordering::Relaxed), 2);

            hew_bytes_drop(ptr);
            assert_eq!(refcount(ptr).load(Ordering::Relaxed), 1);

            hew_bytes_drop(ptr);
            // ptr is now freed — cannot read refcount.
        }
    }

    #[test]
    fn push_and_read() {
        let mut triple = BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };

        // SAFETY: triple is a valid empty BytesTriple.
        unsafe {
            hew_bytes_push(&mut triple, b'H');
            hew_bytes_push(&mut triple, b'e');
            hew_bytes_push(&mut triple, b'w');
        }

        assert_eq!(triple.len, 3);
        assert!(!triple.ptr.is_null());

        // Read back.
        // SAFETY: triple.ptr + offset is valid for triple.len bytes.
        let data = unsafe {
            std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
        };
        assert_eq!(data, b"Hew");

        // SAFETY: triple.ptr is valid.
        unsafe { hew_bytes_drop(triple.ptr) };
    }

    #[test]
    fn from_static() {
        let data = b"hello bytes";
        // SAFETY: data is valid for data.len() bytes.
        let triple = unsafe { hew_bytes_from_static(data.as_ptr(), data.len() as u32) };

        assert!(!triple.ptr.is_null());
        assert_eq!(triple.offset, 0);
        assert_eq!(triple.len, data.len() as u32);

        // SAFETY: triple.ptr + offset is valid for triple.len bytes.
        let slice = unsafe {
            std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
        };
        assert_eq!(slice, b"hello bytes");

        // SAFETY: triple.ptr is valid.
        unsafe { hew_bytes_drop(triple.ptr) };
    }

    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test code: every unsafe call is a direct FFI invocation with arguments \
                  explicitly visible inline"
    )]
    fn bytes_len_reads_active_length() {
        // Non-empty triple: len is the active byte count. The runtime entry
        // takes the triple by pointer.
        let data = b"hello bytes";
        let triple = unsafe { hew_bytes_from_static(data.as_ptr(), data.len() as u32) };
        assert_eq!(
            unsafe { hew_bytes_len(std::ptr::addr_of!(triple)) },
            i64::try_from(data.len()).expect("len fits in i64")
        );
        unsafe { hew_bytes_drop(triple.ptr) };

        // A slice reports its sliced length, not the underlying buffer length.
        let base = unsafe { hew_bytes_from_static(b"abcdefgh".as_ptr(), 8) };
        let slice = unsafe { hew_bytes_slice(base.ptr, base.offset, base.len, 2, 5) };
        assert_eq!(unsafe { hew_bytes_len(std::ptr::addr_of!(slice)) }, 3);
        unsafe {
            hew_bytes_drop(slice.ptr);
            hew_bytes_drop(base.ptr);
        }

        // Empty triple (null ptr, len 0) reports 0.
        let empty = BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
        assert_eq!(unsafe { hew_bytes_len(std::ptr::addr_of!(empty)) }, 0);
    }

    #[test]
    fn concat() {
        let a = b"foo";
        let b_data = b"bar";

        // SAFETY: Both pointers are valid for their lengths.
        let result = unsafe {
            hew_bytes_concat(
                a.as_ptr(),
                0,
                a.len() as u32,
                b_data.as_ptr(),
                0,
                b_data.len() as u32,
            )
        };

        assert!(!result.ptr.is_null());
        assert_eq!(result.len, 6);

        // SAFETY: result.ptr + offset is valid for result.len bytes.
        let slice = unsafe {
            std::slice::from_raw_parts(result.ptr.add(result.offset as usize), result.len as usize)
        };
        assert_eq!(slice, b"foobar");

        // SAFETY: result.ptr is valid.
        unsafe { hew_bytes_drop(result.ptr) };
    }

    #[test]
    fn eq_check() {
        let a = b"hello";
        let b_data = b"hello";
        let c = b"world";

        // SAFETY: All pointers valid for their lengths.
        unsafe {
            assert!(hew_bytes_eq(
                a.as_ptr(),
                0,
                a.len() as u32,
                b_data.as_ptr(),
                0,
                b_data.len() as u32,
            ));

            assert!(!hew_bytes_eq(
                a.as_ptr(),
                0,
                a.len() as u32,
                c.as_ptr(),
                0,
                c.len() as u32,
            ));

            assert!(!hew_bytes_eq(
                a.as_ptr(),
                0,
                a.len() as u32,
                a.as_ptr(),
                0,
                3,
            ));

            assert!(hew_bytes_eq(std::ptr::null(), 0, 0, std::ptr::null(), 0, 0,));
        }
    }

    #[test]
    fn null_safety() {
        // SAFETY: Null is a valid input for all these functions.
        unsafe {
            hew_bytes_clone_ref(std::ptr::null_mut());
            hew_bytes_drop(std::ptr::null_mut());

            let mut triple = BytesTriple {
                ptr: std::ptr::null_mut(),
                offset: 0,
                len: 0,
            };
            hew_bytes_push(&mut triple, b'x');
            assert!(!triple.ptr.is_null());
            assert_eq!(triple.len, 1);
            hew_bytes_drop(triple.ptr);

            let mut dst = BytesTriple {
                ptr: std::ptr::null_mut(),
                offset: 0,
                len: 0,
            };
            let src = b"abc";
            hew_bytes_append(&mut dst, src.as_ptr(), 0, 3);
            assert!(!dst.ptr.is_null());
            assert_eq!(dst.len, 3);
            hew_bytes_drop(dst.ptr);

            let result = hew_bytes_concat(std::ptr::null(), 0, 0, std::ptr::null(), 0, 0);
            assert!(result.ptr.is_null());
            assert_eq!(result.len, 0);

            let result = hew_bytes_from_static(std::ptr::null(), 0);
            assert!(result.ptr.is_null());

            let result = hew_bytes_from_str(std::ptr::null());
            assert!(result.ptr.is_null());

            let empty_triple = BytesTriple {
                ptr: std::ptr::null_mut(),
                offset: 0,
                len: 0,
            };
            let s = hew_bytes_to_string(std::ptr::addr_of!(empty_triple));
            assert!(!s.is_null());
            assert_eq!(*s, 0);
            crate::cabi::free_cstring(s); // CSTRING-FREE: str-open (test frees hew_bytes_to_string output)

            assert!(hew_bytes_eq(std::ptr::null(), 0, 0, std::ptr::null(), 0, 0,));
        }
    }

    #[test]
    fn cow_on_push() {
        let data = b"original";
        // SAFETY: data is valid.
        let triple_a = unsafe { hew_bytes_from_static(data.as_ptr(), data.len() as u32) };

        // SAFETY: triple_a.ptr is valid.
        unsafe { hew_bytes_clone_ref(triple_a.ptr) };

        let mut triple_b = BytesTriple {
            ptr: triple_a.ptr,
            offset: triple_a.offset,
            len: triple_a.len,
        };

        // SAFETY: triple_b is a valid BytesTriple.
        unsafe { hew_bytes_push(&mut triple_b, b'!') };

        assert_ne!(triple_b.ptr, triple_a.ptr);
        assert_eq!(triple_b.len, 9);

        // SAFETY: triple_a.ptr is still valid.
        let original = unsafe {
            std::slice::from_raw_parts(
                triple_a.ptr.add(triple_a.offset as usize),
                triple_a.len as usize,
            )
        };
        assert_eq!(original, b"original");

        // SAFETY: triple_b.ptr is valid.
        let cloned = unsafe {
            std::slice::from_raw_parts(
                triple_b.ptr.add(triple_b.offset as usize),
                triple_b.len as usize,
            )
        };
        assert_eq!(cloned, b"original!");

        // SAFETY: Both pointers are valid.
        unsafe {
            hew_bytes_drop(triple_a.ptr);
            hew_bytes_drop(triple_b.ptr);
        }
    }

    #[test]
    fn to_string_and_from_string() {
        let data = b"hello world";
        // SAFETY: data is valid.
        let triple = unsafe { hew_bytes_from_static(data.as_ptr(), data.len() as u32) };

        // SAFETY: triple.ptr + offset is valid for triple.len bytes.
        let cstr = unsafe { hew_bytes_to_string(std::ptr::addr_of!(triple)) };
        assert!(!cstr.is_null());

        // SAFETY: cstr is a valid NUL-terminated C string.
        let s = unsafe { std::ffi::CStr::from_ptr(cstr) };
        assert_eq!(s.to_str().unwrap(), "hello world");

        // SAFETY: cstr is a valid NUL-terminated string.
        let round_trip = unsafe { hew_bytes_from_str(cstr.cast::<u8>()) };
        assert_eq!(round_trip.len, 11);

        // SAFETY: round_trip.ptr + offset is valid for round_trip.len bytes.
        let rt_data = unsafe {
            std::slice::from_raw_parts(
                round_trip.ptr.add(round_trip.offset as usize),
                round_trip.len as usize,
            )
        };
        assert_eq!(rt_data, b"hello world");

        // SAFETY: All pointers are valid.
        unsafe {
            crate::cabi::free_cstring(cstr); // CSTRING-FREE: str-open (test frees hew_bytes_to_string output)
            hew_bytes_drop(triple.ptr);
            hew_bytes_drop(round_trip.ptr);
        }
    }

    #[test]
    fn append_grows() {
        let mut triple = BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };

        let chunk = b"abcdefghijklmnop"; // 16 bytes
                                         // SAFETY: triple and chunk are valid.
        unsafe {
            hew_bytes_append(&mut triple, chunk.as_ptr(), 0, chunk.len() as u32);
            hew_bytes_append(&mut triple, chunk.as_ptr(), 0, chunk.len() as u32);
            hew_bytes_append(&mut triple, chunk.as_ptr(), 0, chunk.len() as u32);
        }

        assert_eq!(triple.len, 48);

        // SAFETY: triple.ptr + offset is valid for triple.len bytes.
        let data = unsafe {
            std::slice::from_raw_parts(triple.ptr.add(triple.offset as usize), triple.len as usize)
        };
        assert_eq!(&data[0..16], chunk);
        assert_eq!(&data[16..32], chunk);
        assert_eq!(&data[32..48], chunk);

        // SAFETY: triple.ptr is valid.
        unsafe { hew_bytes_drop(triple.ptr) };
    }

    // ------------------------------------------------------------------
    // W3 collections-sugar S2 — hew_bytes_index / hew_bytes_slice tests
    // ------------------------------------------------------------------

    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test code: every unsafe call is a direct FFI invocation with arguments \
                  explicitly visible inline; documenting each block adds noise without value"
    )]
    fn bytes_index_in_range() {
        let data = b"Hewlang";
        let triple = unsafe { hew_bytes_from_static(data.as_ptr(), data.len() as u32) };
        assert_eq!(
            unsafe { hew_bytes_index(triple.ptr, triple.offset, triple.len, 0) },
            b'H'
        );
        assert_eq!(
            unsafe { hew_bytes_index(triple.ptr, triple.offset, triple.len, 6) },
            b'g'
        );
        unsafe { hew_bytes_drop(triple.ptr) };
    }

    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test code: inline FFI invocations"
    )]
    fn bytes_index_respects_offset() {
        // Slice first, then index into the slice — confirms `offset` is honoured.
        let data = b"abcdefgh";
        let base = unsafe { hew_bytes_from_static(data.as_ptr(), data.len() as u32) };
        let slice = unsafe { hew_bytes_slice(base.ptr, base.offset, base.len, 2, 5) };
        // slice covers "cde"; index 0 -> 'c', index 2 -> 'e'.
        assert_eq!(slice.len, 3);
        assert_eq!(slice.offset, base.offset + 2);
        assert_eq!(
            unsafe { hew_bytes_index(slice.ptr, slice.offset, slice.len, 0) },
            b'c'
        );
        assert_eq!(
            unsafe { hew_bytes_index(slice.ptr, slice.offset, slice.len, 2) },
            b'e'
        );
        unsafe { hew_bytes_drop(slice.ptr) };
        unsafe { hew_bytes_drop(base.ptr) };
    }

    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test code: inline FFI invocations"
    )]
    fn bytes_slice_bumps_refcount_and_outlives_input() {
        // Drop-safety: build a heap-owned `bytes`, slice it, drop the original,
        // then read through the slice. The slice must remain valid.
        let triple = unsafe { hew_bytes_from_static(b"abcdef".as_ptr(), 6) };
        let slice = unsafe { hew_bytes_slice(triple.ptr, triple.offset, triple.len, 1, 4) };
        // After slicing a 3-byte region, refcount on the shared buffer must be 2.
        unsafe {
            assert_eq!(refcount(triple.ptr).load(Ordering::Relaxed), 2);
        }
        unsafe { hew_bytes_drop(triple.ptr) };
        // The slice still points at "bcd".
        unsafe {
            assert_eq!(refcount(slice.ptr).load(Ordering::Relaxed), 1);
        }
        assert_eq!(slice.len, 3);
        assert_eq!(
            unsafe { hew_bytes_index(slice.ptr, slice.offset, slice.len, 0) },
            b'b'
        );
        assert_eq!(
            unsafe { hew_bytes_index(slice.ptr, slice.offset, slice.len, 2) },
            b'd'
        );
        unsafe { hew_bytes_drop(slice.ptr) };
    }

    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test code: inline FFI invocations"
    )]
    fn bytes_slice_empty_returns_null_triple() {
        let triple = unsafe { hew_bytes_from_static(b"abc".as_ptr(), 3) };
        let empty = unsafe { hew_bytes_slice(triple.ptr, triple.offset, triple.len, 1, 1) };
        assert!(empty.ptr.is_null());
        assert_eq!(empty.len, 0);
        // Refcount on the original must NOT have been bumped for an empty slice.
        unsafe {
            assert_eq!(refcount(triple.ptr).load(Ordering::Relaxed), 1);
        }
        unsafe { hew_bytes_drop(triple.ptr) };
    }

    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test code: inline FFI invocations"
    )]
    fn bytes_slice_full_range_clones() {
        // start == 0, end == len: shares the buffer, refcount = 2.
        let triple = unsafe { hew_bytes_from_static(b"xyz".as_ptr(), 3) };
        let full = unsafe { hew_bytes_slice(triple.ptr, triple.offset, triple.len, 0, 3) };
        assert_eq!(full.len, 3);
        assert_eq!(full.offset, triple.offset);
        unsafe {
            assert_eq!(refcount(triple.ptr).load(Ordering::Relaxed), 2);
        }
        unsafe { hew_bytes_drop(full.ptr) };
        unsafe { hew_bytes_drop(triple.ptr) };
    }

    #[test]
    fn bytes_index_oob_aborts() {
        run_aborting_subprocess("bytes_index_oob_aborts");
    }

    #[test]
    fn bytes_index_negative_aborts() {
        run_aborting_subprocess("bytes_index_negative_aborts");
    }

    #[test]
    fn bytes_slice_oob_aborts() {
        run_aborting_subprocess("bytes_slice_oob_aborts");
    }

    #[test]
    fn bytes_slice_inverted_aborts() {
        run_aborting_subprocess("bytes_slice_inverted_aborts");
    }

    // Offset-arithmetic overflow guards.  A malformed (or hostile)
    // `BytesTriple` with `offset` near `u32::MAX` could wrap when
    // `offset + start` (slice) or `offset + idx` (index) is computed.
    // In release mode Rust does NOT panic on `+` overflow — the wrap
    // would silently mis-point at the wrong place in the underlying
    // allocation. The new `hew_bytes_abort_offset_overflow` path
    // catches it; these tests exercise the abort.
    #[test]
    fn bytes_index_offset_overflow_aborts() {
        run_aborting_subprocess("bytes_index_offset_overflow_aborts");
    }

    #[test]
    fn bytes_slice_offset_overflow_aborts() {
        run_aborting_subprocess("bytes_slice_offset_overflow_aborts");
    }

    /// The refcount-overflow guard in `hew_bytes_clone_ref` actually aborts
    /// when a retain observes a count already past the saturation threshold —
    /// proving the wrap guard is wired into the real path (mirrors
    /// `hew-cabi`'s `cstring_retain_aborts_on_refcount_overflow`).
    #[test]
    fn bytes_clone_ref_overflow_aborts() {
        run_aborting_subprocess("bytes_clone_ref_overflow_aborts");
    }

    /// The overflow predicate trips exactly above the saturation threshold and
    /// nowhere below it. Covers the abort condition without `2^31` retains.
    #[test]
    fn bytes_rc_would_overflow_predicate_boundaries() {
        assert!(!bytes_rc_would_overflow(0), "no owners never overflows");
        assert!(!bytes_rc_would_overflow(1), "a single owner is safe");
        assert!(
            !bytes_rc_would_overflow(BYTES_RC_MAX),
            "at the threshold the count is still representable"
        );
        assert!(
            bytes_rc_would_overflow(BYTES_RC_MAX + 1),
            "one past the threshold must trip"
        );
        assert!(
            bytes_rc_would_overflow(u32::MAX - 1),
            "near the wrap must trip"
        );
        assert!(bytes_rc_would_overflow(u32::MAX), "at the wrap must trip");
    }

    /// A retain whose pre-increment count is just below the threshold does NOT
    /// abort — it bumps the count normally. Seeds the rc directly to avoid the
    /// `2^31` retains a natural climb would require.
    #[test]
    fn bytes_clone_ref_below_threshold_does_not_abort() {
        // SAFETY: ptr is a fresh header-aware allocation; we seed then restore
        // the rc and drop the single owner.
        unsafe {
            let ptr = hew_bytes_new(16);
            refcount(ptr).store(BYTES_RC_MAX, Ordering::Relaxed);
            hew_bytes_clone_ref(ptr); // old == BYTES_RC_MAX → must NOT abort
            assert_eq!(
                refcount(ptr).load(Ordering::Relaxed),
                BYTES_RC_MAX + 1,
                "retain incremented"
            );
            // Restore a sane refcount and free the single owner.
            refcount(ptr).store(1, Ordering::Relaxed);
            hew_bytes_drop(ptr);
        }
    }

    // Spawn ourself with HEW_BYTES_ABORT_CASE set; the matching env-driven
    // test below runs the FFI call that aborts. The parent test asserts
    // (via `should_panic`) that the child exited non-zero (i.e. aborted).
    fn run_aborting_subprocess(case: &str) {
        let exe = std::env::current_exe().expect("current_exe");
        let status = std::process::Command::new(exe)
            .args([
                "--quiet",
                "--exact",
                "--nocapture",
                "bytes::tests::bytes_abort_helper",
            ])
            .env("HEW_BYTES_ABORT_CASE", case)
            .stderr(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .status()
            .expect("spawn");
        // If the child aborted, status.success() is false — the test passes
        // by NOT panicking here. If the child returned cleanly, we panic so
        // the `should_panic` parent test fails loudly.
        assert!(!status.success(), "child did not abort");
    }

    /// In-process helper: when invoked with `HEW_BYTES_ABORT_CASE` set,
    /// trigger the corresponding aborting FFI call. Without the env var,
    /// this test is a no-op (so plain `cargo test` runs the parent tests
    /// which then spawn this helper).
    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test helper: every unsafe call is an FFI invocation that intentionally \
                  aborts the process; the abort path is the documented behaviour under test"
    )]
    fn bytes_abort_helper() {
        let Ok(case) = std::env::var("HEW_BYTES_ABORT_CASE") else {
            return;
        };
        let triple = unsafe { hew_bytes_from_static(b"abc".as_ptr(), 3) };
        match case.as_str() {
            "bytes_index_oob_aborts" => unsafe {
                let _ = hew_bytes_index(triple.ptr, triple.offset, triple.len, 3);
            },
            "bytes_index_negative_aborts" => unsafe {
                let _ = hew_bytes_index(triple.ptr, triple.offset, triple.len, -1);
            },
            "bytes_slice_oob_aborts" => unsafe {
                let _ = hew_bytes_slice(triple.ptr, triple.offset, triple.len, 0, 4);
            },
            "bytes_slice_inverted_aborts" => unsafe {
                let _ = hew_bytes_slice(triple.ptr, triple.offset, triple.len, 2, 1);
            },
            // Construct a synthetic triple with `offset` near u32::MAX.
            // We do NOT dereference the resulting "ptr" — both abort
            // paths trip on the checked-add BEFORE any pointer read.
            // The triple's `ptr` is the real heap allocation; the
            // `offset` field is a forged near-max value (the kind of
            // value a long chain of slice operations on a buffer at
            // capacity could converge on, or that a malformed
            // externally-supplied triple could carry).
            "bytes_index_offset_overflow_aborts" => unsafe {
                // offset = u32::MAX - 4, len = 5. Indexing past 0 will
                // exercise `offset + idx`. Specifically idx=4 makes
                // `offset + 4 = u32::MAX`, which does not overflow;
                // idx=5 would be OOB (caught first); so we need offset
                // such that `offset + idx` overflows BEFORE the OOB
                // check fires. Set offset = u32::MAX, len = 1: idx in
                // [0,1) means idx=0; `u32::MAX + 0` is fine. Need
                // index >= 1 to trigger overflow — but the OOB check
                // fires first. Use a higher `len` so the OOB check
                // passes but the add overflows:
                //   offset = u32::MAX - 2, len = 5
                //   valid index in [0,5); idx=3 -> u32::MAX+1 wraps.
                let bad_triple = BytesTriple {
                    ptr: triple.ptr,
                    offset: u32::MAX - 2,
                    len: 5,
                };
                let _ = hew_bytes_index(bad_triple.ptr, bad_triple.offset, bad_triple.len, 3);
            },
            "bytes_slice_offset_overflow_aborts" => unsafe {
                // offset = u32::MAX - 2, len = 10. Slice [0..10) is in
                // range w.r.t. `end > len`; `offset + 3` then wraps.
                // We use start=3, end=10 so the bounds check passes
                // (start<=end, end<=len) before the checked add fires.
                let bad_triple = BytesTriple {
                    ptr: triple.ptr,
                    offset: u32::MAX - 2,
                    len: 10,
                };
                let _ = hew_bytes_slice(bad_triple.ptr, bad_triple.offset, bad_triple.len, 3, 10);
            },
            "bytes_clone_ref_overflow_aborts" => unsafe {
                // A real header-aware allocation whose refcount we seed one
                // past the saturation threshold; the next retain must abort
                // before the count wraps (Arc MAX_REFCOUNT discipline).
                let ptr = hew_bytes_new(16);
                refcount(ptr).store(BYTES_RC_MAX + 1, Ordering::Relaxed);
                hew_bytes_clone_ref(ptr); // must abort, never returns
            },
            other => panic!("unknown abort case: {other}"),
        }
        // Unreachable on the abort-paths.
        unreachable!("abort case {case} should have terminated the process");
    }
}
