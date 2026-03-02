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
    // Update capacity in the header. Refcount is preserved by realloc.
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

/// Atomically increment the refcount of a byte buffer. No-op if `data_ptr` is null.
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
    rc.fetch_add(1, Ordering::Relaxed);
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
    // SAFETY: Both pointers are valid for their respective lengths per caller contract.
    let a_slice = unsafe { std::slice::from_raw_parts(a_ptr.add(a_off as usize), a_len as usize) };
    let b_slice = unsafe { std::slice::from_raw_parts(b_ptr.add(b_off as usize), b_len as usize) };
    a_slice == b_slice
}

/// Convert a byte region to a NUL-terminated UTF-8 C string (lossy).
///
/// Invalid UTF-8 sequences are replaced with U+FFFD. The returned pointer is
/// allocated via `libc::malloc`; the caller must `libc::free` it.
///
/// # Safety
///
/// If `len > 0`, `ptr + offset` must be valid for `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_bytes_to_str(ptr: *const u8, offset: u32, len: u32) -> *const u8 {
    if len == 0 || ptr.is_null() {
        // Return an empty NUL-terminated string.
        // SAFETY: Allocating 1 byte.
        let out = unsafe { libc::malloc(1) }.cast::<u8>();
        if out.is_null() {
            // SAFETY: abort is always safe.
            unsafe { libc::abort() };
        }
        // SAFETY: out is freshly allocated with 1 byte.
        unsafe { *out = 0 };
        return out;
    }

    // SAFETY: ptr + offset is valid for len bytes per caller contract.
    let data = unsafe { std::slice::from_raw_parts(ptr.add(offset as usize), len as usize) };
    let s = String::from_utf8_lossy(data);
    let bytes = s.as_bytes();
    let alloc_size = bytes.len() + 1; // +1 for NUL
                                      // SAFETY: alloc_size > 0.
    let out = unsafe { libc::malloc(alloc_size) }.cast::<u8>();
    if out.is_null() {
        // SAFETY: abort is always safe.
        unsafe { libc::abort() };
    }
    // SAFETY: out is freshly allocated with alloc_size bytes; bytes.len() < alloc_size.
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), out, bytes.len());
        *out.add(bytes.len()) = 0; // NUL terminator
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
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

            let s = hew_bytes_to_str(std::ptr::null(), 0, 0);
            assert!(!s.is_null());
            assert_eq!(*s, 0);
            libc::free(s as *mut _);

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
        let cstr = unsafe { hew_bytes_to_str(triple.ptr, triple.offset, triple.len) };
        assert!(!cstr.is_null());

        // SAFETY: cstr is a valid NUL-terminated C string.
        let s = unsafe { std::ffi::CStr::from_ptr(cstr.cast()) };
        assert_eq!(s.to_str().unwrap(), "hello world");

        // SAFETY: cstr is a valid NUL-terminated string.
        let round_trip = unsafe { hew_bytes_from_str(cstr) };
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
            libc::free(cstr as *mut _);
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
}
