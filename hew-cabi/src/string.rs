//! Managed UTF-8 string carrier shared by compiled Hew code and its runtime.
//!
//! A Hew string value is one opaque pointer to an immutable allocation. The
//! pointer names the allocation header, never a C character buffer. The exact
//! byte and scalar lengths plus an eagerly built bounded-stride index make
//! scalar indexing constant time and preserve embedded NUL bytes. Foreign C strings enter and leave only through
//! explicit copying adapters.

use core::slice;
use core::str::{self, Utf8Error};
use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::atomic::{fence, AtomicU32, Ordering};

/// Opaque pointee for the managed Hew string handle.
///
/// Its allocation layout is private. Runtime and package code must use the
/// helpers in this module instead of dereferencing or casting the handle.
#[repr(C)]
#[derive(Debug)]
pub struct HewString {
    _private: [u8; 0],
}

#[repr(C)]
struct HewStringHeader<Length = usize> {
    byte_len: Length,
    capacity: Length,
    scalar_len: Length,
    rc: AtomicU32,
}

/// Native-target offset of the byte length for read-only debugger tooling.
/// Runtime and package code should use [`string_as_bytes`] instead.
pub const STRING_DEBUG_BYTE_LEN_OFFSET: usize = core::mem::offset_of!(HewStringHeader, byte_len);

/// Native-target offset of the atomic owner count for read-only inspection.
pub const STRING_DEBUG_REFCOUNT_OFFSET: usize = core::mem::offset_of!(HewStringHeader, rc);

/// Header size for a debugger describing a supported target's managed strings.
/// Derive cross-target offsets from the allocation header declaration as well.
#[must_use]
pub const fn string_debug_header_size(pointer_bytes: u64) -> Option<u64> {
    let size = match pointer_bytes {
        4 => core::mem::size_of::<HewStringHeader<u32>>() as u64,
        8 => core::mem::size_of::<HewStringHeader<u64>>() as u64,
        _ => return None,
    };
    // A 32-bit host can align u64 to four bytes. Apply the target pointer's
    // alignment to the same fields when describing a 64-bit allocation.
    Some(size.next_multiple_of(pointer_bytes))
}

const MAX_REFCOUNT: u32 = u32::MAX / 2;
const INDEX_STRIDE: usize = 32;

// Non-ASCII allocations keep a byte offset for every INDEX_STRIDE-th scalar
// after the UTF-8 capacity. The table belongs to the same allocation and is
// fully initialized before a handle can be shared.
fn index_offset(capacity: usize) -> usize {
    let align = core::mem::align_of::<usize>();
    core::mem::size_of::<HewStringHeader>()
        .checked_add(capacity)
        .and_then(|size| size.checked_add(align - 1))
        .map_or_else(|| std::process::abort(), |size| size & !(align - 1))
}

fn allocation_size(capacity: usize, indexed: bool) -> usize {
    let size = if indexed {
        capacity
            .div_ceil(INDEX_STRIDE)
            .checked_mul(core::mem::size_of::<usize>())
            .and_then(|table| index_offset(capacity).checked_add(table))
    } else {
        core::mem::size_of::<HewStringHeader>().checked_add(capacity)
    };
    size.filter(|size| isize::try_from(*size).is_ok())
        .unwrap_or_else(|| std::process::abort())
}

unsafe fn index_table(value: *const HewString) -> *const usize {
    // SAFETY: callers hold a live non-ASCII allocation. Its table starts at
    // the aligned end of the UTF-8 capacity, within its single owned block.
    unsafe {
        value
            .cast::<u8>()
            .add(index_offset((*header(value)).capacity))
            .cast()
    }
}

unsafe fn write_index(value: *mut HewString, byte_start: usize, scalar_start: usize) {
    // SAFETY: the caller has exclusive access to initialized UTF-8 bytes and
    // allocated table capacity. Table writes are disjoint from the byte slice.
    unsafe {
        let table = index_table(value).cast_mut();
        for (index, (offset, _)) in string_as_str(value)[byte_start..]
            .char_indices()
            .enumerate()
        {
            let scalar = scalar_start + index;
            if scalar.is_multiple_of(INDEX_STRIDE) {
                table.add(scalar / INDEX_STRIDE).write(byte_start + offset);
            }
        }
    }
}

/// Why a foreign C string could not be copied into managed string storage.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringFromCStrError {
    /// A null C pointer does not spell an input string.
    NullInput,
    /// The bytes preceding the C terminator are not valid UTF-8.
    InvalidUtf8,
}

/// Why a managed string could not be copied to a C string.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringToCStrError {
    /// A C string cannot represent an embedded NUL byte.
    InteriorNul,
}

#[inline]
unsafe fn header(value: *const HewString) -> *const HewStringHeader {
    value.cast()
}

/// Retained UTF-8 payload capacity, excluding headers and scalar index caches.
///
/// # Safety
/// A non-null handle must remain live throughout this read.
#[must_use]
pub unsafe fn string_payload_capacity(value: *const HewString) -> usize {
    if value.is_null() {
        return 0;
    }
    // SAFETY: the caller retains the immutable allocation header.
    unsafe { (*header(value)).capacity }
}

#[inline]
unsafe fn data(value: *const HewString) -> *const u8 {
    // SAFETY: callers establish that `value` names a live managed allocation.
    unsafe {
        value
            .cast::<u8>()
            .add(core::mem::size_of::<HewStringHeader>())
    }
}

/// Allocate one managed string from bytes already known to be valid UTF-8.
///
/// The empty string has the canonical null representation. Allocation failure
/// aborts, matching the compiler-private runtime's infallible value producers.
///
/// # Safety
///
/// When `len > 0`, `source` must be readable for `len` bytes and those bytes
/// must be valid UTF-8.
#[must_use]
pub unsafe fn string_alloc_utf8_unchecked(source: *const u8, len: usize) -> *mut HewString {
    if len == 0 {
        return core::ptr::null_mut();
    }
    // SAFETY: the caller supplies readable UTF-8 bytes of nonzero length.
    unsafe { string_alloc_nonnull(source, len) }
}

// Shared allocator for the private null-empty and public non-null-empty views.
unsafe fn string_alloc_nonnull(source: *const u8, len: usize) -> *mut HewString {
    if source.is_null() || isize::try_from(len).is_err() {
        std::process::abort();
    }
    // SAFETY: the caller supplies valid UTF-8 readable for its complete length.
    let text = unsafe { str::from_utf8_unchecked(slice::from_raw_parts(source, len)) };
    let scalar_len = text.chars().count();
    let indexed = scalar_len != len;
    let total = allocation_size(len, indexed);
    // SAFETY: the checked allocation size contains the header, bytes and index.
    let allocation = crate::mem::buf_try_alloc(total).cast::<HewStringHeader>();
    if allocation.is_null() {
        std::process::abort();
    }
    // SAFETY: the allocation is aligned and writable for this header and its
    // complete payload. The private index is initialized before publication.
    unsafe {
        allocation.write(HewStringHeader {
            byte_len: len,
            capacity: len,
            scalar_len,
            rc: AtomicU32::new(1),
        });
        core::ptr::copy_nonoverlapping(source, data(allocation.cast()).cast_mut(), len);
        if indexed {
            write_index(allocation.cast(), 0, 0);
        }
    }
    allocation.cast()
}

/// Copy UTF-8 into a non-null managed string, including the empty string.
/// Uses the canonical allocator and the usual [`string_release`] pairing.
#[must_use]
pub fn string_from_str_nonnull(value: &str) -> *mut HewString {
    // SAFETY: str supplies non-null storage and valid UTF-8 even when empty.
    unsafe { string_alloc_nonnull(value.as_ptr(), value.len()) }
}

/// Normalize a transferred managed string owner for the public host boundary.
/// Non-null allocations are transferred unchanged; null becomes an allocated empty.
///
/// # Safety
/// `value` is null or one live owned managed string, consumed by this operation.
#[must_use]
pub unsafe fn string_into_nonnull(value: *mut HewString) -> *mut HewString {
    if value.is_null() {
        string_from_str_nonnull("")
    } else {
        value
    }
}

/// Allocate one managed string by copying a Rust string slice.
#[must_use]
pub fn string_from_str(value: &str) -> *mut HewString {
    // SAFETY: a Rust `str` is valid UTF-8 and readable for its byte length.
    unsafe { string_alloc_utf8_unchecked(value.as_ptr(), value.len()) }
}

/// Validate and copy an arbitrary byte slice into one managed string.
///
/// # Errors
///
/// Returns the UTF-8 validation error without allocating a managed value.
pub fn string_from_utf8(value: &[u8]) -> Result<*mut HewString, Utf8Error> {
    let value = str::from_utf8(value)?;
    Ok(string_from_str(value))
}

/// Validate and copy one explicitly foreign, NUL-terminated C string.
///
/// The returned handle is always managed; the foreign pointer is never stored
/// or reinterpreted as a Hew string.
///
/// # Errors
///
/// Returns [`StringFromCStrError::NullInput`] for null or
/// [`StringFromCStrError::InvalidUtf8`] when the foreign bytes are not UTF-8.
///
/// # Safety
///
/// `value` must be null or point to a readable NUL-terminated C string.
pub unsafe fn string_from_cstr_copy(
    value: *const c_char,
) -> Result<*mut HewString, StringFromCStrError> {
    if value.is_null() {
        return Err(StringFromCStrError::NullInput);
    }
    // SAFETY: the caller supplies a readable NUL-terminated C string.
    let value = unsafe { CStr::from_ptr(value) }
        .to_str()
        .map_err(|_| StringFromCStrError::InvalidUtf8)?;
    Ok(string_from_str(value))
}

/// Copy a managed value into an owned foreign `CString`.
///
/// Rust owns this adapter's allocation; drop the `CString` normally. Use
/// [`cstring_from_string_copy`] and [`cstring_copy_release`] for raw C ownership.
///
/// # Errors
///
/// Returns [`StringToCStrError::InteriorNul`] instead of truncating input.
///
/// # Safety
///
/// `value` must be null (empty) or a live managed string handle.
pub unsafe fn string_to_cstring(
    value: *const HewString,
) -> Result<std::ffi::CString, StringToCStrError> {
    // SAFETY: the caller holds a live managed owner during the copy.
    std::ffi::CString::new(unsafe { string_as_bytes(value) })
        .map_err(|_| StringToCStrError::InteriorNul)
}

/// Copy a managed string into a freshly allocated NUL-terminated C string.
///
/// The allocation belongs exclusively to [`cstring_copy_release`]. It is not
/// a managed Hew string and must never be passed to [`string_release`].
///
/// # Errors
///
/// Returns [`StringToCStrError::InteriorNul`] when the managed value cannot be
/// represented by a NUL-terminated C string without truncation.
///
/// # Safety
///
/// `value` must be null (the canonical empty string) or a live managed handle.
pub unsafe fn cstring_from_string_copy(
    value: *const HewString,
) -> Result<*mut c_char, StringToCStrError> {
    // SAFETY: the caller provides a live managed handle.
    let bytes = unsafe { string_as_bytes(value) };
    if bytes.contains(&0) {
        return Err(StringToCStrError::InteriorNul);
    }
    let Some(size) = bytes.len().checked_add(1) else {
        std::process::abort();
    };
    // SAFETY: `size` is positive and was checked for arithmetic overflow.
    let out = crate::mem::buf_try_alloc(size).cast::<c_char>();
    if out.is_null() {
        std::process::abort();
    }
    // SAFETY: `out` owns `size` writable bytes and the source is disjoint.
    unsafe {
        core::ptr::copy_nonoverlapping(bytes.as_ptr(), out.cast::<u8>(), bytes.len());
        out.add(bytes.len()).write(0);
    }
    Ok(out)
}

/// Release a C-string copy returned by [`cstring_from_string_copy`].
///
/// # Safety
///
/// `value` must be null or an outstanding result from
/// [`cstring_from_string_copy`].
pub unsafe fn cstring_copy_release(value: *mut c_char) {
    // SAFETY: this is the documented allocator pair for the copied C string.
    unsafe { crate::mem::buf_free(value.cast()) };
}

/// Borrow the complete UTF-8 byte contents of a managed string.
///
/// Null is the canonical empty value and returns an empty slice.
///
/// # Safety
///
/// `value` must be null or a live managed string handle, and the returned
/// borrow must not outlive an owning reference to that handle.
#[must_use]
pub unsafe fn string_as_bytes<'a>(value: *const HewString) -> &'a [u8] {
    if value.is_null() {
        return &[];
    }
    // SAFETY: the handle contract gives a live initialized header.
    let len = unsafe { (*header(value)).byte_len };
    // SAFETY: the allocation carries exactly `len` initialized payload bytes.
    unsafe { slice::from_raw_parts(data(value), len) }
}

/// Borrow the complete text contents of a managed string.
///
/// # Safety
///
/// The requirements of [`string_as_bytes`] apply. Every managed allocation is
/// constructed from valid UTF-8, so no validation is repeated here.
#[must_use]
pub unsafe fn string_as_str<'a>(value: *const HewString) -> &'a str {
    // SAFETY: managed-string construction establishes the UTF-8 invariant.
    unsafe { str::from_utf8_unchecked(string_as_bytes(value)) }
}

/// Number of Unicode scalar values, in constant time.
///
/// # Safety
/// `value` must be null or a live managed string handle.
#[must_use]
pub unsafe fn string_scalar_len(value: *const HewString) -> usize {
    if value.is_null() {
        return 0;
    }
    // SAFETY: the caller holds a live initialized allocation.
    unsafe { (*header(value)).scalar_len }
}

/// Byte offset of a scalar boundary, including the end boundary.
/// At most 31 scalars are skipped, independent of string length.
///
/// # Safety
/// `value` must be null or a live managed string handle.
#[must_use]
pub unsafe fn string_scalar_byte_offset(value: *const HewString, index: usize) -> Option<usize> {
    // SAFETY: the caller holds the managed owner throughout this read.
    let scalar_len = unsafe { string_scalar_len(value) };
    if index > scalar_len {
        return None;
    }
    if value.is_null() {
        return Some(0);
    }
    // SAFETY: a non-null managed handle has an initialized header.
    let byte_len = unsafe { (*header(value)).byte_len };
    if index == scalar_len {
        return Some(byte_len);
    }
    if byte_len == scalar_len {
        return Some(index);
    }
    // SAFETY: construction initialized an entry for each group of scalars.
    let offset = unsafe { *index_table(value).add(index / INDEX_STRIDE) };
    // SAFETY: both offsets name scalar boundaries within the live UTF-8 bytes.
    (unsafe { string_as_str(value) })[offset..]
        .char_indices()
        .nth(index % INDEX_STRIDE)
        .map(|(tail, _)| offset + tail)
}

/// Read one Unicode scalar in constant time, or report an absent index.
///
/// # Safety
/// `value` must be null or a live managed string handle.
#[must_use]
pub unsafe fn string_scalar_at(value: *const HewString, index: usize) -> Option<char> {
    // SAFETY: all reads borrow the same live owner.
    unsafe {
        if index >= string_scalar_len(value) {
            return None;
        }
        let offset = string_scalar_byte_offset(value, index)?;
        string_as_str(value)[offset..].chars().next()
    }
}

/// Scalar offset of a byte boundary found by a UTF-8 substring search.
/// The index selects a group by binary search, then scans at most one group.
///
/// # Safety
/// `value` must be null or a live managed string handle.
#[must_use]
pub unsafe fn string_byte_scalar_offset(value: *const HewString, offset: usize) -> Option<usize> {
    // SAFETY: the caller holds the managed owner throughout this read.
    let text = unsafe { string_as_str(value) };
    if !text.is_char_boundary(offset) {
        return None;
    }
    // SAFETY: the same managed owner remains live.
    let scalar_len = unsafe { string_scalar_len(value) };
    if offset == text.len() {
        return Some(scalar_len);
    }
    if scalar_len == text.len() {
        return Some(offset);
    }
    // SAFETY: non-ASCII strings initialize exactly this many index entries.
    let table =
        unsafe { slice::from_raw_parts(index_table(value), scalar_len.div_ceil(INDEX_STRIDE)) };
    let group = table.partition_point(|start| *start <= offset) - 1;
    Some(group * INDEX_STRIDE + text[table[group]..offset].chars().count())
}

/// Append a borrowed string to an owned receiver, copying shared storage.
/// Unique storage grows geometrically; only newly appended scalars are indexed.
///
/// # Safety
/// `value` transfers one live owner (or null); `suffix` is borrowed and must
/// remain live through this call. It may name the receiver's allocation.
#[must_use]
pub unsafe fn string_append(value: *mut HewString, suffix: *const HewString) -> *mut HewString {
    // SAFETY: both handles remain live until the receiver is consumed below.
    unsafe {
        if value.cast_const() == suffix && !value.is_null() {
            // A self-append can reallocate its source. Copy the appended bytes
            // before consuming that source; their size is the appended size.
            let bytes = string_as_bytes(suffix).to_vec();
            return append_utf8(value, &bytes, string_scalar_len(suffix));
        }
        append_utf8(value, string_as_bytes(suffix), string_scalar_len(suffix))
    }
}

/// Append one scalar to an owned receiver, copying shared storage.
///
/// # Safety
/// `value` transfers one live owner or is null.
#[must_use]
pub unsafe fn string_push(value: *mut HewString, scalar: char) -> *mut HewString {
    let mut bytes = [0u8; 4];
    // SAFETY: char encoding is valid UTF-8 and contains exactly one scalar.
    unsafe { append_utf8(value, scalar.encode_utf8(&mut bytes).as_bytes(), 1) }
}

/// Exact payload capacity and COW uniqueness before appending UTF-8 bytes.
/// The allocation path consumes this same growth policy.
/// # Safety
/// `value` is a live borrowed string or null, with no concurrent mutation.
#[must_use]
pub unsafe fn string_append_plan(value: *const HewString, added: usize) -> Option<(usize, bool)> {
    if value.is_null() {
        return Some((added, true));
    }
    // SAFETY: the caller pins this immutable header until mutation commits.
    let old = unsafe { &*header(value) };
    let length = old.byte_len.checked_add(added)?;
    Some((
        append_capacity(old.capacity, length),
        old.rc.load(Ordering::Acquire) == 1,
    ))
}

fn append_capacity(old: usize, length: usize) -> usize {
    if length > old {
        length.max(old.saturating_mul(2)).max(16)
    } else {
        old
    }
}

unsafe fn append_utf8(
    mut value: *mut HewString,
    suffix: &[u8],
    added_scalars: usize,
) -> *mut HewString {
    if suffix.is_empty() {
        return value;
    }
    if value.is_null() {
        // SAFETY: callers supply valid UTF-8 and the empty receiver has no owner.
        return unsafe { string_alloc_nonnull(suffix.as_ptr(), suffix.len()) };
    }
    // SAFETY: the transferred receiver is a live allocation throughout the
    // capacity change. A shared receiver is only read, then released once.
    unsafe {
        let old = &*header(value);
        let old_len = old.byte_len;
        let old_scalars = old.scalar_len;
        let old_capacity = old.capacity;
        let old_indexed = old_scalars != old_len;
        let new_len = old_len
            .checked_add(suffix.len())
            .unwrap_or_else(|| std::process::abort());
        let new_scalars = old_scalars
            .checked_add(added_scalars)
            .unwrap_or_else(|| std::process::abort());
        let indexed = new_scalars != new_len;
        let capacity = append_capacity(old_capacity, new_len);
        let total = allocation_size(capacity, indexed);
        if old.rc.load(Ordering::Acquire) != 1 {
            let allocation = crate::mem::buf_try_alloc(total).cast::<HewStringHeader>();
            if allocation.is_null() {
                std::process::abort();
            }
            allocation.write(HewStringHeader {
                byte_len: new_len,
                capacity,
                scalar_len: new_scalars,
                rc: AtomicU32::new(1),
            });
            let new_value = allocation.cast::<HewString>();
            core::ptr::copy_nonoverlapping(data(value), data(new_value).cast_mut(), old_len);
            core::ptr::copy_nonoverlapping(
                suffix.as_ptr(),
                data(new_value).cast_mut().add(old_len),
                suffix.len(),
            );
            if indexed {
                write_index(new_value, 0, 0);
            }
            string_release(value);
            return new_value;
        }
        if capacity != old_capacity || indexed != old_indexed {
            value = crate::mem::buf_realloc(value.cast(), total).cast();
            if old_indexed {
                // The index moves when the UTF-8 capacity grows. Copy permits
                // overlap and runs before appended bytes overwrite its old area.
                core::ptr::copy(
                    value.cast::<u8>().add(index_offset(old_capacity)),
                    value.cast::<u8>().add(index_offset(capacity)),
                    old_scalars.div_ceil(INDEX_STRIDE) * core::mem::size_of::<usize>(),
                );
            }
        }
        let current = &mut *value.cast::<HewStringHeader>();
        current.byte_len = new_len;
        current.capacity = capacity;
        current.scalar_len = new_scalars;
        core::ptr::copy_nonoverlapping(
            suffix.as_ptr(),
            data(value).cast_mut().add(old_len),
            suffix.len(),
        );
        if indexed {
            if old_indexed {
                write_index(value, old_len, old_scalars);
            } else {
                write_index(value, 0, 0);
            }
        }
    }
    value
}

/// Retain one additional owner of a managed string and return the same handle.
///
/// Null is the canonical empty value and remains null.
///
/// # Safety
///
/// `value` must be null or a live managed string handle.
#[must_use]
pub unsafe fn string_retain(value: *const HewString) -> *mut HewString {
    if value.is_null() {
        return core::ptr::null_mut();
    }
    // SAFETY: the caller supplies a live managed allocation.
    let rc = unsafe { &(*header(value)).rc };
    let old = rc.fetch_add(1, Ordering::Relaxed);
    if old > MAX_REFCOUNT {
        std::process::abort();
    }
    value.cast_mut()
}

/// Release one owner of a managed string.
///
/// Null is the canonical empty value and is a no-op.
///
/// # Safety
///
/// `value` must be null or a live managed string owner that has not already
/// been released or transferred.
pub unsafe fn string_release(value: *mut HewString) {
    if value.is_null() {
        return;
    }
    // SAFETY: the caller supplies a live managed allocation.
    let rc = unsafe { &(*header(value)).rc };
    match rc.fetch_sub(1, Ordering::Release) {
        1 => {
            fence(Ordering::Acquire);
            // SAFETY: this was the final owner and the handle is the allocation base.
            unsafe { crate::mem::buf_free(value.cast()) };
        }
        0 => std::process::abort(),
        _ => {}
    }
}

#[cfg(test)]
mod tests;
