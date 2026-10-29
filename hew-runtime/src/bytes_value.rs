//! Canonical C ABI representation of a bytes value.

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
