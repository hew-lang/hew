//! Common C ABI helpers used across runtime modules.
//!
//! These functions handle the repetitive patterns of converting between Rust
//! and C types in `#[no_mangle] extern "C"` functions: allocating
//! `malloc`-backed C strings, converting `*const c_char` to `&str`, etc.

use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::atomic::{AtomicU32, Ordering};

/// Allocate a NUL-terminated, **header-aware** C string, copying `len` bytes
/// from `src`. Returns null on allocation failure.
///
/// This is the single shared Hew-string producer: it delegates to
/// [`alloc_cstring`], so every `str_to_malloc` / `malloc_cstring` caller (the
/// ~80 std `-> string` externs, runtime string ops, `env`, `http/server`)
/// produces a header-aware string. The returned pointer carries a 16-byte
/// header at `data - CSTRING_HEADER_SIZE` and MUST be released through
/// [`free_cstring`] / `hew_string_drop` — never bare `libc::free`, which would
/// free `data` instead of the allocation base and corrupt the heap.
///
/// # Safety
///
/// If `len > 0`, `src` must point to at least `len` readable bytes.
#[must_use]
pub unsafe fn malloc_cstring(src: *const u8, len: usize) -> *mut c_char {
    // SAFETY: caller guarantees src is valid for len bytes when len > 0; the
    // header-aware allocator copies exactly len bytes and NUL-terminates.
    unsafe { alloc_cstring(src, len) } // CSTRING-ALLOC: str-open (SHARED producer — header-aware via alloc_cstring; S1 flip propagates to all `-> string` externs)
}

/// Copy a Rust `&str` into a **header-aware**, NUL-terminated C string.
///
/// Returns null on allocation failure. The result must be released through
/// [`free_cstring`] / `hew_string_drop`, never bare `libc::free`.
#[must_use]
pub fn str_to_malloc(s: &str) -> *mut c_char {
    alloc_cstring_from_str(s)
}

/// Allocate `src.len()` bytes via `libc::malloc`, copying all bytes from
/// `src`. If `src` is empty, allocates 1 byte (a sentinel) so the returned
/// pointer is always valid for a subsequent `libc::free`. Returns null on
/// allocation failure.
///
/// Null handling is the caller's responsibility.
#[must_use]
pub fn malloc_bytes(src: &[u8]) -> *mut u8 {
    let len = src.len();
    // Allocate at least 1 byte so malloc(0) implementation-defined behaviour
    // is avoided and the sentinel pointer can always be freed by the caller.
    // SAFETY: We request len.max(1) bytes from malloc; it returns a valid
    // pointer or null.
    let ptr = unsafe { libc::malloc(len.max(1)) }.cast::<u8>(); // ALLOCATOR-PAIRING: libc
    if ptr.is_null() {
        return ptr;
    }
    if len > 0 {
        // SAFETY: src is a valid Rust slice of len bytes; ptr is freshly
        // allocated with len bytes, so both regions are valid and
        // non-overlapping.
        unsafe { std::ptr::copy_nonoverlapping(src.as_ptr(), ptr, len) };
    }
    ptr
}

/// Allocate a 1-byte sentinel via `libc::malloc`. Use this when a zero-length
/// allocation is needed but a non-null, freeable pointer is required (e.g. an
/// empty body buffer). Returns null on allocation failure.
///
/// Null handling is the caller's responsibility.
#[must_use]
pub fn malloc_empty() -> *mut u8 {
    // SAFETY: We request 1 byte from malloc; it returns a valid pointer or null.
    unsafe { libc::malloc(1) }.cast::<u8>() // ALLOCATOR-PAIRING: libc
}

/// Extract a NUL-terminated C string pointer into a `&str`, returning `None`
/// if the pointer is null or contains invalid UTF-8.
///
/// # Safety
///
/// If non-null, `ptr` must point to a valid NUL-terminated C string.
#[must_use]
pub unsafe fn cstr_to_str<'a>(ptr: *const c_char) -> Option<&'a str> {
    if ptr.is_null() {
        return None;
    }
    // SAFETY: Caller guarantees ptr is a valid NUL-terminated C string.
    unsafe { CStr::from_ptr(ptr) }.to_str().ok()
}

/// Extract a NUL-terminated C string pointer into an owned `String`, replacing
/// invalid UTF-8 sequences with U+FFFD.
///
/// Returns an empty string when `ptr` is null.
///
/// # Safety
///
/// If non-null, `ptr` must point to a valid NUL-terminated C string.
#[must_use]
pub unsafe fn cstr_to_string_lossy(ptr: *const c_char) -> String {
    if ptr.is_null() {
        return String::new();
    }
    // SAFETY: Caller guarantees ptr is a valid NUL-terminated C string.
    unsafe { CStr::from_ptr(ptr) }
        .to_string_lossy()
        .into_owned()
}

// ---------------------------------------------------------------------------
// Header-aware C-string allocator
// ---------------------------------------------------------------------------
//
// This is the unified allocator/free pair that Hew's C-string plumbing is
// migrating onto. As of P2a the refcount header is **LIVE for `String`**:
// [`cstring_retain`] adds an owner (refcount bump), [`free_cstring`] releases
// one owner and frees only at zero, and [`cstring_ensure_unique`] forks a
// shared string copy-on-write. Memory ordering matches `std::sync::Arc` /
// `hew-runtime/src/bytes.rs`: retain `fetch_add(Relaxed)`, release
// `fetch_sub(Release)` then `fence(Acquire)` before free. A freshly produced
// string still starts at `rc == 1`, so a producer→single-drop round trip is
// behaviour-identical to the inert-header era.
//
// Container string *elements*: `vec<string>` elements are now header-aware and
// refcounted (W5.011 P2b-vec — `hew_vec_push_str`/`set_str` copy in a
// header-bearing element via `malloc_cstring`), so they MAY ride this refcounted
// path. `hashmap`/`hashset` elements remain bare-`strdup` **headerless** until
// the P2b-maps migration and MUST NOT reach this refcounted path — see
// `hew-mir/tests/cstring_container_domain_canary.rs`.
//
// ## Heap layout
//
// ```text
// [ magic:u64 | rc:AtomicU32 | reserved:u32 | data[0..len] | NUL ]
//  \________________ HEADER (16 bytes) ________________/
// ```
//
// The pointer returned by [`alloc_cstring`] points to `data[0]`; the header
// lives at `data - CSTRING_HEADER_SIZE`. `data` is suitable for use as a
// NUL-terminated `*mut c_char` exactly like the legacy `malloc_cstring` result.
//
// ## What the sentinel does — and does NOT — do
//
// The magic is a **best-effort integrity check on a pointer the caller already
// knows came from [`alloc_cstring`]**. It catches *corruption of a
// header-allocated pointer* and (best-effort) a *double-free*. It is NOT a way
// to classify arbitrary or foreign pointers: [`free_cstring`] must compute
// `base = data - CSTRING_HEADER_SIZE` and read the header BEFORE it can inspect
// the magic, and for a pointer that did not come from [`alloc_cstring`] (e.g. a
// plain `strdup`/`malloc` C string sitting at the start of its own allocation)
// that read is out-of-bounds / invalid-provenance UB. The sentinel therefore
// cannot safely detect a still-headerless or otherwise mis-provenanced pointer.
//
// Correctness of the eventual wiring comes from **migrating every site that can
// reach [`free_cstring`] together** (out-of-band provenance), so that only
// header-allocated pointers ever reach it — NOT from runtime probing of unknown
// pointers.
//
// ## Header sizing and alignment
//
// A full `u64` magic makes an accidental false-accept on *corrupted* header
// bytes astronomically unlikely (~1 in 1.8e19) versus a `u32` magic (~1 in
// 4e9). The struct is `magic:u64 + rc:AtomicU32 + reserved:u32` = 16 bytes with
// `align_of == 8`. `libc::malloc` guarantees alignment suitable for any
// fundamental type (>= 8 on every supported target, including wasm32), so
// `base` is >= 8-byte aligned and `data = base + 16` is therefore 8-byte
// aligned — more than enough for `c_char` and for the header's own fields.
// `bytes.rs` keeps its own 8-byte `[rc|cap]` layout because it needs a live
// capacity field, not a magic; unifying the two headers is deferred.

/// Size of the header preceding the data region, in bytes. Equals
/// `size_of::<CStringHeader>()`; chosen so `data` (= `base + CSTRING_HEADER_SIZE`)
/// inherits malloc's alignment guarantee (>= 8 bytes).
pub const CSTRING_HEADER_SIZE: usize = 16;

/// Magic sentinel written into every header-aware C-string allocation and
/// verified on every header-aware free. Spells `HEW_CSTR` in ASCII.
const CSTRING_MAGIC: u64 = 0x4845_575F_4353_5452;

/// Poison value stamped over the magic on free as a best-effort debug aid.
const CSTRING_POISON: u64 = 0xDEAD_BEEF_DEAD_BEEF;

/// Header preceding the data region of a header-aware C string.
///
/// `repr(C)` pins the field order so the magic always lives at offset 0 (where
/// the integrity check reads it). `rc` is **inert** (always 1).
#[repr(C)]
struct CStringHeader {
    magic: u64,
    rc: AtomicU32,
    _reserved: u32,
}

/// Allocate a NUL-terminated, header-aware C string via `libc::malloc`, copying
/// `len` bytes from `src`. The header is initialized with `rc == 1` and the
/// magic sentinel; the returned pointer is `data = base + CSTRING_HEADER_SIZE`.
///
/// Returns null on allocation failure **or** if the requested size would
/// overflow / exceed `isize::MAX` (fail-closed: a wrapped size would otherwise
/// under-allocate and the header write / copy would overflow the buffer).
///
/// This is the header-aware replacement for [`malloc_cstring`]. The returned
/// pointer must be freed with [`free_cstring`] (never bare `libc::free`, which
/// would free `data` instead of `base` and corrupt the heap).
///
/// # Safety
///
/// If `len > 0`, `src` must point to at least `len` readable bytes.
#[must_use]
pub unsafe fn alloc_cstring(src: *const u8, len: usize) -> *mut c_char {
    // Reserve `len` data bytes + 1 for the NUL terminator. `alloc_cstring_data`
    // adds the header and fails closed (null) on overflow / allocation failure.
    let Some(data_len) = len.checked_add(1) else {
        return std::ptr::null_mut();
    };
    let data = alloc_cstring_data(data_len);
    if data.is_null() {
        return std::ptr::null_mut();
    }
    let bytes = data.cast::<u8>();
    if len > 0 {
        // SAFETY: caller guarantees src is valid for len bytes; data is freshly
        // allocated with len + 1 bytes, so both regions are valid and disjoint.
        unsafe { std::ptr::copy_nonoverlapping(src, bytes, len) };
    }
    // SAFETY: bytes + len is the final byte of the data region (len + 1 bytes).
    unsafe { *bytes.add(len) = 0 };
    data
}

/// Allocate a header-aware buffer with `data_len` bytes of **uninitialized**
/// data space, returning the data pointer (`base + CSTRING_HEADER_SIZE`). The
/// header is initialized (magic + `rc == 1`); the caller MUST write all
/// `data_len` data bytes itself, **including any NUL terminator** it needs.
///
/// Returns null on allocation failure **or** if the size would overflow /
/// exceed `isize::MAX` (fail-closed). The result must be released through
/// [`free_cstring`] — never bare `libc::free`.
///
/// This is the header-aware replacement for a bare `libc::malloc(data_len)` in
/// a string producer that fills its buffer incrementally (e.g. `hew_string_concat`,
/// `hew_string_replace`), where the copy cannot be expressed as a single
/// [`alloc_cstring`] call.
#[must_use]
pub fn alloc_cstring_data(data_len: usize) -> *mut c_char {
    // Total = HEADER_SIZE + data_len. Compute with checked arithmetic and cap at
    // isize::MAX: a wrapped or over-large size would under-allocate and make the
    // header write overflow the buffer, and the size must be a valid isize for
    // pointer arithmetic. Fail closed to null, consistent with the malloc-null
    // path below.
    let total = match CSTRING_HEADER_SIZE.checked_add(data_len) {
        Some(n) if isize::try_from(n).is_ok() => n,
        _ => return std::ptr::null_mut(),
    };
    // SAFETY: total >= CSTRING_HEADER_SIZE > 0; malloc returns a valid pointer or null.
    let base = unsafe { libc::malloc(total) }.cast::<u8>(); // ALLOCATOR-PAIRING: cstring
    if base.is_null() {
        return std::ptr::null_mut();
    }
    // Write the header. `base` is malloc-aligned (>= 8 on every target),
    // satisfying `align_of::<CStringHeader>()` (8).
    #[expect(
        clippy::cast_ptr_alignment,
        reason = "base is libc::malloc-aligned (>= 8 on every target), \
                  satisfying CStringHeader's 8-byte alignment"
    )]
    let header = base.cast::<CStringHeader>();
    // SAFETY: `base` points to `total >= CSTRING_HEADER_SIZE` writable bytes and
    // is suitably aligned for `CStringHeader`.
    unsafe {
        std::ptr::write(
            header,
            CStringHeader {
                magic: CSTRING_MAGIC,
                rc: AtomicU32::new(1),
                _reserved: 0,
            },
        );
    }
    // SAFETY: data region begins at base + HEADER_SIZE, valid for data_len bytes.
    unsafe { base.add(CSTRING_HEADER_SIZE).cast::<c_char>() }
}

/// Copy a Rust `&str` into a header-aware, NUL-terminated C string.
/// Header-aware replacement for [`str_to_malloc`]. Returns null on failure.
#[must_use]
pub fn alloc_cstring_from_str(s: &str) -> *mut c_char {
    // SAFETY: s.as_ptr() is valid for s.len() bytes.
    unsafe { alloc_cstring(s.as_ptr(), s.len()) }
}

/// Recover the allocation base from a header-aware `data` pointer and verify the
/// magic sentinel. Returns `Some(base)` if the magic is intact, `None` if it is
/// absent (the header was clobbered).
///
/// Note: when [`free_cstring`] poisons the magic just before releasing the
/// allocation, a *subsequent* call on the same pointer may observe the poison
/// and return `None` — but only as a best-effort debugging aid, NOT a contract.
/// Reading the header of an already-freed allocation is itself a use-after-free
/// (the block may have been reused); the SAFETY precondition below forbids it.
///
/// # Safety
///
/// `data` MUST be a pointer previously returned by [`alloc_cstring`] (non-null)
/// **whose allocation is still live — i.e. not already freed through
/// [`free_cstring`]** (matching `free_cstring`'s "not already freed"
/// precondition). This is a hard precondition: the function reads
/// `data - CSTRING_HEADER_SIZE`, which is only a valid, in-bounds read for a
/// real, still-live `alloc_cstring` allocation. Passing any other pointer (a
/// plain `strdup`/`malloc` C string, a stack pointer, a foreign pointer) is
/// out-of-bounds / invalid-provenance UB, and passing a freed pointer is
/// use-after-free UB — the magic check cannot rescue either, because the
/// offending read happens first. This function does NOT classify unknown or
/// freed pointers; provenance and liveness are the caller's responsibility.
#[must_use]
unsafe fn validate_cstring_header(data: *mut c_char) -> Option<*mut u8> {
    // SAFETY: per precondition, data came from alloc_cstring, so data - HEADER
    // is the in-bounds start of that allocation.
    let base = unsafe { data.cast::<u8>().sub(CSTRING_HEADER_SIZE) };
    #[expect(
        clippy::cast_ptr_alignment,
        reason = "base is libc::malloc-aligned, satisfying CStringHeader's 8-byte alignment"
    )]
    let header = base.cast::<CStringHeader>();
    // SAFETY: base is the allocation start; the header is initialized by
    // alloc_cstring before the pointer is ever handed out.
    let magic = unsafe { (*header).magic };
    if magic == CSTRING_MAGIC {
        Some(base)
    } else {
        None
    }
}

/// Borrow the atomic refcount stored in the header preceding a header-aware
/// `data` pointer. The refcount lives at offset 8 within the 16-byte header
/// (`[magic:u64 | rc:AtomicU32 | reserved:u32]`).
///
/// # Safety
///
/// `data` MUST be a pointer previously returned by [`alloc_cstring`] /
/// [`alloc_cstring_from_str`] (non-null, still live, header intact). Reading
/// `data - CSTRING_HEADER_SIZE` is only valid for such a pointer — see
/// [`validate_cstring_header`] for the full provenance/liveness contract.
/// Static-string literals (no header) MUST be filtered out by the caller
/// (`hew_string_drop` / `hew_string_clone` skip them via `is_static_string`).
#[inline]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "base is libc::malloc-aligned (>= 8 on every target), satisfying \
              CStringHeader's 8-byte alignment; rc (AtomicU32) needs 4"
)]
unsafe fn cstring_rc<'a>(data: *mut c_char) -> &'a AtomicU32 {
    // SAFETY: per precondition, data came from alloc_cstring, so data - HEADER
    // is the in-bounds start of that allocation and the header is initialized.
    let base = unsafe { data.cast::<u8>().sub(CSTRING_HEADER_SIZE) };
    let header = base.cast::<CStringHeader>();
    // SAFETY: base is the allocation start; rc is initialized by alloc_cstring.
    unsafe { &(*header).rc }
}

/// Refcount saturation threshold, mirroring `std::sync::Arc`'s `MAX_REFCOUNT`
/// discipline. If a retain ever observes an old count above half the `u32`
/// range the count has run away (a leaked retain loop) and is one step from
/// wrapping; a later release would then mistake a still-aliased buffer for the
/// final owner and free it — a use-after-free. We abort well before the wrap.
const CSTRING_RC_MAX: u32 = u32::MAX / 2;

/// Pure predicate: would retaining a string whose current (pre-increment)
/// refcount is `old` push the count past the safe saturation threshold?
/// Extracted so the abort condition is unit-testable without performing
/// `2^31` real retains. Mirrors `bytes.rs::bytes_rc_would_overflow`.
#[inline]
#[must_use]
fn cstring_rc_would_overflow(old: u32) -> bool {
    old > CSTRING_RC_MAX
}

/// Atomically retain (add one owner of) a header-aware C string by bumping its
/// refcount. No-op on null. String analogue of
/// `hew-runtime/src/bytes.rs::hew_bytes_clone_ref`.
///
/// Memory ordering matches `std::sync::Arc`: `fetch_add(Relaxed)` — a retain
/// only needs atomicity, not synchronization (the release/free path carries the
/// happens-before edge).
///
/// Overflow is fail-closed: if the pre-increment count is already past
/// [`CSTRING_RC_MAX`] the refcount is about to wrap, so we abort rather than
/// let a future release free a buffer that still has live aliases (the
/// `std::sync::Arc` `MAX_REFCOUNT` discipline).
///
/// # Safety
///
/// `data` must be null or a still-live pointer returned by [`alloc_cstring`] /
/// [`alloc_cstring_from_str`]. Static-string literals (no header) MUST be
/// filtered out by the caller (`hew_string_clone` skips them via
/// `is_static_string` before reaching here).
pub unsafe fn cstring_retain(data: *mut c_char) {
    if data.is_null() {
        return;
    }
    // SAFETY: caller guarantees data is a live header-aware allocation.
    let rc = unsafe { cstring_rc(data) };
    let old = rc.fetch_add(1, Ordering::Relaxed);
    if cstring_rc_would_overflow(old) {
        eprintln!(
            "hew-cabi: cstring_retain refcount overflow (old={old} > {CSTRING_RC_MAX}); \
             aborting to avoid a use-after-free when the count wraps."
        );
        // SAFETY: abort is always safe; it does not return.
        unsafe { libc::abort() };
    }
}

/// Copy-on-write fork for a header-aware C string. If the string is uniquely
/// owned (`rc == 1`) returns `data` unchanged. Otherwise allocates a fresh
/// header-aware copy of the NUL-terminated contents, releases the shared
/// reference, and returns the new (uniquely-owned, `rc == 1`) pointer. String
/// analogue of `hew-runtime/src/bytes.rs::ensure_unique`.
///
/// Aborts (fail-loud) if the fork allocation fails — it cannot return a shared
/// pointer while promising uniqueness, mirroring `bytes.rs`'s OOM-abort on the
/// fork allocation.
///
/// # Safety
///
/// `data` must be null or a still-live header-aware pointer from
/// [`alloc_cstring`]. Static-string literals (no header) MUST be filtered out
/// by the caller before reaching here.
#[must_use]
pub unsafe fn cstring_ensure_unique(data: *mut c_char) -> *mut c_char {
    if data.is_null() {
        return data;
    }
    // SAFETY: caller guarantees data is a live header-aware allocation.
    let rc = unsafe { cstring_rc(data) };
    if rc.load(Ordering::Acquire) == 1 {
        return data;
    }
    // Shared — clone the active region (the NUL-terminated contents) into a
    // fresh, uniquely-owned header-aware allocation.
    // SAFETY: data is a valid NUL-terminated C string.
    let len = unsafe { libc::strlen(data) };
    // SAFETY: data is readable for `len` bytes; alloc_cstring copies them and
    // NUL-terminates.
    let fresh = unsafe { alloc_cstring(data.cast::<u8>(), len) };
    if fresh.is_null() {
        eprintln!(
            "hew-cabi: cstring_ensure_unique: copy-on-write fork allocation \
             failed; aborting to avoid returning a shared buffer as unique."
        );
        // SAFETY: abort is always safe; it does not return.
        unsafe { libc::abort() };
    }
    // Drop the shared reference now that we hold a private copy.
    // SAFETY: data is a live header-aware allocation per the precondition.
    unsafe { free_cstring(data) };
    fresh
}

/// Free a header-aware C string produced by [`alloc_cstring`] /
/// [`alloc_cstring_from_str`].
///
/// Computes `base = data - CSTRING_HEADER_SIZE`, verifies the magic sentinel,
/// then **releases one owner**: decrements the refcount and frees `base` only
/// when the count reaches zero. As of P2a the refcount is LIVE — a retained
/// (`rc > 1`) string is not freed until every owner has released it. A string
/// that was never shared starts at `rc == 1`, so a single release frees it,
/// behaviour-identical to the inert-header era.
///
/// Memory ordering matches `std::sync::Arc` / `hew-runtime/src/bytes.rs`:
/// `fetch_sub(Release)` then, on the final release, `fence(Acquire)` before
/// `libc::free`.
///
/// **Integrity check (best-effort, CLAUDE §1):** the magic guards against a
/// *corrupted header* on a header-allocated pointer; if it is absent, this
/// aborts rather than freeing a bad base. It does **not** detect a
/// mis-provenanced (e.g. still-headerless `strdup`/`malloc`) pointer — see the
/// safety precondition: such a pointer is UB before the check runs. The wiring's
/// safety comes from migrating every reaching site in lockstep so only
/// header-allocated pointers ever reach here, not from this probe.
///
/// Null is accepted (no-op).
///
/// # Safety
///
/// `data` must be null or a pointer previously returned by [`alloc_cstring`],
/// and not already released to zero. Passing any other non-null pointer is UB.
pub unsafe fn free_cstring(data: *mut c_char) {
    if data.is_null() {
        return;
    }
    // SAFETY: data is a real alloc_cstring result per the precondition.
    let Some(base) = (unsafe { validate_cstring_header(data) }) else {
        // The magic is absent on a pointer the caller asserted came from
        // alloc_cstring — the header is corrupted, or this is a double-free of a
        // pointer whose magic we poisoned. Either way, freeing `base` could
        // corrupt the heap: abort, never proceed.
        eprintln!(
            "hew-cabi: free_cstring: C-string header sentinel missing at {data:p} \
             (corrupted header or double-free). Aborting to avoid heap corruption."
        );
        // SAFETY: abort is always safe to call; it does not return.
        unsafe { libc::abort() };
    };
    // Release one owner. `fetch_sub(Release)` pairs with the `fence(Acquire)`
    // below exactly like `std::sync::Arc` and `bytes.rs`: every prior write by
    // any owner happens-before the deallocation.
    // SAFETY: data is a live header-aware allocation (validated above).
    let rc = unsafe { cstring_rc(data) };
    if rc.fetch_sub(1, Ordering::Release) != 1 {
        // Other owners remain; do not poison and do not free.
        return;
    }
    // Last owner — synchronize with all prior releases before reclaiming.
    std::sync::atomic::fence(Ordering::Acquire);
    // Best-effort double-free aid: stamp the magic with a poison so an immediate
    // re-free through this path is more likely to trip the check above. This is
    // NOT a reliable double-free guard — the write is immediately followed by
    // `libc::free`, so a later free reads freed memory (use-after-free, which
    // may fault or, if the block was reused, read arbitrary bytes). With correct
    // retain/release pairing the count only reaches zero once; the poison is a
    // debugging convenience.
    #[expect(
        clippy::cast_ptr_alignment,
        reason = "base is libc::malloc-aligned, satisfying CStringHeader's 8-byte alignment"
    )]
    let header = base.cast::<CStringHeader>();
    // SAFETY: base/header are valid; the refcount reached zero so we hold the
    // only reference.
    unsafe { (*header).magic = CSTRING_POISON };
    // SAFETY: base was allocated by alloc_cstring via libc::malloc.
    unsafe { libc::free(base.cast()) }; // ALLOCATOR-PAIRING: cstring
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;

    /// Helper: free a header-aware Hew string (reduces boilerplate).
    unsafe fn free_ptr(ptr: *mut c_char) {
        // SAFETY: `ptr` was produced by `malloc_cstring` / `str_to_malloc`, which
        // are header-aware (S1), so it must be released through `free_cstring`.
        unsafe { free_cstring(ptr) };
    }

    // ── malloc_cstring ───────────────────────────────────────────────────

    #[test]
    fn malloc_cstring_copies_exact_bytes() {
        let src = b"ABC";
        // SAFETY: src points to 3 valid bytes.
        let ptr = unsafe { malloc_cstring(src.as_ptr(), src.len()) };
        assert!(!ptr.is_null());
        // Verify each byte individually, plus the NUL terminator.
        let raw = ptr.cast::<u8>();
        // SAFETY: ptr is freshly allocated with 4 bytes (3 + NUL).
        unsafe {
            assert_eq!(*raw, b'A');
            assert_eq!(*raw.add(1), b'B');
            assert_eq!(*raw.add(2), b'C');
            assert_eq!(*raw.add(3), 0u8, "missing NUL terminator");
            free_ptr(ptr);
        }
    }

    #[test]
    fn malloc_cstring_zero_length_produces_empty_cstr() {
        // SAFETY: len=0 means src is never read.
        let ptr = unsafe { malloc_cstring(std::ptr::null(), 0) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated string of length 0.
        unsafe {
            assert_eq!(*ptr.cast::<u8>(), 0u8, "should be only a NUL byte");
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "");
            free_ptr(ptr);
        }
    }

    #[test]
    fn malloc_cstring_single_byte() {
        let src = b"X";
        // SAFETY: src points to 1 valid byte.
        let ptr = unsafe { malloc_cstring(src.as_ptr(), 1) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is a freshly allocated 2-byte buffer.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, "X");
            free_ptr(ptr);
        }
    }

    // ── str_to_malloc ────────────────────────────────────────────────────

    #[test]
    fn str_to_malloc_roundtrip() {
        let input = "hello, hew!";
        let ptr = str_to_malloc(input);
        assert!(!ptr.is_null());
        // SAFETY: ptr was just allocated by str_to_malloc with a NUL terminator.
        let recovered = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        assert_eq!(recovered, input);
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { free_ptr(ptr) };
    }

    #[test]
    fn str_to_malloc_empty() {
        let ptr = str_to_malloc("");
        assert!(!ptr.is_null());
        // SAFETY: ptr was just allocated by str_to_malloc with a NUL terminator.
        let recovered = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap();
        assert_eq!(recovered, "");
        // SAFETY: ptr was allocated with libc::malloc.
        unsafe { free_ptr(ptr) };
    }

    #[test]
    fn str_to_malloc_multibyte_utf8() {
        // Covers multi-byte sequences: 2-byte (é), 3-byte (€), 4-byte (🍁).
        let input = "café €42 🍁";
        let ptr = str_to_malloc(input);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated string.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered, input);
            assert_eq!(recovered.len(), input.len(), "byte length must match");
            free_ptr(ptr);
        }
    }

    #[test]
    fn str_to_malloc_embedded_nul_bytes_are_not_special() {
        // Rust &str can't contain NUL, but we verify the contract: the full
        // byte content of the str is copied and the result is NUL-terminated.
        let input = "ab";
        let ptr = str_to_malloc(input);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a freshly allocated 3-byte buffer ("ab\0").
        unsafe {
            let raw = ptr.cast::<u8>();
            assert_eq!(*raw, b'a');
            assert_eq!(*raw.add(1), b'b');
            assert_eq!(*raw.add(2), 0u8);
            free_ptr(ptr);
        }
    }

    #[test]
    fn str_to_malloc_long_string() {
        // Exercise a longer allocation to catch off-by-one in the copy.
        let input: String = "hew".repeat(1000);
        let ptr = str_to_malloc(&input);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated string.
        unsafe {
            let recovered = CStr::from_ptr(ptr).to_str().unwrap();
            assert_eq!(recovered.len(), 3000);
            assert_eq!(recovered, input);
            free_ptr(ptr);
        }
    }

    // ── malloc_bytes ─────────────────────────────────────────────────────

    #[test]
    fn malloc_bytes_empty_returns_nonnull_and_freeable() {
        let ptr = malloc_bytes(&[]);
        assert!(!ptr.is_null(), "empty slice must yield a non-null sentinel");
        // SAFETY: ptr was allocated by malloc_bytes via libc::malloc.
        unsafe { libc::free(ptr.cast::<c_void>()) }; // ALLOCATOR-PAIRING: libc
    }

    #[test]
    fn malloc_bytes_roundtrips_1kb_buffer() {
        // Build a 1 KiB buffer with a known pattern.
        let src: Vec<u8> = (0u8..=255).cycle().take(1024).collect();
        let ptr = malloc_bytes(&src);
        assert!(!ptr.is_null());
        // SAFETY: ptr was allocated with 1024 bytes by malloc_bytes.
        unsafe {
            for (i, &expected) in src.iter().enumerate() {
                assert_eq!(*ptr.add(i), expected, "byte mismatch at index {i}");
            }
            libc::free(ptr.cast::<c_void>()); // ALLOCATOR-PAIRING: libc
        }
    }

    #[test]
    fn malloc_bytes_single_byte_roundtrip() {
        let src = [0x42u8];
        let ptr = malloc_bytes(&src);
        assert!(!ptr.is_null());
        // SAFETY: ptr points to 1 byte allocated by malloc_bytes.
        unsafe {
            assert_eq!(*ptr, 0x42);
            libc::free(ptr.cast::<c_void>()); // ALLOCATOR-PAIRING: libc
        }
    }

    // ── malloc_empty ─────────────────────────────────────────────────────

    #[test]
    fn malloc_empty_returns_nonnull_and_freeable() {
        let ptr = malloc_empty();
        assert!(
            !ptr.is_null(),
            "malloc_empty must return a non-null sentinel"
        );
        // SAFETY: ptr was allocated by malloc_empty via libc::malloc.
        unsafe { libc::free(ptr.cast::<c_void>()) }; // ALLOCATOR-PAIRING: libc
    }

    // ── cstr_to_str ──────────────────────────────────────────────────────

    #[test]
    fn cstr_to_str_null_returns_none() {
        // SAFETY: Passing null is the scenario under test.
        assert!(unsafe { cstr_to_str(std::ptr::null()) }.is_none());
    }

    #[test]
    fn cstr_to_str_valid() {
        let s = c"hello";
        // SAFETY: s is a valid C string literal.
        let result = unsafe { cstr_to_str(s.as_ptr()) };
        assert_eq!(result, Some("hello"));
    }

    #[test]
    fn cstr_to_str_empty_cstring() {
        let s = c"";
        // SAFETY: s is a valid (empty) C string literal.
        let result = unsafe { cstr_to_str(s.as_ptr()) };
        assert_eq!(result, Some(""));
    }

    #[test]
    fn cstr_to_str_invalid_utf8_returns_none() {
        // 0xFF is not valid in any UTF-8 sequence.
        let bytes: &[u8] = &[0xFF, 0xFE, 0x00];
        // SAFETY: bytes is a NUL-terminated buffer; invalid UTF-8 is intentional.
        let result = unsafe { cstr_to_str(bytes.as_ptr().cast::<c_char>()) };
        assert_eq!(result, None, "invalid UTF-8 should produce None");
    }

    #[test]
    fn cstr_to_str_multibyte_utf8() {
        let s = c"héllo 🍁";
        // SAFETY: s is a valid C string literal.
        let result = unsafe { cstr_to_str(s.as_ptr()) };
        assert_eq!(result, Some("héllo 🍁"));
    }

    // ── cstr_to_string_lossy ───────────────────────────────────────────────

    #[test]
    fn cstr_to_string_lossy_null_returns_empty_string() {
        // SAFETY: Passing null is the scenario under test.
        let result = unsafe { cstr_to_string_lossy(std::ptr::null()) };
        assert!(result.is_empty());
    }

    #[test]
    fn cstr_to_string_lossy_valid_utf8_preserves_text() {
        let s = c"metadata";
        // SAFETY: s is a valid C string literal.
        let result = unsafe { cstr_to_string_lossy(s.as_ptr()) };
        assert_eq!(result, "metadata");
    }

    #[test]
    fn cstr_to_string_lossy_invalid_utf8_replaces_bad_bytes() {
        let bytes: &[u8] = &[b'f', b'o', 0x80, 0x00];
        // SAFETY: bytes is a NUL-terminated buffer; invalid UTF-8 is intentional.
        let result = unsafe { cstr_to_string_lossy(bytes.as_ptr().cast::<c_char>()) };
        assert_eq!(result, "fo�");
    }

    // ── roundtrip: str_to_malloc → cstr_to_str ───────────────────────────

    #[test]
    fn str_to_malloc_then_cstr_to_str_roundtrip() {
        let original = "roundtrip through the ABI boundary";
        let ptr = str_to_malloc(original);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from str_to_malloc.
        unsafe {
            let recovered = cstr_to_str(ptr);
            assert_eq!(recovered, Some(original));
            free_ptr(ptr);
        }
    }

    #[test]
    fn roundtrip_empty_string() {
        let ptr = str_to_malloc("");
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from str_to_malloc.
        unsafe {
            let recovered = cstr_to_str(ptr);
            assert_eq!(recovered, Some(""));
            free_ptr(ptr);
        }
    }

    #[test]
    fn roundtrip_unicode() {
        let original = "日本語テスト 🎵";
        let ptr = str_to_malloc(original);
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid NUL-terminated C string from str_to_malloc.
        unsafe {
            let recovered = cstr_to_str(ptr);
            assert_eq!(recovered, Some(original));
            free_ptr(ptr);
        }
    }

    // ── header-aware allocator: alloc_cstring / free_cstring ──

    /// The data pointer must sit exactly `CSTRING_HEADER_SIZE` past the base,
    /// and the header must carry the magic sentinel with `rc == 1`.
    #[test]
    fn alloc_cstring_writes_header_and_data() {
        let src = b"ABC";
        // SAFETY: src points to 3 valid bytes.
        let data = unsafe { alloc_cstring(src.as_ptr(), src.len()) };
        assert!(!data.is_null());
        // SAFETY: data is a header-aware allocation just produced above.
        unsafe {
            // Data content + NUL terminator are byte-identical to malloc_cstring.
            let raw = data.cast::<u8>();
            assert_eq!(*raw, b'A');
            assert_eq!(*raw.add(1), b'B');
            assert_eq!(*raw.add(2), b'C');
            assert_eq!(*raw.add(3), 0u8, "missing NUL terminator");
            assert_eq!(CStr::from_ptr(data).to_str().unwrap(), "ABC");

            // Header lives at data - HEADER_SIZE with magic + inert rc == 1.
            let base = data.cast::<u8>().sub(CSTRING_HEADER_SIZE);
            #[expect(
                clippy::cast_ptr_alignment,
                reason = "base is malloc-aligned, satisfying CStringHeader's alignment"
            )]
            let header = base.cast::<CStringHeader>();
            assert_eq!((*header).magic, CSTRING_MAGIC, "magic sentinel missing");
            assert_eq!(
                (*header).rc.load(std::sync::atomic::Ordering::Relaxed),
                1,
                "refcount must be initialized to 1 (inert)"
            );

            // validate_cstring_header recovers exactly that base.
            assert_eq!(validate_cstring_header(data), Some(base));

            free_cstring(data);
        }
    }

    /// `data` inherits malloc's alignment guarantee (>= 8 bytes), which is the
    /// real cross-platform contract — enough for `c_char` and the header fields.
    #[test]
    fn alloc_cstring_data_is_aligned() {
        // SAFETY: zero-length alloc; src is never read.
        let data = unsafe { alloc_cstring(std::ptr::null(), 0) };
        assert!(!data.is_null());
        assert_eq!(
            (data as usize) % 8,
            0,
            "data pointer must be at least 8-byte aligned (malloc guarantee + 16B header)"
        );
        // SAFETY: header-aware allocation.
        unsafe {
            assert_eq!(*data.cast::<u8>(), 0u8, "empty string is a lone NUL");
            free_cstring(data);
        }
    }

    /// Round-trip across the full byte-length range, including multibyte UTF-8
    /// and a long allocation (off-by-one in the copy would show here).
    #[test]
    fn alloc_cstring_roundtrips_varied_inputs() {
        for input in ["", "X", "café €42 🍁", &"hew".repeat(1000)] {
            let data = alloc_cstring_from_str(input);
            assert!(!data.is_null());
            // SAFETY: data is a valid header-aware NUL-terminated string.
            unsafe {
                let recovered = CStr::from_ptr(data).to_str().unwrap();
                assert_eq!(recovered, input);
                assert_eq!(recovered.len(), input.len(), "byte length must match");
                free_cstring(data);
            }
        }
    }

    /// A size that overflows or exceeds `isize::MAX` must fail closed (null), not
    /// wrap into an undersized allocation. `len` is never read for these sizes
    /// (allocation is rejected before any copy), so a null `src` is fine.
    #[test]
    fn alloc_cstring_rejects_overflowing_size() {
        // HEADER_SIZE + usize::MAX + 1 overflows usize → null.
        // SAFETY: src is never dereferenced; the size check returns early.
        let wrapped = unsafe { alloc_cstring(std::ptr::null(), usize::MAX) };
        assert!(wrapped.is_null(), "usize-overflowing size must yield null");

        // A size that does not wrap usize but exceeds the isize::MAX cap → null.
        // SAFETY: src is never dereferenced; the cap check returns early.
        let too_big = unsafe { alloc_cstring(std::ptr::null(), isize::MAX as usize) };
        assert!(too_big.is_null(), "size > isize::MAX must yield null");
    }

    /// The integrity check detects a **corrupted header on a header-allocated
    /// pointer** (its sole, in-contract job). It does NOT — and cannot — detect
    /// a foreign/headerless pointer: that read would be out-of-bounds before the
    /// magic could be inspected, so we never test that case.
    #[test]
    fn validate_detects_corrupted_header() {
        let data = alloc_cstring_from_str("corrupt me");
        assert!(!data.is_null());
        // SAFETY: data came from alloc_cstring, so data - HEADER is in-bounds.
        unsafe {
            let base = data.cast::<u8>().sub(CSTRING_HEADER_SIZE);
            // Intact header validates.
            assert_eq!(validate_cstring_header(data), Some(base));
            // Clobber the magic (simulating header corruption).
            #[expect(
                clippy::cast_ptr_alignment,
                reason = "base is malloc-aligned, satisfying CStringHeader's alignment"
            )]
            let header = base.cast::<CStringHeader>();
            (*header).magic = 0;
            assert_eq!(
                validate_cstring_header(data),
                None,
                "a corrupted magic must not validate"
            );
            // Free the real base directly (free_cstring would abort on the bad
            // magic, which is the point — see the subprocess abort test).
            libc::free(base.cast()); // ALLOCATOR-PAIRING: libc
        }
    }

    /// Documents that the first `free_cstring` poisons the magic before
    /// releasing. The poison is a best-effort debug aid only: a real second free
    /// reads freed memory (UAF), so we do not — and cannot safely — re-read
    /// `data` here. The abort path itself is covered by the subprocess test.
    #[test]
    fn first_free_poisons_before_release() {
        let data = alloc_cstring_from_str("poison me");
        assert!(!data.is_null());
        // SAFETY: header-aware allocation.
        unsafe {
            assert!(validate_cstring_header(data).is_some());
            free_cstring(data);
        }
    }

    /// The abort actually fires when `free_cstring` sees a missing/poisoned magic
    /// on a header-allocated pointer (here: a real allocation whose magic we
    /// clobbered, staying in-contract). Run in a subprocess so the SIGABRT does
    /// not take down the test runner.
    #[test]
    fn free_cstring_aborts_on_corrupted_header() {
        if std::env::var("HEW_CABI_RUN_ABORT_PROBE").is_ok() {
            let data = alloc_cstring_from_str("corrupt me");
            // SAFETY: data came from alloc_cstring; clobber its magic so the
            // free path's integrity check fails and must abort.
            unsafe {
                let base = data.cast::<u8>().sub(CSTRING_HEADER_SIZE);
                #[expect(
                    clippy::cast_ptr_alignment,
                    reason = "base is malloc-aligned, satisfying CStringHeader's alignment"
                )]
                let header = base.cast::<CStringHeader>();
                (*header).magic = 0;
                free_cstring(data); // must abort, never returns
            }
            // If we reach here the abort did NOT fire — exit cleanly so the
            // parent assertion fails.
            std::process::exit(0);
        }

        let exe = std::env::current_exe().expect("current_exe");
        let status = std::process::Command::new(exe)
            .args([
                "--exact",
                "cabi::tests::free_cstring_aborts_on_corrupted_header",
            ])
            .env("HEW_CABI_RUN_ABORT_PROBE", "1")
            .env("RUST_BACKTRACE", "0")
            .output()
            .expect("spawn abort probe");
        assert!(
            !status.status.success(),
            "free_cstring on a corrupted header must abort (subprocess should \
             not exit 0); stdout={:?} stderr={:?}",
            String::from_utf8_lossy(&status.stdout),
            String::from_utf8_lossy(&status.stderr),
        );
    }

    // ── P2a: live refcount (retain / release / copy-on-write) ─────────────

    /// Read the current refcount of a live header-aware string (test-only
    /// observation of the now-live `rc`).
    ///
    /// # Safety
    /// `data` must be a live header-aware allocation.
    unsafe fn rc_of(data: *mut c_char) -> u32 {
        // SAFETY: caller guarantees data is a live header-aware allocation.
        unsafe { cstring_rc(data) }.load(Ordering::Relaxed)
    }

    /// retain → release → free-at-0: a retained string survives the first
    /// release and is freed only when the final owner releases it. The header
    /// is now LIVE — the refcount is observable through `cstring_rc`.
    #[test]
    fn cstring_retain_release_frees_at_zero() {
        let data = alloc_cstring_from_str("shared");
        assert!(!data.is_null());
        // SAFETY: data is a freshly produced live header-aware allocation.
        unsafe {
            assert_eq!(rc_of(data), 1, "fresh string starts uniquely owned");

            cstring_retain(data);
            assert_eq!(rc_of(data), 2, "retain bumps the refcount");

            cstring_retain(data);
            assert_eq!(rc_of(data), 3, "retain is additive");

            // Releases below 0 do not free; contents stay readable.
            free_cstring(data);
            assert_eq!(rc_of(data), 2, "release decrements without freeing");
            assert_eq!(CStr::from_ptr(data).to_str().unwrap(), "shared");

            free_cstring(data);
            assert_eq!(rc_of(data), 1, "still one owner remaining");
            assert_eq!(CStr::from_ptr(data).to_str().unwrap(), "shared");

            // Final release frees the allocation (verified under ASan in the
            // runtime ffi_boundary suite; here we just exercise the path).
            free_cstring(data);
        }
    }

    /// retain returns sharing: a retained pointer aliases the same buffer; a
    /// single underlying allocation backs both owners (the COW share win).
    #[test]
    fn cstring_retain_shares_one_buffer() {
        let data = alloc_cstring_from_str("alias me");
        // SAFETY: live header-aware allocation.
        unsafe {
            cstring_retain(data);
            assert_eq!(rc_of(data), 2);
            // No fork while shared-but-unmutated: ensure_unique on rc==1 returns
            // the same pointer; here rc==2 so a fork WOULD occur (tested below).
            free_cstring(data); // back to 1
            free_cstring(data); // free
        }
    }

    /// copy-on-write fork on a shared write: when `rc > 1`, `ensure_unique`
    /// allocates a private copy (distinct pointer, identical bytes) and drops
    /// the shared reference; the original buffer is left untouched for the
    /// remaining owner.
    #[test]
    fn cstring_ensure_unique_forks_when_shared() {
        let original = alloc_cstring_from_str("payload");
        // SAFETY: live header-aware allocation.
        unsafe {
            // Simulate a second owner (e.g. an alias share).
            cstring_retain(original);
            assert_eq!(rc_of(original), 2, "two owners share the buffer");

            // The would-be mutator forks before writing.
            let forked = cstring_ensure_unique(original);
            assert_ne!(
                original as usize, forked as usize,
                "shared (rc>1) ensure_unique must allocate a private copy"
            );
            assert_eq!(rc_of(forked), 1, "the fork is uniquely owned");
            assert_eq!(
                CStr::from_ptr(forked).to_str().unwrap(),
                "payload",
                "the fork copies the active region byte-identically"
            );

            // The original lost the forking owner's reference and is back to a
            // single owner, contents unchanged.
            assert_eq!(rc_of(original), 1, "ensure_unique released the shared ref");
            assert_eq!(
                CStr::from_ptr(original).to_str().unwrap(),
                "payload",
                "the remaining owner's buffer is untouched by the fork"
            );

            free_cstring(forked);
            free_cstring(original);
        }
    }

    /// `ensure_unique` on a uniquely-owned (`rc == 1`) string is a no-op: it
    /// returns the same pointer without allocating — no needless copy.
    #[test]
    fn cstring_ensure_unique_noop_when_unique() {
        let data = alloc_cstring_from_str("solo");
        // SAFETY: live header-aware allocation, rc == 1.
        unsafe {
            assert_eq!(rc_of(data), 1);
            let same = cstring_ensure_unique(data);
            assert_eq!(
                data as usize, same as usize,
                "uniquely-owned ensure_unique returns the pointer unchanged"
            );
            assert_eq!(rc_of(same), 1);
            free_cstring(same);
        }
    }

    /// The overflow predicate trips exactly above the saturation threshold and
    /// nowhere below it. Covers the abort condition without `2^31` retains.
    #[test]
    fn cstring_rc_would_overflow_predicate_boundaries() {
        assert!(!cstring_rc_would_overflow(0), "no owners never overflows");
        assert!(!cstring_rc_would_overflow(1), "a single owner is safe");
        assert!(
            !cstring_rc_would_overflow(CSTRING_RC_MAX),
            "at the threshold the increment to MAX+? is still representable"
        );
        assert!(
            cstring_rc_would_overflow(CSTRING_RC_MAX + 1),
            "one past the threshold must trip"
        );
        assert!(
            cstring_rc_would_overflow(u32::MAX - 1),
            "near the wrap must trip"
        );
        assert!(cstring_rc_would_overflow(u32::MAX), "at the wrap must trip");
    }

    /// A retain whose pre-increment count is just below the threshold does NOT
    /// abort — it bumps the count normally. Seeds the rc directly to avoid the
    /// `2^31` retains a natural climb would require.
    #[test]
    fn cstring_retain_below_threshold_does_not_abort() {
        let data = alloc_cstring_from_str("near limit");
        // SAFETY: live header-aware allocation; we seed and then restore the rc.
        unsafe {
            cstring_rc(data).store(CSTRING_RC_MAX, Ordering::Relaxed);
            cstring_retain(data); // old == CSTRING_RC_MAX → must NOT abort
            assert_eq!(rc_of(data), CSTRING_RC_MAX + 1, "retain incremented");
            // Restore a sane refcount and free the single owner.
            cstring_rc(data).store(1, Ordering::Relaxed);
            free_cstring(data);
        }
    }

    /// The abort actually fires when a retain observes a count already past the
    /// saturation threshold — proving the wrap guard is wired into the real
    /// path. Mirrors `free_cstring_aborts_on_corrupted_header`'s subprocess
    /// pattern (an abort cannot be caught in-process).
    #[test]
    fn cstring_retain_aborts_on_refcount_overflow() {
        if std::env::var("HEW_CABI_RUN_ABORT_PROBE").is_ok() {
            let data = alloc_cstring_from_str("overflow me");
            // SAFETY: live header-aware allocation; seed the rc one past the
            // threshold so the next retain observes an overflowing old value.
            unsafe {
                cstring_rc(data).store(CSTRING_RC_MAX + 1, Ordering::Relaxed);
                cstring_retain(data); // must abort, never returns
            }
            // Reaching here means the abort did NOT fire — exit cleanly so the
            // parent assertion fails.
            std::process::exit(0);
        }

        let exe = std::env::current_exe().expect("current_exe");
        let status = std::process::Command::new(exe)
            .args([
                "--exact",
                "cabi::tests::cstring_retain_aborts_on_refcount_overflow",
            ])
            .env("HEW_CABI_RUN_ABORT_PROBE", "1")
            .env("RUST_BACKTRACE", "0")
            .output()
            .expect("spawn abort probe");
        assert!(
            !status.status.success(),
            "cstring_retain past the overflow threshold must abort (subprocess \
             should not exit 0); stdout={:?} stderr={:?}",
            String::from_utf8_lossy(&status.stdout),
            String::from_utf8_lossy(&status.stderr),
        );
    }
}
