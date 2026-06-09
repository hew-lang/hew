//! Hew runtime: static `HewMapKeyLayout` / `HewMapValueLayout` descriptors.
//!
//! W4.001 Stage C0b — the kernel ABI (descriptor structs + drop hooks) landed
//! in Stage C0a (`hew-cabi/src/map.rs`, `hew-runtime/src/hashmap.rs`). This
//! module is the first producer of *real* descriptor instances exercising
//! that ABI: per-scalar / string / bytes / unit `#[no_mangle]` statics that
//! `hew-cabi` re-declares for codegen-rs (Stage C consumer) and any parallel
//! back-end to take the address of.
//!
//! **C0b boundary (plan §4 Stage C0b):** descriptors are *checker-visible
//! artifacts only*. No HIR consumer reads them in C0b; the first production
//! reader is Stage C's `HashMapLoweringFact` materialiser. DI-003
//! fail-closed-by-absence is preserved — float K descriptors are shipped
//! deliberately with `hash_fn = None` / `eq_fn = None` so that the
//! constructor's existing guard (`hashmap.rs::validate_key_layout`) aborts
//! before a float-keyed map can be built.
//!
//! # Symbol naming
//!
//! Two families, one per (K | V) role per type:
//!
//! - `hew_layout_key_<type>` — `HewMapKeyLayout` (carries hash + eq + drop).
//! - `hew_layout_val_<type>` — `HewMapValueLayout` (carries drop + optional
//!   clone; never hashes, per the kernel's get-borrows contract).
//!
//! Scope per plan §4 Stage C0b:
//!
//! | type     | key descriptor              | value descriptor              |
//! |----------|-----------------------------|-------------------------------|
//! | i32      | `hew_layout_key_i32`        | `hew_layout_val_i32`          |
//! | i64      | `hew_layout_key_i64`        | `hew_layout_val_i64`          |
//! | u32      | `hew_layout_key_u32`        | `hew_layout_val_u32`          |
//! | u64      | `hew_layout_key_u64`        | `hew_layout_val_u64`          |
//! | f32      | `hew_layout_key_f32` *(fail-closed: hash/eq None)* | `hew_layout_val_f32` |
//! | f64      | `hew_layout_key_f64` *(fail-closed: hash/eq None)* | `hew_layout_val_f64` |
//! | bool     | `hew_layout_key_bool`       | `hew_layout_val_bool`         |
//! | char     | `hew_layout_key_char`       | `hew_layout_val_char`         |
//! | string   | `hew_layout_key_string`     | `hew_layout_val_string`       |
//! | bytes    | `hew_layout_key_bytes`      | `hew_layout_val_bytes`        |
//! | unit     | n/a (zero-size keys are inadmissible) | `hew_layout_val_unit` |
//!
//! Named-record descriptors are *not* shipped here: the existing per-record
//! Layout machinery (Stage C-1c) materialises them on demand from the
//! checker-authoritative record layout. Brief §"Surface 1" and plan §4 Stage
//! C0b ("descriptors materialized on demand via the existing Named-record
//! Layout machinery") explicitly defer that to the consumer.
//!
//! # Thunk discipline (Q281=A)
//!
//! Every thunk is a free-standing `extern "C" fn` named `hew_layout_<role>_<type>_<op>`.
//! Closures or trait objects would not round-trip through `#[repr(C)]`
//! `Option<fn>` cleanly; the niche-optimised null discriminant relies on the
//! function pointer being a bare ABI-stable pointer.
//!
//! # WASM parity (#1820)
//!
//! These descriptors are linked into the wasm32-wasip1 runtime archive and are
//! used by wasm HashMap/HashSet codegen for primitive keys and values. Record
//! descriptors are still synthesized by codegen, with LLVM lowering their
//! address-taken thunks through the wasm function table.

#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI thunk module; SAFETY documented per-thunk."
)]

use core::ffi::{c_char, c_void};

use hew_cabi::map::{HewMapKeyLayout, HewMapValueLayout};
use hew_cabi::vec::HewTypeOwnershipKind;

// ---------------------------------------------------------------------------
// FNV-1a-64 helpers
// ---------------------------------------------------------------------------
//
// The descriptor ABI returns u64; using FNV-1a-64 across all scalar /
// byte-buffer thunks keeps the kernel
// load-factor / bucket-index math uniform.

const FNV_OFFSET_64: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME_64: u64 = 0x0000_0100_0000_01b3;

#[inline]
fn fnv1a_64(bytes: &[u8]) -> u64 {
    let mut h = FNV_OFFSET_64;
    for &b in bytes {
        h ^= u64::from(b);
        h = h.wrapping_mul(FNV_PRIME_64);
    }
    h
}

/// FNV-1a-64 with a `len` prefix mixed into the hash, used for variable-length
/// blobs (`bytes`). The length prefix prevents two equal-byte prefixes of
/// different lengths from colliding when one is a prefix of the other.
#[inline]
fn fnv1a_64_with_len_prefix(bytes: &[u8]) -> u64 {
    let mut h = FNV_OFFSET_64;
    let len_bytes = (bytes.len() as u64).to_le_bytes();
    for &b in &len_bytes {
        h ^= u64::from(b);
        h = h.wrapping_mul(FNV_PRIME_64);
    }
    for &b in bytes {
        h ^= u64::from(b);
        h = h.wrapping_mul(FNV_PRIME_64);
    }
    h
}

// ---------------------------------------------------------------------------
// Scalar hash / eq thunks (typed reload + FNV-1a-64 over LE bytes)
// ---------------------------------------------------------------------------
//
// SAFETY (all scalar thunks below): `key` (and `lhs` / `rhs` for eq) must be
// non-null and point to a valid blob of the declared type's size + alignment.
// The kernel enforces this by allocating slot storage with the descriptor's
// `size` + `align` and only invoking the thunk on OCCUPIED slots.

macro_rules! scalar_hash_eq {
    ($ty:ty, $hash_fn:ident, $eq_fn:ident) => {
        unsafe extern "C" fn $hash_fn(key: *const c_void) -> u64 {
            let v: $ty = core::ptr::read(key.cast::<$ty>());
            fnv1a_64(&v.to_le_bytes())
        }
        unsafe extern "C" fn $eq_fn(lhs: *const c_void, rhs: *const c_void) -> i32 {
            let l: $ty = core::ptr::read(lhs.cast::<$ty>());
            let r: $ty = core::ptr::read(rhs.cast::<$ty>());
            i32::from(l == r)
        }
    };
}

scalar_hash_eq!(i32, hew_layout_key_i32_hash, hew_layout_key_i32_eq);
scalar_hash_eq!(i64, hew_layout_key_i64_hash, hew_layout_key_i64_eq);
scalar_hash_eq!(u32, hew_layout_key_u32_hash, hew_layout_key_u32_eq);
scalar_hash_eq!(u64, hew_layout_key_u64_hash, hew_layout_key_u64_eq);

// bool: one byte, hash + eq via that byte. Hew never admits bool as a HashMap
// key in practice (Hash impl is gated at the checker), but the descriptor is
// shipped for ABI completeness and runtime fail-closed routing.
unsafe extern "C" fn hew_layout_key_bool_hash(key: *const c_void) -> u64 {
    let v: u8 = core::ptr::read(key.cast::<u8>());
    fnv1a_64(&[v])
}
unsafe extern "C" fn hew_layout_key_bool_eq(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let l: u8 = core::ptr::read(lhs.cast::<u8>());
    let r: u8 = core::ptr::read(rhs.cast::<u8>());
    i32::from(l == r)
}

// char: Hew char is a 32-bit Unicode codepoint (LLVM lowering: i32 — see
// hew-codegen-rs/src/llvm.rs:2490). Hash + eq over the u32 LE bytes.
unsafe extern "C" fn hew_layout_key_char_hash(key: *const c_void) -> u64 {
    let v: u32 = core::ptr::read(key.cast::<u32>());
    fnv1a_64(&v.to_le_bytes())
}
unsafe extern "C" fn hew_layout_key_char_eq(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let l: u32 = core::ptr::read(lhs.cast::<u32>());
    let r: u32 = core::ptr::read(rhs.cast::<u32>());
    i32::from(l == r)
}

// ---------------------------------------------------------------------------
// String hash / eq thunks
// ---------------------------------------------------------------------------
//
// The K blob is `*const c_char` (8 bytes on 64-bit, pointer alignment). We
// reload the pointer from the blob and hash / compare the NUL-terminated
// payload it points at. Caller (kernel) guarantees the slot's pointer was
// produced by the descriptor's String-ownership insert path, so it is either
// null (vacant slot — never invoked) or a heap / static C string.

unsafe extern "C" fn hew_layout_key_string_hash(key: *const c_void) -> u64 {
    let p: *const c_char = core::ptr::read(key.cast::<*const c_char>());
    if p.is_null() {
        // Null key reaching the hash thunk would be a kernel bug (only
        // OCCUPIED slots are hashed) — fail closed loudly rather than
        // silently hashing zero.
        crate::set_last_error("hew_layout_key_string_hash: null inner pointer in OCCUPIED slot");
        std::process::abort();
    }
    let len = libc::strlen(p);
    let slice = core::slice::from_raw_parts(p.cast::<u8>(), len);
    fnv1a_64(slice)
}

unsafe extern "C" fn hew_layout_key_string_eq(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let lp: *const c_char = core::ptr::read(lhs.cast::<*const c_char>());
    let rp: *const c_char = core::ptr::read(rhs.cast::<*const c_char>());
    if lp.is_null() || rp.is_null() {
        crate::set_last_error("hew_layout_key_string_eq: null inner pointer in OCCUPIED slot");
        std::process::abort();
    }
    i32::from(libc::strcmp(lp, rp) == 0)
}

// ---------------------------------------------------------------------------
// Bytes hash / eq thunks
// ---------------------------------------------------------------------------
//
// The K blob is a `BytesTriple`: { ptr: *mut u8, offset: u32, len: u32 } —
// 16 bytes, 8-byte alignment. Hash / eq operate on the active byte range
// `(ptr + offset)[0..len]`. A length-prefixed FNV-1a-64 prevents prefix
// collisions.

#[repr(C)]
#[derive(Clone, Copy)]
struct BytesTripleRepr {
    ptr: *mut u8,
    offset: u32,
    len: u32,
}

unsafe extern "C" fn hew_layout_key_bytes_hash(key: *const c_void) -> u64 {
    let triple: BytesTripleRepr = core::ptr::read(key.cast::<BytesTripleRepr>());
    if triple.len == 0 {
        return fnv1a_64_with_len_prefix(&[]);
    }
    if triple.ptr.is_null() {
        crate::set_last_error("hew_layout_key_bytes_hash: null ptr with non-zero len");
        std::process::abort();
    }
    let start = triple.ptr.add(triple.offset as usize);
    let slice = core::slice::from_raw_parts(start, triple.len as usize);
    fnv1a_64_with_len_prefix(slice)
}

unsafe extern "C" fn hew_layout_key_bytes_eq(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let l: BytesTripleRepr = core::ptr::read(lhs.cast::<BytesTripleRepr>());
    let r: BytesTripleRepr = core::ptr::read(rhs.cast::<BytesTripleRepr>());
    if l.len != r.len {
        return 0;
    }
    if l.len == 0 {
        return 1;
    }
    if l.ptr.is_null() || r.ptr.is_null() {
        crate::set_last_error("hew_layout_key_bytes_eq: null ptr with non-zero len");
        std::process::abort();
    }
    let ls = core::slice::from_raw_parts(l.ptr.add(l.offset as usize), l.len as usize);
    let rs = core::slice::from_raw_parts(r.ptr.add(r.offset as usize), r.len as usize);
    i32::from(ls == rs)
}

// ---------------------------------------------------------------------------
// Drop thunks
// ---------------------------------------------------------------------------
//
// Per the C0a ownership contract (hew-cabi/src/map.rs §"Acquisition /
// ownership contract"), the kernel invokes drop_fn on stored K + V at
// remove / free (and on stored V at insert-overwrite). The thunks here
// release the heap allocation owned by the K / V blob *without* freeing the
// blob storage itself — the kernel owns the slot bytes.
//
// `String` drops the inner `*const c_char` via `hew_string_drop` (which
// no-ops on null and on static binary-section strings).
//
// `Bytes` drops the inner triple's `ptr` via `hew_bytes_drop` (which decrements
// the refcount and frees the buffer when the count hits zero).

extern "C" fn hew_layout_string_drop(blob: *mut c_void) {
    // SAFETY: blob is non-null and points to a `*mut c_char` slot owned by
    // the kernel. Reading the pointer-by-value is a fixed-size load; passing
    // it to `hew_string_drop` is correct per that fn's null-safe contract.
    unsafe {
        let p: *mut c_char = core::ptr::read(blob.cast::<*mut c_char>());
        crate::string::hew_string_drop(p);
    }
}

extern "C" fn hew_layout_bytes_drop(blob: *mut c_void) {
    // SAFETY: blob is non-null and points to a `BytesTriple` slot owned by
    // the kernel. Reload + drop the inner buffer via the bytes runtime.
    unsafe {
        let triple: BytesTripleRepr = core::ptr::read(blob.cast::<BytesTripleRepr>());
        if !triple.ptr.is_null() {
            crate::bytes::hew_bytes_drop(triple.ptr);
        }
    }
}

// ---------------------------------------------------------------------------
// Key descriptors (HewMapKeyLayout)
// ---------------------------------------------------------------------------
//
// Float K descriptors are shipped with `hash_fn = None` / `eq_fn = None`.
// Float never satisfies Hash at the checker; the kernel's
// `validate_key_layout` (hashmap.rs:932-939) already aborts on None thunks.
// Belt-and-suspenders DI-003 fail-closed-by-absence per plan §4 Stage C0b.

macro_rules! key_layout {
    ($name:ident, $ty:ty, $hash:expr, $eq:expr, $ownership:expr, $drop:expr) => {
        #[no_mangle]
        pub static $name: HewMapKeyLayout = HewMapKeyLayout {
            size: core::mem::size_of::<$ty>(),
            align: core::mem::align_of::<$ty>(),
            ownership_kind: $ownership,
            hash_fn: $hash,
            eq_fn: $eq,
            drop_fn: $drop,
        };
    };
}

key_layout!(
    hew_layout_key_i32,
    i32,
    Some(hew_layout_key_i32_hash),
    Some(hew_layout_key_i32_eq),
    HewTypeOwnershipKind::Plain,
    None
);
key_layout!(
    hew_layout_key_i64,
    i64,
    Some(hew_layout_key_i64_hash),
    Some(hew_layout_key_i64_eq),
    HewTypeOwnershipKind::Plain,
    None
);
key_layout!(
    hew_layout_key_u32,
    u32,
    Some(hew_layout_key_u32_hash),
    Some(hew_layout_key_u32_eq),
    HewTypeOwnershipKind::Plain,
    None
);
key_layout!(
    hew_layout_key_u64,
    u64,
    Some(hew_layout_key_u64_hash),
    Some(hew_layout_key_u64_eq),
    HewTypeOwnershipKind::Plain,
    None
);

// Float K descriptors: shipped with None thunks for DI-003 belt-and-suspenders.
key_layout!(
    hew_layout_key_f32,
    f32,
    None,
    None,
    HewTypeOwnershipKind::Plain,
    None
);
key_layout!(
    hew_layout_key_f64,
    f64,
    None,
    None,
    HewTypeOwnershipKind::Plain,
    None
);

// bool: 1 byte, align 1.
#[no_mangle]
pub static hew_layout_key_bool: HewMapKeyLayout = HewMapKeyLayout {
    size: 1,
    align: 1,
    ownership_kind: HewTypeOwnershipKind::Plain,
    hash_fn: Some(hew_layout_key_bool_hash),
    eq_fn: Some(hew_layout_key_bool_eq),
    drop_fn: None,
};

// char: 4 bytes, align 4 (Unicode codepoint as u32).
#[no_mangle]
pub static hew_layout_key_char: HewMapKeyLayout = HewMapKeyLayout {
    size: 4,
    align: 4,
    ownership_kind: HewTypeOwnershipKind::Plain,
    hash_fn: Some(hew_layout_key_char_hash),
    eq_fn: Some(hew_layout_key_char_eq),
    drop_fn: None,
};

// string: pointer-sized blob (`*const c_char`).
#[no_mangle]
pub static hew_layout_key_string: HewMapKeyLayout = HewMapKeyLayout {
    size: core::mem::size_of::<*const c_char>(),
    align: core::mem::align_of::<*const c_char>(),
    ownership_kind: HewTypeOwnershipKind::String,
    hash_fn: Some(hew_layout_key_string_hash),
    eq_fn: Some(hew_layout_key_string_eq),
    drop_fn: Some(hew_layout_string_drop),
};

// bytes: BytesTriple (ptr + offset + len), 16 bytes, align 8.
#[no_mangle]
pub static hew_layout_key_bytes: HewMapKeyLayout = HewMapKeyLayout {
    size: core::mem::size_of::<BytesTripleRepr>(),
    align: core::mem::align_of::<BytesTripleRepr>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    hash_fn: Some(hew_layout_key_bytes_hash),
    eq_fn: Some(hew_layout_key_bytes_eq),
    drop_fn: Some(hew_layout_bytes_drop),
};

// ---------------------------------------------------------------------------
// Value descriptors (HewMapValueLayout)
// ---------------------------------------------------------------------------
//
// Value descriptors carry no hash / eq (the kernel never hashes V — see
// the get-borrows contract in hew-cabi/src/map.rs). They carry drop (always)
// and an optional clone (consulted only by the opt-in cloning-get variant,
// per C0a). C0b ships clone_fn = None for every value descriptor; Stage C
// or later may wire concrete clone thunks when an owned-V return path lands.

macro_rules! val_layout_plain {
    ($name:ident, $ty:ty) => {
        #[no_mangle]
        pub static $name: HewMapValueLayout = HewMapValueLayout {
            size: core::mem::size_of::<$ty>(),
            align: core::mem::align_of::<$ty>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
            drop_fn: None,
            clone_fn: None,
        };
    };
}

val_layout_plain!(hew_layout_val_i32, i32);
val_layout_plain!(hew_layout_val_i64, i64);
val_layout_plain!(hew_layout_val_u32, u32);
val_layout_plain!(hew_layout_val_u64, u64);
val_layout_plain!(hew_layout_val_f32, f32);
val_layout_plain!(hew_layout_val_f64, f64);

#[no_mangle]
pub static hew_layout_val_bool: HewMapValueLayout = HewMapValueLayout {
    size: 1,
    align: 1,
    ownership_kind: HewTypeOwnershipKind::Plain,
    drop_fn: None,
    clone_fn: None,
};

#[no_mangle]
pub static hew_layout_val_char: HewMapValueLayout = HewMapValueLayout {
    size: 4,
    align: 4,
    ownership_kind: HewTypeOwnershipKind::Plain,
    drop_fn: None,
    clone_fn: None,
};

#[no_mangle]
pub static hew_layout_val_string: HewMapValueLayout = HewMapValueLayout {
    size: core::mem::size_of::<*const c_char>(),
    align: core::mem::align_of::<*const c_char>(),
    ownership_kind: HewTypeOwnershipKind::String,
    drop_fn: Some(hew_layout_string_drop),
    clone_fn: None,
};

#[no_mangle]
pub static hew_layout_val_bytes: HewMapValueLayout = HewMapValueLayout {
    size: core::mem::size_of::<BytesTripleRepr>(),
    align: core::mem::align_of::<BytesTripleRepr>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    drop_fn: Some(hew_layout_bytes_drop),
    clone_fn: None,
};

// unit V (ZST): the HashSet-as-HashMap<T,()> pattern. The kernel admits
// size == 0 only when align == 1 (hashmap.rs:980-983); Plain ownership,
// no drop, no clone.
#[no_mangle]
pub static hew_layout_val_unit: HewMapValueLayout = HewMapValueLayout {
    size: 0,
    align: 1,
    ownership_kind: HewTypeOwnershipKind::Plain,
    drop_fn: None,
    clone_fn: None,
};

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fnv1a_64_matches_known_vectors() {
        // Reference vectors from the FNV-1a spec.
        assert_eq!(fnv1a_64(b""), FNV_OFFSET_64);
        // "a" -> 0xaf63dc4c8601ec8c
        assert_eq!(fnv1a_64(b"a"), 0xaf63_dc4c_8601_ec8c);
        // "foobar" -> 0x85944171f73967e8
        assert_eq!(fnv1a_64(b"foobar"), 0x8594_4171_f739_67e8);
    }

    #[test]
    fn scalar_descriptors_have_thunks() {
        assert!(hew_layout_key_i32.hash_fn.is_some());
        assert!(hew_layout_key_i32.eq_fn.is_some());
        assert!(hew_layout_key_i32.drop_fn.is_none());
        assert_eq!(hew_layout_key_i32.size, 4);
        assert_eq!(hew_layout_key_i32.align, 4);

        assert!(hew_layout_key_i64.hash_fn.is_some());
        assert_eq!(hew_layout_key_i64.size, 8);
    }

    #[test]
    fn float_key_descriptors_are_fail_closed() {
        // DI-003 belt-and-suspenders: float K layouts must carry None thunks
        // so `hew_hashmap_new_with_layout`'s validate_key_layout guard fires
        // on the first construction attempt (hashmap.rs:932-939).
        assert!(hew_layout_key_f32.hash_fn.is_none());
        assert!(hew_layout_key_f32.eq_fn.is_none());
        assert!(hew_layout_key_f64.hash_fn.is_none());
        assert!(hew_layout_key_f64.eq_fn.is_none());
    }

    #[test]
    fn string_descriptor_has_drop() {
        assert!(hew_layout_key_string.drop_fn.is_some());
        assert!(hew_layout_val_string.drop_fn.is_some());
        assert_eq!(
            hew_layout_key_string.ownership_kind,
            HewTypeOwnershipKind::String
        );
    }

    #[test]
    fn bytes_descriptor_has_drop_and_layout_managed_ownership() {
        assert!(hew_layout_key_bytes.drop_fn.is_some());
        assert!(hew_layout_val_bytes.drop_fn.is_some());
        assert_eq!(
            hew_layout_key_bytes.ownership_kind,
            HewTypeOwnershipKind::LayoutManaged
        );
        assert_eq!(hew_layout_key_bytes.size, 16);
        assert_eq!(hew_layout_key_bytes.align, 8);
    }

    #[test]
    fn unit_val_descriptor_is_zst() {
        assert_eq!(hew_layout_val_unit.size, 0);
        assert_eq!(hew_layout_val_unit.align, 1);
        assert!(hew_layout_val_unit.drop_fn.is_none());
    }

    #[test]
    fn scalar_hash_eq_round_trip_i64() {
        let a: i64 = 0x1234_5678_9abc_def0;
        let b: i64 = 0x1234_5678_9abc_def0;
        let c: i64 = 0x1111_1111_1111_1111;
        // SAFETY: `a` / `b` / `c` are i64 locals; raw-const addresses are
        // properly aligned 8-byte i64 blobs as the thunks require.
        unsafe {
            let h_a = hew_layout_key_i64_hash((&raw const a).cast());
            let h_b = hew_layout_key_i64_hash((&raw const b).cast());
            let h_c = hew_layout_key_i64_hash((&raw const c).cast());
            assert_eq!(h_a, h_b);
            assert_ne!(h_a, h_c);
            assert_eq!(
                hew_layout_key_i64_eq((&raw const a).cast(), (&raw const b).cast()),
                1
            );
            assert_eq!(
                hew_layout_key_i64_eq((&raw const a).cast(), (&raw const c).cast()),
                0
            );
        }
    }

    #[test]
    fn string_hash_matches_fnv1a_64_of_payload() {
        // Build a C-string blob (the K blob is a *const c_char).
        let s = c"hello";
        let p: *const c_char = s.as_ptr();
        let blob: *const *const c_char = &raw const p;
        // SAFETY: `blob` points to a properly-aligned `*const c_char`
        // local that holds a valid NUL-terminated C string pointer.
        unsafe {
            let h = hew_layout_key_string_hash(blob.cast());
            assert_eq!(h, fnv1a_64(b"hello"));
            let eq = hew_layout_key_string_eq(blob.cast(), blob.cast());
            assert_eq!(eq, 1);
        }
    }
}
