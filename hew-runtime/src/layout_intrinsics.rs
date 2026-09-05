//! Static scalar, string and byte-value protocols for maps and sets.
//!
//! Key descriptors add hash/equality callbacks to the same copy/drop descriptor
//! used by vector elements. Hashing reads typed fields and complete string or
//! byte contents; it never includes padding. Floating-point keys intentionally
//! have no hash/equality callbacks because they do not satisfy the key contract.
//! These pure descriptors and their callbacks also support wasm32-wasip1.

#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI thunk module; SAFETY documented per-thunk."
)]

use core::ffi::c_void;

use hew_cabi::map::{HewMapKeyLayout, HewVecElemLayout};
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
// The K blob is one pointer-sized managed string handle. Hash and equality use
// the carrier's complete length-bounded byte range, including embedded NUL.
// Null is a valid occupied key because it is the canonical empty string.

unsafe extern "C" fn hew_layout_key_string_hash(key: *const c_void) -> u64 {
    let p: *const hew_cabi::string::HewString =
        core::ptr::read(key.cast::<*const hew_cabi::string::HewString>());
    // SAFETY: the descriptor contract supplies null or a live managed handle.
    fnv1a_64(unsafe { hew_cabi::string::string_as_bytes(p) })
}

unsafe extern "C" fn hew_layout_key_string_eq(lhs: *const c_void, rhs: *const c_void) -> i32 {
    let lp: *const hew_cabi::string::HewString =
        core::ptr::read(lhs.cast::<*const hew_cabi::string::HewString>());
    let rp: *const hew_cabi::string::HewString =
        core::ptr::read(rhs.cast::<*const hew_cabi::string::HewString>());
    // SAFETY: the descriptor contract supplies null or live managed handles.
    crate::string::hew_string_equals(lp, rp)
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
// `String` drops the inner managed handle via `hew_string_drop` (which
// treats null as the canonical empty value).
//
// `Bytes` drops the inner triple's `ptr` via `hew_bytes_drop` (which decrements
// the refcount and frees the buffer when the count hits zero).

extern "C" fn hew_layout_string_drop(blob: *mut c_void) {
    // SAFETY: blob is non-null and points to a managed-string handle slot owned by
    // the kernel. Reading the pointer-by-value is a fixed-size load; passing
    // it to `hew_string_drop` is correct per that fn's null-safe contract.
    unsafe {
        let p: *mut hew_cabi::string::HewString =
            core::ptr::read(blob.cast::<*mut hew_cabi::string::HewString>());
        crate::string::hew_string_drop(p);
    }
}

unsafe extern "C" fn hew_layout_string_clone(src: *const c_void, dst: *mut c_void) -> i32 {
    // SAFETY: src/dst point at pointer-sized string slots. The runtime already
    // copied dst <- src; overwrite dst with an independently retained owner.
    let src_ptr: *const hew_cabi::string::HewString =
        unsafe { core::ptr::read(src.cast::<*const hew_cabi::string::HewString>()) };
    // SAFETY: `src_ptr` is null or a valid Hew string per the descriptor's
    // String ownership contract.
    let cloned = unsafe { crate::string::hew_string_clone(src_ptr) };
    if !src_ptr.is_null() && cloned.is_null() {
        return 1;
    }
    // SAFETY: `dst` is writable pointer-sized string storage.
    unsafe { core::ptr::write(dst.cast::<*mut hew_cabi::string::HewString>(), cloned) };
    0
}

unsafe extern "C" fn hew_layout_bytes_clone(src: *const c_void, dst: *mut c_void) -> i32 {
    // SAFETY: src/dst point at BytesTriple slots. Refcount the backing buffer,
    // then copy the by-value view into dst.
    let triple: BytesTripleRepr = unsafe { core::ptr::read(src.cast::<BytesTripleRepr>()) };
    if !triple.ptr.is_null() {
        // SAFETY: non-null buffer pointer comes from a valid BytesTriple.
        unsafe { crate::bytes::hew_bytes_clone_ref(triple.ptr) };
    }
    // SAFETY: `dst` is writable BytesTriple storage.
    unsafe { core::ptr::write(dst.cast::<BytesTripleRepr>(), triple) };
    0
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
            value: HewVecElemLayout {
                size: core::mem::size_of::<$ty>(),
                align: core::mem::align_of::<$ty>(),
                ownership_kind: $ownership,
                clone_fn: None,
                drop_fn: $drop,
            },
            hash_fn: $hash,
            eq_fn: $eq,
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
    value: HewVecElemLayout {
        size: 1,
        align: 1,
        ownership_kind: HewTypeOwnershipKind::Plain,
        clone_fn: None,
        drop_fn: None,
    },
    hash_fn: Some(hew_layout_key_bool_hash),
    eq_fn: Some(hew_layout_key_bool_eq),
};

// char: 4 bytes, align 4 (Unicode codepoint as u32).
#[no_mangle]
pub static hew_layout_key_char: HewMapKeyLayout = HewMapKeyLayout {
    value: HewVecElemLayout {
        size: 4,
        align: 4,
        ownership_kind: HewTypeOwnershipKind::Plain,
        clone_fn: None,
        drop_fn: None,
    },
    hash_fn: Some(hew_layout_key_char_hash),
    eq_fn: Some(hew_layout_key_char_eq),
};

// string: pointer-sized opaque managed handle.
#[no_mangle]
pub static hew_layout_key_string: HewMapKeyLayout = HewMapKeyLayout {
    value: HewVecElemLayout {
        size: core::mem::size_of::<*const hew_cabi::string::HewString>(),
        align: core::mem::align_of::<*const hew_cabi::string::HewString>(),
        ownership_kind: HewTypeOwnershipKind::String,
        clone_fn: Some(hew_layout_string_clone),
        drop_fn: Some(hew_layout_string_drop),
    },
    hash_fn: Some(hew_layout_key_string_hash),
    eq_fn: Some(hew_layout_key_string_eq),
};

// bytes: BytesTriple (ptr + offset + len), 16 bytes, align 8.
#[no_mangle]
pub static hew_layout_key_bytes: HewMapKeyLayout = HewMapKeyLayout {
    value: HewVecElemLayout {
        size: core::mem::size_of::<BytesTripleRepr>(),
        align: core::mem::align_of::<BytesTripleRepr>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: Some(hew_layout_bytes_clone),
        drop_fn: Some(hew_layout_bytes_drop),
    },
    hash_fn: Some(hew_layout_key_bytes_hash),
    eq_fn: Some(hew_layout_key_bytes_eq),
};

// ---------------------------------------------------------------------------
// Value descriptors (HewVecElemLayout)
// ---------------------------------------------------------------------------
//
// Value descriptors carry no hash / eq (the kernel never hashes V — see
// the get-borrows contract in hew-cabi/src/map.rs). They carry drop (always)
// and a clone thunk for non-Plain values. Plain descriptors keep clone_fn =
// None; string/bytes descriptors publish semantic clone thunks so
// HashMap::get can return an owned Option<V> without aliasing the slot.

macro_rules! val_layout_plain {
    ($name:ident, $ty:ty) => {
        #[no_mangle]
        pub static $name: HewVecElemLayout = HewVecElemLayout {
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
pub static hew_layout_val_bool: HewVecElemLayout = HewVecElemLayout {
    size: 1,
    align: 1,
    ownership_kind: HewTypeOwnershipKind::Plain,
    drop_fn: None,
    clone_fn: None,
};

#[no_mangle]
pub static hew_layout_val_char: HewVecElemLayout = HewVecElemLayout {
    size: 4,
    align: 4,
    ownership_kind: HewTypeOwnershipKind::Plain,
    drop_fn: None,
    clone_fn: None,
};

#[no_mangle]
pub static hew_layout_val_string: HewVecElemLayout = HewVecElemLayout {
    size: core::mem::size_of::<*const hew_cabi::string::HewString>(),
    align: core::mem::align_of::<*const hew_cabi::string::HewString>(),
    ownership_kind: HewTypeOwnershipKind::String,
    drop_fn: Some(hew_layout_string_drop),
    clone_fn: Some(hew_layout_string_clone),
};

#[no_mangle]
pub static hew_layout_val_bytes: HewVecElemLayout = HewVecElemLayout {
    size: core::mem::size_of::<BytesTripleRepr>(),
    align: core::mem::align_of::<BytesTripleRepr>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    drop_fn: Some(hew_layout_bytes_drop),
    clone_fn: Some(hew_layout_bytes_clone),
};

// unit V (ZST): the HashSet-as-HashMap<T,()> pattern. The kernel admits
// size == 0 only when align == 1 (hashmap.rs:980-983); Plain ownership,
// no drop, no clone.
#[no_mangle]
pub static hew_layout_val_unit: HewVecElemLayout = HewVecElemLayout {
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
        assert!(hew_layout_key_i32.value.drop_fn.is_none());
        assert_eq!(hew_layout_key_i32.value.size, 4);
        assert_eq!(hew_layout_key_i32.value.align, 4);

        assert!(hew_layout_key_i64.hash_fn.is_some());
        assert_eq!(hew_layout_key_i64.value.size, 8);
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
        assert!(hew_layout_key_string.value.drop_fn.is_some());
        assert!(hew_layout_val_string.drop_fn.is_some());
        assert_eq!(
            hew_layout_key_string.value.ownership_kind,
            HewTypeOwnershipKind::String
        );
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn bytes_descriptor_has_drop_and_layout_managed_ownership() {
        assert!(hew_layout_key_bytes.value.drop_fn.is_some());
        assert!(hew_layout_val_bytes.drop_fn.is_some());
        assert_eq!(
            hew_layout_key_bytes.value.ownership_kind,
            HewTypeOwnershipKind::LayoutManaged
        );
        assert_eq!(hew_layout_key_bytes.value.size, 16);
        assert_eq!(hew_layout_key_bytes.value.align, 8);
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
        let p = hew_cabi::string::string_from_str("hello");
        let blob = &raw const p;
        // SAFETY: `blob` points to a properly aligned live managed-string slot.
        unsafe {
            let h = hew_layout_key_string_hash(blob.cast());
            assert_eq!(h, fnv1a_64(b"hello"));
            let eq = hew_layout_key_string_eq(blob.cast(), blob.cast());
            assert_eq!(eq, 1);
            hew_cabi::string::string_release(p);
        }
    }

    #[test]
    fn string_key_hash_and_equality_include_embedded_nul() {
        let left = hew_cabi::string::string_from_str("a\0b");
        let same = hew_cabi::string::string_from_str("a\0b");
        let prefix = hew_cabi::string::string_from_str("a");
        // SAFETY: all blobs point to aligned live managed-string slots.
        unsafe {
            assert_eq!(
                hew_layout_key_string_hash((&raw const left).cast()),
                fnv1a_64(b"a\0b")
            );
            assert_eq!(
                hew_layout_key_string_eq((&raw const left).cast(), (&raw const same).cast()),
                1
            );
            assert_eq!(
                hew_layout_key_string_eq((&raw const left).cast(), (&raw const prefix).cast()),
                0
            );
            hew_cabi::string::string_release(left);
            hew_cabi::string::string_release(same);
            hew_cabi::string::string_release(prefix);
        }
    }
}
