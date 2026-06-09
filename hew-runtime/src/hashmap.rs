//! Hew runtime: `hashmap` module.
//!
//! Open-addressing hash map (`HewHashMap`) with C ABI, matching the C runtime
//! layout exactly. Uses FNV-1a hashing and linear probing with tombstones.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]
// Internal `find_entry` returns isize (-1 for not-found), matching C semantics.
// The value is always checked before use as a usize index.
#![expect(
    clippy::cast_sign_loss,
    reason = "find_entry returns isize (-1 sentinel); always checked >= 0 before usize cast. hew_hashmap_len casts usize→i64 which is lossless."
)]
#![expect(
    clippy::cast_possible_wrap,
    reason = "table index fits in isize on all supported platforms; usize→i64 is safe for realistic lengths"
)]

use core::ffi::{c_char, c_void};
use core::ptr;

use hew_cabi::map::{HewMapKeyLayout, HewMapValueLayout};
use hew_cabi::vec::HewTypeOwnershipKind;

/// Entry states.
const EMPTY: u8 = 0;
const OCCUPIED: u8 = 1;
const TOMBSTONE: u8 = 2;

/// Initial table capacity (must be a power of two).
const INIT_CAP: usize = 8;
/// Load factor percentage threshold for resize.
const LOAD_PCTG: usize = 75;

/// A single hash-map entry matching the C `HewMapEntry` layout.
#[repr(C)]
#[derive(Debug)]
pub struct HewMapEntry {
    /// 0 = empty, 1 = occupied, 2 = tombstone.
    pub state: u8,
    /// `strdup`'d key (null when empty/tombstone).
    pub key: *mut c_char,
    /// Integer value.
    pub value_i32: i32,
    /// `strdup`'d string value (or null).
    pub value_str: *mut c_char,
    /// 64-bit integer value.
    pub value_i64: i64,
    /// Floating-point value.
    pub value_f64: f64,
}

/// Open-addressing hash map matching the C `HewHashMap` layout.
#[repr(C)]
#[derive(Debug)]
pub struct HewHashMap {
    /// Pointer to the entries array.
    pub entries: *mut HewMapEntry,
    /// Number of occupied entries.
    pub len: usize,
    /// Total capacity (number of slots).
    pub cap: usize,
}

// ---------------------------------------------------------------------------
// FNV-1a hash (32-bit, matching the C implementation)
// ---------------------------------------------------------------------------

/// Compute FNV-1a 32-bit hash of a C string.
///
/// # Safety
///
/// `key` must be a valid, null-terminated C string.
unsafe fn fnv1a(key: *const c_char) -> u32 {
    // SAFETY: caller guarantees `key` is a valid C string.
    unsafe {
        let mut h: u32 = 2_166_136_261;
        let mut p = key.cast::<u8>();
        while *p != 0 {
            h ^= u32::from(*p);
            h = h.wrapping_mul(16_777_619);
            p = p.add(1);
        }
        h
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Find the index of the entry with `key`, or -1 if not found.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
unsafe fn find_entry(m: *mut HewHashMap, key: *const c_char) -> isize {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        let map = &*m;
        if map.cap == 0 {
            return -1;
        }
        let h = fnv1a(key);
        let mask = map.cap - 1;
        let start = (h as usize) & mask;
        let mut idx = start;
        loop {
            let entry = &*map.entries.add(idx);
            if entry.state == EMPTY {
                return -1;
            }
            if entry.state == OCCUPIED && libc::strcmp(entry.key, key) == 0 {
                return idx as isize;
            }
            idx = (idx + 1) & mask;
            if idx == start {
                return -1;
            }
        }
    }
}

/// Find the slot to update or insert for `key`, continuing past tombstones.
///
/// Returns a pointer to either the existing occupied entry for `key`, the first
/// tombstone in the probe chain, or the first empty slot if no tombstone was
/// encountered.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
unsafe fn find_insert_slot(m: *mut HewHashMap, key: *const c_char) -> *mut HewMapEntry {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        let map = &mut *m;
        let mask = map.cap - 1;
        let start = (fnv1a(key) as usize) & mask;
        let mut idx = start;
        let mut first_tombstone = ptr::null_mut::<HewMapEntry>();

        loop {
            let entry = map.entries.add(idx);
            match (*entry).state {
                OCCUPIED => {
                    if libc::strcmp((*entry).key, key) == 0 {
                        return entry;
                    }
                }
                TOMBSTONE => {
                    if first_tombstone.is_null() {
                        first_tombstone = entry;
                    }
                }
                EMPTY => {
                    return if first_tombstone.is_null() {
                        entry
                    } else {
                        first_tombstone
                    };
                }
                _ => unreachable!("invalid hashmap entry state"),
            }
            idx = (idx + 1) & mask;
            if idx == start {
                return if first_tombstone.is_null() {
                    unreachable!("hashmap insert probe found no reusable slot")
                } else {
                    first_tombstone
                };
            }
        }
    }
}

/// Resize the map to double its current capacity.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer with `cap > 0`.
unsafe fn resize(m: *mut HewHashMap) {
    // SAFETY: caller guarantees `m` is valid.
    unsafe {
        let map = &mut *m;
        let old_cap = map.cap;
        let old_entries = map.entries;
        let Some(new_cap) = old_cap.checked_mul(2) else {
            libc::abort();
        };

        let Some(layout_size) = new_cap.checked_mul(core::mem::size_of::<HewMapEntry>()) else {
            libc::abort();
        };
        let new_entries: *mut HewMapEntry =
            libc::calloc(new_cap, core::mem::size_of::<HewMapEntry>()).cast();
        if new_entries.is_null() {
            let _ = layout_size;
            libc::abort();
        }

        map.entries = new_entries;
        map.cap = new_cap;
        map.len = 0;

        let mask = new_cap - 1;
        for i in 0..old_cap {
            let old = &*old_entries.add(i);
            if old.state == OCCUPIED {
                let h = fnv1a(old.key);
                let mut idx = (h as usize) & mask;
                while (*new_entries.add(idx)).state == OCCUPIED {
                    idx = (idx + 1) & mask;
                }
                let dst = &mut *new_entries.add(idx);
                dst.state = OCCUPIED;
                dst.key = old.key;
                dst.value_i32 = old.value_i32;
                dst.value_str = old.value_str;
                dst.value_i64 = old.value_i64;
                dst.value_f64 = old.value_f64;
                map.len += 1;
            }
        }
        libc::free(old_entries.cast()); // ALLOCATOR-PAIRING: libc
    }
}

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

/// Create a new, empty `HewHashMap`.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_hashmap_free_impl`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_new_impl() -> *mut HewHashMap {
    // SAFETY: allocating with libc::malloc/calloc.
    unsafe {
        let m: *mut HewHashMap = libc::malloc(core::mem::size_of::<HewHashMap>()).cast(); // ALLOCATOR-PAIRING: libc
        if m.is_null() {
            libc::abort();
        }
        let entries: *mut HewMapEntry =
            libc::calloc(INIT_CAP, core::mem::size_of::<HewMapEntry>()).cast();
        if entries.is_null() {
            libc::free(m.cast()); // ALLOCATOR-PAIRING: libc
            libc::abort();
        }
        (*m).entries = entries;
        (*m).len = 0;
        (*m).cap = INIT_CAP;
        m
    }
}

// ---------------------------------------------------------------------------
// Insert
// ---------------------------------------------------------------------------

// CUTOVER-TODO(W4.001-C3-S4): The seven legacy `hew_hashmap_*_impl` /
// `hew_hashmap_*_i64` / `hew_hashmap_*_f64` runtime exports below are no
// longer reached from codegen — Stage C3 retired the per-V `_impl` /
// `_<prim>` dispatch arms in `hew-types/src/check/methods.rs` and routes
// every HashMap method call through the resolver-authority
// `hew_hashmap_*_layout` family. The legacy symbols remain in this file
// solely because `hew-runtime/src/hashset.rs` still calls
// `hew_hashmap_insert_impl` / `hew_hashmap_insert_i64` directly (see
// `hashset.rs:27, 88, 112`). Retiring them is blocked on porting the
// HashSet runtime to the layout-descriptor path (tracked as Stage C3
// follow-up: HashSet layout migration). Do not add new callers; do not
// reuse the symbol names for layout-path adapters.

/// Insert or update a key with both `i32` and optional string values.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
/// `val_str` may be null.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_insert_impl(
    m: *mut HewHashMap,
    key: *const c_char,
    val_i32: i32,
    val_str: *const c_char,
) {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        if (*m).len * 100 >= (*m).cap * LOAD_PCTG {
            resize(m);
        }
        let entry = &mut *find_insert_slot(m, key);
        if entry.state == OCCUPIED {
            // Update existing entry.
            entry.value_i32 = val_i32;
            if !entry.value_str.is_null() {
                libc::free(entry.value_str.cast()); // ALLOCATOR-PAIRING: libc
            }
            entry.value_str = if val_str.is_null() {
                ptr::null_mut()
            } else {
                libc::strdup(val_str)
            };
            if !val_str.is_null() && entry.value_str.is_null() {
                libc::abort();
            }
            return;
        }
        // Empty or tombstone slot — insert here.
        entry.state = OCCUPIED;
        entry.key = libc::strdup(key);
        if entry.key.is_null() {
            libc::abort();
        }
        entry.value_i32 = val_i32;
        entry.value_str = if val_str.is_null() {
            ptr::null_mut()
        } else {
            libc::strdup(val_str)
        };
        if !val_str.is_null() && entry.value_str.is_null() {
            libc::abort();
        }
        entry.value_i64 = 0;
        entry.value_f64 = 0.0;
        (*m).len += 1;
    }
}

/// Insert or update a key with an `i64` value.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_insert_i64(m: *mut HewHashMap, key: *const c_char, val: i64) {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        if (*m).len * 100 >= (*m).cap * LOAD_PCTG {
            resize(m);
        }
        let entry = &mut *find_insert_slot(m, key);
        if entry.state == OCCUPIED {
            entry.value_i64 = val;
            return;
        }
        entry.state = OCCUPIED;
        entry.key = libc::strdup(key);
        if entry.key.is_null() {
            libc::abort();
        }
        entry.value_i64 = val;
        entry.value_i32 = 0;
        entry.value_str = ptr::null_mut();
        entry.value_f64 = 0.0;
        (*m).len += 1;
    }
}

/// Insert or update a key with an `f64` value.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_insert_f64(m: *mut HewHashMap, key: *const c_char, val: f64) {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        if (*m).len * 100 >= (*m).cap * LOAD_PCTG {
            resize(m);
        }
        let entry = &mut *find_insert_slot(m, key);
        if entry.state == OCCUPIED {
            entry.value_f64 = val;
            return;
        }
        entry.state = OCCUPIED;
        entry.key = libc::strdup(key);
        if entry.key.is_null() {
            libc::abort();
        }
        entry.value_f64 = val;
        entry.value_i32 = 0;
        entry.value_str = ptr::null_mut();
        entry.value_i64 = 0;
        (*m).len += 1;
    }
}

// ---------------------------------------------------------------------------
// Getters
// ---------------------------------------------------------------------------

/// Get the `i32` value for `key`. Returns 0 if the key is not found.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_i32(m: *mut HewHashMap, key: *const c_char) -> i32 {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        let idx = find_entry(m, key);
        if idx < 0 {
            return 0;
        }
        (*(*m).entries.add(idx as usize)).value_i32
    }
}

/// Get the string value for `key`. Returns null if the key is not found.
///
/// **Note:** Returns a `strdup`'d copy. The caller must `free()` the returned string.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_str_impl(
    m: *mut HewHashMap,
    key: *const c_char,
) -> *const c_char {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        let idx = find_entry(m, key);
        if idx < 0 {
            return ptr::null();
        }
        let raw = (*(*m).entries.add(idx as usize)).value_str;
        if raw.is_null() {
            ptr::null()
        } else {
            let result = libc::strdup(raw);
            if result.is_null() {
                libc::abort();
            }
            result
        }
    }
}

/// Get the `i64` value for `key`. Returns 0 if the key is not found.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_i64(m: *mut HewHashMap, key: *const c_char) -> i64 {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        let idx = find_entry(m, key);
        if idx < 0 {
            return 0;
        }
        (*(*m).entries.add(idx as usize)).value_i64
    }
}

/// Get the `f64` value for `key`. Returns 0.0 if the key is not found.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_f64(m: *mut HewHashMap, key: *const c_char) -> f64 {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        let idx = find_entry(m, key);
        if idx < 0 {
            return 0.0;
        }
        (*(*m).entries.add(idx as usize)).value_f64
    }
}

// ---------------------------------------------------------------------------
// Contains / Remove
// ---------------------------------------------------------------------------

/// Return `true` if the map contains `key`.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_contains_key(m: *mut HewHashMap, key: *const c_char) -> bool {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe { find_entry(m, key) >= 0 }
}

/// Remove `key` from the map. Returns `true` if the key was present.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_remove(m: *mut HewHashMap, key: *const c_char) -> bool {
    // SAFETY: caller guarantees `m` and `key` are valid.
    unsafe {
        let idx = find_entry(m, key);
        if idx < 0 {
            return false;
        }
        let entry = &mut *(*m).entries.add(idx as usize);
        libc::free(entry.key.cast()); // ALLOCATOR-PAIRING: libc
        if !entry.value_str.is_null() {
            libc::free(entry.value_str.cast()); // ALLOCATOR-PAIRING: libc
        }
        entry.state = TOMBSTONE;
        entry.key = ptr::null_mut();
        entry.value_str = ptr::null_mut();
        (*m).len -= 1;
        true
    }
}

// ---------------------------------------------------------------------------
// Queries
// ---------------------------------------------------------------------------

/// Return the number of entries in the map.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_len(m: *mut HewHashMap) -> i64 {
    // SAFETY: caller guarantees `m` is valid.
    unsafe { (*m).len as i64 }
}

/// Return `true` if the map has no entries.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_is_empty(m: *mut HewHashMap) -> bool {
    // SAFETY: caller guarantees `m` is valid.
    unsafe { (*m).len == 0 }
}

// ---------------------------------------------------------------------------
// Iteration / Bulk operations
// ---------------------------------------------------------------------------

/// Return a `HewVec` of all keys (as strings) in the map.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. The returned vec must be freed
/// with [`crate::vec::hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_keys(m: *const HewHashMap) -> *mut crate::vec::HewVec {
    // SAFETY: caller guarantees `m` is valid.
    unsafe {
        let map = &*m;
        let v = crate::vec::hew_vec_new_str();
        for i in 0..map.cap {
            let entry = &*map.entries.add(i);
            if entry.state == OCCUPIED {
                crate::vec::hew_vec_push_str(v, entry.key);
            }
        }
        v
    }
}

/// Return a `HewVec` of all `i32` values in the map.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. The returned vec must be freed
/// with [`crate::vec::hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_values_i32(m: *const HewHashMap) -> *mut crate::vec::HewVec {
    // SAFETY: caller guarantees `m` is valid.
    unsafe {
        let map = &*m;
        let v = crate::vec::hew_vec_new();
        for i in 0..map.cap {
            let entry = &*map.entries.add(i);
            if entry.state == OCCUPIED {
                crate::vec::hew_vec_push_i32(v, entry.value_i32);
            }
        }
        v
    }
}

/// Return a `HewVec` of all non-null string values in the map.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. The returned vec must be freed
/// with [`crate::vec::hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_values_str(m: *const HewHashMap) -> *mut crate::vec::HewVec {
    // SAFETY: caller guarantees `m` is valid.
    unsafe {
        let map = &*m;
        let v = crate::vec::hew_vec_new_str();
        for i in 0..map.cap {
            let entry = &*map.entries.add(i);
            if entry.state == OCCUPIED && !entry.value_str.is_null() {
                crate::vec::hew_vec_push_str(v, entry.value_str);
            }
        }
        v
    }
}

/// Remove all entries from the map.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_clear(m: *mut HewHashMap) {
    // SAFETY: caller guarantees `m` is valid.
    unsafe {
        let map = &mut *m;
        for i in 0..map.cap {
            let entry = &mut *map.entries.add(i);
            if entry.state == OCCUPIED {
                libc::free(entry.key.cast()); // ALLOCATOR-PAIRING: libc
                if !entry.value_str.is_null() {
                    libc::free(entry.value_str.cast()); // ALLOCATOR-PAIRING: libc
                }
            }
            entry.state = EMPTY;
            entry.key = ptr::null_mut();
            entry.value_str = ptr::null_mut();
        }
        map.len = 0;
    }
}

/// Get the `i32` value for `key`, or `default` if not found.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer. `key` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_or_default_i32(
    m: *const HewHashMap,
    key: *const c_char,
    default: i32,
) -> i32 {
    // SAFETY: caller guarantees `m` and `key` are valid. find_entry does not
    // mutate the map; it only takes `*mut` for legacy reasons.
    unsafe {
        let idx = find_entry(m.cast_mut(), key);
        if idx < 0 {
            return default;
        }
        (*(*m).entries.add(idx as usize)).value_i32
    }
}

// ---------------------------------------------------------------------------
// Free
// ---------------------------------------------------------------------------

/// Free all entries (including keys and string values) and the map struct.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer (or null). After this call, `m` is
/// invalid.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_free_impl(m: *mut HewHashMap) {
    // SAFETY: caller guarantees `m` was allocated with malloc (or is null).
    unsafe {
        cabi_guard!(m.is_null());
        for i in 0..(*m).cap {
            let entry = &*(*m).entries.add(i);
            if entry.state == OCCUPIED {
                libc::free(entry.key.cast()); // ALLOCATOR-PAIRING: libc
                if !entry.value_str.is_null() {
                    libc::free(entry.value_str.cast()); // ALLOCATOR-PAIRING: libc
                }
            }
        }
        libc::free((*m).entries.cast()); // ALLOCATOR-PAIRING: libc
        libc::free(m.cast()); // ALLOCATOR-PAIRING: libc
    }
}

// ===========================================================================
// Layout-backed HashMap (`HewLayoutHashMap`) — C-1b (W3.003 slice C-1b)
// ===========================================================================
//
// A sibling of `HewHashMap` that stores opaque key/value blobs whose identity
// is delegated to caller-supplied hash and equality thunks. Slot layout per
// council Rev 1:
//
//   [state: u8][pad to key_align][key blob: key_size]
//                  [pad to val_align][val blob: val_size][pad to max(key_align,val_align)]
//
// All `#[no_mangle]` entry points are fail-closed per LESSONS `boundary-fail-closed`
// (P0): null pointers, missing thunks, zero-size keys, non-power-of-two alignment,
// stride overflow, and `LayoutManaged` ownership are rejected at the boundary.
//
// Ownership contract (LESSONS `ffi-ownership-contracts` P0):
//   - The map owns the byte blobs it copies into its slots.
//   - The map does NOT own `key_layout` / `val_layout` — caller keeps these alive
//     for the lifetime of the map (the runtime reads `hash_fn`/`eq_fn` and the
//     size/align fields on every probe and on free).
//   - For `ownership_kind == Plain` (only supported variant in this slice), blobs
//     are byte-copy-safe — no per-slot drop is run on free / overwrite / removal.

/// Initial capacity for a layout-backed map (must be a power of two).
const LAYOUT_INIT_CAP: usize = 16;

/// Test-only linker anchor (W4.001 Stage C0b).
///
/// Returns the layout-init capacity as a `u64`. The body is trivial; the
/// real purpose is to give external dev-dependency users (e.g.
/// `hew-types/tests/resolved_call_kernel_symbols`) a safe Rust function
/// to reference so the linker pulls `hew_runtime`'s archive into the
/// test binary — which folds in the `#[no_mangle]` layout descriptor
/// statics that the test then resolves via `dlsym`.
///
/// `#[inline(never)]` prevents constant-fold-through-LTO from elision.
#[inline(never)]
#[doc(hidden)]
#[must_use]
pub fn layout_init_capacity_for_link_anchor() -> u64 {
    LAYOUT_INIT_CAP as u64
}

/// Layout-backed open-addressing hash map with variable-stride slots.
///
/// The struct shape is fixed by the C ABI; field order and types are part of
/// the contract that C-3 codegen synthesizes against.
///
/// # Concurrency
///
/// This type contains **no internal synchronisation**. Every operation
/// (`insert_layout` / `get_layout` / `contains_key_layout` / `remove_layout`
/// / `len_layout` / `free_layout`) requires the caller to guarantee exclusive
/// access for the duration of the call. Borrowed value pointers returned by
/// `get_layout` are invalidated by any subsequent mutating call (insert /
/// remove / free) — concurrent access from another thread, or retention of
/// such pointers across a free, is undefined behaviour. See
/// [`hew_hashmap_free_layout`] for the precise free-time contract.
#[repr(C)]
#[derive(Debug)]
pub struct HewLayoutHashMap {
    /// Pointer to the raw byte storage holding `cap * stride` bytes.
    pub entries: *mut u8,
    /// Number of `OCCUPIED` slots.
    pub len: usize,
    /// Total slot count; always a power of two.
    pub cap: usize,
    /// Byte offset of the key blob inside a slot (after the `state` byte and
    /// any padding required to reach `key_layout.align`).
    pub key_offset: usize,
    /// Byte offset of the value blob inside a slot.
    pub val_offset: usize,
    /// Total slot size in bytes (already padded to `max(key_align, val_align)`).
    pub stride: usize,
    /// Key descriptor — **owned by-value snapshot** taken at constructor entry
    /// (W4.001 Stage C0a; plan rev6 §4 Blocker B2). Snapshotting decouples the
    /// map's hash/eq/drop discipline from any post-construction mutation the
    /// caller may make to their original descriptor: the constructor's
    /// fail-closed consistency check is therefore a lifetime invariant for the
    /// map, not just a one-shot at-entry check.
    pub key_layout: HewMapKeyLayout,
    /// Value descriptor — owned by-value snapshot (same rationale).
    pub val_layout: HewMapValueLayout,
}

/// Round `offset` up to the next multiple of `align`. `align` must be a
/// non-zero power of two. Returns `None` on overflow.
#[inline]
fn align_up(offset: usize, align: usize) -> Option<usize> {
    debug_assert!(
        align.is_power_of_two(),
        "align_up requires power-of-two alignment"
    );
    let mask = align - 1;
    offset.checked_add(mask).map(|sum| sum & !mask)
}

/// Compute `(key_offset, val_offset, stride, entries_align)` for a given key
/// and value layout. Returns `None` on any overflow.
fn compute_slot_layout(
    key_size: usize,
    key_align: usize,
    val_size: usize,
    val_align: usize,
) -> Option<(usize, usize, usize, usize)> {
    // State byte sits at offset 0; key blob follows after padding to key_align.
    let key_offset = align_up(1, key_align)?;
    let after_key = key_offset.checked_add(key_size)?;
    let val_offset = align_up(after_key, val_align)?;
    let after_val = val_offset.checked_add(val_size)?;
    let entries_align = core::cmp::max(key_align, val_align);
    let stride = align_up(after_val, entries_align)?;
    // Guard total table size against isize::MAX/4 per council Rev 1.
    // Caller will multiply stride * cap; pre-check stride alone now and recheck
    // after multiplying by capacity at allocation time.
    if stride > (isize::MAX as usize) / 4 {
        return None;
    }
    Some((key_offset, val_offset, stride, entries_align))
}

/// Allocate the entries byte array for a layout map with `cap` slots of size
/// `stride`, zero-initialised. Aborts on overflow or allocation failure.
///
/// # Safety
///
/// `stride > 0`, `cap > 0`, `entries_align` is a power of two.
unsafe fn alloc_layout_entries(cap: usize, stride: usize, entries_align: usize) -> *mut u8 {
    let total = match cap.checked_mul(stride) {
        Some(n) if n <= (isize::MAX as usize) / 4 => n,
        Some(_) | None => {
            crate::set_last_error("HewLayoutHashMap: entries allocation size overflow");
            std::process::abort();
        }
    };
    let Ok(layout) = std::alloc::Layout::from_size_align(total, entries_align) else {
        crate::set_last_error("HewLayoutHashMap: invalid entries Layout");
        std::process::abort();
    };
    // SAFETY: layout has non-zero size (stride > 0, cap > 0).
    let ptr = unsafe { std::alloc::alloc_zeroed(layout) };
    if ptr.is_null() {
        std::alloc::handle_alloc_error(layout);
    }
    ptr
}

/// Free the entries byte array allocated by `alloc_layout_entries`.
///
/// # Safety
///
/// `entries` must have been returned by `alloc_layout_entries` with the same
/// `cap`, `stride`, and `entries_align`.
unsafe fn dealloc_layout_entries(
    entries: *mut u8,
    cap: usize,
    stride: usize,
    entries_align: usize,
) {
    if entries.is_null() || cap == 0 || stride == 0 {
        return;
    }
    let total = cap.saturating_mul(stride);
    if let Ok(layout) = std::alloc::Layout::from_size_align(total, entries_align) {
        // SAFETY: pointer came from alloc_zeroed with this exact layout.
        unsafe { std::alloc::dealloc(entries, layout) };
    }
}

/// Slot view helpers — pure pointer arithmetic; never reads through the layout
/// pointers, so safe to call during resize while layout fields are mid-update.
#[inline]
unsafe fn slot_state(entries: *mut u8, idx: usize, stride: usize) -> *mut u8 {
    // SAFETY: caller guarantees idx < cap and stride matches allocation.
    unsafe { entries.add(idx * stride) }
}

#[inline]
unsafe fn slot_key(entries: *mut u8, idx: usize, stride: usize, key_offset: usize) -> *mut u8 {
    // SAFETY: caller guarantees idx < cap and offsets match allocation.
    unsafe { entries.add(idx * stride + key_offset) }
}

#[inline]
unsafe fn slot_val(entries: *mut u8, idx: usize, stride: usize, val_offset: usize) -> *mut u8 {
    // SAFETY: caller guarantees idx < cap and offsets match allocation.
    unsafe { entries.add(idx * stride + val_offset) }
}

/// Validate a `HewMapKeyLayout` at constructor time. Aborts fail-closed on any
/// violation (LESSONS `boundary-fail-closed` P0).
///
/// Exposed `pub` so `should_panic` tests can target the validator directly:
/// panics cannot unwind across the `extern "C"` boundary under
/// `panic = "abort"`, so test coverage of the abort gates lives at this
/// layer.
///
/// # Panics
///
/// Panics if `key_layout` is null, `hash_fn`/`eq_fn` are `None`, `size == 0`,
/// or `align` is not a power of two.
///
/// **W4.001 Stage C0a:** `LayoutManaged` ownership is no longer rejected
/// here. The new descriptor-consistency check in `new_layout` rejects
/// `String`/`LayoutManaged` ownership combined with `drop_fn == None`
/// instead — fail-closed but more precise.
///
/// # Safety
///
/// `key_layout` must be non-null and point to a valid `HewMapKeyLayout`.
pub unsafe fn validate_key_layout(key_layout: *const HewMapKeyLayout) {
    if key_layout.is_null() {
        crate::set_last_error("HewLayoutHashMap: key_layout is null");
        panic!("HewLayoutHashMap: key_layout is null");
    }
    // SAFETY: caller guarantees non-null + valid.
    let kl = unsafe { &*key_layout };
    if kl.hash_fn.is_none() {
        crate::set_last_error("HewLayoutHashMap: key_layout.hash_fn is None");
        panic!("HewLayoutHashMap: key_layout.hash_fn is None");
    }
    if kl.eq_fn.is_none() {
        crate::set_last_error("HewLayoutHashMap: key_layout.eq_fn is None");
        panic!("HewLayoutHashMap: key_layout.eq_fn is None");
    }
    if kl.size == 0 {
        crate::set_last_error("HewLayoutHashMap: zero-size keys are not admissible");
        panic!("HewLayoutHashMap: zero-size keys are not admissible");
    }
    if !kl.align.is_power_of_two() {
        crate::set_last_error("HewLayoutHashMap: key_layout.align is not a power of two");
        panic!("HewLayoutHashMap: key_layout.align is not a power of two");
    }
    // C0a: LayoutManaged / String ownership are admitted; their drop_fn
    // requirement is enforced by validate_descriptor_ownership (called from
    // the constructor).
}

/// Validate a `HewMapValueLayout` at constructor time.
///
/// Exposed `pub` for the same reason as [`validate_key_layout`].
///
/// # Panics
///
/// Panics if `val_layout` is null, `align` is not a power of two, or a
/// zero-size value layout has `align != 1`.
///
/// **W4.001 Stage C0a:** `LayoutManaged` ownership is no longer rejected
/// here; see [`validate_key_layout`].
///
/// # Safety
///
/// `val_layout` must be non-null and point to a valid `HewMapValueLayout`.
pub unsafe fn validate_val_layout(val_layout: *const HewMapValueLayout) {
    if val_layout.is_null() {
        crate::set_last_error("HewLayoutHashMap: val_layout is null");
        panic!("HewLayoutHashMap: val_layout is null");
    }
    // SAFETY: caller guarantees non-null + valid.
    let vl = unsafe { &*val_layout };
    if !vl.align.is_power_of_two() {
        crate::set_last_error("HewLayoutHashMap: val_layout.align is not a power of two");
        panic!("HewLayoutHashMap: val_layout.align is not a power of two");
    }
    if vl.size == 0 && vl.align != 1 {
        crate::set_last_error(
            "HewLayoutHashMap: zero-size value layout must have align == 1 (HashSet ZST contract)",
        );
        panic!("HewLayoutHashMap: zero-size value layout must have align == 1");
    }
    // C0a: see validate_key_layout for the LayoutManaged-relaxation rationale.
}

/// Fail-closed descriptor-consistency check (W4.001 Stage C0a; plan rev6
/// §4 "Fail-closed descriptor consistency check"; CLAUDE.md §2).
///
/// Rejects descriptors that declare owned-ownership semantics without a
/// matching drop thunk. The four enumerated rejection cases are asserted
/// by `hashmap_layout_descriptor_rejection.rs`.
///
/// Allowed: `Plain` ownership with `drop_fn = Some(_)` (no-op cleanup
/// is harmless; the relaxed-direction does not violate fail-closed
/// because extra cleanup is safe and missing cleanup is the leak hazard).
///
/// # Panics
///
/// Panics with message
/// `"HewLayoutHashMap: {key_layout|val_layout} ownership_kind={String|LayoutManaged} requires drop_fn"`
/// on any of the four rejection cases.
///
/// # Safety
///
/// Both pointers must be non-null and point to valid descriptors (call
/// `validate_key_layout` / `validate_val_layout` first).
pub unsafe fn validate_descriptor_ownership(
    key_layout: *const HewMapKeyLayout,
    val_layout: *const HewMapValueLayout,
) {
    // SAFETY: caller-guaranteed non-null + valid.
    let kl = unsafe { &*key_layout };
    // SAFETY: same.
    let vl = unsafe { &*val_layout };
    match kl.ownership_kind {
        HewTypeOwnershipKind::Plain => {}
        HewTypeOwnershipKind::String => {
            if kl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: key_layout ownership_kind=String requires drop_fn",
                );
                panic!("HewLayoutHashMap: key_layout ownership_kind=String requires drop_fn");
            }
        }
        HewTypeOwnershipKind::LayoutManaged => {
            if kl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: key_layout ownership_kind=LayoutManaged requires drop_fn",
                );
                panic!(
                    "HewLayoutHashMap: key_layout ownership_kind=LayoutManaged requires drop_fn"
                );
            }
        }
    }
    match vl.ownership_kind {
        HewTypeOwnershipKind::Plain => {}
        HewTypeOwnershipKind::String => {
            if vl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: val_layout ownership_kind=String requires drop_fn",
                );
                panic!("HewLayoutHashMap: val_layout ownership_kind=String requires drop_fn");
            }
        }
        HewTypeOwnershipKind::LayoutManaged => {
            if vl.drop_fn.is_none() {
                crate::set_last_error(
                    "HewLayoutHashMap: val_layout ownership_kind=LayoutManaged requires drop_fn",
                );
                panic!(
                    "HewLayoutHashMap: val_layout ownership_kind=LayoutManaged requires drop_fn"
                );
            }
        }
    }
}

/// Validate both layouts and return the slot geometry. Panics on overflow.
///
/// Exposed `pub` so `should_panic` tests can drive the full constructor-side
/// gate chain without crossing an `extern "C"` frame.
///
/// # Panics
///
/// Panics under any condition documented by [`validate_key_layout`] and
/// [`validate_val_layout`], plus on slot stride overflow.
///
/// # Safety
///
/// Both pointers must be non-null and point to valid descriptors.
#[must_use]
pub unsafe fn validate_and_compute_slot_layout(
    key_layout: *const HewMapKeyLayout,
    val_layout: *const HewMapValueLayout,
) -> (usize, usize, usize, usize) {
    // SAFETY: forwarded; the validator itself null-checks.
    unsafe { validate_key_layout(key_layout) };
    // SAFETY: same.
    unsafe { validate_val_layout(val_layout) };
    // SAFETY: both pointers validated non-null above; ownership check is
    // the W4.001 Stage C0a fail-closed descriptor-consistency gate.
    unsafe { validate_descriptor_ownership(key_layout, val_layout) };
    // SAFETY: validated non-null above.
    let kl = unsafe { &*key_layout };
    // SAFETY: validated non-null above.
    let vl = unsafe { &*val_layout };
    let Some(t) = compute_slot_layout(kl.size, kl.align, vl.size, vl.align) else {
        crate::set_last_error("HewLayoutHashMap: slot stride overflow");
        panic!("HewLayoutHashMap: slot stride overflow");
    };
    t
}

/// Fail-closed gate for operational entry points (`insert`/`get`/`contains`/
/// `remove`). Centralises the null checks so both the `extern "C"` boundary
/// and `should_panic` tests drive the same code path — under `panic = "abort"`
/// the panics on this path abort the process (matching the precedent set by
/// `hew_vtable_dispatch_panic_on_oob` in `trait_object.rs`), while under the
/// test profile they unwind so the negative gates can be observed.
///
/// `val` carries insert semantics: `Some(v)` means "the caller is performing
/// an insert with payload `v`"; the gate then consults `(*m).val_layout.size`
/// and rejects `v.is_null()` when `size > 0`. `None` means the op carries no
/// value payload (get / contains / remove).
///
/// # Panics
///
/// Panics if `m` is null, `key` is null, or `val` is `Some(null)` while the
/// registered value layout has non-zero size.
///
/// # Safety
///
/// When `val` is `Some(_)` and `m` is non-null, the function dereferences
/// `(*m).val_layout` to read its size; the layout pointer must remain valid
/// for the duration of the call (it is caller-stable per the ownership
/// contract documented on [`HewLayoutHashMap`]).
pub unsafe fn validate_op_inputs(
    m: *const HewLayoutHashMap,
    key: *const c_void,
    val: Option<*const c_void>,
) {
    if m.is_null() {
        crate::set_last_error("HewLayoutHashMap op: m is null");
        panic!("HewLayoutHashMap op: m is null");
    }
    if key.is_null() {
        crate::set_last_error("HewLayoutHashMap op: key is null");
        panic!("HewLayoutHashMap op: key is null");
    }
    if let Some(v) = val {
        // SAFETY: m non-null per check above; val_layout is now an owned
        // by-value field on the map (W4.001 Stage C0a snapshot), no
        // pointer chase required.
        let val_size = unsafe { (*m).val_layout.size };
        if val_size > 0 && v.is_null() {
            crate::set_last_error("HewLayoutHashMap op: val is null but value size > 0");
            panic!("HewLayoutHashMap op: val is null but value size > 0");
        }
    }
}

/// Fail-closed null check for the single-pointer ops (`len`). Exposed `pub`
/// so `should_panic` tests can drive the gate directly.
///
/// # Panics
///
/// Panics if `m` is null.
///
/// # Safety
///
/// Performs only a null check; safe to call with any `*const`.
pub unsafe fn validate_op_map(m: *const HewLayoutHashMap) {
    if m.is_null() {
        crate::set_last_error("HewLayoutHashMap op: m is null");
        panic!("HewLayoutHashMap op: m is null");
    }
}

/// Probe the map for `key` starting at its hashed slot. Returns the slot index
/// of an existing OCCUPIED match, the first TOMBSTONE encountered, or the
/// first EMPTY slot. The shared walker is used by insert, get, contains, and
/// remove — there is exactly one probe implementation in this module.
///
/// `found_existing` is set to `true` when the returned slot already contains
/// the searched-for key (OCCUPIED + eq).
///
/// # Safety
///
/// All pointers must come from a live `HewLayoutHashMap`. `hash_fn` and
/// `eq_fn` must be the thunks registered on its `key_layout`. `cap` must be a
/// non-zero power of two.
#[allow(
    clippy::too_many_arguments,
    reason = "layout fields are read as separate locals before reborrow to avoid \
              re-deriving them through &self mid-resize (council Rev 1 §7)"
)]
#[allow(
    clippy::cast_possible_truncation,
    reason = "hash truncation u64 -> usize is intentional masking via probe modulus"
)]
unsafe fn layout_probe(
    entries: *mut u8,
    cap: usize,
    stride: usize,
    key_offset: usize,
    key: *const c_void,
    hash_fn: unsafe extern "C" fn(*const c_void) -> u64,
    eq_fn: unsafe extern "C" fn(*const c_void, *const c_void) -> i32,
) -> (usize, bool) {
    let mask = cap - 1;
    // SAFETY: caller guarantees key is valid for the type.
    let h = unsafe { hash_fn(key) };
    let start = (h as usize) & mask;
    let mut idx = start;
    let mut first_tombstone: Option<usize> = None;
    loop {
        // SAFETY: idx < cap; stride/key_offset come from a valid map.
        let state_ptr = unsafe { slot_state(entries, idx, stride) };
        // SAFETY: state byte is in-bounds.
        let state = unsafe { *state_ptr };
        match state {
            EMPTY => {
                return (first_tombstone.unwrap_or(idx), false);
            }
            OCCUPIED => {
                // SAFETY: key offset is in-bounds.
                let slot_key_ptr = unsafe { slot_key(entries, idx, stride, key_offset) };
                // SAFETY: caller-supplied thunk; both pointers valid for layout.
                let eq = unsafe { eq_fn(slot_key_ptr.cast::<c_void>(), key) };
                if eq != 0 {
                    return (idx, true);
                }
            }
            TOMBSTONE => {
                if first_tombstone.is_none() {
                    first_tombstone = Some(idx);
                }
            }
            _ => unreachable!("HewLayoutHashMap: invalid slot state"),
        }
        idx = (idx + 1) & mask;
        if idx == start {
            // Probed the whole table; must have a tombstone or we would have
            // hit EMPTY (resize keeps load < 100%).
            let Some(slot) = first_tombstone else {
                crate::set_last_error("HewLayoutHashMap: full table without empty/tombstone");
                std::process::abort();
            };
            return (slot, false);
        }
    }
}

/// Resize the table to double capacity, re-hashing every OCCUPIED slot.
///
/// # Safety
///
/// `m` must be a valid, fully-initialised `HewLayoutHashMap` pointer.
#[allow(
    clippy::cast_possible_truncation,
    reason = "hash truncation u64 -> usize on 32-bit targets is intentional probe masking"
)]
unsafe fn layout_resize(m: *mut HewLayoutHashMap) {
    // Council Rev 1 §7: bind every layout-derived value as a scalar / raw-pointer
    // local BEFORE any allocator call. No Rust reference (`&*m`, `&mut *m`, or
    // a reference borrowed through them) is retained across `alloc(...)` or
    // `dealloc(...)`. The rebuild loop below operates exclusively on these
    // locals and the raw entries pointers; field writes back to `*m` happen
    // only after the dealloc of the old entries.

    // -- Step 1: copy all needed map fields as scalars / raw pointers ----------
    // SAFETY: caller guarantees `m` is a valid, fully-initialised pointer; the
    // raw field projections below read plain scalar / raw-pointer fields and
    // do not borrow `*m`.
    //
    // W4.001 Stage C0a: `key_layout` / `val_layout` are now owned by-value
    // snapshots on the map (not raw pointers). We read their *fields* as
    // scalar locals here — never borrowing the descriptor itself across the
    // allocator — preserving the council Rev 1 §7 discipline.
    let (old_entries, old_cap, stride, key_offset, val_offset): (
        *mut u8,
        usize,
        usize,
        usize,
        usize,
    ) = unsafe {
        (
            (*m).entries,
            (*m).cap,
            (*m).stride,
            (*m).key_offset,
            (*m).val_offset,
        )
    };

    // -- Step 2: copy needed layout descriptor fields as scalars ---------------
    // SAFETY: `m` valid; reads of the by-value descriptor's scalar fields are
    // plain scalar loads and do not borrow `*m`.
    let (key_size, key_align, val_size, val_align, hash_fn_opt) = unsafe {
        (
            (*m).key_layout.size,
            (*m).key_layout.align,
            (*m).val_layout.size,
            (*m).val_layout.align,
            (*m).key_layout.hash_fn,
        )
    };
    let entries_align = core::cmp::max(key_align, val_align);

    let Some(hash_fn) = hash_fn_opt else {
        crate::set_last_error(
            "HewLayoutHashMap: hash_fn None at resize (constructor guard violated)",
        );
        std::process::abort();
    };

    let Some(new_cap) = old_cap.checked_mul(2) else {
        crate::set_last_error("HewLayoutHashMap: capacity doubling overflow");
        std::process::abort();
    };

    // -- Step 3: allocate new entries (no Rust reference to *m is live here) --
    // SAFETY: stride > 0 by construction; new_cap > 0.
    let new_entries = unsafe { alloc_layout_entries(new_cap, stride, entries_align) };

    // -- Step 4: rebuild via raw pointers and scalar locals only --------------
    let new_mask = new_cap - 1;
    for i in 0..old_cap {
        // SAFETY: i < old_cap; old_entries valid for the original table.
        let state = unsafe { *slot_state(old_entries, i, stride) };
        if state != OCCUPIED {
            continue;
        }
        // SAFETY: occupied slot has valid key + value blobs at known offsets.
        let src_key = unsafe { slot_key(old_entries, i, stride, key_offset) };
        // SAFETY: hash_fn is the thunk registered on key_layout, which has not
        // moved across the resize (caller-stable descriptors).
        let h = unsafe { hash_fn(src_key.cast::<c_void>()) };
        let mut idx = (h as usize) & new_mask;
        loop {
            // SAFETY: idx < new_cap; new_entries freshly zero-initialised so
            // every state byte is EMPTY (0) until we write OCCUPIED.
            let dst_state = unsafe { slot_state(new_entries, idx, stride) };
            // SAFETY: in-bounds; state byte is u8.
            if unsafe { *dst_state } == EMPTY {
                // SAFETY: dst_state is in-bounds; OCCUPIED constant is u8.
                unsafe { *dst_state = OCCUPIED };
                // SAFETY: dst key/value offsets in-bounds; src equally so;
                // key_size / val_size are the scalar copies bound above.
                unsafe {
                    let dst_key = slot_key(new_entries, idx, stride, key_offset);
                    ptr::copy_nonoverlapping(src_key, dst_key, key_size);
                    if val_size > 0 {
                        let src_val = slot_val(old_entries, i, stride, val_offset);
                        let dst_val = slot_val(new_entries, idx, stride, val_offset);
                        ptr::copy_nonoverlapping(src_val, dst_val, val_size);
                    }
                }
                break;
            }
            idx = (idx + 1) & new_mask;
        }
    }

    // -- Step 5: release the old entries before writing back ------------------
    // SAFETY: old_entries was allocated with (old_cap, stride, entries_align).
    unsafe { dealloc_layout_entries(old_entries, old_cap, stride, entries_align) };

    // -- Step 6: write the new pointer / capacity back via raw field writes ---
    // No `&mut *m` borrow is taken; the writes go through the raw pointer
    // directly. `len` is unchanged (every OCCUPIED slot moved exactly once).
    // SAFETY: `m` is a valid pointer; fields are plain scalars / raw pointers.
    unsafe {
        (*m).entries = new_entries;
        (*m).cap = new_cap;
    }
}

// ---------------------------------------------------------------------------
// Constructor (layout-backed)
// ---------------------------------------------------------------------------

/// Create a new layout-backed `HewLayoutHashMap`.
///
/// Fail-closed gates (council Rev 2/3 + W4.001 Stage C0a): aborts on null
/// layout pointers, missing hash/eq thunks, zero-size key, non-power-of-two
/// alignment, malformed ZST value layout, owned ownership without matching
/// `drop_fn` (`String`/`LayoutManaged` require `Some(_)`), or stride overflow.
///
/// **W4.001 Stage C0a — descriptor snapshot by value.** Caller-provided
/// descriptors are copied into the map at constructor entry (after all
/// fail-closed validation). Subsequent caller mutation of the original
/// descriptor bytes is harmless — the map honours its snapshot for the
/// remainder of its lifetime (plan rev6 §4 Blocker B2).
///
/// # Safety
///
/// `key_layout` and `val_layout` must point to valid descriptors **at
/// constructor entry**. After this call returns, the caller may free or
/// mutate the descriptor bytes; the map no longer reads through the
/// original pointers. The returned pointer must be freed with
/// [`hew_hashmap_free_layout`].
// WASM-TODO(#1820): hew_hashmap_new_with_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_new_with_layout(
    key_layout: *const HewMapKeyLayout,
    val_layout: *const HewMapValueLayout,
) -> *mut HewLayoutHashMap {
    if key_layout.is_null() {
        crate::set_last_error("hew_hashmap_new_with_layout: key_layout is null");
        // SAFETY: extern "C" cannot unwind; abort the process. Tests cover the
        // null-layout case via the testable `validate_key_layout` helper.
        std::process::abort();
    }
    if val_layout.is_null() {
        crate::set_last_error("hew_hashmap_new_with_layout: val_layout is null");
        // SAFETY: same — abort across the C boundary.
        std::process::abort();
    }
    // SAFETY: non-null checked above; validator panics fail-closed on any
    // gate violation (including the C0a ownership/drop-fn consistency check).
    // Under `panic = "abort"` the panic aborts the process before unwinding
    // crosses this frame, preserving the C ABI contract.
    let (key_offset, val_offset, stride, entries_align) =
        unsafe { validate_and_compute_slot_layout(key_layout, val_layout) };

    // W4.001 Stage C0a: snapshot descriptors by value *after* validation.
    // SAFETY: validated non-null + valid by the call above; descriptors are
    // `#[repr(C)] Copy` PODs, so `read` is a fixed-size memcpy with no drop
    // glue. After this point the map no longer reads through the caller's
    // pointer.
    let key_layout_snapshot = unsafe { ptr::read(key_layout) };
    // SAFETY: same — see comment above.
    let val_layout_snapshot = unsafe { ptr::read(val_layout) };

    let cap = LAYOUT_INIT_CAP;
    // SAFETY: cap > 0, stride > 0, entries_align is power of two.
    let entries = unsafe { alloc_layout_entries(cap, stride, entries_align) };

    let struct_layout = std::alloc::Layout::new::<HewLayoutHashMap>();
    // SAFETY: non-zero size; alloc returns a pointer aligned to
    // `align_of::<HewLayoutHashMap>()` per Layout, so the typed cast below
    // is sound. We cast through a typed `NonNull` to make the intent obvious.
    let raw_bytes = unsafe { std::alloc::alloc(struct_layout) };
    if raw_bytes.is_null() {
        // SAFETY: entries just allocated with these params.
        unsafe { dealloc_layout_entries(entries, cap, stride, entries_align) };
        std::alloc::handle_alloc_error(struct_layout);
    }
    let raw: *mut HewLayoutHashMap = raw_bytes.cast();
    // SAFETY: raw is a fresh allocation of HewLayoutHashMap size.
    unsafe {
        ptr::write(
            raw,
            HewLayoutHashMap {
                entries,
                len: 0,
                cap,
                key_offset,
                val_offset,
                stride,
                key_layout: key_layout_snapshot,
                val_layout: val_layout_snapshot,
            },
        );
    }
    raw
}

fn abort_layout_clone(reason: impl Into<String>) -> ! {
    crate::set_last_error(reason);
    std::process::abort();
}

unsafe fn clone_layout_string_blob(src: *const u8, dst: *mut u8, label: &str) {
    // SAFETY: caller guarantees `src` points to a slot blob containing a
    // C-string pointer and `dst` points to writable slot storage for the same
    // blob shape.
    let src_ptr: *const c_char = unsafe { ptr::read_unaligned(src.cast::<*const c_char>()) };
    // SAFETY: `src_ptr` is either null or a valid NUL-terminated string by the
    // descriptor's String ownership contract.
    let cloned = unsafe { crate::string::hew_string_clone(src_ptr) };
    if !src_ptr.is_null() && cloned.is_null() {
        abort_layout_clone(format!("{label}: string clone allocation failed"));
    }
    // SAFETY: `dst` is writable for a pointer-sized String blob.
    unsafe { ptr::write_unaligned(dst.cast::<*mut c_char>(), cloned) };
}

unsafe fn clone_layout_key_blob(
    ownership_kind: HewTypeOwnershipKind,
    src: *const u8,
    dst: *mut u8,
    size: usize,
) {
    match ownership_kind {
        HewTypeOwnershipKind::Plain => {
            // SAFETY: caller guarantees both blobs are valid for `size` bytes.
            unsafe { ptr::copy_nonoverlapping(src, dst, size) };
        }
        HewTypeOwnershipKind::String => {
            // SAFETY: forwarded blob contract.
            unsafe { clone_layout_string_blob(src, dst, "hew_hashmap_clone_layout key") };
        }
        HewTypeOwnershipKind::LayoutManaged => abort_layout_clone(
            "hew_hashmap_clone_layout: key layout-managed clone thunk is unavailable",
        ),
    }
}

unsafe fn clone_layout_value_blob(layout: HewMapValueLayout, src: *const u8, dst: *mut u8) {
    match layout.ownership_kind {
        HewTypeOwnershipKind::Plain => {
            if layout.size > 0 {
                // SAFETY: caller guarantees both blobs are valid for
                // `layout.size` bytes.
                unsafe { ptr::copy_nonoverlapping(src, dst, layout.size) };
            }
        }
        HewTypeOwnershipKind::String => {
            // SAFETY: forwarded blob contract.
            unsafe { clone_layout_string_blob(src, dst, "hew_hashmap_clone_layout value") };
        }
        HewTypeOwnershipKind::LayoutManaged => {
            let Some(clone_fn) = layout.clone_fn else {
                abort_layout_clone(
                    "hew_hashmap_clone_layout: value layout-managed clone thunk is unavailable",
                );
            };
            clone_fn(src.cast::<c_void>(), dst.cast::<c_void>());
        }
    }
}

/// Deep-clone a layout-backed map, duplicating owned slot blobs when the
/// descriptor provides a concrete clone discipline. Layout-managed keys fail
/// closed because `HewMapKeyLayout` has no key clone thunk field.
///
/// # Safety
///
/// `m` must have been returned by [`hew_hashmap_new_with_layout`] (or be null).
/// The returned pointer must eventually be freed with
/// [`hew_hashmap_free_layout`].
// WASM-TODO(#1820): hew_hashmap_clone_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_clone_layout(
    m: *const HewLayoutHashMap,
) -> *mut HewLayoutHashMap {
    if m.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: shared fail-closed gate (map-only variant).
    unsafe { validate_op_map(m) };

    // SAFETY: m non-null and constructed via hew_hashmap_new_with_layout.
    let src = unsafe { &*m };
    let entries_align = core::cmp::max(src.key_layout.align, src.val_layout.align);
    if matches!(
        src.key_layout.ownership_kind,
        HewTypeOwnershipKind::LayoutManaged
    ) {
        abort_layout_clone(
            "hew_hashmap_clone_layout: key layout-managed clone thunk is unavailable",
        );
    }
    if matches!(
        src.val_layout.ownership_kind,
        HewTypeOwnershipKind::LayoutManaged
    ) && src.val_layout.clone_fn.is_none()
    {
        abort_layout_clone(
            "hew_hashmap_clone_layout: value layout-managed clone thunk is unavailable",
        );
    }

    // SAFETY: source map was validated at construction time.
    let cloned_entries = unsafe { alloc_layout_entries(src.cap, src.stride, entries_align) };
    let struct_layout = std::alloc::Layout::new::<HewLayoutHashMap>();
    // SAFETY: non-zero sized layout allocation.
    let raw_bytes = unsafe { std::alloc::alloc(struct_layout) };
    if raw_bytes.is_null() {
        // SAFETY: entries just allocated with these params.
        unsafe { dealloc_layout_entries(cloned_entries, src.cap, src.stride, entries_align) };
        std::alloc::handle_alloc_error(struct_layout);
    }
    let cloned: *mut HewLayoutHashMap = raw_bytes.cast();
    // SAFETY: cloned is a fresh allocation of HewLayoutHashMap size.
    unsafe {
        ptr::write(
            cloned,
            HewLayoutHashMap {
                entries: cloned_entries,
                len: src.len,
                cap: src.cap,
                key_offset: src.key_offset,
                val_offset: src.val_offset,
                stride: src.stride,
                key_layout: src.key_layout,
                val_layout: src.val_layout,
            },
        );
    }

    for idx in 0..src.cap {
        // SAFETY: idx < cap, offsets/stride came from the source map.
        let src_state = unsafe { *slot_state(src.entries, idx, src.stride) };
        // SAFETY: idx < cap in the cloned allocation.
        let dst_state = unsafe { slot_state(cloned_entries, idx, src.stride) };
        // SAFETY: dst_state is in-bounds.
        unsafe { *dst_state = src_state };
        if src_state != OCCUPIED {
            continue;
        }
        // SAFETY: occupied slot has valid blobs at the stored offsets.
        let src_key = unsafe { slot_key(src.entries, idx, src.stride, src.key_offset) };
        // SAFETY: destination slot is in-bounds in the cloned allocation.
        let dst_key = unsafe { slot_key(cloned_entries, idx, src.stride, src.key_offset) };
        // SAFETY: forwarded blob contracts.
        unsafe {
            clone_layout_key_blob(
                src.key_layout.ownership_kind,
                src_key,
                dst_key,
                src.key_layout.size,
            );
        }
        if src.val_layout.size > 0 {
            // SAFETY: occupied slot has a valid value blob.
            let src_val = unsafe { slot_val(src.entries, idx, src.stride, src.val_offset) };
            // SAFETY: destination slot is in-bounds in the cloned allocation.
            let dst_val = unsafe { slot_val(cloned_entries, idx, src.stride, src.val_offset) };
            // SAFETY: forwarded blob contracts.
            unsafe { clone_layout_value_blob(src.val_layout, src_val, dst_val) };
        }
    }

    cloned
}

// ---------------------------------------------------------------------------
// Insert / Get / Contains / Remove / Len (layout-backed)
// ---------------------------------------------------------------------------

/// Insert or overwrite `key -> val`. Returns `true` if a new entry was added,
/// `false` if an existing key was overwritten.
///
/// `val` may be null only when the value layout's `size` is zero (`HashSet`
/// contract); otherwise null aborts fail-closed.
///
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a readable
/// blob of the registered key layout. `val` likewise for the value layout
/// (when size > 0).
// WASM-TODO(#1820): hew_hashmap_insert_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_insert_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
    val: *const c_void,
) -> bool {
    // SAFETY: forwarded to the shared `pub` gate; null inputs abort under the
    // workspace `panic = "abort"` profile.
    unsafe { validate_op_inputs(m.cast_const(), key, Some(val)) };

    // Council Rev 1 §7: do NOT hold a Rust reference (`&mut *m`) across the
    // possible `layout_resize` call below. Read every needed scalar / raw
    // pointer via raw projection BEFORE the resize decision. Re-read fields
    // that resize may mutate (`entries`, `cap`) AFTER resize completes.

    // Snapshot layout descriptor fields we need (descriptors are now owned
    // by-value snapshots on the map; W4.001 Stage C0a).
    // SAFETY: m non-null per validate_op_inputs above; reads of the by-value
    // descriptor fields are plain scalar loads.
    let (key_size, val_size, hash_fn_opt, eq_fn_opt, val_drop_fn_opt) = unsafe {
        (
            (*m).key_layout.size,
            (*m).val_layout.size,
            (*m).key_layout.hash_fn,
            (*m).key_layout.eq_fn,
            (*m).val_layout.drop_fn,
        )
    };
    let (Some(hash_fn), Some(eq_fn)) = (hash_fn_opt, eq_fn_opt) else {
        crate::set_last_error(
            "hew_hashmap_insert_layout: hash_fn/eq_fn None (constructor guard violated)",
        );
        std::process::abort();
    };

    // Resize BEFORE probing so the slot we choose lives in the post-resize table.
    // Load factor threshold = 75%.
    // SAFETY: m non-null; raw field reads only — no Rust reference is held
    // across the resize call.
    let (len_now, cap_now) = unsafe { ((*m).len, (*m).cap) };
    if len_now.saturating_add(1) * 100 >= cap_now * LOAD_PCTG {
        // SAFETY: m valid; resize itself observes the Rev 1 §7 discipline.
        unsafe { layout_resize(m) };
    }

    // Re-read post-resize geometry via raw projection.
    // SAFETY: m valid; raw scalar field reads only.
    let (entries, cap, stride, key_offset, val_offset) = unsafe {
        (
            (*m).entries,
            (*m).cap,
            (*m).stride,
            (*m).key_offset,
            (*m).val_offset,
        )
    };

    // SAFETY: thunks valid; key valid per caller contract.
    let (idx, existed) =
        unsafe { layout_probe(entries, cap, stride, key_offset, key, hash_fn, eq_fn) };

    // SAFETY: idx < cap, offsets in-bounds.
    let state_ptr = unsafe { slot_state(entries, idx, stride) };
    // SAFETY: idx < cap.
    let dst_key = unsafe { slot_key(entries, idx, stride, key_offset) };
    // SAFETY: idx < cap.
    let dst_val = unsafe { slot_val(entries, idx, stride, val_offset) };

    // SAFETY: state byte in-bounds.
    unsafe { *state_ptr = OCCUPIED };

    if existed {
        // W4.001 Stage C0a contract (plan rev6 §4 "Acquisition / ownership
        // contract", occupied-slot row + invariant 5: "Overwrite never
        // drops K, always drops old V").
        //
        // 1. The stored slot K stays in place — it is the equality-witness
        //    that the probe found, and re-copying caller-K bytes would leak
        //    the stored K's owned allocation when K is `String` /
        //    `LayoutManaged`. The duplicate K_in the caller passed is the
        //    caller's responsibility to drop or recycle (hoisted to the
        //    Stage C HIR consumer / codegen materializer).
        // 2. The old V is dropped via `val_layout.drop_fn` before the new V
        //    overwrites it. `drop_fn = None` (Plain ownership) is the
        //    no-op fast path.
        // 3. The stored K's `drop_fn` is NEVER invoked here — the slot K is
        //    reused, not freed.
        if val_size > 0 {
            if let Some(val_drop) = val_drop_fn_opt {
                // SAFETY: dst_val points to the old V blob owned by the map;
                // `drop_fn` is the descriptor-registered owned-V drop thunk and
                // runs the type's drop plan in place (it must not deallocate
                // the slot bytes themselves — the kernel owns slot storage).
                val_drop(dst_val.cast::<c_void>());
            }
        }
        // Raw-copy new V over the (now-dropped) old V slot bytes.
        if val_size > 0 {
            // SAFETY: dst_val and val are valid blobs of `val_size` bytes (val
            // non-null was enforced by validate_op_inputs when val_size > 0).
            unsafe { ptr::copy_nonoverlapping(val.cast::<u8>(), dst_val, val_size) };
        }
    } else {
        // Vacant slot: raw-copy K and V bytes into slot; possession transfers
        // to the map (plan rev6 §4 contract-table vacant-slot row).
        // SAFETY: dst_key and key are valid blobs of `key_size` bytes.
        unsafe { ptr::copy_nonoverlapping(key.cast::<u8>(), dst_key, key_size) };
        if val_size > 0 {
            // SAFETY: dst_val and val are valid blobs of `val_size` bytes.
            unsafe { ptr::copy_nonoverlapping(val.cast::<u8>(), dst_val, val_size) };
        }
        // SAFETY: m valid; raw scalar increment.
        unsafe { (*m).len += 1 };
    }
    !existed
}

/// Look up a key. Returns a borrowed pointer to the value blob, or null if the
/// key is absent.
///
/// **Pointer validity contract.** When the returned pointer is non-null it is
/// valid for reads of exactly `val_layout.size` bytes (the size registered on
/// the value layout descriptor at construction). The pointer remains valid
/// only until the next mutation of `m` (`insert_layout`, `remove_layout`,
/// `free_layout`, or any operation that may trigger a resize) — after such a
/// mutation the pointer is dangling and must not be read.
///
/// **ZST values (`HashSet` contract).** When `val_layout.size == 0` the
/// returned pointer is a *presence token* only: it indicates the key is
/// present but does not point to any readable byte. Dereferencing it is
/// undefined behaviour. Callers that only need a presence answer should use
/// [`hew_hashmap_contains_key_layout`] instead, which returns a `bool` and
/// avoids the misuse hazard.
///
/// The caller must not free the returned pointer or write through it.
///
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap`. `key` must point to a valid key blob.
// WASM-TODO(#1820): hew_hashmap_get_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_get_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
) -> *const c_void {
    // SAFETY: shared fail-closed gate; no value pointer involved in lookup.
    unsafe { validate_op_inputs(m, key, None) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &*m };
    if map.cap == 0 || map.len == 0 {
        return ptr::null();
    }
    // W4.001 Stage C0a: descriptors are owned by-value snapshots.
    let kl = &map.key_layout;
    let (Some(hash_fn), Some(eq_fn)) = (kl.hash_fn, kl.eq_fn) else {
        crate::set_last_error(
            "hew_hashmap_get_layout: hash_fn/eq_fn None (constructor guard violated)",
        );
        std::process::abort();
    };
    // SAFETY: layout fields valid.
    let (idx, found) = unsafe {
        layout_probe(
            map.entries,
            map.cap,
            map.stride,
            map.key_offset,
            key,
            hash_fn,
            eq_fn,
        )
    };
    if !found {
        return ptr::null();
    }
    // SAFETY: idx < cap; val_offset valid.
    let val_ptr = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
    val_ptr.cast::<c_void>().cast_const()
}

/// Predicate form of `hew_hashmap_get_layout`.
///
/// # Safety
///
/// Same as [`hew_hashmap_get_layout`].
// WASM-TODO(#1820): hew_hashmap_contains_key_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_contains_key_layout(
    m: *const HewLayoutHashMap,
    key: *const c_void,
) -> bool {
    // SAFETY: forwarded to get_layout which validates inputs.
    !unsafe { hew_hashmap_get_layout(m, key) }.is_null()
}

/// Remove a key. Returns `true` if a matching entry was found and tombstoned,
/// `false` otherwise.
///
/// # Safety
///
/// Same as [`hew_hashmap_get_layout`].
// WASM-TODO(#1820): hew_hashmap_remove_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_remove_layout(
    m: *mut HewLayoutHashMap,
    key: *const c_void,
) -> bool {
    // SAFETY: shared fail-closed gate.
    unsafe { validate_op_inputs(m.cast_const(), key, None) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &mut *m };
    if map.cap == 0 || map.len == 0 {
        return false;
    }
    // W4.001 Stage C0a: descriptors are owned by-value snapshots.
    // Snapshot the drop thunks + scalar layout fields BEFORE taking the
    // mutable handle to the entries (avoids any aliasing between the
    // descriptor read and the slot pointer arithmetic below).
    let (hash_fn_opt, eq_fn_opt, key_drop_fn_opt, val_drop_fn_opt, val_size) = (
        map.key_layout.hash_fn,
        map.key_layout.eq_fn,
        map.key_layout.drop_fn,
        map.val_layout.drop_fn,
        map.val_layout.size,
    );
    let (Some(hash_fn), Some(eq_fn)) = (hash_fn_opt, eq_fn_opt) else {
        crate::set_last_error(
            "hew_hashmap_remove_layout: hash_fn/eq_fn None (constructor guard violated)",
        );
        std::process::abort();
    };
    // SAFETY: layout fields valid.
    let (idx, found) = unsafe {
        layout_probe(
            map.entries,
            map.cap,
            map.stride,
            map.key_offset,
            key,
            hash_fn,
            eq_fn,
        )
    };
    if !found {
        return false;
    }
    // W4.001 Stage C0a (plan rev6 §4 contract-table remove row + invariant
    // 3): drop the stored K + V via the descriptor thunks BEFORE tombstoning.
    // The caller's borrowed lookup K is untouched (kernel never sees it
    // again after `eq_fn` returned true). The drop+tombstone sequence is
    // sync C with no `.await` between (CLAUDE.md §1 — kernel invariant
    // documented in plan §4).
    //
    // SAFETY: idx < cap; offsets in-bounds; slot is OCCUPIED.
    let slot_key_ptr = unsafe { slot_key(map.entries, idx, map.stride, map.key_offset) };
    if let Some(key_drop) = key_drop_fn_opt {
        // SAFETY: slot K is an owned blob registered to the K drop thunk.
        key_drop(slot_key_ptr.cast::<c_void>());
    }
    if val_size > 0 {
        if let Some(val_drop) = val_drop_fn_opt {
            // SAFETY: slot V is an owned blob registered to the V drop thunk;
            // val_size > 0 guarantees there is an actual blob to drop.
            let slot_val_ptr = unsafe { slot_val(map.entries, idx, map.stride, map.val_offset) };
            val_drop(slot_val_ptr.cast::<c_void>());
        }
    }
    // SAFETY: idx < cap.
    let state_ptr = unsafe { slot_state(map.entries, idx, map.stride) };
    // SAFETY: state byte in-bounds.
    unsafe { *state_ptr = TOMBSTONE };
    map.len -= 1;
    true
}

/// Number of occupied entries.
///
/// # Safety
///
/// `m` must be a valid `HewLayoutHashMap`.
// WASM-TODO(#1820): hew_hashmap_len_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_len_layout(m: *const HewLayoutHashMap) -> i64 {
    // SAFETY: shared fail-closed gate (map-only variant).
    unsafe { validate_op_map(m) };
    // SAFETY: m non-null per gate.
    let map = unsafe { &*m };
    // Workspace caps len well below i64::MAX; the cast is documented lossless
    // by the crate-level `cast_possible_wrap` allow above.
    map.len as i64
}

/// Free a layout-backed map and its byte storage. A null `m` is a documented
/// no-op (LESSONS `boundary-fail-closed`: fail-closed shape, not silent
/// success — null in / null out, no further work).
///
/// # Concurrency contract
///
/// All `HewLayoutHashMap` operations — including `free_layout` — require
/// **external synchronisation by the caller**. The type does *not* contain
/// internal locking. Specifically, when this function is called:
///
/// * No other thread may hold a reference to (or pointer into) `*m`.
/// * No other thread may be mid-probe (mid-call) on any of the
///   `insert_layout` / `get_layout` / `contains_key_layout` / `remove_layout`
///   / `len_layout` entry points against `m`.
/// * No thread may still be holding a borrowed value pointer returned by a
///   prior `get_layout` call against `m` (such pointers become dangling the
///   instant this function returns; reading them after the free is undefined
///   behaviour).
///
/// Violating any of these requirements is undefined behaviour. The map ABI is
/// designed for single-owner / single-threaded use at the boundary; callers
/// that need shared ownership must layer their own synchronisation
/// (`Mutex<*mut HewLayoutHashMap>` or equivalent) above this surface.
///
/// # Safety
///
/// `m` must have been returned by [`hew_hashmap_new_with_layout`] (or be null).
/// After this call, `m` is invalid.
// WASM-TODO(#1820): hew_hashmap_free_layout not yet ported to wasm32
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_free_layout(m: *mut HewLayoutHashMap) {
    if m.is_null() {
        return;
    }
    // SAFETY: m non-null and constructed via hew_hashmap_new_with_layout.
    let map_ref = unsafe { &*m };
    let entries = map_ref.entries;
    let cap = map_ref.cap;
    let stride = map_ref.stride;
    let key_offset = map_ref.key_offset;
    let val_offset = map_ref.val_offset;
    // W4.001 Stage C0a: descriptors are owned by-value snapshots.
    let kl = &map_ref.key_layout;
    let vl = &map_ref.val_layout;
    let entries_align = core::cmp::max(kl.align, vl.align);
    let key_drop_fn_opt = kl.drop_fn;
    let val_drop_fn_opt = vl.drop_fn;
    let val_size = vl.size;

    // W4.001 Stage C0a (plan rev6 §4 contract-table free row + invariant 4):
    // iterate occupied slots, drop K + V on each via the descriptor thunks
    // before deallocating the entries buffer. Tombstoned slots already had
    // their blobs dropped at remove-time and must not be re-dropped.
    //
    // `Plain` ownership descriptors carry `drop_fn = None`; the per-slot loop
    // skips the call and the overall cost collapses to a single pass over
    // the state bytes (≈ one branch-not-taken per slot) plus the eventual
    // dealloc. The constructor's fail-closed check guarantees that owned
    // ownership kinds (`String`/`LayoutManaged`) always have a `drop_fn`,
    // so this loop cannot leak on those paths.
    if entries.is_null() || cap == 0 {
        // Defensive: nothing to iterate. Fall through to dealloc which is
        // itself a no-op on null entries (`dealloc_layout_entries`).
    } else if key_drop_fn_opt.is_some() || (val_size > 0 && val_drop_fn_opt.is_some()) {
        for idx in 0..cap {
            // SAFETY: idx < cap; stride matches allocation.
            let state = unsafe { *slot_state(entries, idx, stride) };
            if state != OCCUPIED {
                continue;
            }
            if let Some(key_drop) = key_drop_fn_opt {
                // SAFETY: occupied slot has a valid K blob at key_offset.
                let slot_key_ptr = unsafe { slot_key(entries, idx, stride, key_offset) };
                key_drop(slot_key_ptr.cast::<c_void>());
            }
            if val_size > 0 {
                if let Some(val_drop) = val_drop_fn_opt {
                    // SAFETY: occupied slot has a valid V blob at val_offset.
                    let slot_val_ptr = unsafe { slot_val(entries, idx, stride, val_offset) };
                    val_drop(slot_val_ptr.cast::<c_void>());
                }
            }
        }
    }

    // SAFETY: entries allocated by alloc_layout_entries with these exact params.
    unsafe { dealloc_layout_entries(entries, cap, stride, entries_align) };

    // SAFETY: m allocated by std::alloc::alloc with Layout::new::<HewLayoutHashMap>().
    unsafe {
        ptr::drop_in_place(m);
        std::alloc::dealloc(
            m.cast::<u8>(),
            std::alloc::Layout::new::<HewLayoutHashMap>(),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    fn colliding_keys() -> (CString, CString) {
        for lhs in 0..64 {
            let key_a = CString::new(format!("collision_{lhs}")).unwrap();
            // SAFETY: key_a is a valid NUL-terminated CString.
            let bucket_a = unsafe { fnv1a(key_a.as_ptr()) as usize & (INIT_CAP - 1) };
            for rhs in lhs + 1..64 {
                let key_b = CString::new(format!("collision_{rhs}")).unwrap();
                // SAFETY: key_b is a valid NUL-terminated CString.
                let bucket_b = unsafe { fnv1a(key_b.as_ptr()) as usize & (INIT_CAP - 1) };
                if bucket_a == bucket_b {
                    return (key_a, key_b);
                }
            }
        }
        panic!("expected at least one colliding key pair");
    }

    #[test]
    fn test_hashmap_new_and_len() {
        // SAFETY: FFI calls use valid pointers returned by hew_hashmap_new_impl.
        unsafe {
            let m = hew_hashmap_new_impl();
            assert!(!m.is_null());
            assert_eq!(hew_hashmap_len(m), 0);
            assert!(hew_hashmap_is_empty(m));
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_insert_and_get() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("hello").unwrap();
            hew_hashmap_insert_impl(m, key.as_ptr(), 42, core::ptr::null());
            assert_eq!(hew_hashmap_len(m), 1);
            assert_eq!(hew_hashmap_get_i32(m, key.as_ptr()), 42);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_contains_key() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("present").unwrap();
            let missing = CString::new("missing").unwrap();
            hew_hashmap_insert_impl(m, key.as_ptr(), 1, core::ptr::null());
            assert!(hew_hashmap_contains_key(m, key.as_ptr()));
            assert!(!hew_hashmap_contains_key(m, missing.as_ptr()));
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_overwrite() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("key").unwrap();
            hew_hashmap_insert_impl(m, key.as_ptr(), 10, core::ptr::null());
            assert_eq!(hew_hashmap_get_i32(m, key.as_ptr()), 10);
            hew_hashmap_insert_impl(m, key.as_ptr(), 20, core::ptr::null());
            assert_eq!(hew_hashmap_get_i32(m, key.as_ptr()), 20);
            assert_eq!(hew_hashmap_len(m), 1);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_remove() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("key").unwrap();
            hew_hashmap_insert_impl(m, key.as_ptr(), 42, core::ptr::null());
            let removed = hew_hashmap_remove(m, key.as_ptr());
            assert!(removed);
            assert!(!hew_hashmap_contains_key(m, key.as_ptr()));
            assert_eq!(hew_hashmap_len(m), 0);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_remove_missing_key() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("missing").unwrap();
            assert!(!hew_hashmap_remove(m, key.as_ptr()));
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_get_missing_returns_zero() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("missing").unwrap();
            assert_eq!(hew_hashmap_get_i32(m, key.as_ptr()), 0);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_multiple_entries() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let k1 = CString::new("a").unwrap();
            let k2 = CString::new("b").unwrap();
            let k3 = CString::new("c").unwrap();
            hew_hashmap_insert_impl(m, k1.as_ptr(), 1, core::ptr::null());
            hew_hashmap_insert_impl(m, k2.as_ptr(), 2, core::ptr::null());
            hew_hashmap_insert_impl(m, k3.as_ptr(), 3, core::ptr::null());
            assert_eq!(hew_hashmap_len(m), 3);
            assert_eq!(hew_hashmap_get_i32(m, k1.as_ptr()), 1);
            assert_eq!(hew_hashmap_get_i32(m, k2.as_ptr()), 2);
            assert_eq!(hew_hashmap_get_i32(m, k3.as_ptr()), 3);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_clear() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let k1 = CString::new("x").unwrap();
            let k2 = CString::new("y").unwrap();
            hew_hashmap_insert_impl(m, k1.as_ptr(), 1, core::ptr::null());
            hew_hashmap_insert_impl(m, k2.as_ptr(), 2, core::ptr::null());
            hew_hashmap_clear(m);
            assert_eq!(hew_hashmap_len(m), 0);
            assert!(hew_hashmap_is_empty(m));
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_get_or_default() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("key").unwrap();
            let missing = CString::new("missing").unwrap();
            hew_hashmap_insert_impl(m, key.as_ptr(), 42, core::ptr::null());
            assert_eq!(hew_hashmap_get_or_default_i32(m, key.as_ptr(), -1), 42);
            assert_eq!(hew_hashmap_get_or_default_i32(m, missing.as_ptr(), -1), -1);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_many_entries_triggers_resize() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            for i in 0..50 {
                let key = CString::new(format!("key_{i}")).unwrap();
                hew_hashmap_insert_impl(m, key.as_ptr(), i, core::ptr::null());
            }
            assert_eq!(hew_hashmap_len(m), 50);
            for i in 0..50 {
                let key = CString::new(format!("key_{i}")).unwrap();
                assert_eq!(hew_hashmap_get_i32(m, key.as_ptr()), i);
            }
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_insert_after_remove() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("key").unwrap();
            hew_hashmap_insert_impl(m, key.as_ptr(), 10, core::ptr::null());
            hew_hashmap_remove(m, key.as_ptr());
            hew_hashmap_insert_impl(m, key.as_ptr(), 20, core::ptr::null());
            assert_eq!(hew_hashmap_get_i32(m, key.as_ptr()), 20);
            assert_eq!(hew_hashmap_len(m), 1);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_reinsert_after_tombstone_collision_keeps_single_entry() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let (first, second) = colliding_keys();

            hew_hashmap_insert_impl(m, first.as_ptr(), 10, core::ptr::null());
            hew_hashmap_insert_impl(m, second.as_ptr(), 20, core::ptr::null());
            assert!(hew_hashmap_remove(m, first.as_ptr()));

            hew_hashmap_insert_impl(m, second.as_ptr(), 30, core::ptr::null());

            assert_eq!(hew_hashmap_len(m), 1);
            assert_eq!(hew_hashmap_get_i32(m, second.as_ptr()), 30);
            assert!(!hew_hashmap_contains_key(m, first.as_ptr()));
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_keys() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let k1 = CString::new("alpha").unwrap();
            let k2 = CString::new("beta").unwrap();
            hew_hashmap_insert_impl(m, k1.as_ptr(), 1, core::ptr::null());
            hew_hashmap_insert_impl(m, k2.as_ptr(), 2, core::ptr::null());
            let keys = hew_hashmap_keys(m);
            assert!(!keys.is_null());
            assert_eq!(crate::vec::hew_vec_len(keys), 2);
            crate::vec::hew_vec_free(keys);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_free_null() {
        // SAFETY: Null is explicitly handled by hew_hashmap_free_impl.
        unsafe { hew_hashmap_free_impl(core::ptr::null_mut()) };
    }

    #[test]
    fn test_hashmap_with_string_values() {
        // SAFETY: FFI calls use valid hashmap pointer and valid C strings.
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("greeting").unwrap();
            let val = CString::new("hello").unwrap();
            hew_hashmap_insert_impl(m, key.as_ptr(), 0, val.as_ptr());
            let result = hew_hashmap_get_str_impl(m, key.as_ptr());
            assert!(!result.is_null());
            assert_eq!(std::ffi::CStr::from_ptr(result).to_string_lossy(), "hello");
            hew_hashmap_free_impl(m);
        }
    }
}
