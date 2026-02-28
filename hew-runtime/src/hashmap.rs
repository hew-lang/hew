//! Hew runtime: `hashmap` module.
//!
//! Open-addressing hash map (`HewHashMap`) with C ABI, matching the C runtime
//! layout exactly. Uses FNV-1a hashing and linear probing with tombstones.

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

use core::ffi::c_char;
use core::ptr;

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
        let new_cap = old_cap * 2;

        let layout_size = new_cap * core::mem::size_of::<HewMapEntry>();
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
        libc::free(old_entries.cast());
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
        let m: *mut HewHashMap = libc::malloc(core::mem::size_of::<HewHashMap>()).cast();
        if m.is_null() {
            libc::abort();
        }
        let entries: *mut HewMapEntry =
            libc::calloc(INIT_CAP, core::mem::size_of::<HewMapEntry>()).cast();
        if entries.is_null() {
            libc::free(m.cast());
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
        let mask = (*m).cap - 1;
        let h = fnv1a(key);
        let mut idx = (h as usize) & mask;

        loop {
            let entry = &mut *(*m).entries.add(idx);
            if entry.state == OCCUPIED {
                if libc::strcmp(entry.key, key) == 0 {
                    // Update existing entry.
                    entry.value_i32 = val_i32;
                    if !entry.value_str.is_null() {
                        libc::free(entry.value_str.cast());
                    }
                    entry.value_str = if val_str.is_null() {
                        ptr::null_mut()
                    } else {
                        libc::strdup(val_str)
                    };
                    return;
                }
                idx = (idx + 1) & mask;
                continue;
            }
            // Empty or tombstone slot — insert here.
            entry.state = OCCUPIED;
            entry.key = libc::strdup(key);
            entry.value_i32 = val_i32;
            entry.value_str = if val_str.is_null() {
                ptr::null_mut()
            } else {
                libc::strdup(val_str)
            };
            entry.value_i64 = 0;
            entry.value_f64 = 0.0;
            (*m).len += 1;
            return;
        }
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
        let mask = (*m).cap - 1;
        let h = fnv1a(key);
        let mut idx = (h as usize) & mask;

        loop {
            let entry = &mut *(*m).entries.add(idx);
            if entry.state == OCCUPIED {
                if libc::strcmp(entry.key, key) == 0 {
                    entry.value_i64 = val;
                    return;
                }
                idx = (idx + 1) & mask;
                continue;
            }
            entry.state = OCCUPIED;
            entry.key = libc::strdup(key);
            entry.value_i64 = val;
            entry.value_i32 = 0;
            entry.value_str = ptr::null_mut();
            entry.value_f64 = 0.0;
            (*m).len += 1;
            return;
        }
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
        let mask = (*m).cap - 1;
        let h = fnv1a(key);
        let mut idx = (h as usize) & mask;

        loop {
            let entry = &mut *(*m).entries.add(idx);
            if entry.state == OCCUPIED {
                if libc::strcmp(entry.key, key) == 0 {
                    entry.value_f64 = val;
                    return;
                }
                idx = (idx + 1) & mask;
                continue;
            }
            entry.state = OCCUPIED;
            entry.key = libc::strdup(key);
            entry.value_f64 = val;
            entry.value_i32 = 0;
            entry.value_str = ptr::null_mut();
            entry.value_i64 = 0;
            (*m).len += 1;
            return;
        }
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
            libc::strdup(raw)
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
        libc::free(entry.key.cast());
        if !entry.value_str.is_null() {
            libc::free(entry.value_str.cast());
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
                libc::free(entry.key.cast());
                if !entry.value_str.is_null() {
                    libc::free(entry.value_str.cast());
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

/// Deep-clone a `HewHashMap`, duplicating keys and string values.
///
/// # Safety
///
/// `m` must be a valid `HewHashMap` pointer (or null, which returns null).
/// The returned pointer must eventually be freed with [`hew_hashmap_free_impl`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_clone_impl(m: *const HewHashMap) -> *mut HewHashMap {
    cabi_guard!(m.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `m` is valid.
    unsafe {
        let src = &*m;
        let cloned: *mut HewHashMap = libc::malloc(core::mem::size_of::<HewHashMap>()).cast();
        if cloned.is_null() {
            libc::abort();
        }
        let entries: *mut HewMapEntry =
            libc::calloc(src.cap, core::mem::size_of::<HewMapEntry>()).cast();
        if entries.is_null() {
            libc::free(cloned.cast());
            libc::abort();
        }
        (*cloned).entries = entries;
        (*cloned).len = src.len;
        (*cloned).cap = src.cap;

        for i in 0..src.cap {
            let src_entry = &*src.entries.add(i);
            let dst_entry = &mut *entries.add(i);
            dst_entry.state = src_entry.state;
            if src_entry.state != OCCUPIED {
                continue;
            }
            dst_entry.key = libc::strdup(src_entry.key);
            if dst_entry.key.is_null() {
                libc::abort();
            }
            dst_entry.value_i32 = src_entry.value_i32;
            dst_entry.value_i64 = src_entry.value_i64;
            dst_entry.value_f64 = src_entry.value_f64;
            dst_entry.value_str = if src_entry.value_str.is_null() {
                ptr::null_mut()
            } else {
                libc::strdup(src_entry.value_str)
            };
            if !src_entry.value_str.is_null() && dst_entry.value_str.is_null() {
                libc::abort();
            }
        }
        cloned
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
                libc::free(entry.key.cast());
                if !entry.value_str.is_null() {
                    libc::free(entry.value_str.cast());
                }
            }
        }
        libc::free((*m).entries.cast());
        libc::free(m.cast());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    #[test]
    fn test_hashmap_new_and_len() {
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
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("missing").unwrap();
            assert!(!hew_hashmap_remove(m, key.as_ptr()));
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_get_missing_returns_zero() {
        unsafe {
            let m = hew_hashmap_new_impl();
            let key = CString::new("missing").unwrap();
            assert_eq!(hew_hashmap_get_i32(m, key.as_ptr()), 0);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn test_hashmap_multiple_entries() {
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
    fn test_hashmap_keys() {
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
        unsafe { hew_hashmap_free_impl(core::ptr::null_mut()) };
    }

    #[test]
    fn test_hashmap_with_string_values() {
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

    #[test]
    fn test_hashmap_clone_impl_deep_copy() {
        unsafe {
            let m = hew_hashmap_new_impl();
            let key_str = CString::new("greeting").unwrap();
            let key_i64 = CString::new("count").unwrap();
            let val = CString::new("hello").unwrap();
            hew_hashmap_insert_impl(m, key_str.as_ptr(), 0, val.as_ptr());
            hew_hashmap_insert_i64(m, key_i64.as_ptr(), 99);

            let cloned = hew_hashmap_clone_impl(m);
            assert!(!cloned.is_null());
            hew_hashmap_free_impl(m);

            let cloned_str = hew_hashmap_get_str_impl(cloned, key_str.as_ptr());
            assert_eq!(CStr::from_ptr(cloned_str).to_string_lossy(), "hello");
            assert_eq!(hew_hashmap_get_i64(cloned, key_i64.as_ptr()), 99);
            hew_hashmap_free_impl(cloned);
        }
    }
}
