//! Hew runtime: `hashset` module.
//!
//! Hash set implementation backed by `HashMap<T, ()>` with C ABI.
//! Uses the existing HashMap infrastructure for storage.

#![expect(
    clippy::cast_sign_loss,
    reason = "len casts usize→i64 which is lossless for realistic sizes"
)]

use core::ffi::c_char;
use core::ptr;

use crate::hashmap::{
    hew_hashmap_clear, hew_hashmap_contains_key, hew_hashmap_free_impl, hew_hashmap_insert_i64,
    hew_hashmap_insert_impl, hew_hashmap_len, hew_hashmap_new_impl, hew_hashmap_remove, HewHashMap,
};

/// Hash set backed by a `HewHashMap` where values are unused.
#[repr(C)]
#[derive(Debug)]
pub struct HewHashSet {
    /// Underlying hash map (keys are set elements, values are ignored).
    map: *mut HewHashMap,
}

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

/// Create a new, empty `HewHashSet`.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_hashset_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_new() -> *mut HewHashSet {
    // SAFETY: allocating with libc::malloc.
    unsafe {
        let set: *mut HewHashSet = libc::malloc(core::mem::size_of::<HewHashSet>()).cast();
        if set.is_null() {
            libc::abort();
        }
        (*set).map = hew_hashmap_new_impl();
        set
    }
}

// ---------------------------------------------------------------------------
// Insert (returns true if value was newly inserted)
// ---------------------------------------------------------------------------

/// Insert an `i64` value into the set.
///
/// Returns `true` if the value was newly inserted, `false` if it was already present.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_insert_int(set: *mut HewHashSet, value: i64) -> bool {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        // Convert i64 to string key for HashMap storage
        let key_str = format!("{value}\0");
        let key_cstr = key_str.as_ptr().cast::<c_char>();

        let was_present = hew_hashmap_contains_key(map, key_cstr);
        if !was_present {
            hew_hashmap_insert_i64(map, key_cstr, value);
        }
        !was_present
    }
}

/// Insert a string value into the set.
///
/// Returns `true` if the value was newly inserted, `false` if it was already present.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer. `value` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_insert_string(
    set: *mut HewHashSet,
    value: *const c_char,
) -> bool {
    // SAFETY: caller guarantees `set` and `value` are valid.
    unsafe {
        let map = (*set).map;
        let was_present = hew_hashmap_contains_key(map, value);
        if !was_present {
            // Use the string itself as the key, with a dummy value
            hew_hashmap_insert_impl(map, value, 0, ptr::null());
        }
        !was_present
    }
}

// ---------------------------------------------------------------------------
// Contains
// ---------------------------------------------------------------------------

/// Check if the set contains an `i64` value.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_contains_int(set: *mut HewHashSet, value: i64) -> bool {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        let key_str = format!("{value}\0");
        let key_cstr = key_str.as_ptr().cast::<c_char>();
        hew_hashmap_contains_key(map, key_cstr)
    }
}

/// Check if the set contains a string value.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer. `value` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_contains_string(
    set: *mut HewHashSet,
    value: *const c_char,
) -> bool {
    // SAFETY: caller guarantees `set` and `value` are valid.
    unsafe {
        let map = (*set).map;
        hew_hashmap_contains_key(map, value)
    }
}

// ---------------------------------------------------------------------------
// Remove
// ---------------------------------------------------------------------------

/// Remove an `i64` value from the set.
///
/// Returns `true` if the value was present and removed.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_remove_int(set: *mut HewHashSet, value: i64) -> bool {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        let key_str = format!("{value}\0");
        let key_cstr = key_str.as_ptr().cast::<c_char>();
        hew_hashmap_remove(map, key_cstr)
    }
}

/// Remove a string value from the set.
///
/// Returns `true` if the value was present and removed.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer. `value` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_remove_string(
    set: *mut HewHashSet,
    value: *const c_char,
) -> bool {
    // SAFETY: caller guarantees `set` and `value` are valid.
    unsafe {
        let map = (*set).map;
        hew_hashmap_remove(map, value)
    }
}

// ---------------------------------------------------------------------------
// Queries
// ---------------------------------------------------------------------------

/// Return the number of elements in the set.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_len(set: *mut HewHashSet) -> i64 {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        hew_hashmap_len(map)
    }
}

/// Return `true` if the set has no elements.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_is_empty(set: *mut HewHashSet) -> bool {
    // SAFETY: caller guarantees `set` is valid.
    unsafe { hew_hashset_len(set) == 0 }
}

/// Remove all elements from the set.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_clear(set: *mut HewHashSet) {
    // SAFETY: caller guarantees `set` is valid.
    unsafe {
        let map = (*set).map;
        hew_hashmap_clear(map);
    }
}

// ---------------------------------------------------------------------------
// Free
// ---------------------------------------------------------------------------

/// Free the set and its underlying storage.
///
/// # Safety
///
/// `set` must be a valid `HewHashSet` pointer (or null). After this call, `set` is
/// invalid.
#[no_mangle]
pub unsafe extern "C" fn hew_hashset_free(set: *mut HewHashSet) {
    // SAFETY: caller guarantees `set` was allocated with malloc (or is null).
    unsafe {
        cabi_guard!(set.is_null());
        hew_hashmap_free_impl((*set).map);
        libc::free(set.cast());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_hashset_new_and_len() {
        unsafe {
            let s = hew_hashset_new();
            assert!(!s.is_null());
            assert_eq!(hew_hashset_len(s), 0);
            assert!(hew_hashset_is_empty(s));
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_insert_int() {
        unsafe {
            let s = hew_hashset_new();
            assert!(hew_hashset_insert_int(s, 1));
            assert!(hew_hashset_insert_int(s, 2));
            assert!(!hew_hashset_insert_int(s, 1)); // duplicate
            assert_eq!(hew_hashset_len(s), 2);
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_contains_int() {
        unsafe {
            let s = hew_hashset_new();
            hew_hashset_insert_int(s, 42);
            assert!(hew_hashset_contains_int(s, 42));
            assert!(!hew_hashset_contains_int(s, 99));
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_remove_int() {
        unsafe {
            let s = hew_hashset_new();
            hew_hashset_insert_int(s, 10);
            assert!(hew_hashset_remove_int(s, 10));
            assert!(!hew_hashset_contains_int(s, 10));
            assert_eq!(hew_hashset_len(s), 0);
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_insert_string() {
        unsafe {
            let s = hew_hashset_new();
            let hello = CString::new("hello").unwrap();
            let world = CString::new("world").unwrap();
            assert!(hew_hashset_insert_string(s, hello.as_ptr()));
            assert!(hew_hashset_insert_string(s, world.as_ptr()));
            assert!(!hew_hashset_insert_string(s, hello.as_ptr())); // duplicate
            assert_eq!(hew_hashset_len(s), 2);
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_contains_string() {
        unsafe {
            let s = hew_hashset_new();
            let val = CString::new("test").unwrap();
            let missing = CString::new("missing").unwrap();
            hew_hashset_insert_string(s, val.as_ptr());
            assert!(hew_hashset_contains_string(s, val.as_ptr()));
            assert!(!hew_hashset_contains_string(s, missing.as_ptr()));
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_remove_string() {
        unsafe {
            let s = hew_hashset_new();
            let val = CString::new("remove_me").unwrap();
            hew_hashset_insert_string(s, val.as_ptr());
            assert!(hew_hashset_remove_string(s, val.as_ptr()));
            assert!(!hew_hashset_contains_string(s, val.as_ptr()));
            assert_eq!(hew_hashset_len(s), 0);
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_clear() {
        unsafe {
            let s = hew_hashset_new();
            hew_hashset_insert_int(s, 1);
            hew_hashset_insert_int(s, 2);
            hew_hashset_insert_int(s, 3);
            hew_hashset_clear(s);
            assert_eq!(hew_hashset_len(s), 0);
            assert!(hew_hashset_is_empty(s));
            hew_hashset_free(s);
        }
    }

    #[test]
    fn test_hashset_free_null() {
        unsafe { hew_hashset_free(core::ptr::null_mut()) };
    }

    #[test]
    fn test_hashset_many_elements() {
        unsafe {
            let s = hew_hashset_new();
            for i in 0..100 {
                assert!(hew_hashset_insert_int(s, i));
            }
            assert_eq!(hew_hashset_len(s), 100);
            for i in 0..100 {
                assert!(hew_hashset_contains_int(s, i));
            }
            hew_hashset_free(s);
        }
    }
}
