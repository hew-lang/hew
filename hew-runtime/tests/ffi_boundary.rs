//! FFI boundary integration tests for hew-runtime.
//!
//! Tests the `extern "C"` functions that compiled Hew programs call across
//! the C ABI. This is the highest-risk surface for memory safety bugs.

// Many tests deliberately exercise raw FFI functions that are inherently unsafe.
#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness — safety invariants are documented per-test"
)]
#![expect(
    clippy::missing_docs_in_crate_items,
    reason = "integration tests don't need doc comments"
)]

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

// Re-export the C ABI functions under test.
use hew_runtime::actor::{hew_actor_free, hew_actor_send, hew_actor_spawn};
use hew_runtime::hashmap::{
    hew_hashmap_contains_key, hew_hashmap_free_impl, hew_hashmap_get_f64, hew_hashmap_get_i32,
    hew_hashmap_get_i64, hew_hashmap_get_str_impl, hew_hashmap_insert_f64, hew_hashmap_insert_i64,
    hew_hashmap_insert_impl, hew_hashmap_is_empty, hew_hashmap_len, hew_hashmap_new_impl,
    hew_hashmap_remove,
};
use hew_runtime::iter::{
    hew_iter_free, hew_iter_next, hew_iter_reset, hew_iter_value_i32, hew_iter_vec,
};
use hew_runtime::mailbox::{
    hew_mailbox_free, hew_mailbox_has_messages, hew_mailbox_new, hew_mailbox_new_bounded,
    hew_mailbox_send, hew_mailbox_send_sys, hew_mailbox_try_recv, hew_mailbox_try_recv_sys,
    hew_msg_node_free,
};
use hew_runtime::option::{
    hew_option_contains_i32, hew_option_is_none, hew_option_is_some, hew_option_none,
    hew_option_replace_i32, hew_option_some_f64, hew_option_some_i32, hew_option_some_i64,
    hew_option_take, hew_option_unwrap_f64, hew_option_unwrap_i32, hew_option_unwrap_i64,
    hew_option_unwrap_or_f64, hew_option_unwrap_or_i32, hew_option_unwrap_or_i64,
};
use hew_runtime::result::{
    hew_result_err, hew_result_err_code, hew_result_error_code, hew_result_error_msg,
    hew_result_free, hew_result_is_err, hew_result_is_ok, hew_result_ok_i32, hew_result_ok_i64,
    hew_result_unwrap_i32, hew_result_unwrap_i64, hew_result_unwrap_or_i32,
    hew_result_unwrap_or_i64,
};
use hew_runtime::string::{
    hew_bool_to_string, hew_float_to_string, hew_int_to_string, hew_string_byte_length,
    hew_string_char_at, hew_string_char_at_utf8, hew_string_char_count, hew_string_concat,
    hew_string_contains, hew_string_ends_with, hew_string_equals, hew_string_find,
    hew_string_from_char, hew_string_index_of, hew_string_is_ascii, hew_string_length,
    hew_string_repeat, hew_string_replace, hew_string_reverse_utf8, hew_string_slice,
    hew_string_split, hew_string_starts_with, hew_string_substring_utf8, hew_string_to_bytes,
    hew_string_to_int, hew_string_to_lowercase, hew_string_to_uppercase, hew_string_trim,
};
use hew_runtime::vec::{
    hew_vec_clear, hew_vec_clone, hew_vec_contains_f64, hew_vec_contains_i32, hew_vec_contains_i64,
    hew_vec_contains_str, hew_vec_free, hew_vec_get_f64, hew_vec_get_generic, hew_vec_get_i32,
    hew_vec_get_i64, hew_vec_get_str, hew_vec_is_empty, hew_vec_len, hew_vec_new, hew_vec_new_f64,
    hew_vec_new_generic, hew_vec_new_i64, hew_vec_new_str, hew_vec_pop_f64, hew_vec_pop_generic,
    hew_vec_pop_i32, hew_vec_pop_i64, hew_vec_push_f64, hew_vec_push_generic, hew_vec_push_i32,
    hew_vec_push_i64, hew_vec_push_str, hew_vec_reverse_i32, hew_vec_set_f64, hew_vec_set_generic,
    hew_vec_set_i32, hew_vec_set_i64, hew_vec_set_str, hew_vec_sort_f64, hew_vec_sort_i32,
    hew_vec_sort_i64, hew_vec_swap, hew_vec_truncate,
};

// ── Helpers ─────────────────────────────────────────────────────────────

/// Create a C string literal and return a raw pointer.
fn cstr(s: &str) -> CString {
    CString::new(s).expect("CString::new failed")
}

/// Read a C string pointer into a Rust `&str` (panics on null/invalid UTF-8).
unsafe fn read_cstr<'a>(p: *const c_char) -> &'a str {
    assert!(!p.is_null(), "unexpected null C string");
    unsafe { CStr::from_ptr(p) }
        .to_str()
        .expect("invalid UTF-8")
}

/// Free a malloc'd C string.
unsafe fn free_cstr(p: *mut c_char) {
    if !p.is_null() {
        unsafe { libc::free(p.cast()) };
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Vec<i32> via C ABI
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn vec_i32_lifecycle() {
    unsafe {
        let v = hew_vec_new();
        assert!(!v.is_null());
        assert!(hew_vec_is_empty(v));
        assert_eq!(hew_vec_len(v), 0);

        // Push values.
        hew_vec_push_i32(v, 10);
        hew_vec_push_i32(v, 20);
        hew_vec_push_i32(v, 30);
        assert_eq!(hew_vec_len(v), 3);
        assert!(!hew_vec_is_empty(v));

        // Get values.
        assert_eq!(hew_vec_get_i32(v, 0), 10);
        assert_eq!(hew_vec_get_i32(v, 1), 20);
        assert_eq!(hew_vec_get_i32(v, 2), 30);

        // Set value.
        hew_vec_set_i32(v, 1, 99);
        assert_eq!(hew_vec_get_i32(v, 1), 99);

        // Pop.
        assert_eq!(hew_vec_pop_i32(v), 30);
        assert_eq!(hew_vec_len(v), 2);

        // Clear.
        hew_vec_clear(v);
        assert_eq!(hew_vec_len(v), 0);
        assert!(hew_vec_is_empty(v));

        hew_vec_free(v);
    }
}

#[test]
fn vec_i32_growth_past_initial_capacity() {
    unsafe {
        let v = hew_vec_new();
        // Push more than the initial capacity (4) to trigger realloc.
        for i in 0..100 {
            hew_vec_push_i32(v, i);
        }
        assert_eq!(hew_vec_len(v), 100);
        for i in 0..100 {
            assert_eq!(hew_vec_get_i32(v, i), i as i32);
        }
        hew_vec_free(v);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Vec<i64> via C ABI
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn vec_i64_lifecycle() {
    unsafe {
        let v = hew_vec_new_i64();
        assert!(hew_vec_is_empty(v));

        hew_vec_push_i64(v, i64::MAX);
        hew_vec_push_i64(v, i64::MIN);
        hew_vec_push_i64(v, 0);

        assert_eq!(hew_vec_len(v), 3);
        assert_eq!(hew_vec_get_i64(v, 0), i64::MAX);
        assert_eq!(hew_vec_get_i64(v, 1), i64::MIN);
        assert_eq!(hew_vec_get_i64(v, 2), 0);

        hew_vec_set_i64(v, 2, 42);
        assert_eq!(hew_vec_get_i64(v, 2), 42);

        assert_eq!(hew_vec_pop_i64(v), 42);
        assert_eq!(hew_vec_len(v), 2);

        hew_vec_free(v);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Vec<f64> via C ABI
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn vec_f64_lifecycle() {
    unsafe {
        let v = hew_vec_new_f64();
        assert!(hew_vec_is_empty(v));

        hew_vec_push_f64(v, 3.14);
        hew_vec_push_f64(v, 2.718);
        hew_vec_push_f64(v, -0.0);

        assert_eq!(hew_vec_len(v), 3);
        assert!((hew_vec_get_f64(v, 0) - 3.14).abs() < f64::EPSILON);
        assert!((hew_vec_get_f64(v, 1) - 2.718).abs() < f64::EPSILON);

        hew_vec_set_f64(v, 0, 1.0);
        assert!((hew_vec_get_f64(v, 0) - 1.0).abs() < f64::EPSILON);

        let popped = hew_vec_pop_f64(v);
        assert!(popped.is_sign_negative() && popped == 0.0);

        hew_vec_free(v);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Vec<string> via C ABI
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn vec_str_lifecycle() {
    unsafe {
        let v = hew_vec_new_str();
        assert!(hew_vec_is_empty(v));

        let hello = cstr("hello");
        let world = cstr("world");
        let hew = cstr("hew");

        hew_vec_push_str(v, hello.as_ptr());
        hew_vec_push_str(v, world.as_ptr());
        assert_eq!(hew_vec_len(v), 2);

        assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "hello");
        assert_eq!(read_cstr(hew_vec_get_str(v, 1)), "world");

        // Set replaces and frees old string.
        hew_vec_set_str(v, 0, hew.as_ptr());
        assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "hew");

        hew_vec_free(v);
    }
}

#[test]
fn vec_free_null_is_noop() {
    // Passing null to hew_vec_free must not crash.
    unsafe {
        hew_vec_free(ptr::null_mut());
    }
}

// ═══════════════════════════════════════════════════════════════════════
// String operations via C ABI
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn string_concat() {
    unsafe {
        let a = cstr("hello ");
        let b = cstr("world");
        let result = hew_string_concat(a.as_ptr(), b.as_ptr());
        assert_eq!(read_cstr(result), "hello world");
        free_cstr(result);
    }
}

#[test]
fn string_concat_with_null() {
    unsafe {
        let a = cstr("hello");
        // null + string → just the string
        let r1 = hew_string_concat(ptr::null(), a.as_ptr());
        assert_eq!(read_cstr(r1), "hello");
        free_cstr(r1);

        // string + null → just the string
        let r2 = hew_string_concat(a.as_ptr(), ptr::null());
        assert_eq!(read_cstr(r2), "hello");
        free_cstr(r2);

        // null + null → empty string
        let r3 = hew_string_concat(ptr::null(), ptr::null());
        assert_eq!(read_cstr(r3), "");
        free_cstr(r3);
    }
}

#[test]
fn string_length() {
    unsafe {
        let s = cstr("hello");
        assert_eq!(hew_string_length(s.as_ptr()), 5);
        assert_eq!(hew_string_length(ptr::null()), 0);
    }
}

#[test]
fn string_equals() {
    unsafe {
        let a = cstr("abc");
        let b = cstr("abc");
        let c = cstr("xyz");
        assert_eq!(hew_string_equals(a.as_ptr(), b.as_ptr()), 1);
        assert_eq!(hew_string_equals(a.as_ptr(), c.as_ptr()), 0);
        // Both null → equal.
        assert_eq!(hew_string_equals(ptr::null(), ptr::null()), 1);
        // One null → not equal.
        assert_eq!(hew_string_equals(a.as_ptr(), ptr::null()), 0);
    }
}

#[test]
fn string_slice() {
    unsafe {
        let s = cstr("hello world");
        let sliced = hew_string_slice(s.as_ptr(), 6, 11);
        assert_eq!(read_cstr(sliced), "world");
        free_cstr(sliced);

        // Clamped: start < 0, end > len.
        let full = hew_string_slice(s.as_ptr(), -5, 999);
        assert_eq!(read_cstr(full), "hello world");
        free_cstr(full);

        // Null input → empty string.
        let empty = hew_string_slice(ptr::null(), 0, 5);
        assert_eq!(read_cstr(empty), "");
        free_cstr(empty);
    }
}

#[test]
fn string_find() {
    unsafe {
        let s = cstr("hello world");
        let sub = cstr("world");
        assert_eq!(hew_string_find(s.as_ptr(), sub.as_ptr()), 6);

        let missing = cstr("xyz");
        assert_eq!(hew_string_find(s.as_ptr(), missing.as_ptr()), -1);

        assert_eq!(hew_string_find(ptr::null(), sub.as_ptr()), -1);
    }
}

#[test]
fn string_starts_and_ends_with() {
    unsafe {
        let s = cstr("hello world");
        let prefix = cstr("hello");
        let suffix = cstr("world");
        let no = cstr("xyz");

        assert!(hew_string_starts_with(s.as_ptr(), prefix.as_ptr()));
        assert!(!hew_string_starts_with(s.as_ptr(), no.as_ptr()));
        assert!(hew_string_ends_with(s.as_ptr(), suffix.as_ptr()));
        assert!(!hew_string_ends_with(s.as_ptr(), no.as_ptr()));

        // Null args.
        assert!(!hew_string_starts_with(ptr::null(), prefix.as_ptr()));
        assert!(!hew_string_ends_with(ptr::null(), suffix.as_ptr()));
    }
}

#[test]
fn string_contains() {
    unsafe {
        let s = cstr("hello world");
        let sub = cstr("lo wo");
        assert!(hew_string_contains(s.as_ptr(), sub.as_ptr()));
        assert!(!hew_string_contains(ptr::null(), sub.as_ptr()));
    }
}

#[test]
fn string_conversions() {
    unsafe {
        // int → string
        let s = hew_int_to_string(42);
        assert_eq!(read_cstr(s), "42");
        free_cstr(s);

        let neg = hew_int_to_string(-7);
        assert_eq!(read_cstr(neg), "-7");
        free_cstr(neg);

        // string → int
        let n = cstr("123");
        assert_eq!(hew_string_to_int(n.as_ptr()), 123);
        assert_eq!(hew_string_to_int(ptr::null()), 0);

        // float → string
        let fs = hew_float_to_string(3.14);
        let fs_str = read_cstr(fs);
        assert!(fs_str.starts_with("3.14"), "got: {fs_str}");
        free_cstr(fs);

        // bool → string
        let t = hew_bool_to_string(true);
        assert_eq!(read_cstr(t), "true");
        free_cstr(t);

        let f = hew_bool_to_string(false);
        assert_eq!(read_cstr(f), "false");
        free_cstr(f);
    }
}

#[test]
fn string_trim() {
    unsafe {
        let s = cstr("  hello  ");
        let trimmed = hew_string_trim(s.as_ptr());
        assert_eq!(read_cstr(trimmed), "hello");
        free_cstr(trimmed);

        // Null → empty.
        let empty = hew_string_trim(ptr::null());
        assert_eq!(read_cstr(empty), "");
        free_cstr(empty);
    }
}

#[test]
fn string_replace() {
    unsafe {
        let s = cstr("aabbcc");
        let old = cstr("bb");
        let new = cstr("XX");
        let result = hew_string_replace(s.as_ptr(), old.as_ptr(), new.as_ptr());
        assert_eq!(read_cstr(result), "aaXXcc");
        free_cstr(result);

        // Replace with empty.
        let empty = cstr("");
        let r2 = hew_string_replace(s.as_ptr(), old.as_ptr(), empty.as_ptr());
        assert_eq!(read_cstr(r2), "aacc");
        free_cstr(r2);

        // Null source → empty.
        let r3 = hew_string_replace(ptr::null(), old.as_ptr(), new.as_ptr());
        assert_eq!(read_cstr(r3), "");
        free_cstr(r3);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// HashMap via C ABI
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn hashmap_i32_lifecycle() {
    unsafe {
        let m = hew_hashmap_new_impl();
        assert!(!m.is_null());
        assert!(hew_hashmap_is_empty(m));
        assert_eq!(hew_hashmap_len(m), 0);

        let key_a = cstr("a");
        let key_b = cstr("b");

        hew_hashmap_insert_impl(m, key_a.as_ptr(), 42, ptr::null());
        hew_hashmap_insert_impl(m, key_b.as_ptr(), 99, ptr::null());
        assert_eq!(hew_hashmap_len(m), 2);
        assert!(!hew_hashmap_is_empty(m));

        assert_eq!(hew_hashmap_get_i32(m, key_a.as_ptr()), 42);
        assert_eq!(hew_hashmap_get_i32(m, key_b.as_ptr()), 99);

        assert!(hew_hashmap_contains_key(m, key_a.as_ptr()));

        let missing = cstr("missing");
        assert!(!hew_hashmap_contains_key(m, missing.as_ptr()));
        assert_eq!(hew_hashmap_get_i32(m, missing.as_ptr()), 0);

        // Update existing.
        hew_hashmap_insert_impl(m, key_a.as_ptr(), 100, ptr::null());
        assert_eq!(hew_hashmap_get_i32(m, key_a.as_ptr()), 100);
        assert_eq!(hew_hashmap_len(m), 2); // no new entry

        // Remove.
        assert!(hew_hashmap_remove(m, key_a.as_ptr()));
        assert!(!hew_hashmap_contains_key(m, key_a.as_ptr()));
        assert_eq!(hew_hashmap_len(m), 1);

        // Remove non-existent.
        assert!(!hew_hashmap_remove(m, missing.as_ptr()));

        hew_hashmap_free_impl(m);
    }
}

#[test]
fn hashmap_str_values() {
    unsafe {
        let m = hew_hashmap_new_impl();
        let key = cstr("greeting");
        let val = cstr("hello");

        hew_hashmap_insert_impl(m, key.as_ptr(), 0, val.as_ptr());
        let got = hew_hashmap_get_str_impl(m, key.as_ptr());
        assert_eq!(read_cstr(got), "hello");

        // Overwrite string value.
        let val2 = cstr("world");
        hew_hashmap_insert_impl(m, key.as_ptr(), 0, val2.as_ptr());
        let got2 = hew_hashmap_get_str_impl(m, key.as_ptr());
        assert_eq!(read_cstr(got2), "world");

        hew_hashmap_free_impl(m);
    }
}

#[test]
fn hashmap_i64_and_f64() {
    unsafe {
        let m = hew_hashmap_new_impl();
        let key = cstr("num");

        hew_hashmap_insert_i64(m, key.as_ptr(), i64::MAX);
        assert_eq!(hew_hashmap_get_i64(m, key.as_ptr()), i64::MAX);

        hew_hashmap_insert_f64(m, key.as_ptr(), 2.718);
        assert!((hew_hashmap_get_f64(m, key.as_ptr()) - 2.718).abs() < f64::EPSILON);

        hew_hashmap_free_impl(m);
    }
}

#[test]
fn hashmap_growth_past_load_factor() {
    unsafe {
        let m = hew_hashmap_new_impl();
        // Insert enough keys to force resize (initial cap is 8, load factor 75%).
        for i in 0..20 {
            let key = CString::new(format!("key_{i}")).unwrap();
            hew_hashmap_insert_impl(m, key.as_ptr(), i, ptr::null());
        }
        assert_eq!(hew_hashmap_len(m), 20);

        // Verify all entries survived the resize.
        for i in 0..20 {
            let key = CString::new(format!("key_{i}")).unwrap();
            assert!(hew_hashmap_contains_key(m, key.as_ptr()));
            assert_eq!(hew_hashmap_get_i32(m, key.as_ptr()), i);
        }

        hew_hashmap_free_impl(m);
    }
}

#[test]
fn hashmap_free_null_is_noop() {
    unsafe {
        hew_hashmap_free_impl(ptr::null_mut());
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Mailbox via C ABI
// ═══════════════════════════════════════════════════════════════════════

#[test]
fn mailbox_send_recv_roundtrip() {
    unsafe {
        let mb = hew_mailbox_new();
        assert!(!mb.is_null());
        assert_eq!(hew_mailbox_has_messages(mb), 0);

        // Send an i32 message.
        let val: i32 = 42;
        let rc = hew_mailbox_send(mb, 1, (&raw const val).cast_mut().cast(), size_of::<i32>());
        assert_eq!(rc, 0);
        assert_eq!(hew_mailbox_has_messages(mb), 1);

        // Receive it.
        let node = hew_mailbox_try_recv(mb);
        assert!(!node.is_null());
        assert_eq!((*node).msg_type, 1);
        assert_eq!((*node).data_size, size_of::<i32>());
        assert_eq!(*((*node).data.cast::<i32>()), 42);
        hew_msg_node_free(node);

        // Queue is empty.
        assert_eq!(hew_mailbox_has_messages(mb), 0);
        assert!(hew_mailbox_try_recv(mb).is_null());

        hew_mailbox_free(mb);
    }
}

#[test]
fn mailbox_bounded_rejects_when_full() {
    unsafe {
        let mb = hew_mailbox_new_bounded(2);
        let val: i32 = 1;
        let p = (&raw const val).cast_mut().cast();

        assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
        assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
        // Third message should be rejected (ErrMailboxFull = -1).
        assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), -1);

        hew_mailbox_free(mb);
    }
}

#[test]
fn mailbox_sys_has_priority() {
    unsafe {
        let mb = hew_mailbox_new();
        let user_val: i32 = 10;
        let sys_val: i32 = 99;

        // Send user message first, then system message.
        hew_mailbox_send(
            mb,
            1,
            (&raw const user_val).cast_mut().cast(),
            size_of::<i32>(),
        );
        hew_mailbox_send_sys(
            mb,
            2,
            (&raw const sys_val).cast_mut().cast(),
            size_of::<i32>(),
        );

        // System message should come out first.
        let n1 = hew_mailbox_try_recv(mb);
        assert_eq!((*n1).msg_type, 2);
        hew_msg_node_free(n1);

        let n2 = hew_mailbox_try_recv(mb);
        assert_eq!((*n2).msg_type, 1);
        hew_msg_node_free(n2);

        hew_mailbox_free(mb);
    }
}

#[test]
fn mailbox_sys_bypasses_capacity() {
    unsafe {
        let mb = hew_mailbox_new_bounded(1);
        let val: i32 = 1;
        let p = (&raw const val).cast_mut().cast();

        // Fill user queue.
        assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
        assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), -1);

        // System message always succeeds.
        hew_mailbox_send_sys(mb, 99, p, size_of::<i32>());
        assert_eq!(hew_mailbox_has_messages(mb), 1);

        hew_mailbox_free(mb);
    }
}

#[test]
fn mailbox_try_recv_sys_only() {
    unsafe {
        let mb = hew_mailbox_new();
        let val: i32 = 5;
        let p = (&raw const val).cast_mut().cast();

        hew_mailbox_send(mb, 1, p, size_of::<i32>());
        hew_mailbox_send_sys(mb, 2, p, size_of::<i32>());

        // try_recv_sys should only return system messages.
        let node = hew_mailbox_try_recv_sys(mb);
        assert!(!node.is_null());
        assert_eq!((*node).msg_type, 2);
        hew_msg_node_free(node);

        // No more system messages.
        assert!(hew_mailbox_try_recv_sys(mb).is_null());

        hew_mailbox_free(mb);
    }
}

#[test]
fn mailbox_null_data_message() {
    unsafe {
        let mb = hew_mailbox_new();
        let rc = hew_mailbox_send(mb, 7, ptr::null_mut(), 0);
        assert_eq!(rc, 0);

        let node = hew_mailbox_try_recv(mb);
        assert!(!node.is_null());
        assert!((*node).data.is_null());
        assert_eq!((*node).data_size, 0);
        assert_eq!((*node).msg_type, 7);
        hew_msg_node_free(node);

        hew_mailbox_free(mb);
    }
}

#[test]
fn mailbox_deep_copy_isolation() {
    unsafe {
        let mb = hew_mailbox_new();
        let mut val: i32 = 100;
        hew_mailbox_send(mb, 0, (&raw mut val).cast(), size_of::<i32>());

        // Mutate original after send.
        val = 999;

        let node = hew_mailbox_try_recv(mb);
        // Message should contain the original value.
        assert_eq!(*((*node).data.cast::<i32>()), 100);
        hew_msg_node_free(node);

        let _ = val; // suppress unused-value warning
        hew_mailbox_free(mb);
    }
}

#[test]
fn mailbox_free_null_is_noop() {
    unsafe {
        hew_mailbox_free(ptr::null_mut());
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Actor spawn/send/free via C ABI (without scheduler)
// ═══════════════════════════════════════════════════════════════════════

/// Dispatch notification: tests wait on the condvar instead of polling
/// with sleep, eliminating timing-dependent flakiness under load.
static DISPATCH_SIGNAL: (Mutex<i32>, Condvar) = (Mutex::new(0), Condvar::new());

fn reset_dispatch_signal() {
    *DISPATCH_SIGNAL.0.lock().unwrap() = 0;
}

fn wait_for_dispatches(expected: i32, timeout: Duration) -> bool {
    let mut count = DISPATCH_SIGNAL.0.lock().unwrap();
    let deadline = Instant::now() + timeout;
    while *count < expected {
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return false;
        }
        let (guard, result) = DISPATCH_SIGNAL.1.wait_timeout(count, remaining).unwrap();
        count = guard;
        if result.timed_out() && *count < expected {
            return false;
        }
    }
    true
}

/// Test dispatch function matching the Hew 4-param canonical signature.
unsafe extern "C" fn test_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
) {
    let mut count = DISPATCH_SIGNAL.0.lock().unwrap();
    *count += 1;
    DISPATCH_SIGNAL.1.notify_all();
}

#[test]
fn actor_spawn_and_free_no_scheduler() {
    unsafe {
        // Spawn an actor without starting the scheduler — tests the
        // allocation path in isolation.
        let mut state: i32 = 42;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(test_dispatch),
        );
        assert!(!actor.is_null());

        // Actor should have a unique ID > 0.
        assert!((*actor).id > 0);

        // State should be a deep copy, not the original pointer.
        assert_ne!((*actor).state, (&raw mut state).cast());

        // Mailbox should be allocated.
        assert!(!(*actor).mailbox.is_null());

        hew_actor_free(actor);
    }
}

#[test]
fn actor_spawn_null_state() {
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(test_dispatch));
        assert!(!actor.is_null());
        assert!((*actor).state.is_null());
        hew_actor_free(actor);
    }
}

#[test]
fn actor_free_null_is_noop() {
    unsafe {
        hew_actor_free(ptr::null_mut());
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Actor + scheduler integration (send triggers dispatch)
// ═══════════════════════════════════════════════════════════════════════

/// Global init guard — the scheduler is global state and can only be
/// initialized once per process.
static SCHED_INIT: std::sync::Once = std::sync::Once::new();

fn ensure_scheduler() {
    SCHED_INIT.call_once(|| {
        // SAFETY: hew_sched_init has no preconditions.
        hew_runtime::scheduler::hew_sched_init();
    });
}

#[test]
fn actor_send_dispatches_via_scheduler() {
    ensure_scheduler();
    reset_dispatch_signal();

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(test_dispatch),
        );

        // Send a message — the scheduler should pick it up.
        let val: i32 = 7;
        hew_actor_send(
            actor,
            1,
            (&raw const val).cast_mut().cast(),
            size_of::<i32>(),
        );

        assert!(
            wait_for_dispatches(1, Duration::from_secs(10)),
            "dispatch was never called"
        );

        hew_actor_free(actor);
    }
}

#[test]
fn actor_send_multiple_messages() {
    ensure_scheduler();

    static MULTI_SIGNAL: (Mutex<i32>, Condvar) = (Mutex::new(0), Condvar::new());

    unsafe extern "C" fn multi_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        let mut count = MULTI_SIGNAL.0.lock().unwrap();
        *count += 1;
        MULTI_SIGNAL.1.notify_all();
    }

    *MULTI_SIGNAL.0.lock().unwrap() = 0;

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(multi_dispatch),
        );

        // Send 10 messages.
        for i in 0..10 {
            let val: i32 = i;
            hew_actor_send(
                actor,
                0,
                (&raw const val).cast_mut().cast(),
                size_of::<i32>(),
            );
        }

        // Wait for all to be dispatched.
        let done = {
            let mut count = MULTI_SIGNAL.0.lock().unwrap();
            let deadline = Instant::now() + Duration::from_secs(10);
            while *count < 10 {
                let remaining = deadline.saturating_duration_since(Instant::now());
                if remaining.is_zero() {
                    break;
                }
                let (guard, _) = MULTI_SIGNAL.1.wait_timeout(count, remaining).unwrap();
                count = guard;
            }
            *count
        };

        assert_eq!(done, 10);

        hew_actor_free(actor);
    }
}

#[test]
fn actor_dispatch_receives_correct_data() {
    ensure_scheduler();

    static DATA_SIGNAL: (Mutex<i32>, Condvar) = (Mutex::new(-1), Condvar::new());

    unsafe extern "C" fn data_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
    ) {
        if !data.is_null() && data_size >= size_of::<i32>() {
            let val = unsafe { *(data.cast::<i32>()) };
            let mut received = DATA_SIGNAL.0.lock().unwrap();
            *received = val;
            DATA_SIGNAL.1.notify_all();
        }
    }

    *DATA_SIGNAL.0.lock().unwrap() = -1;

    unsafe {
        let mut state: i32 = 0;
        let actor = hew_actor_spawn(
            (&raw mut state).cast(),
            size_of::<i32>(),
            Some(data_dispatch),
        );

        let payload: i32 = 12345;
        hew_actor_send(
            actor,
            0,
            (&raw const payload).cast_mut().cast(),
            size_of::<i32>(),
        );

        let received = {
            let mut val = DATA_SIGNAL.0.lock().unwrap();
            let deadline = Instant::now() + Duration::from_secs(10);
            while *val == -1 {
                let remaining = deadline.saturating_duration_since(Instant::now());
                if remaining.is_zero() {
                    break;
                }
                let (guard, _) = DATA_SIGNAL.1.wait_timeout(val, remaining).unwrap();
                val = guard;
            }
            *val
        };

        assert_eq!(received, 12345);

        hew_actor_free(actor);
    }
}

// ═══════════════════════════════════════════════════════════════════════
// HashMap extended operations via C ABI
// ═══════════════════════════════════════════════════════════════════════

mod hashmap_extended {
    use super::*;
    use hew_runtime::hashmap::{
        hew_hashmap_clear, hew_hashmap_get_or_default_i32, hew_hashmap_keys,
        hew_hashmap_values_i32, hew_hashmap_values_str,
    };

    #[test]
    fn keys_returns_all_keys() {
        unsafe {
            let m = hew_hashmap_new_impl();
            let ka = cstr("alpha");
            let kb = cstr("beta");
            hew_hashmap_insert_impl(m, ka.as_ptr(), 1, ptr::null());
            hew_hashmap_insert_impl(m, kb.as_ptr(), 2, ptr::null());

            let keys = hew_hashmap_keys(m);
            assert_eq!(hew_vec_len(keys), 2);

            let k0 = read_cstr(hew_vec_get_str(keys, 0));
            let k1 = read_cstr(hew_vec_get_str(keys, 1));
            let mut got = vec![k0, k1];
            got.sort();
            assert_eq!(got, vec!["alpha", "beta"]);

            hew_vec_free(keys);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn values_i32_returns_all_values() {
        unsafe {
            let m = hew_hashmap_new_impl();
            let ka = cstr("x");
            let kb = cstr("y");
            hew_hashmap_insert_impl(m, ka.as_ptr(), 10, ptr::null());
            hew_hashmap_insert_impl(m, kb.as_ptr(), 20, ptr::null());

            let vals = hew_hashmap_values_i32(m);
            assert_eq!(hew_vec_len(vals), 2);

            let mut got = vec![hew_vec_get_i32(vals, 0), hew_vec_get_i32(vals, 1)];
            got.sort();
            assert_eq!(got, vec![10, 20]);

            hew_vec_free(vals);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn values_str_returns_all_string_values() {
        unsafe {
            let m = hew_hashmap_new_impl();
            let ka = cstr("a");
            let kb = cstr("b");
            let va = cstr("hello");
            let vb = cstr("world");
            hew_hashmap_insert_impl(m, ka.as_ptr(), 0, va.as_ptr());
            hew_hashmap_insert_impl(m, kb.as_ptr(), 0, vb.as_ptr());

            let vals = hew_hashmap_values_str(m);
            assert_eq!(hew_vec_len(vals), 2);

            let v0 = read_cstr(hew_vec_get_str(vals, 0));
            let v1 = read_cstr(hew_vec_get_str(vals, 1));
            let mut got = vec![v0, v1];
            got.sort();
            assert_eq!(got, vec!["hello", "world"]);

            hew_vec_free(vals);
            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn clear_removes_all_entries() {
        unsafe {
            let m = hew_hashmap_new_impl();
            let ka = cstr("one");
            let kb = cstr("two");
            hew_hashmap_insert_impl(m, ka.as_ptr(), 1, ptr::null());
            hew_hashmap_insert_impl(m, kb.as_ptr(), 2, ptr::null());
            assert_eq!(hew_hashmap_len(m), 2);

            hew_hashmap_clear(m);
            assert_eq!(hew_hashmap_len(m), 0);
            assert!(hew_hashmap_is_empty(m));
            assert!(!hew_hashmap_contains_key(m, ka.as_ptr()));
            assert!(!hew_hashmap_contains_key(m, kb.as_ptr()));

            // Can reinsert after clear.
            hew_hashmap_insert_impl(m, ka.as_ptr(), 99, ptr::null());
            assert_eq!(hew_hashmap_len(m), 1);
            assert_eq!(hew_hashmap_get_i32(m, ka.as_ptr()), 99);

            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn get_or_default_returns_value_when_present() {
        unsafe {
            let m = hew_hashmap_new_impl();
            let k = cstr("present");
            hew_hashmap_insert_impl(m, k.as_ptr(), 42, ptr::null());

            assert_eq!(hew_hashmap_get_or_default_i32(m, k.as_ptr(), -1), 42);

            hew_hashmap_free_impl(m);
        }
    }

    #[test]
    fn get_or_default_returns_default_when_absent() {
        unsafe {
            let m = hew_hashmap_new_impl();
            let missing = cstr("absent");

            assert_eq!(
                hew_hashmap_get_or_default_i32(m, missing.as_ptr(), -99),
                -99
            );

            hew_hashmap_free_impl(m);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Mailbox overflow policies via C ABI
// ═══════════════════════════════════════════════════════════════════════

mod mailbox_policies {
    use super::*;
    use hew_runtime::mailbox::{
        hew_mailbox_capacity, hew_mailbox_len, hew_mailbox_new_with_policy, hew_mailbox_try_push,
        OverflowPolicy,
    };

    #[test]
    fn new_with_policy_creates_bounded_mailbox() {
        unsafe {
            let mb = hew_mailbox_new_with_policy(5, OverflowPolicy::DropNew);
            assert_eq!(hew_mailbox_capacity(mb), 5);
            assert_eq!(hew_mailbox_len(mb), 0);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn unbounded_capacity_is_zero() {
        unsafe {
            let mb = hew_mailbox_new();
            assert_eq!(hew_mailbox_capacity(mb), 0);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_ok() {
        unsafe {
            let mb = hew_mailbox_new_with_policy(10, OverflowPolicy::DropNew);
            let val: i32 = 42;
            let rc = hew_mailbox_try_push(mb, 1, (&raw const val).cast(), size_of::<i32>());
            assert_eq!(rc, 0);
            assert_eq!(hew_mailbox_len(mb), 1);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_drop_new_returns_1() {
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, OverflowPolicy::DropNew);
            let val: i32 = 1;
            let p: *const c_void = (&raw const val).cast();
            assert_eq!(hew_mailbox_try_push(mb, 0, p, size_of::<i32>()), 0);
            assert_eq!(hew_mailbox_try_push(mb, 0, p, size_of::<i32>()), 1);
            assert_eq!(hew_mailbox_len(mb), 1);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_drop_old_returns_2() {
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, OverflowPolicy::DropOld);
            let v1: i32 = 10;
            let v2: i32 = 20;
            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const v1).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const v2).cast(), size_of::<i32>()),
                2
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            // The remaining message should be the new one.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 2);
            assert_eq!(*((*node).data.cast::<i32>()), 20);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_push_fail_returns_neg1() {
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, OverflowPolicy::Fail);
            let val: i32 = 1;
            let p: *const c_void = (&raw const val).cast();
            assert_eq!(hew_mailbox_try_push(mb, 0, p, size_of::<i32>()), 0);
            assert_eq!(hew_mailbox_try_push(mb, 0, p, size_of::<i32>()), -1);
            assert_eq!(hew_mailbox_len(mb), 1);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn len_tracks_send_and_recv() {
        unsafe {
            let mb = hew_mailbox_new();
            assert_eq!(hew_mailbox_len(mb), 0);

            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();
            hew_mailbox_send(mb, 0, p, size_of::<i32>());
            assert_eq!(hew_mailbox_len(mb), 1);

            hew_mailbox_send(mb, 0, p, size_of::<i32>());
            assert_eq!(hew_mailbox_len(mb), 2);

            let node = hew_mailbox_try_recv(mb);
            hew_msg_node_free(node);
            assert_eq!(hew_mailbox_len(mb), 1);

            hew_mailbox_free(mb);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Coalesce mailbox overflow policy
// ═══════════════════════════════════════════════════════════════════════

mod coalesce_tests {
    use std::ffi::c_void;

    use hew_runtime::mailbox::{
        hew_mailbox_free, hew_mailbox_len, hew_mailbox_new_coalesce, hew_mailbox_send,
        hew_mailbox_try_push, hew_mailbox_try_recv, hew_msg_node_free,
    };

    #[test]
    fn coalesce_replaces_matching_msg_type() {
        unsafe {
            let mb = hew_mailbox_new_coalesce(3);

            // Fill with 3 different msg_types.
            let v1: i32 = 10;
            let v2: i32 = 20;
            let v3: i32 = 30;
            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const v1).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const v2).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 3, (&raw const v3).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(hew_mailbox_len(mb), 3);

            // Send a 4th message with the same msg_type as message 1 — should coalesce.
            let v4: i32 = 999;
            let rc = hew_mailbox_try_push(mb, 1, (&raw const v4).cast(), size_of::<i32>());
            assert_eq!(rc, 3); // coalesced
            assert_eq!(hew_mailbox_len(mb), 3); // length unchanged

            // Receive messages in order and verify the coalesced one has new data.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 1);
            assert_eq!(*((*node).data.cast::<i32>()), 999); // new data
            hew_msg_node_free(node);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 2);
            assert_eq!(*((*node).data.cast::<i32>()), 20);
            hew_msg_node_free(node);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 3);
            assert_eq!(*((*node).data.cast::<i32>()), 30);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_falls_back_to_drop_old() {
        unsafe {
            let mb = hew_mailbox_new_coalesce(2);

            let v1: i32 = 10;
            let v2: i32 = 20;
            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const v1).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const v2).cast(), size_of::<i32>()),
                0
            );

            // msg_type 3 doesn't match any existing — should drop oldest.
            let v3: i32 = 30;
            let rc = hew_mailbox_try_push(mb, 3, (&raw const v3).cast(), size_of::<i32>());
            assert_eq!(rc, 2); // dropped oldest
            assert_eq!(hew_mailbox_len(mb), 2);

            // The remaining messages should be msg_type 2 and 3.
            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 2);
            hew_msg_node_free(node);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 3);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_send_replaces_matching() {
        unsafe {
            let mb = hew_mailbox_new_coalesce(2);

            let v1: i32 = 10;
            let v2: i32 = 20;
            let p1: *mut c_void = (&raw const v1).cast_mut().cast();
            let p2: *mut c_void = (&raw const v2).cast_mut().cast();
            assert_eq!(hew_mailbox_send(mb, 1, p1, size_of::<i32>()), 0);
            assert_eq!(hew_mailbox_send(mb, 2, p2, size_of::<i32>()), 0);

            // Coalesce via hew_mailbox_send.
            let v3: i32 = 77;
            let p3: *mut c_void = (&raw const v3).cast_mut().cast();
            assert_eq!(hew_mailbox_send(mb, 1, p3, size_of::<i32>()), 0);
            assert_eq!(hew_mailbox_len(mb), 2);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 1);
            assert_eq!(*((*node).data.cast::<i32>()), 77);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Extended Vec operations
// ═══════════════════════════════════════════════════════════════════════

mod vec_extended {
    use super::*;

    #[test]
    fn sort_i32_ascending() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 5);
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 3);
            hew_vec_push_i32(v, 2);
            hew_vec_push_i32(v, 4);
            hew_vec_sort_i32(v);
            for i in 0..5 {
                assert_eq!(hew_vec_get_i32(v, i), (i as i32) + 1);
            }
            hew_vec_free(v);
        }
    }

    #[test]
    fn sort_i32_empty_and_single() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_sort_i32(v); // empty — no crash
            hew_vec_push_i32(v, 42);
            hew_vec_sort_i32(v); // single element
            assert_eq!(hew_vec_get_i32(v, 0), 42);
            hew_vec_free(v);
        }
    }

    #[test]
    fn sort_i32_null_is_noop() {
        unsafe {
            hew_vec_sort_i32(ptr::null_mut());
        }
    }

    #[test]
    fn sort_i64_ascending() {
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 100);
            hew_vec_push_i64(v, -50);
            hew_vec_push_i64(v, 0);
            hew_vec_sort_i64(v);
            assert_eq!(hew_vec_get_i64(v, 0), -50);
            assert_eq!(hew_vec_get_i64(v, 1), 0);
            assert_eq!(hew_vec_get_i64(v, 2), 100);
            hew_vec_free(v);
        }
    }

    #[test]
    fn sort_f64_ascending() {
        unsafe {
            let v = hew_vec_new_f64();
            hew_vec_push_f64(v, 3.14);
            hew_vec_push_f64(v, 1.0);
            hew_vec_push_f64(v, 2.718);
            hew_vec_sort_f64(v);
            assert!((hew_vec_get_f64(v, 0) - 1.0).abs() < f64::EPSILON);
            assert!((hew_vec_get_f64(v, 1) - 2.718).abs() < f64::EPSILON);
            assert!((hew_vec_get_f64(v, 2) - 3.14).abs() < f64::EPSILON);
            hew_vec_free(v);
        }
    }

    #[test]
    fn clone_i32_vec() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            hew_vec_push_i32(v, 30);
            let c = hew_vec_clone(v);
            assert_eq!(hew_vec_len(c), 3);
            assert_eq!(hew_vec_get_i32(c, 0), 10);
            assert_eq!(hew_vec_get_i32(c, 1), 20);
            assert_eq!(hew_vec_get_i32(c, 2), 30);
            // Mutating original does not affect clone.
            hew_vec_set_i32(v, 0, 99);
            assert_eq!(hew_vec_get_i32(c, 0), 10);
            hew_vec_free(v);
            hew_vec_free(c);
        }
    }

    #[test]
    fn clone_str_vec_deep_copies() {
        unsafe {
            let v = hew_vec_new_str();
            let hello = cstr("hello");
            let world = cstr("world");
            hew_vec_push_str(v, hello.as_ptr());
            hew_vec_push_str(v, world.as_ptr());
            let c = hew_vec_clone(v);
            assert_eq!(hew_vec_len(c), 2);
            assert_eq!(read_cstr(hew_vec_get_str(c, 0)), "hello");
            assert_eq!(read_cstr(hew_vec_get_str(c, 1)), "world");
            // Freeing original should not invalidate clone.
            hew_vec_free(v);
            assert_eq!(read_cstr(hew_vec_get_str(c, 0)), "hello");
            hew_vec_free(c);
        }
    }

    #[test]
    fn clone_empty_vec() {
        unsafe {
            let v = hew_vec_new();
            let c = hew_vec_clone(v);
            assert_eq!(hew_vec_len(c), 0);
            hew_vec_free(v);
            hew_vec_free(c);
        }
    }

    #[test]
    fn clone_null_returns_null() {
        unsafe {
            let c = hew_vec_clone(ptr::null());
            assert!(c.is_null());
        }
    }

    #[test]
    fn contains_i32_found_and_missing() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            hew_vec_push_i32(v, 30);
            assert_eq!(hew_vec_contains_i32(v, 20), 1);
            assert_eq!(hew_vec_contains_i32(v, 99), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn contains_i32_empty_vec() {
        unsafe {
            let v = hew_vec_new();
            assert_eq!(hew_vec_contains_i32(v, 0), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn contains_i32_null_is_zero() {
        unsafe {
            assert_eq!(hew_vec_contains_i32(ptr::null(), 0), 0);
        }
    }

    #[test]
    fn contains_i64_found_and_missing() {
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, i64::MAX);
            hew_vec_push_i64(v, i64::MIN);
            assert_eq!(hew_vec_contains_i64(v, i64::MAX), 1);
            assert_eq!(hew_vec_contains_i64(v, 0), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn contains_f64_found_and_missing() {
        unsafe {
            let v = hew_vec_new_f64();
            hew_vec_push_f64(v, 3.14);
            hew_vec_push_f64(v, 2.718);
            assert_eq!(hew_vec_contains_f64(v, 3.14), 1);
            assert_eq!(hew_vec_contains_f64(v, 1.0), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn contains_str_found_and_missing() {
        unsafe {
            let v = hew_vec_new_str();
            let a = cstr("alpha");
            let b = cstr("beta");
            let c = cstr("gamma");
            hew_vec_push_str(v, a.as_ptr());
            hew_vec_push_str(v, b.as_ptr());
            assert_eq!(hew_vec_contains_str(v, a.as_ptr()), 1);
            assert_eq!(hew_vec_contains_str(v, c.as_ptr()), 0);
            assert_eq!(hew_vec_contains_str(v, ptr::null()), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn swap_elements() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            hew_vec_push_i32(v, 30);
            hew_vec_swap(v, 0, 2);
            assert_eq!(hew_vec_get_i32(v, 0), 30);
            assert_eq!(hew_vec_get_i32(v, 2), 10);
            assert_eq!(hew_vec_get_i32(v, 1), 20);
            hew_vec_free(v);
        }
    }

    #[test]
    fn swap_same_index_is_noop() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 42);
            hew_vec_swap(v, 0, 0);
            assert_eq!(hew_vec_get_i32(v, 0), 42);
            hew_vec_free(v);
        }
    }

    #[test]
    fn swap_null_is_noop() {
        unsafe {
            hew_vec_swap(ptr::null_mut(), 0, 1);
        }
    }

    #[test]
    fn truncate_shrinks_vec() {
        unsafe {
            let v = hew_vec_new();
            for i in 0..5 {
                hew_vec_push_i32(v, i);
            }
            hew_vec_truncate(v, 3);
            assert_eq!(hew_vec_len(v), 3);
            assert_eq!(hew_vec_get_i32(v, 0), 0);
            assert_eq!(hew_vec_get_i32(v, 2), 2);
            hew_vec_free(v);
        }
    }

    #[test]
    fn truncate_to_zero() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_truncate(v, 0);
            assert_eq!(hew_vec_len(v), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn truncate_larger_is_noop() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);
            hew_vec_truncate(v, 10);
            assert_eq!(hew_vec_len(v), 2);
            hew_vec_free(v);
        }
    }

    #[test]
    fn truncate_null_is_noop() {
        unsafe {
            hew_vec_truncate(ptr::null_mut(), 0);
        }
    }

    #[test]
    fn truncate_str_frees_elements() {
        unsafe {
            let v = hew_vec_new_str();
            let a = cstr("aaa");
            let b = cstr("bbb");
            let c = cstr("ccc");
            hew_vec_push_str(v, a.as_ptr());
            hew_vec_push_str(v, b.as_ptr());
            hew_vec_push_str(v, c.as_ptr());
            hew_vec_truncate(v, 1);
            assert_eq!(hew_vec_len(v), 1);
            assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "aaa");
            hew_vec_free(v);
        }
    }

    #[test]
    fn reverse_i32() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);
            hew_vec_push_i32(v, 3);
            hew_vec_push_i32(v, 4);
            hew_vec_reverse_i32(v);
            assert_eq!(hew_vec_get_i32(v, 0), 4);
            assert_eq!(hew_vec_get_i32(v, 1), 3);
            assert_eq!(hew_vec_get_i32(v, 2), 2);
            assert_eq!(hew_vec_get_i32(v, 3), 1);
            hew_vec_free(v);
        }
    }

    #[test]
    fn reverse_i32_single_and_empty() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_reverse_i32(v); // empty
            hew_vec_push_i32(v, 42);
            hew_vec_reverse_i32(v); // single
            assert_eq!(hew_vec_get_i32(v, 0), 42);
            hew_vec_free(v);
        }
    }

    #[test]
    fn reverse_i32_null_is_noop() {
        unsafe {
            hew_vec_reverse_i32(ptr::null_mut());
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Extended String operations
// ═══════════════════════════════════════════════════════════════════════

mod string_extended {
    use super::*;

    #[test]
    fn split_basic() {
        unsafe {
            let s = cstr("a,b,c");
            let d = cstr(",");
            let v = hew_string_split(s.as_ptr(), d.as_ptr());
            assert_eq!(hew_vec_len(v), 3);
            assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "a");
            assert_eq!(read_cstr(hew_vec_get_str(v, 1)), "b");
            assert_eq!(read_cstr(hew_vec_get_str(v, 2)), "c");
            hew_vec_free(v);
        }
    }

    #[test]
    fn split_no_delimiter_found() {
        unsafe {
            let s = cstr("hello");
            let d = cstr(",");
            let v = hew_string_split(s.as_ptr(), d.as_ptr());
            assert_eq!(hew_vec_len(v), 1);
            assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "hello");
            hew_vec_free(v);
        }
    }

    #[test]
    fn split_multi_char_delimiter() {
        unsafe {
            let s = cstr("a::b::c");
            let d = cstr("::");
            let v = hew_string_split(s.as_ptr(), d.as_ptr());
            assert_eq!(hew_vec_len(v), 3);
            assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "a");
            assert_eq!(read_cstr(hew_vec_get_str(v, 1)), "b");
            assert_eq!(read_cstr(hew_vec_get_str(v, 2)), "c");
            hew_vec_free(v);
        }
    }

    #[test]
    fn split_trailing_delimiter() {
        unsafe {
            let s = cstr("a,b,");
            let d = cstr(",");
            let v = hew_string_split(s.as_ptr(), d.as_ptr());
            assert_eq!(hew_vec_len(v), 3);
            assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "a");
            assert_eq!(read_cstr(hew_vec_get_str(v, 1)), "b");
            assert_eq!(read_cstr(hew_vec_get_str(v, 2)), "");
            hew_vec_free(v);
        }
    }

    #[test]
    fn split_null_input() {
        unsafe {
            let d = cstr(",");
            let v = hew_string_split(ptr::null(), d.as_ptr());
            assert_eq!(hew_vec_len(v), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn split_null_delimiter() {
        unsafe {
            let s = cstr("hello");
            let v = hew_string_split(s.as_ptr(), ptr::null());
            assert_eq!(hew_vec_len(v), 1);
            assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "hello");
            hew_vec_free(v);
        }
    }

    #[test]
    fn split_empty_delimiter() {
        unsafe {
            let s = cstr("hello");
            let d = cstr("");
            let v = hew_string_split(s.as_ptr(), d.as_ptr());
            assert_eq!(hew_vec_len(v), 1);
            assert_eq!(read_cstr(hew_vec_get_str(v, 0)), "hello");
            hew_vec_free(v);
        }
    }

    #[test]
    fn to_lowercase() {
        unsafe {
            let s = cstr("Hello WORLD 123");
            let r = hew_string_to_lowercase(s.as_ptr());
            assert_eq!(read_cstr(r), "hello world 123");
            free_cstr(r);
        }
    }

    #[test]
    fn to_lowercase_null() {
        unsafe {
            let r = hew_string_to_lowercase(ptr::null());
            assert_eq!(read_cstr(r), "");
            free_cstr(r);
        }
    }

    #[test]
    fn to_uppercase() {
        unsafe {
            let s = cstr("Hello world 123");
            let r = hew_string_to_uppercase(s.as_ptr());
            assert_eq!(read_cstr(r), "HELLO WORLD 123");
            free_cstr(r);
        }
    }

    #[test]
    fn to_uppercase_null() {
        unsafe {
            let r = hew_string_to_uppercase(ptr::null());
            assert_eq!(read_cstr(r), "");
            free_cstr(r);
        }
    }

    #[test]
    fn char_at_valid() {
        unsafe {
            let s = cstr("abc");
            assert_eq!(hew_string_char_at(s.as_ptr(), 0), i32::from(b'a'));
            assert_eq!(hew_string_char_at(s.as_ptr(), 1), i32::from(b'b'));
            assert_eq!(hew_string_char_at(s.as_ptr(), 2), i32::from(b'c'));
        }
    }

    #[test]
    fn char_at_oob() {
        unsafe {
            let s = cstr("ab");
            assert_eq!(hew_string_char_at(s.as_ptr(), 2), -1);
            assert_eq!(hew_string_char_at(s.as_ptr(), -1), -1);
            assert_eq!(hew_string_char_at(ptr::null(), 0), -1);
        }
    }

    #[test]
    fn from_char() {
        unsafe {
            let r = hew_string_from_char(i32::from(b'Z'));
            assert_eq!(read_cstr(r), "Z");
            free_cstr(r);
        }
    }

    #[test]
    fn repeat_string() {
        unsafe {
            let s = cstr("ab");
            let r = hew_string_repeat(s.as_ptr(), 3);
            assert_eq!(read_cstr(r), "ababab");
            free_cstr(r);
        }
    }

    #[test]
    fn repeat_zero_and_negative() {
        unsafe {
            let s = cstr("x");
            let r0 = hew_string_repeat(s.as_ptr(), 0);
            assert_eq!(read_cstr(r0), "");
            free_cstr(r0);

            let rn = hew_string_repeat(s.as_ptr(), -1);
            assert_eq!(read_cstr(rn), "");
            free_cstr(rn);
        }
    }

    #[test]
    fn repeat_null() {
        unsafe {
            let r = hew_string_repeat(ptr::null(), 5);
            assert_eq!(read_cstr(r), "");
            free_cstr(r);
        }
    }

    #[test]
    fn repeat_one() {
        unsafe {
            let s = cstr("hello");
            let r = hew_string_repeat(s.as_ptr(), 1);
            assert_eq!(read_cstr(r), "hello");
            free_cstr(r);
        }
    }

    #[test]
    fn index_of_basic() {
        unsafe {
            let s = cstr("hello world");
            let sub = cstr("world");
            assert_eq!(hew_string_index_of(s.as_ptr(), sub.as_ptr(), 0), 6);
        }
    }

    #[test]
    fn index_of_from_position() {
        unsafe {
            let s = cstr("abcabc");
            let sub = cstr("abc");
            assert_eq!(hew_string_index_of(s.as_ptr(), sub.as_ptr(), 0), 0);
            assert_eq!(hew_string_index_of(s.as_ptr(), sub.as_ptr(), 1), 3);
        }
    }

    #[test]
    fn index_of_not_found() {
        unsafe {
            let s = cstr("hello");
            let sub = cstr("xyz");
            assert_eq!(hew_string_index_of(s.as_ptr(), sub.as_ptr(), 0), -1);
        }
    }

    #[test]
    fn index_of_null_args() {
        unsafe {
            let s = cstr("hello");
            assert_eq!(hew_string_index_of(ptr::null(), s.as_ptr(), 0), -1);
            assert_eq!(hew_string_index_of(s.as_ptr(), ptr::null(), 0), -1);
        }
    }

    #[test]
    fn index_of_negative_from() {
        unsafe {
            let s = cstr("abc");
            let sub = cstr("a");
            // Negative from is clamped to 0.
            assert_eq!(hew_string_index_of(s.as_ptr(), sub.as_ptr(), -5), 0);
        }
    }
}

// ── Generic Vec tests ──────────────────────────────────────────────────

mod generic_vec_tests {
    use super::*;

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct Point {
        x: i32,
        y: i32,
        z: i32,
    }

    #[test]
    fn push_get_struct() {
        unsafe {
            let v = hew_vec_new_generic(core::mem::size_of::<Point>() as i64, 0);
            let pts = [
                Point { x: 1, y: 2, z: 3 },
                Point { x: 4, y: 5, z: 6 },
                Point { x: 7, y: 8, z: 9 },
            ];
            for p in &pts {
                hew_vec_push_generic(v, (p as *const Point).cast());
            }
            assert_eq!(hew_vec_len(v), 3);
            for (i, expected) in pts.iter().enumerate() {
                let ptr = hew_vec_get_generic(v, i as i64).cast::<Point>();
                assert_eq!(*ptr, *expected);
            }
            hew_vec_free(v);
        }
    }

    #[test]
    fn set_generic() {
        unsafe {
            let v = hew_vec_new_generic(core::mem::size_of::<Point>() as i64, 0);
            let p1 = Point { x: 1, y: 2, z: 3 };
            hew_vec_push_generic(v, (&p1 as *const Point).cast());
            let p2 = Point {
                x: 10,
                y: 20,
                z: 30,
            };
            hew_vec_set_generic(v, 0, (&p2 as *const Point).cast());
            let got = &*hew_vec_get_generic(v, 0).cast::<Point>();
            assert_eq!(*got, p2);
            hew_vec_free(v);
        }
    }

    #[test]
    fn pop_generic() {
        unsafe {
            let v = hew_vec_new_generic(core::mem::size_of::<Point>() as i64, 0);
            let p = Point { x: 42, y: 99, z: 0 };
            hew_vec_push_generic(v, (&p as *const Point).cast());
            let mut out = core::mem::MaybeUninit::<Point>::uninit();
            let ok = hew_vec_pop_generic(v, out.as_mut_ptr().cast());
            assert_eq!(ok, 1);
            assert_eq!(out.assume_init(), p);
            // pop on empty returns 0
            let empty = hew_vec_pop_generic(v, out.as_mut_ptr().cast());
            assert_eq!(empty, 0);
            hew_vec_free(v);
        }
    }
}

// ── Iterator tests ─────────────────────────────────────────────────────

mod iter_tests {
    use super::*;

    #[test]
    fn iterate_i32_vec() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            hew_vec_push_i32(v, 30);

            let iter = hew_iter_vec(v);
            let mut collected = Vec::new();
            while hew_iter_next(iter) == 1 {
                collected.push(hew_iter_value_i32(iter));
            }
            assert_eq!(collected, vec![10, 20, 30]);
            // next after done returns 0
            assert_eq!(hew_iter_next(iter), 0);

            hew_iter_free(iter);
            hew_vec_free(v);
        }
    }

    #[test]
    fn iter_reset() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);

            let iter = hew_iter_vec(v);
            // consume all
            while hew_iter_next(iter) == 1 {}
            // reset and re-iterate
            hew_iter_reset(iter);
            let mut collected = Vec::new();
            while hew_iter_next(iter) == 1 {
                collected.push(hew_iter_value_i32(iter));
            }
            assert_eq!(collected, vec![1, 2]);

            hew_iter_free(iter);
            hew_vec_free(v);
        }
    }

    #[test]
    fn iter_empty_vec() {
        unsafe {
            let v = hew_vec_new();
            let iter = hew_iter_vec(v);
            assert_eq!(hew_iter_next(iter), 0);
            hew_iter_free(iter);
            hew_vec_free(v);
        }
    }
}

// ── Option<T> tests ────────────────────────────────────────────────────

mod option_tests {
    use super::*;

    #[test]
    fn none_is_none() {
        let opt = hew_option_none();
        assert_eq!(hew_option_is_none(opt), 1);
        assert_eq!(hew_option_is_some(opt), 0);
    }

    #[test]
    fn some_i32_roundtrip() {
        let opt = hew_option_some_i32(42);
        assert_eq!(hew_option_is_some(opt), 1);
        assert_eq!(hew_option_is_none(opt), 0);
        assert_eq!(hew_option_unwrap_i32(opt), 42);
    }

    #[test]
    fn some_i64_roundtrip() {
        let opt = hew_option_some_i64(999_999_999_999i64);
        assert_eq!(hew_option_unwrap_i64(opt), 999_999_999_999i64);
    }

    #[test]
    fn some_f64_roundtrip() {
        let opt = hew_option_some_f64(3.14159);
        let val = hew_option_unwrap_f64(opt);
        assert!((val - 3.14159).abs() < 1e-10);
    }

    #[test]
    fn unwrap_or_on_none() {
        let opt = hew_option_none();
        assert_eq!(hew_option_unwrap_or_i32(opt, 99), 99);
        assert_eq!(hew_option_unwrap_or_i64(opt, 77), 77);
        let f = hew_option_unwrap_or_f64(opt, 2.718);
        assert!((f - 2.718).abs() < 1e-10);
    }

    #[test]
    fn unwrap_or_on_some() {
        let opt = hew_option_some_i32(10);
        assert_eq!(hew_option_unwrap_or_i32(opt, 99), 10);
    }

    #[test]
    fn contains_i32() {
        let opt = hew_option_some_i32(42);
        assert_eq!(hew_option_contains_i32(opt, 42), 1);
        assert_eq!(hew_option_contains_i32(opt, 43), 0);
        let none = hew_option_none();
        assert_eq!(hew_option_contains_i32(none, 42), 0);
    }

    #[test]
    fn take_leaves_none() {
        let mut opt = hew_option_some_i32(7);
        let taken = hew_option_take(&mut opt);
        assert_eq!(hew_option_unwrap_i32(taken), 7);
        assert_eq!(hew_option_is_none(opt), 1);
    }

    #[test]
    fn replace_returns_old() {
        let mut opt = hew_option_some_i32(10);
        let old = hew_option_replace_i32(&mut opt, 20);
        assert_eq!(hew_option_unwrap_i32(old), 10);
        assert_eq!(hew_option_unwrap_i32(opt), 20);
    }

    #[test]
    fn negative_i32() {
        let opt = hew_option_some_i32(-100);
        assert_eq!(hew_option_unwrap_i32(opt), -100);
    }
}

// ── Result<T, E> tests ─────────────────────────────────────────────────

mod result_tests {
    use super::*;

    #[test]
    fn ok_i32_roundtrip() {
        let res = hew_result_ok_i32(42);
        assert_eq!(hew_result_is_ok(&res), 1);
        assert_eq!(hew_result_is_err(&res), 0);
        assert_eq!(hew_result_unwrap_i32(&res), 42);
    }

    #[test]
    fn ok_i64_roundtrip() {
        let res = hew_result_ok_i64(123_456_789_000i64);
        assert_eq!(hew_result_unwrap_i64(&res), 123_456_789_000i64);
    }

    #[test]
    fn err_code_only() {
        let res = hew_result_err_code(404);
        assert_eq!(hew_result_is_err(&res), 1);
        assert_eq!(hew_result_is_ok(&res), 0);
        assert_eq!(hew_result_error_code(&res), 404);
        let msg = hew_result_error_msg(&res);
        assert!(msg.is_null());
    }

    #[test]
    fn err_with_message() {
        let msg = cstr("file not found");
        let mut res = unsafe { hew_result_err(404, msg.as_ptr()) };
        assert_eq!(hew_result_error_code(&res), 404);
        let err_msg = hew_result_error_msg(&res);
        assert!(!err_msg.is_null());
        let s = unsafe { read_cstr(err_msg) };
        assert_eq!(s, "file not found");
        unsafe { hew_result_free(&mut res) };
    }

    #[test]
    fn unwrap_or_on_err() {
        let res = hew_result_err_code(500);
        assert_eq!(hew_result_unwrap_or_i32(&res, 99), 99);
        assert_eq!(hew_result_unwrap_or_i64(&res, 77), 77);
    }

    #[test]
    fn unwrap_or_on_ok() {
        let res = hew_result_ok_i32(10);
        assert_eq!(hew_result_unwrap_or_i32(&res, 99), 10);
    }

    #[test]
    fn ok_error_code_is_zero() {
        let res = hew_result_ok_i32(5);
        assert_eq!(hew_result_error_code(&res), 0);
    }

    #[test]
    fn free_ok_is_safe() {
        let mut res = hew_result_ok_i32(0);
        unsafe { hew_result_free(&mut res) };
        // Should not crash — no error_msg to free.
    }

    #[test]
    fn free_null_is_safe() {
        unsafe { hew_result_free(ptr::null_mut()) };
    }
}

// ═══════════════════════════════════════════════════════════════════════
// UTF-8 string operations
// ═══════════════════════════════════════════════════════════════════════

mod utf8_string_tests {
    use super::*;

    // --- hew_string_char_count ---

    #[test]
    fn char_count_ascii() {
        let s = CString::new("hello").unwrap();
        assert_eq!(unsafe { hew_string_char_count(s.as_ptr()) }, 5);
    }

    #[test]
    fn char_count_multibyte() {
        // "héllo" — é is 2 bytes, so 5 codepoints, 6 bytes
        let s = CString::new("héllo").unwrap();
        assert_eq!(unsafe { hew_string_char_count(s.as_ptr()) }, 5);
        assert_eq!(unsafe { hew_string_byte_length(s.as_ptr()) }, 6);
    }

    #[test]
    fn char_count_cjk() {
        // "日本語" — 3 codepoints, 9 bytes
        let s = CString::new("日本語").unwrap();
        assert_eq!(unsafe { hew_string_char_count(s.as_ptr()) }, 3);
        assert_eq!(unsafe { hew_string_byte_length(s.as_ptr()) }, 9);
    }

    #[test]
    fn char_count_emoji() {
        // "🦀" — 1 codepoint, 4 bytes
        let s = CString::new("🦀").unwrap();
        assert_eq!(unsafe { hew_string_char_count(s.as_ptr()) }, 1);
        assert_eq!(unsafe { hew_string_byte_length(s.as_ptr()) }, 4);
    }

    #[test]
    fn char_count_empty() {
        let s = CString::new("").unwrap();
        assert_eq!(unsafe { hew_string_char_count(s.as_ptr()) }, 0);
    }

    #[test]
    fn char_count_null() {
        assert_eq!(unsafe { hew_string_char_count(ptr::null()) }, 0);
    }

    // --- hew_string_byte_length ---

    #[test]
    fn byte_length_ascii() {
        let s = CString::new("hello").unwrap();
        assert_eq!(unsafe { hew_string_byte_length(s.as_ptr()) }, 5);
        // Should match hew_string_length
        assert_eq!(unsafe { hew_string_byte_length(s.as_ptr()) }, unsafe {
            hew_string_length(s.as_ptr())
        });
    }

    #[test]
    fn byte_length_null() {
        assert_eq!(unsafe { hew_string_byte_length(ptr::null()) }, 0);
    }

    // --- hew_string_is_ascii ---

    #[test]
    fn is_ascii_true() {
        let s = CString::new("hello world 123!").unwrap();
        assert_eq!(unsafe { hew_string_is_ascii(s.as_ptr()) }, 1);
    }

    #[test]
    fn is_ascii_false_accented() {
        let s = CString::new("héllo").unwrap();
        assert_eq!(unsafe { hew_string_is_ascii(s.as_ptr()) }, 0);
    }

    #[test]
    fn is_ascii_false_cjk() {
        let s = CString::new("日本語").unwrap();
        assert_eq!(unsafe { hew_string_is_ascii(s.as_ptr()) }, 0);
    }

    #[test]
    fn is_ascii_false_emoji() {
        let s = CString::new("hello 🦀").unwrap();
        assert_eq!(unsafe { hew_string_is_ascii(s.as_ptr()) }, 0);
    }

    #[test]
    fn is_ascii_null() {
        assert_eq!(unsafe { hew_string_is_ascii(ptr::null()) }, 1);
    }

    #[test]
    fn is_ascii_empty() {
        let s = CString::new("").unwrap();
        assert_eq!(unsafe { hew_string_is_ascii(s.as_ptr()) }, 1);
    }

    // --- hew_string_char_at_utf8 ---

    #[test]
    fn char_at_utf8_ascii() {
        let s = CString::new("hello").unwrap();
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 0) },
            'h' as i32
        );
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 4) },
            'o' as i32
        );
    }

    #[test]
    fn char_at_utf8_multibyte() {
        // "héllo" — codepoint index 1 is 'é'
        let s = CString::new("héllo").unwrap();
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 0) },
            'h' as i32
        );
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 1) },
            'é' as i32
        );
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 2) },
            'l' as i32
        );
    }

    #[test]
    fn char_at_utf8_cjk() {
        let s = CString::new("日本語").unwrap();
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 0) },
            '日' as i32
        );
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 1) },
            '本' as i32
        );
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 2) },
            '語' as i32
        );
    }

    #[test]
    fn char_at_utf8_emoji() {
        let s = CString::new("🦀").unwrap();
        assert_eq!(
            unsafe { hew_string_char_at_utf8(s.as_ptr(), 0) },
            '🦀' as i32
        );
    }

    #[test]
    fn char_at_utf8_out_of_bounds() {
        let s = CString::new("hi").unwrap();
        assert_eq!(unsafe { hew_string_char_at_utf8(s.as_ptr(), 2) }, -1);
        assert_eq!(unsafe { hew_string_char_at_utf8(s.as_ptr(), -1) }, -1);
    }

    #[test]
    fn char_at_utf8_null() {
        assert_eq!(unsafe { hew_string_char_at_utf8(ptr::null(), 0) }, -1);
    }

    // --- hew_string_substring_utf8 ---

    #[test]
    fn substring_utf8_ascii() {
        let s = CString::new("hello world").unwrap();
        let result = unsafe { hew_string_substring_utf8(s.as_ptr(), 0, 5) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "hello");
        unsafe { libc::free(result.cast()) };
    }

    #[test]
    fn substring_utf8_multibyte() {
        // "héllo" — take codepoints [1, 4) = "éll"
        let s = CString::new("héllo").unwrap();
        let result = unsafe { hew_string_substring_utf8(s.as_ptr(), 1, 4) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "éll");
        unsafe { libc::free(result.cast()) };
    }

    #[test]
    fn substring_utf8_cjk() {
        let s = CString::new("日本語").unwrap();
        let result = unsafe { hew_string_substring_utf8(s.as_ptr(), 1, 3) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "本語");
        unsafe { libc::free(result.cast()) };
    }

    #[test]
    fn substring_utf8_invalid_range() {
        let s = CString::new("hello").unwrap();
        assert!(unsafe { hew_string_substring_utf8(s.as_ptr(), 3, 1) }.is_null());
        assert!(unsafe { hew_string_substring_utf8(s.as_ptr(), -1, 3) }.is_null());
    }

    #[test]
    fn substring_utf8_null() {
        assert!(unsafe { hew_string_substring_utf8(ptr::null(), 0, 1) }.is_null());
    }

    // --- hew_string_reverse_utf8 ---

    #[test]
    fn reverse_utf8_ascii() {
        let s = CString::new("hello").unwrap();
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "olleh");
        unsafe { libc::free(result.cast()) };
    }

    #[test]
    fn reverse_utf8_multibyte() {
        let s = CString::new("héllo").unwrap();
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "olléh");
        unsafe { libc::free(result.cast()) };
    }

    #[test]
    fn reverse_utf8_cjk() {
        let s = CString::new("日本語").unwrap();
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "語本日");
        unsafe { libc::free(result.cast()) };
    }

    #[test]
    fn reverse_utf8_emoji() {
        let s = CString::new("a🦀b").unwrap();
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "b🦀a");
        unsafe { libc::free(result.cast()) };
    }

    #[test]
    fn reverse_utf8_null() {
        assert!(unsafe { hew_string_reverse_utf8(ptr::null()) }.is_null());
    }

    // --- hew_string_to_bytes ---

    #[test]
    fn to_bytes_ascii() {
        let s = CString::new("hi").unwrap();
        let v = unsafe { hew_string_to_bytes(s.as_ptr()) };
        assert!(!v.is_null());
        let vec = unsafe { &*v };
        assert_eq!(vec.len, 2);
        // Each byte is stored as an i32 element (matches hew_tcp_read convention).
        let b0 = unsafe { hew_runtime::vec::hew_vec_get_i32(v, 0) };
        let b1 = unsafe { hew_runtime::vec::hew_vec_get_i32(v, 1) };
        assert_eq!(b0, i32::from(b'h'));
        assert_eq!(b1, i32::from(b'i'));
        unsafe { hew_runtime::vec::hew_vec_free(v) };
    }

    #[test]
    fn to_bytes_multibyte() {
        // "é" is 2 bytes: 0xC3 0xA9
        let s = CString::new("é").unwrap();
        let v = unsafe { hew_string_to_bytes(s.as_ptr()) };
        assert!(!v.is_null());
        let vec = unsafe { &*v };
        assert_eq!(vec.len, 2);
        let b0 = unsafe { hew_runtime::vec::hew_vec_get_i32(v, 0) };
        let b1 = unsafe { hew_runtime::vec::hew_vec_get_i32(v, 1) };
        assert_eq!(b0, 0xC3);
        assert_eq!(b1, 0xA9);
        unsafe { hew_runtime::vec::hew_vec_free(v) };
    }

    #[test]
    fn to_bytes_null() {
        let v = unsafe { hew_string_to_bytes(ptr::null()) };
        assert!(!v.is_null());
        let vec = unsafe { &*v };
        assert_eq!(vec.len, 0);
        unsafe { hew_runtime::vec::hew_vec_free(v) };
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Supervisor nesting
// ═══════════════════════════════════════════════════════════════════════

mod supervisor_nesting_tests {
    use hew_runtime::supervisor::{
        hew_supervisor_add_child_supervisor, hew_supervisor_child_count, hew_supervisor_new,
        hew_supervisor_start, hew_supervisor_stop,
    };

    /// Restart strategy constants (mirror supervisor.rs).
    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const STRATEGY_ONE_FOR_ALL: i32 = 1;

    #[test]
    fn supervisor_nesting_add_and_count() {
        unsafe {
            let parent = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
            let child = hew_supervisor_new(STRATEGY_ONE_FOR_ALL, 3, 30);
            assert!(!parent.is_null());
            assert!(!child.is_null());

            // Initially no children.
            assert_eq!(hew_supervisor_child_count(parent), 0);

            // Add child supervisor.
            let rc = hew_supervisor_add_child_supervisor(parent, child);
            assert_eq!(rc, 0);
            assert_eq!(hew_supervisor_child_count(parent), 1);

            // Stop parent — should recursively stop child.
            hew_supervisor_stop(parent);
            // parent and child are freed; no use-after-free if we get here.
        }
    }

    #[test]
    fn supervisor_nesting_null_checks() {
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);

            // Null parent or child returns -1.
            assert_eq!(
                hew_supervisor_add_child_supervisor(std::ptr::null_mut(), sup),
                -1
            );
            assert_eq!(
                hew_supervisor_add_child_supervisor(sup, std::ptr::null_mut()),
                -1
            );

            // Self-referencing returns -1.
            assert_eq!(hew_supervisor_add_child_supervisor(sup, sup), -1);

            // Null supervisor child_count returns 0.
            assert_eq!(hew_supervisor_child_count(std::ptr::null_mut()), 0);

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn supervisor_nesting_started_parent_stops_child() {
        unsafe {
            let parent = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
            let child = hew_supervisor_new(STRATEGY_ONE_FOR_ALL, 3, 30);

            // Start both supervisors (creates their self-actors).
            assert_eq!(hew_supervisor_start(child), 0);
            assert_eq!(hew_supervisor_start(parent), 0);

            assert_eq!(hew_supervisor_add_child_supervisor(parent, child), 0);
            assert_eq!(hew_supervisor_child_count(parent), 1);

            // Stop parent — child supervisor should also be stopped and freed.
            hew_supervisor_stop(parent);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// File I/O via C ABI
// ═══════════════════════════════════════════════════════════════════════

mod file_io_tests {
    use std::ffi::{c_char, CStr, CString};

    use hew_runtime::file_io::{
        hew_file_append, hew_file_delete, hew_file_exists, hew_file_read, hew_file_size,
        hew_file_write,
    };

    fn tmp_path(name: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(name)
    }

    fn cstr(s: &str) -> CString {
        CString::new(s).unwrap()
    }

    unsafe fn read_cstr_and_free(p: *mut c_char) -> String {
        assert!(!p.is_null(), "unexpected null from file_read");
        // SAFETY: `p` is a valid, NUL-terminated, heap-allocated C string.
        let s = unsafe { CStr::from_ptr(p) }
            .to_str()
            .expect("invalid UTF-8")
            .to_owned();
        // SAFETY: `p` was allocated by `libc::strdup`.
        unsafe { libc::free(p.cast()) };
        s
    }

    #[test]
    fn file_io_write_read_roundtrip() {
        let path = tmp_path("hew_test_write_read.txt");
        let c_path = cstr(path.to_str().unwrap());
        let content = cstr("hello hew\n");

        unsafe {
            assert_eq!(hew_file_write(c_path.as_ptr(), content.as_ptr()), 0);
            let got = hew_file_read(c_path.as_ptr());
            let s = read_cstr_and_free(got);
            assert_eq!(s, "hello hew\n");

            // Cleanup.
            hew_file_delete(c_path.as_ptr());
        }
    }

    #[test]
    fn file_io_exists_and_delete() {
        let path = tmp_path("hew_test_exists.txt");
        let c_path = cstr(path.to_str().unwrap());
        let content = cstr("data");

        unsafe {
            // File doesn't exist yet.
            assert_eq!(hew_file_exists(c_path.as_ptr()), 0);

            // Create it.
            hew_file_write(c_path.as_ptr(), content.as_ptr());
            assert_eq!(hew_file_exists(c_path.as_ptr()), 1);

            // Delete it.
            assert_eq!(hew_file_delete(c_path.as_ptr()), 0);
            assert_eq!(hew_file_exists(c_path.as_ptr()), 0);
        }
    }

    #[test]
    fn file_io_append() {
        let path = tmp_path("hew_test_append.txt");
        let c_path = cstr(path.to_str().unwrap());
        let a = cstr("aaa");
        let b = cstr("bbb");

        unsafe {
            // Start fresh.
            let _ = hew_file_delete(c_path.as_ptr());
            assert_eq!(hew_file_write(c_path.as_ptr(), a.as_ptr()), 0);
            assert_eq!(hew_file_append(c_path.as_ptr(), b.as_ptr()), 0);

            let got = hew_file_read(c_path.as_ptr());
            let s = read_cstr_and_free(got);
            assert_eq!(s, "aaabbb");

            hew_file_delete(c_path.as_ptr());
        }
    }

    #[test]
    fn file_io_size() {
        let path = tmp_path("hew_test_size.txt");
        let c_path = cstr(path.to_str().unwrap());
        let content = cstr("12345");

        unsafe {
            hew_file_write(c_path.as_ptr(), content.as_ptr());
            assert_eq!(hew_file_size(c_path.as_ptr()), 5);

            hew_file_delete(c_path.as_ptr());
        }
    }

    #[test]
    fn file_io_nonexistent_returns_error() {
        let c_path = cstr("/tmp/hew_definitely_does_not_exist_xyz.txt");

        unsafe {
            let p = hew_file_read(c_path.as_ptr());
            assert!(p.is_null());
            assert_eq!(hew_file_size(c_path.as_ptr()), -1);
            assert_eq!(hew_file_delete(c_path.as_ptr()), -1);
        }
    }

    #[test]
    fn file_io_null_inputs() {
        unsafe {
            assert!(hew_file_read(std::ptr::null()).is_null());
            assert_eq!(hew_file_write(std::ptr::null(), std::ptr::null()), -1);
            assert_eq!(hew_file_append(std::ptr::null(), std::ptr::null()), -1);
            assert_eq!(hew_file_exists(std::ptr::null()), 0);
            assert_eq!(hew_file_delete(std::ptr::null()), -1);
            assert_eq!(hew_file_size(std::ptr::null()), -1);
        }
    }
}

// ── Scheduler metrics ──

mod sched_metrics_tests {
    use hew_runtime::mailbox::{
        hew_mailbox_free, hew_mailbox_new, hew_mailbox_send, hew_mailbox_try_recv,
        hew_msg_node_free,
    };
    use hew_runtime::scheduler::{
        hew_sched_init, hew_sched_metrics_active_workers, hew_sched_metrics_messages_received,
        hew_sched_metrics_messages_sent, hew_sched_metrics_reset, hew_sched_metrics_steals,
        hew_sched_metrics_tasks_completed, hew_sched_metrics_tasks_spawned, hew_sched_shutdown,
    };
    use std::ffi::c_void;
    use std::ptr;

    #[test]
    fn reset_zeroes_all_counters() {
        hew_sched_metrics_reset();

        assert_eq!(hew_sched_metrics_tasks_spawned(), 0);
        assert_eq!(hew_sched_metrics_tasks_completed(), 0);
        assert_eq!(hew_sched_metrics_steals(), 0);
        assert_eq!(hew_sched_metrics_messages_sent(), 0);
        assert_eq!(hew_sched_metrics_messages_received(), 0);
        assert_eq!(hew_sched_metrics_active_workers(), 0);
    }

    #[test]
    fn mailbox_send_increments_messages_sent() {
        hew_sched_metrics_reset();

        // SAFETY: no preconditions for creating a new mailbox.
        let mb = unsafe { hew_mailbox_new() };
        let val: i32 = 42;
        // SAFETY: mb is valid, val is a readable i32 pointer.
        unsafe {
            hew_mailbox_send(
                mb,
                1,
                ptr::addr_of!(val).cast::<c_void>().cast_mut(),
                size_of::<i32>(),
            );
        }

        assert!(hew_sched_metrics_messages_sent() > 0);
        // SAFETY: mb was allocated by hew_mailbox_new.
        unsafe { hew_mailbox_free(mb) };
    }

    #[test]
    fn mailbox_recv_increments_messages_received() {
        hew_sched_metrics_reset();

        // SAFETY: no preconditions for creating a new mailbox.
        let mb = unsafe { hew_mailbox_new() };
        let val: i32 = 7;
        // SAFETY: mb is valid, val is readable, msg is non-null before free.
        unsafe {
            hew_mailbox_send(
                mb,
                1,
                ptr::addr_of!(val).cast::<c_void>().cast_mut(),
                size_of::<i32>(),
            );
            let msg = hew_mailbox_try_recv(mb);
            assert!(!msg.is_null());
            hew_msg_node_free(msg);
        }

        assert!(hew_sched_metrics_messages_received() > 0);
        // SAFETY: mb was allocated by hew_mailbox_new.
        unsafe { hew_mailbox_free(mb) };
    }

    #[test]
    fn active_workers_is_zero_without_scheduler() {
        hew_sched_metrics_reset();
        assert_eq!(hew_sched_metrics_active_workers(), 0);
    }

    #[test]
    fn counters_readable_after_sched_init() {
        hew_sched_init();
        hew_sched_metrics_reset();

        // Counters should be zero after reset, even with scheduler running.
        assert_eq!(hew_sched_metrics_tasks_spawned(), 0);
        assert_eq!(hew_sched_metrics_tasks_completed(), 0);

        hew_sched_shutdown();
    }
}

// ── Rc / Arc ───────────────────────────────────────────────────────────

mod rc_arc_tests {
    use hew_runtime::arc::{
        hew_arc_clone, hew_arc_count, hew_arc_drop, hew_arc_get, hew_arc_new, hew_arc_strong_count,
    };
    use hew_runtime::rc::{
        hew_rc_clone, hew_rc_count, hew_rc_drop, hew_rc_get, hew_rc_is_unique, hew_rc_new,
        hew_rc_strong_count,
    };

    #[test]
    fn rc_create_clone_drop() {
        unsafe {
            let val: i32 = 42;
            let rc = hew_rc_new((&raw const val).cast(), size_of::<i32>(), None);
            assert!(!rc.is_null());
            assert_eq!(hew_rc_count(rc), 1);

            // Clone twice → count = 3
            let rc2 = hew_rc_clone(rc);
            let rc3 = hew_rc_clone(rc);
            assert_eq!(hew_rc_count(rc), 3);
            assert_eq!(hew_rc_strong_count(rc), 3);

            // Read data through hew_rc_get
            let data = hew_rc_get(rc);
            assert_eq!(data.cast::<i32>().read(), 42);

            // Drop all
            hew_rc_drop(rc3);
            assert_eq!(hew_rc_count(rc), 2);
            hew_rc_drop(rc2);
            assert_eq!(hew_rc_count(rc), 1);
            hew_rc_drop(rc);
            // rc is now freed — no further access
        }
    }

    #[test]
    fn rc_is_unique() {
        unsafe {
            let val: i32 = 7;
            let rc = hew_rc_new((&raw const val).cast(), size_of::<i32>(), None);
            assert_eq!(hew_rc_is_unique(rc), 1);

            let rc2 = hew_rc_clone(rc);
            assert_eq!(hew_rc_is_unique(rc), 0);

            hew_rc_drop(rc2);
            assert_eq!(hew_rc_is_unique(rc), 1);

            hew_rc_drop(rc);
        }
    }

    #[test]
    fn arc_create_clone_drop_threaded() {
        unsafe {
            let val: i32 = 100;
            let arc = hew_arc_new((&raw const val).cast(), size_of::<i32>(), None);
            assert!(!arc.is_null());
            assert_eq!(hew_arc_count(arc), 1);

            // Read via hew_arc_get
            let data = hew_arc_get(arc);
            assert_eq!(data.cast::<i32>().read(), 100);

            // Clone from multiple threads
            let mut handles = Vec::new();
            for _ in 0..4 {
                let arc_addr = arc as usize;
                handles.push(std::thread::spawn(move || {
                    let arc_ptr = arc_addr as *mut u8;
                    // SAFETY: arc_ptr is a valid Arc data pointer; the
                    // original strong ref keeps the allocation alive.
                    let cloned = hew_arc_clone(arc_ptr);
                    assert_eq!(hew_arc_get(cloned).cast::<i32>().read(), 100);
                    hew_arc_drop(cloned);
                }));
            }
            for h in handles {
                h.join().unwrap();
            }

            // After threads join, only original ref remains
            assert_eq!(hew_arc_count(arc), 1);
            assert_eq!(hew_arc_strong_count(arc), 1);

            hew_arc_drop(arc);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Condvar-based Block overflow policy
// ═══════════════════════════════════════════════════════════════════════

mod condvar_block_tests {
    use std::ffi::c_void;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;
    use std::thread;
    use std::time::Duration;

    use hew_runtime::mailbox::{
        hew_mailbox_free, hew_mailbox_new_with_policy, hew_mailbox_send, hew_mailbox_try_recv,
        hew_msg_node_free, HewMailbox, OverflowPolicy,
    };

    #[test]
    fn condvar_block_sender_blocks_then_unblocks() {
        unsafe {
            let mb = hew_mailbox_new_with_policy(1, OverflowPolicy::Block);
            let val: i32 = 1;
            let p: *mut c_void = (&raw const val).cast_mut().cast();

            // Fill the mailbox.
            assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);

            // Spawn a sender that should block because the mailbox is full.
            let sender_done = Arc::new(AtomicBool::new(false));
            let done_clone = sender_done.clone();
            let mb_addr = mb as usize;
            let handle = thread::spawn(move || {
                let mb = mb_addr as *mut HewMailbox;
                let val: i32 = 2;
                let rc =
                    hew_mailbox_send(mb, 1, (&raw const val).cast_mut().cast(), size_of::<i32>());
                done_clone.store(true, Ordering::Release);
                rc
            });

            // Give the sender time to block.
            thread::sleep(Duration::from_millis(100));
            assert!(
                !sender_done.load(Ordering::Acquire),
                "sender should still be blocked"
            );

            // Consume a message to free space.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            hew_msg_node_free(node);

            // Sender should unblock and succeed.
            let rc = handle.join().expect("sender thread panicked");
            assert_eq!(rc, 0);
            assert!(sender_done.load(Ordering::Acquire));

            // Verify the blocked sender's message arrived.
            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 1);
            assert_eq!(*((*node).data.cast::<i32>()), 2);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }
}

mod registry_tests {
    use std::ffi::{c_void, CString};
    use std::sync::Mutex;

    use hew_runtime::registry::{
        hew_registry_clear, hew_registry_count, hew_registry_lookup, hew_registry_register,
        hew_registry_unregister,
    };

    /// All registry tests share a global `REGISTRY` and call `hew_registry_clear`,
    /// so they must not run in parallel.
    static LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn register_and_lookup() {
        let _guard = LOCK.lock().unwrap();
        unsafe {
            hew_registry_clear();
            let name = CString::new("reg_lookup_actor").unwrap();
            let mut dummy: i32 = 42;
            let ptr: *mut c_void = (&raw mut dummy).cast();

            assert_eq!(hew_registry_register(name.as_ptr(), ptr), 0);
            let found = hew_registry_lookup(name.as_ptr());
            assert_eq!(found, ptr);
            hew_registry_clear();
        }
    }

    #[test]
    fn register_duplicate_returns_error() {
        let _guard = LOCK.lock().unwrap();
        unsafe {
            hew_registry_clear();
            let name = CString::new("reg_dup_actor").unwrap();
            let mut d1: i32 = 1;
            let mut d2: i32 = 2;
            let p1: *mut c_void = (&raw mut d1).cast();
            let p2: *mut c_void = (&raw mut d2).cast();

            assert_eq!(hew_registry_register(name.as_ptr(), p1), 0);
            assert_eq!(hew_registry_register(name.as_ptr(), p2), -1);
            assert_eq!(hew_registry_lookup(name.as_ptr()), p1);
            hew_registry_clear();
        }
    }

    #[test]
    fn unregister_then_lookup_returns_null() {
        let _guard = LOCK.lock().unwrap();
        unsafe {
            hew_registry_clear();
            let name = CString::new("reg_unreg_actor").unwrap();
            let mut dummy: i32 = 7;
            let ptr: *mut c_void = (&raw mut dummy).cast();

            assert_eq!(hew_registry_register(name.as_ptr(), ptr), 0);
            assert_eq!(hew_registry_unregister(name.as_ptr()), 0);
            assert!(hew_registry_lookup(name.as_ptr()).is_null());
            assert_eq!(hew_registry_unregister(name.as_ptr()), -1);
            hew_registry_clear();
        }
    }

    #[test]
    fn clear_resets_count_to_zero() {
        let _guard = LOCK.lock().unwrap();
        unsafe {
            hew_registry_clear();
            let a = CString::new("reg_clear_a").unwrap();
            let b = CString::new("reg_clear_b").unwrap();
            let mut d: i32 = 0;
            let p: *mut c_void = (&raw mut d).cast();

            assert_eq!(hew_registry_register(a.as_ptr(), p), 0);
            assert_eq!(hew_registry_register(b.as_ptr(), p), 0);
            assert!(hew_registry_count() >= 2);

            hew_registry_clear();
            assert_eq!(hew_registry_count(), 0);
        }
    }
}

// ── Scope cancellation ──

mod cancellation_tests {
    use hew_runtime::scope::{
        hew_scope_cancel, hew_scope_create, hew_scope_free, hew_scope_is_cancelled,
    };

    #[test]
    fn is_cancelled_default_false() {
        unsafe {
            let scope = hew_scope_create();
            assert!(!scope.is_null());
            assert_eq!(hew_scope_is_cancelled(scope), 0);
            hew_scope_free(scope);
        }
    }

    #[test]
    fn cancel_sets_flag() {
        unsafe {
            let scope = hew_scope_create();
            assert_eq!(hew_scope_is_cancelled(scope), 0);
            hew_scope_cancel(scope);
            assert_eq!(hew_scope_is_cancelled(scope), 1);
            hew_scope_free(scope);
        }
    }

    #[test]
    fn is_cancelled_null_returns_zero() {
        unsafe {
            assert_eq!(hew_scope_is_cancelled(std::ptr::null_mut()), 0);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Supervisor RestForOne strategy tests
// ═══════════════════════════════════════════════════════════════════════

mod rest_for_one_tests {
    use std::ffi::{c_char, c_void};
    use std::ptr;
    use std::sync::atomic::{AtomicI32, Ordering};

    use hew_runtime::actor::hew_actor_trap;
    use hew_runtime::supervisor::{
        hew_supervisor_add_child_spec, hew_supervisor_get_child, hew_supervisor_new,
        hew_supervisor_start, hew_supervisor_stop, HewChildSpec,
    };

    const STRATEGY_REST_FOR_ONE: i32 = 2;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    static DISPATCH_CALL_COUNT: AtomicI32 = AtomicI32::new(0);

    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
        DISPATCH_CALL_COUNT.fetch_add(1, Ordering::SeqCst);
    }

    static SCHED_INIT: std::sync::Once = std::sync::Once::new();

    fn ensure_scheduler() {
        SCHED_INIT.call_once(|| {
            hew_runtime::scheduler::hew_sched_init();
        });
    }

    #[test]
    fn rest_for_one_restarts_subsequent_children() {
        ensure_scheduler();

        unsafe {
            let sup = hew_supervisor_new(STRATEGY_REST_FOR_ONE, 10, 60);
            assert!(!sup.is_null());

            let mut states: [i32; 3] = [0, 0, 0];

            for i in 0..3 {
                let spec = HewChildSpec {
                    name: ptr::null::<c_char>(),
                    init_state: (&raw mut states[i]).cast(),
                    init_state_size: size_of::<i32>(),
                    dispatch: Some(noop_dispatch),
                    restart_policy: RESTART_PERMANENT,
                    mailbox_capacity: -1,
                    overflow: OVERFLOW_DROP_NEW,
                };
                assert_eq!(hew_supervisor_add_child_spec(sup, &spec), 0);
            }

            assert_eq!(hew_supervisor_start(sup), 0);

            let child0 = hew_supervisor_get_child(sup, 0);
            let child1 = hew_supervisor_get_child(sup, 1);
            let child2 = hew_supervisor_get_child(sup, 2);
            assert!(!child0.is_null());
            assert!(!child1.is_null());
            assert!(!child2.is_null());

            let id0_before = (*child0).id;
            let id1_before = (*child1).id;
            let id2_before = (*child2).id;

            // Crash child 1 (middle child).
            hew_actor_trap(child1, 1);

            // Wait for the supervisor to restart children.
            // Use a generous 10s deadline — under heavy workspace parallelism
            // the supervisor's restart timer thread may be starved for CPU.
            let mut restarted = false;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
            while std::time::Instant::now() < deadline {
                std::thread::sleep(std::time::Duration::from_millis(20));
                let c1 = hew_supervisor_get_child(sup, 1);
                if !c1.is_null() && (*c1).id != id1_before {
                    restarted = true;
                    break;
                }
            }
            assert!(restarted, "child 1 was never restarted");

            // Child 0 should NOT be restarted.
            let new_child0 = hew_supervisor_get_child(sup, 0);
            assert!(!new_child0.is_null());
            assert_eq!(
                (*new_child0).id,
                id0_before,
                "child 0 should NOT be restarted"
            );

            // Child 1 should be restarted (new ID).
            let new_child1 = hew_supervisor_get_child(sup, 1);
            assert!(!new_child1.is_null());
            assert_ne!((*new_child1).id, id1_before, "child 1 should be restarted");

            // Child 2 should be restarted (new ID).
            let new_child2 = hew_supervisor_get_child(sup, 2);
            assert!(!new_child2.is_null());
            assert_ne!((*new_child2).id, id2_before, "child 2 should be restarted");

            hew_supervisor_stop(sup);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Supervisor failure escalation tests
// ═══════════════════════════════════════════════════════════════════════

mod supervisor_escalation_tests {
    use std::ffi::{c_char, c_void};
    use std::ptr;

    use hew_runtime::actor::hew_actor_trap;
    use hew_runtime::supervisor::{
        hew_supervisor_add_child_spec, hew_supervisor_add_child_supervisor,
        hew_supervisor_get_child, hew_supervisor_is_running, hew_supervisor_new,
        hew_supervisor_start, hew_supervisor_stop, HewChildSpec,
    };

    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
    }

    static SCHED_INIT: std::sync::Once = std::sync::Once::new();

    fn ensure_scheduler() {
        SCHED_INIT.call_once(|| {
            hew_runtime::scheduler::hew_sched_init();
        });
    }

    #[test]
    #[ignore = "flaky: timing-sensitive supervisor escalation race"]
    fn exhausted_child_supervisor_escalates_to_parent() {
        ensure_scheduler();

        unsafe {
            // Parent: generous budget.
            let parent = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 100, 60);
            // Child: budget of 1 restart in 60s — will exhaust on 2nd crash.
            let child = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 60);
            assert!(!parent.is_null());
            assert!(!child.is_null());

            // Add an actor to the child supervisor via spec.
            let mut state: i32 = 0;
            let spec = HewChildSpec {
                name: ptr::null::<c_char>(),
                init_state: (&raw mut state).cast(),
                init_state_size: size_of::<i32>(),
                dispatch: Some(noop_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
            };
            assert_eq!(hew_supervisor_add_child_spec(child, &spec), 0);

            // Start both and register child under parent.
            assert_eq!(hew_supervisor_start(child), 0);
            assert_eq!(hew_supervisor_start(parent), 0);
            assert_eq!(hew_supervisor_add_child_supervisor(parent, child), 0);

            assert_eq!(hew_supervisor_is_running(child), 1);
            assert_eq!(hew_supervisor_is_running(parent), 1);

            // First crash — child supervisor restarts actor (within budget).
            let actor = hew_supervisor_get_child(child, 0);
            assert!(!actor.is_null());
            let original_id = (*actor).id;
            hew_actor_trap(actor, 1);

            // Wait for first restart to complete.
            let mut first_restarted = false;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
            while std::time::Instant::now() < deadline {
                std::thread::sleep(std::time::Duration::from_millis(20));
                let c = hew_supervisor_get_child(child, 0);
                if !c.is_null() && (*c).id != original_id {
                    first_restarted = true;
                    break;
                }
            }
            assert!(first_restarted, "first restart should have completed");
            assert_eq!(
                hew_supervisor_is_running(child),
                1,
                "child should still be running after 1st crash",
            );

            // Second crash — child supervisor exhausts budget, should escalate.
            let child_actor = hew_supervisor_get_child(child, 0);
            if !child_actor.is_null() {
                hew_actor_trap(child_actor, 1);
            }

            // Wait for escalation to propagate.
            let mut escalated = false;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
            while std::time::Instant::now() < deadline {
                std::thread::sleep(std::time::Duration::from_millis(20));
                if hew_supervisor_is_running(child) == 0 {
                    escalated = true;
                    break;
                }
            }
            assert!(
                escalated,
                "child supervisor should stop after exhausting restart budget",
            );

            // Parent should have received the escalation and shut down.
            let mut parent_stopped = false;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
            while std::time::Instant::now() < deadline {
                std::thread::sleep(std::time::Duration::from_millis(20));
                if hew_supervisor_is_running(parent) == 0 {
                    parent_stopped = true;
                    break;
                }
            }
            assert!(
                parent_stopped,
                "parent supervisor should stop after child escalation",
            );

            hew_supervisor_stop(parent);
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Circuit breaker tests
// ═══════════════════════════════════════════════════════════════════════

mod circuit_breaker_tests {
    use super::*;
    use hew_runtime::supervisor::{
        hew_supervisor_add_child_spec, hew_supervisor_get_child_circuit_state, hew_supervisor_new,
        hew_supervisor_set_circuit_breaker, hew_supervisor_start, hew_supervisor_stop,
        HewChildSpec, HEW_CIRCUIT_BREAKER_CLOSED, HEW_CIRCUIT_BREAKER_HALF_OPEN,
        HEW_CIRCUIT_BREAKER_OPEN,
    };

    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
    }

    #[test]
    fn circuit_breaker_configuration_and_state() {
        ensure_scheduler();

        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
            assert!(!sup.is_null());

            // Add a child
            let mut state: i32 = 0;
            let name = cstr("test-child");
            let child_spec = HewChildSpec {
                name: name.as_ptr(),
                init_state: (&raw mut state).cast(),
                init_state_size: size_of::<i32>(),
                dispatch: Some(noop_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const child_spec), 0);
            assert_eq!(hew_supervisor_start(sup), 0);

            // Initially, circuit should be closed
            assert_eq!(
                hew_supervisor_get_child_circuit_state(sup, 0),
                HEW_CIRCUIT_BREAKER_CLOSED
            );

            // Configure circuit breaker: max 2 crashes in 10 seconds, 5 second cooldown
            assert_eq!(hew_supervisor_set_circuit_breaker(sup, 0, 2, 10, 5), 0);

            // State should still be closed
            assert_eq!(
                hew_supervisor_get_child_circuit_state(sup, 0),
                HEW_CIRCUIT_BREAKER_CLOSED
            );

            // Test invalid child index
            assert_eq!(hew_supervisor_set_circuit_breaker(sup, 99, 2, 10, 5), -1);
            assert_eq!(hew_supervisor_get_child_circuit_state(sup, 99), -1);

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn circuit_breaker_constants_accessible() {
        // Verify the constants are accessible from C
        assert_eq!(HEW_CIRCUIT_BREAKER_CLOSED, 0);
        assert_eq!(HEW_CIRCUIT_BREAKER_OPEN, 1);
        assert_eq!(HEW_CIRCUIT_BREAKER_HALF_OPEN, 2);
    }
}

mod dynamic_supervision_tests {
    use hew_runtime::supervisor::{
        hew_supervisor_add_child_dynamic, hew_supervisor_child_count, hew_supervisor_get_child,
        hew_supervisor_new, hew_supervisor_remove_child, hew_supervisor_stop, HewChildSpec,
    };
    use std::ffi::c_void;

    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    unsafe extern "C" fn noop_dispatch(
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
    ) {
    }

    #[test]
    fn add_child_dynamic_returns_index() {
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
            assert!(!sup.is_null());

            let spec = HewChildSpec {
                name: std::ptr::null_mut(),
                init_state: std::ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: 16,
                overflow: OVERFLOW_DROP_NEW,
            };

            // Not started yet, so child won't be spawned but slot is allocated.
            let idx = hew_supervisor_add_child_dynamic(sup, &spec);
            assert_eq!(idx, 0);

            let idx2 = hew_supervisor_add_child_dynamic(sup, &spec);
            assert_eq!(idx2, 1);

            assert_eq!(hew_supervisor_child_count(sup), 2);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn remove_child_shrinks_count() {
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
            assert!(!sup.is_null());

            let spec = HewChildSpec {
                name: std::ptr::null_mut(),
                init_state: std::ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: 16,
                overflow: OVERFLOW_DROP_NEW,
            };

            hew_supervisor_add_child_dynamic(sup, &spec);
            hew_supervisor_add_child_dynamic(sup, &spec);
            assert_eq!(hew_supervisor_child_count(sup), 2);

            let rc = hew_supervisor_remove_child(sup, 0);
            assert_eq!(rc, 0);
            assert_eq!(hew_supervisor_child_count(sup), 1);

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn remove_child_invalid_index_returns_error() {
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
            assert!(!sup.is_null());

            assert_eq!(hew_supervisor_remove_child(sup, 0), -1); // no children
            assert_eq!(hew_supervisor_remove_child(sup, -1), -1); // negative
            assert_eq!(hew_supervisor_remove_child(std::ptr::null_mut(), 0), -1);

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn no_fixed_child_limit() {
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
            assert!(!sup.is_null());

            let spec = HewChildSpec {
                name: std::ptr::null_mut(),
                init_state: std::ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: 16,
                overflow: OVERFLOW_DROP_NEW,
            };

            // Add more than the old SUP_MAX_CHILDREN (64) limit.
            for i in 0..100 {
                let idx = hew_supervisor_add_child_dynamic(sup, &spec);
                assert_eq!(idx, i);
            }
            assert_eq!(hew_supervisor_child_count(sup), 100);

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn get_child_after_remove() {
        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60);
            assert!(!sup.is_null());

            let spec = HewChildSpec {
                name: std::ptr::null_mut(),
                init_state: std::ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: 16,
                overflow: OVERFLOW_DROP_NEW,
            };

            hew_supervisor_add_child_dynamic(sup, &spec);
            hew_supervisor_add_child_dynamic(sup, &spec);

            // Remove first child (swap-removes with last).
            hew_supervisor_remove_child(sup, 0);

            // After swap-remove, index 0 now has what was index 1.
            // Index 1 is out of range.
            assert!(hew_supervisor_get_child(sup, 1).is_null());

            hew_supervisor_stop(sup);
        }
    }
}
