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
    clippy::approx_constant,
    reason = "test data uses hardcoded floats, not mathematical constants"
)]
#![expect(
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_ptr_alignment,
    reason = "FFI tests use deliberate casts between pointer and integer types"
)]

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

// Re-export the C ABI functions under test.
use hew_runtime::actor::{hew_actor_free, hew_actor_send, hew_actor_spawn};
use hew_runtime::iter::{
    hew_iter_free, hew_iter_next, hew_iter_reset, hew_iter_value_i32, hew_iter_vec,
};
use hew_runtime::mailbox::{
    hew_mailbox_free, hew_mailbox_has_messages, hew_mailbox_new, hew_mailbox_new_bounded,
    hew_mailbox_send, hew_mailbox_send_sys, hew_mailbox_try_recv, hew_mailbox_try_recv_sys,
    hew_msg_node_free,
};
use hew_runtime::string::{
    hew_bool_to_string, hew_float_to_string, hew_int_to_string, hew_string_byte_length,
    hew_string_char_at, hew_string_char_at_utf8, hew_string_char_count, hew_string_clone,
    hew_string_concat, hew_string_contains, hew_string_drop, hew_string_ends_with,
    hew_string_equals, hew_string_find, hew_string_from_char, hew_string_index_of,
    hew_string_is_ascii, hew_string_length, hew_string_repeat, hew_string_replace,
    hew_string_reverse_utf8, hew_string_slice, hew_string_split, hew_string_starts_with,
    hew_string_substring_utf8, hew_string_to_bytes, hew_string_to_int, hew_string_to_lowercase,
    hew_string_to_uppercase, hew_string_trim,
};
use hew_runtime::vec::{
    hew_vec_clear, hew_vec_clone, hew_vec_contains_f64, hew_vec_contains_i32, hew_vec_contains_i64,
    hew_vec_contains_str, hew_vec_contains_thunk, hew_vec_free, hew_vec_get_f64,
    hew_vec_get_generic, hew_vec_get_i32, hew_vec_get_i64, hew_vec_get_layout, hew_vec_get_ptr,
    hew_vec_get_str, hew_vec_is_empty, hew_vec_len, hew_vec_new, hew_vec_new_f64,
    hew_vec_new_generic, hew_vec_new_i64, hew_vec_new_str, hew_vec_new_with_layout,
    hew_vec_pop_f64, hew_vec_pop_generic, hew_vec_pop_i32, hew_vec_pop_i64, hew_vec_pop_layout,
    hew_vec_push_f64, hew_vec_push_generic, hew_vec_push_i32, hew_vec_push_i64,
    hew_vec_push_layout, hew_vec_push_str, hew_vec_reverse_i32, hew_vec_set_f64,
    hew_vec_set_generic, hew_vec_set_i32, hew_vec_set_i64, hew_vec_set_layout, hew_vec_set_str,
    hew_vec_sort_f64, hew_vec_sort_i32, hew_vec_sort_i64, hew_vec_swap, hew_vec_truncate, ElemKind,
    HewTypeLayout, HewTypeOwnershipKind,
};
use hew_runtime::{hew_clear_error, hew_last_error};

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

/// Free a header-aware Hew `String` producer result.
///
/// Every `-> string` C-ABI producer (`concat`, `slice`, case, `trim`,
/// `replace`, `repeat`, `clone`, numeric-to-string, `substring_utf8`,
/// `reverse_utf8`, `from_char`, ...) now returns a header-aware allocation
/// (`[CStringHeader][data]`, data pointer = `base+16`). Such results MUST be
/// released through the public `hew_string_drop` consumer (which recovers the
/// base via `data-16`, validates the magic sentinel, and skips static strings)
/// — never via bare `libc::free`, which would interior-free `base+16` and
/// corrupt the heap. Genuinely raw `libc::malloc` results (e.g.
/// `hew_reply_wait` payloads) keep bare `libc::free`.
unsafe fn free_hew_string(p: *mut c_char) {
    if !p.is_null() {
        unsafe { hew_string_drop(p) };
    }
}

/// Read a *retained* string-vec element owner (`hew_vec_get_str` /
/// `hew_vec_pop_str` result) into an owned `String`, then release it via
/// `hew_string_drop`. The typed string-vec getters hand back a retained owner
/// (refcount bump) the caller MUST release; reading inline through `read_cstr`
/// and discarding the pointer leaks that owner (`ASan` flags the leak). The value
/// is copied out first so it outlives the release.
unsafe fn read_str_and_drop(p: *const c_char) -> String {
    assert!(!p.is_null(), "unexpected null string element");
    // SAFETY: `p` is a live, NUL-terminated, retained element owner.
    let owned = unsafe { CStr::from_ptr(p) }
        .to_str()
        .expect("invalid UTF-8")
        .to_owned();
    // SAFETY: `p` is a retained header-aware owner; release exactly once.
    unsafe { free_hew_string(p.cast_mut()) };
    owned
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

        // Producers may hand push_str a C string of any provenance (here plain
        // headerless `CString`s); the vec stores an independent header-aware
        // copy, so the caller keeps owning and dropping its own buffers.
        let hello = cstr("hello");
        let world = cstr("world");
        let hew = cstr("hew");

        hew_vec_push_str(v, hello.as_ptr());
        hew_vec_push_str(v, world.as_ptr());
        assert_eq!(hew_vec_len(v), 2);

        // get_str returns a retained owner — read it, then release via the
        // universal String consumer (header-aware free-at-zero / static skip).
        let g0 = hew_vec_get_str(v, 0);
        assert_eq!(read_cstr(g0), "hello");
        free_hew_string(g0.cast_mut());
        let g1 = hew_vec_get_str(v, 1);
        assert_eq!(read_cstr(g1), "world");
        free_hew_string(g1.cast_mut());

        // Set replaces and releases the old element; the new value is copied in.
        hew_vec_set_str(v, 0, hew.as_ptr());
        let g0b = hew_vec_get_str(v, 0);
        assert_eq!(read_cstr(g0b), "hew");
        free_hew_string(g0b.cast_mut());

        // Freeing the vec releases every stored element exactly once.
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
        free_hew_string(result);
    }
}

#[test]
fn string_concat_with_null() {
    unsafe {
        let a = cstr("hello");
        // null + string → just the string
        let r1 = hew_string_concat(ptr::null(), a.as_ptr());
        assert_eq!(read_cstr(r1), "hello");
        free_hew_string(r1);

        // string + null → just the string
        let r2 = hew_string_concat(a.as_ptr(), ptr::null());
        assert_eq!(read_cstr(r2), "hello");
        free_hew_string(r2);

        // null + null → empty string
        let r3 = hew_string_concat(ptr::null(), ptr::null());
        assert_eq!(read_cstr(r3), "");
        free_hew_string(r3);
    }
}

#[test]
fn string_clone_shares_buffer_via_retain() {
    unsafe {
        let original = hew_string_concat(cstr("hello").as_ptr(), cstr(" world").as_ptr());
        // `String` is immutable-shareable: clone is a refcount retain, so the
        // returned pointer ALIASES the same buffer (the copy-on-write win),
        // rather than a distinct deep copy.
        let cloned = hew_string_clone(original);
        assert_eq!(
            original as usize, cloned as usize,
            "hew_string_clone now retains (shares one buffer), not deep-copies"
        );
        assert_eq!(read_cstr(original), "hello world");
        // Releasing one owner must NOT free the shared buffer; the other owner
        // still reads it intact (rc went 2 -> 1).
        free_hew_string(cloned);
        assert_eq!(read_cstr(original), "hello world");
        // Final owner releases -> freed (verified clean under ASan).
        free_hew_string(original);
    }
}

#[test]
fn string_clone_null_returns_null() {
    unsafe {
        let result = hew_string_clone(std::ptr::null());
        assert!(result.is_null());
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
        free_hew_string(sliced);

        // Clamped: start < 0, end > len.
        let full = hew_string_slice(s.as_ptr(), -5, 999);
        assert_eq!(read_cstr(full), "hello world");
        free_hew_string(full);

        // Null input → empty string.
        let empty = hew_string_slice(ptr::null(), 0, 5);
        assert_eq!(read_cstr(empty), "");
        free_hew_string(empty);
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
        free_hew_string(s);

        let neg = hew_int_to_string(-7);
        assert_eq!(read_cstr(neg), "-7");
        free_hew_string(neg);

        // string → int
        let n = cstr("123");
        assert_eq!(hew_string_to_int(n.as_ptr()), 123);
        assert_eq!(hew_string_to_int(ptr::null()), 0);

        // float → string
        let fs = hew_float_to_string(3.14);
        let fs_str = read_cstr(fs);
        assert!(fs_str.starts_with("3.14"), "got: {fs_str}");
        free_hew_string(fs);

        // bool → string
        let t = hew_bool_to_string(true);
        assert_eq!(read_cstr(t), "true");
        free_hew_string(t);

        let f = hew_bool_to_string(false);
        assert_eq!(read_cstr(f), "false");
        free_hew_string(f);
    }
}

#[test]
fn string_to_int_strict_semantics() {
    unsafe {
        let negative = cstr("-7");
        assert_eq!(hew_string_to_int(negative.as_ptr()), -7);

        let positive = cstr("+7");
        assert_eq!(hew_string_to_int(positive.as_ptr()), 7);

        let leading_space = cstr(" 42");
        assert_eq!(hew_string_to_int(leading_space.as_ptr()), 0);

        let trailing_space = cstr("42 ");
        assert_eq!(hew_string_to_int(trailing_space.as_ptr()), 0);

        let partial = cstr("42abc");
        assert_eq!(hew_string_to_int(partial.as_ptr()), 0);

        let empty = cstr("");
        assert_eq!(hew_string_to_int(empty.as_ptr()), 0);

        let sign_only = cstr("-");
        assert_eq!(hew_string_to_int(sign_only.as_ptr()), 0);

        let max = cstr("9223372036854775807");
        assert_eq!(hew_string_to_int(max.as_ptr()), i64::MAX);

        let min = cstr("-9223372036854775808");
        assert_eq!(hew_string_to_int(min.as_ptr()), i64::MIN);

        let overflow = cstr("9223372036854775808");
        assert_eq!(hew_string_to_int(overflow.as_ptr()), 0);

        let underflow = cstr("-9223372036854775809");
        assert_eq!(hew_string_to_int(underflow.as_ptr()), 0);
    }
}

#[test]
fn string_trim() {
    unsafe {
        let s = cstr("  hello  ");
        let trimmed = hew_string_trim(s.as_ptr());
        assert_eq!(read_cstr(trimmed), "hello");
        free_hew_string(trimmed);

        // Null → empty.
        let empty = hew_string_trim(ptr::null());
        assert_eq!(read_cstr(empty), "");
        free_hew_string(empty);
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
        free_hew_string(result);

        // Replace with empty.
        let empty = cstr("");
        let r2 = hew_string_replace(s.as_ptr(), old.as_ptr(), empty.as_ptr());
        assert_eq!(read_cstr(r2), "aacc");
        free_hew_string(r2);

        // Null source → empty.
        let r3 = hew_string_replace(ptr::null(), old.as_ptr(), new.as_ptr());
        assert_eq!(read_cstr(r3), "");
        free_hew_string(r3);
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

/// Serialization lock for tests that share `DISPATCH_SIGNAL`.
static DISPATCH_LOCK: Mutex<()> = Mutex::new(());

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

/// Test dispatch function matching the Hew context-leading canonical signature.
unsafe extern "C-unwind" fn test_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    let mut count = DISPATCH_SIGNAL.0.lock().unwrap();
    *count += 1;
    DISPATCH_SIGNAL.1.notify_all();
    std::ptr::null_mut()
}

#[test]
fn actor_spawn_and_free_no_scheduler() {
    let _guard = DISPATCH_LOCK.lock().unwrap();
    // Spawning records the actor in the runtime-owned live-actor registry
    // (`rt_current().live_actors`), so a default runtime must be installed even
    // though this test never dispatches a message. `ensure_scheduler` installs
    // the idempotent process-wide default runtime; the test still exercises the
    // spawn-allocates / free-deallocates path in isolation from dispatch.
    ensure_scheduler();
    unsafe {
        // Spawn an actor and free it without driving any dispatch — exercises
        // the allocation/deallocation path in isolation.
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
    // Spawn/free record + clear the actor in the runtime-owned live-actor
    // registry, so a default runtime must be installed; `ensure_scheduler` does
    // so idempotently.
    ensure_scheduler();
    unsafe {
        let actor = hew_actor_spawn(ptr::null_mut(), 0, Some(test_dispatch));
        assert!(!actor.is_null());
        assert!((*actor).state.is_null());
        hew_actor_free(actor);
    }
}

#[test]
fn actor_free_null_reports_error() {
    unsafe {
        hew_clear_error();
        assert_eq!(hew_actor_free(ptr::null_mut()), -1);
        assert_eq!(
            read_cstr(hew_last_error()),
            "hew_actor_free: null actor pointer"
        );
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
    let _guard = DISPATCH_LOCK.lock().unwrap();
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
    static MULTI_SIGNAL: (Mutex<i32>, Condvar) = (Mutex::new(0), Condvar::new());

    unsafe extern "C-unwind" fn multi_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let mut count = MULTI_SIGNAL.0.lock().unwrap();
        *count += 1;
        MULTI_SIGNAL.1.notify_all();

        std::ptr::null_mut()
    }

    ensure_scheduler();
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
    static DATA_SIGNAL: (Mutex<i32>, Condvar) = (Mutex::new(-1), Condvar::new());

    unsafe extern "C-unwind" fn data_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        if !data.is_null() && data_size >= size_of::<i32>() {
            let val = unsafe { *(data.cast::<i32>()) };
            let mut received = DATA_SIGNAL.0.lock().unwrap();
            *received = val;
            DATA_SIGNAL.1.notify_all();
        }

        std::ptr::null_mut()
    }

    ensure_scheduler();
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
    use std::sync::mpsc;
    use std::time::Duration;

    use hew_runtime::mailbox::{
        hew_mailbox_free, hew_mailbox_len, hew_mailbox_new_coalesce, hew_mailbox_send,
        hew_mailbox_send_with_reply, hew_mailbox_try_push, hew_mailbox_try_recv, hew_msg_node_free,
    };
    use hew_runtime::reply_channel::{
        hew_reply_channel_free, hew_reply_channel_new, hew_reply_channel_retain, hew_reply_wait,
        hew_reply_wait_timeout, hew_select_first, HewReplyChannel,
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

    #[test]
    fn coalesce_preserves_original_reply_channel_and_retires_incoming_waiter() {
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            let ch1 = hew_reply_channel_new();
            let ch2 = hew_reply_channel_new();
            assert!(!ch1.is_null());
            assert!(!ch2.is_null());

            // Mirror ask-style ownership: the caller keeps one reference while
            // the queued message holds a sender-side reference until it is
            // replied or retired.
            hew_reply_channel_retain(ch1);
            hew_reply_channel_retain(ch2);

            let (tx, rx) = mpsc::channel();
            let waiter_ch2 = ch2 as usize;
            let waiter = std::thread::spawn(move || {
                let reply = hew_reply_wait(waiter_ch2 as *mut HewReplyChannel);
                let is_null = reply.is_null();
                if !reply.is_null() {
                    libc::free(reply);
                }
                tx.send(is_null)
                    .expect("superseded waiter result should send");
            });

            let v1: i32 = 10;
            let v2: i32 = 99;
            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    1,
                    (&raw const v1).cast_mut().cast(),
                    size_of::<i32>(),
                    ch1.cast(),
                ),
                0
            );
            assert_eq!(
                hew_mailbox_send_with_reply(
                    mb,
                    1,
                    (&raw const v2).cast_mut().cast(),
                    size_of::<i32>(),
                    ch2.cast(),
                ),
                0
            );
            assert_eq!(hew_mailbox_len(mb), 1);

            assert_eq!(
                rx.recv_timeout(Duration::from_millis(200)),
                Ok(true),
                "coalescing should retire the superseded incoming waiter immediately"
            );
            waiter
                .join()
                .expect("superseded waiter thread should finish");

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 1);
            assert_eq!(*((*node).data.cast::<i32>()), 99);
            assert_eq!(
                (*node).reply_channel.cast::<HewReplyChannel>(),
                ch1,
                "the surviving coalesced message must preserve the original queued reply channel"
            );
            hew_msg_node_free(node);

            let mut ready = [ch1];
            assert_eq!(
                hew_select_first(ready.as_mut_ptr(), 1, 0),
                0,
                "freeing the surviving coalesced node should still resolve the original waiter"
            );
            assert!(hew_reply_wait_timeout(ch1, 0).is_null());

            hew_reply_channel_free(ch1);
            hew_reply_channel_free(ch2);
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
            assert_eq!(read_str_and_drop(hew_vec_get_str(c, 0)), "hello");
            assert_eq!(read_str_and_drop(hew_vec_get_str(c, 1)), "world");
            // Freeing original should not invalidate clone.
            hew_vec_free(v);
            assert_eq!(read_str_and_drop(hew_vec_get_str(c, 0)), "hello");
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
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 0)), "aaa");
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
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 0)), "a");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 1)), "b");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 2)), "c");
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
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 0)), "hello");
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
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 0)), "a");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 1)), "b");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 2)), "c");
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
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 0)), "a");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 1)), "b");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 2)), "");
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
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 0)), "hello");
            hew_vec_free(v);
        }
    }

    #[test]
    fn split_empty_delimiter() {
        // split(s, "") yields one element per Unicode codepoint.
        // "hello" is 5 ASCII codepoints → ["h", "e", "l", "l", "o"].
        unsafe {
            let s = cstr("hello");
            let d = cstr("");
            let v = hew_string_split(s.as_ptr(), d.as_ptr());
            assert_eq!(hew_vec_len(v), 5);
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 0)), "h");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 1)), "e");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 2)), "l");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 3)), "l");
            assert_eq!(read_str_and_drop(hew_vec_get_str(v, 4)), "o");
            hew_vec_free(v);
        }
    }

    #[test]
    fn to_lowercase() {
        unsafe {
            let s = cstr("Hello WORLD 123");
            let r = hew_string_to_lowercase(s.as_ptr());
            assert_eq!(read_cstr(r), "hello world 123");
            free_hew_string(r);
        }
    }

    #[test]
    fn to_lowercase_null() {
        unsafe {
            let r = hew_string_to_lowercase(ptr::null());
            assert_eq!(read_cstr(r), "");
            free_hew_string(r);
        }
    }

    #[test]
    fn to_uppercase() {
        unsafe {
            let s = cstr("Hello world 123");
            let r = hew_string_to_uppercase(s.as_ptr());
            assert_eq!(read_cstr(r), "HELLO WORLD 123");
            free_hew_string(r);
        }
    }

    #[test]
    fn to_uppercase_null() {
        unsafe {
            let r = hew_string_to_uppercase(ptr::null());
            assert_eq!(read_cstr(r), "");
            free_hew_string(r);
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
            free_hew_string(r);
        }
    }

    #[test]
    fn repeat_string() {
        unsafe {
            let s = cstr("ab");
            let r = hew_string_repeat(s.as_ptr(), 3);
            assert_eq!(read_cstr(r), "ababab");
            free_hew_string(r);
        }
    }

    #[test]
    fn repeat_zero_and_negative() {
        unsafe {
            let s = cstr("x");
            let r0 = hew_string_repeat(s.as_ptr(), 0);
            assert_eq!(read_cstr(r0), "");
            free_hew_string(r0);

            let rn = hew_string_repeat(s.as_ptr(), -1);
            assert_eq!(read_cstr(rn), "");
            free_hew_string(rn);
        }
    }

    #[test]
    fn repeat_null() {
        unsafe {
            let r = hew_string_repeat(ptr::null(), 5);
            assert_eq!(read_cstr(r), "");
            free_hew_string(r);
        }
    }

    #[test]
    fn repeat_one() {
        unsafe {
            let s = cstr("hello");
            let r = hew_string_repeat(s.as_ptr(), 1);
            assert_eq!(read_cstr(r), "hello");
            free_hew_string(r);
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

    unsafe extern "C" fn point_eq(a: *const c_void, b: *const c_void) -> i32 {
        let lhs = unsafe { &*a.cast::<Point>() };
        let rhs = unsafe { &*b.cast::<Point>() };
        i32::from(lhs == rhs)
    }

    unsafe extern "C" fn point_eq_must_not_be_called(_a: *const c_void, _b: *const c_void) -> i32 {
        panic!("empty layout Vec contains must not invoke equality thunk");
    }

    #[test]
    fn push_get_struct() {
        unsafe {
            let v = hew_vec_new_generic(core::mem::size_of::<Point>() as i64, 0);
            assert!((*v).layout.is_null());
            let pts = [
                Point { x: 1, y: 2, z: 3 },
                Point { x: 4, y: 5, z: 6 },
                Point { x: 7, y: 8, z: 9 },
            ];
            for p in &pts {
                hew_vec_push_generic(v, std::ptr::from_ref::<Point>(p).cast());
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
    fn layout_descriptor_constructor_records_nullable_abi_pointer() {
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            assert!(!v.is_null());
            // layout is copied into the vec's inline storage; the pointer
            // no longer equals the caller's address but the values are identical.
            assert!(!(*v).layout.is_null());
            assert_eq!((*(*v).layout).size, layout.size);
            assert_eq!((*(*v).layout).align, layout.align);
            assert_eq!((*(*v).layout).ownership_kind, layout.ownership_kind);
            assert_eq!((*v).elem_size, core::mem::size_of::<Point>());
            assert_eq!(hew_vec_len(v), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn layout_bitcopy_record_push_get_set_pop_copies_values() {
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let first = Point { x: 1, y: 2, z: 3 };
            let second = Point { x: 4, y: 5, z: 6 };
            hew_vec_push_layout(v, (&raw const first).cast(), &raw const layout);
            hew_vec_push_layout(v, (&raw const second).cast(), &raw const layout);
            assert_eq!(hew_vec_len(v), 2);

            let got_first = &*hew_vec_get_layout(v, 0, &raw const layout).cast::<Point>();
            assert_eq!(*got_first, first);

            let replacement = Point {
                x: 10,
                y: 20,
                z: 30,
            };
            hew_vec_set_layout(v, 0, (&raw const replacement).cast(), &raw const layout);
            let got_replacement = &*hew_vec_get_layout(v, 0, &raw const layout).cast::<Point>();
            assert_eq!(*got_replacement, replacement);

            let mut out = core::mem::MaybeUninit::<Point>::uninit();
            assert_eq!(
                hew_vec_pop_layout(v, out.as_mut_ptr().cast(), &raw const layout),
                1
            );
            assert_eq!(out.assume_init(), second);
            assert_eq!(
                hew_vec_pop_layout(v, out.as_mut_ptr().cast(), &raw const layout),
                1
            );
            assert_eq!(out.assume_init(), replacement);
            assert_eq!(
                hew_vec_pop_layout(v, out.as_mut_ptr().cast(), &raw const layout),
                0
            );
            hew_vec_free(v);
        }
    }

    #[test]
    fn contains_thunk_layout_bitcopy_record_found_and_missing() {
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let first = Point { x: 1, y: 2, z: 3 };
            let second = Point { x: 4, y: 5, z: 6 };
            let missing = Point { x: 1, y: 99, z: 3 };
            hew_vec_push_layout(v, (&raw const first).cast(), &raw const layout);
            hew_vec_push_layout(v, (&raw const second).cast(), &raw const layout);

            assert_eq!(
                hew_vec_contains_thunk(v, (&raw const second).cast(), Some(point_eq)),
                1
            );
            assert_eq!(
                hew_vec_contains_thunk(v, (&raw const missing).cast(), Some(point_eq)),
                0
            );
            hew_vec_free(v);
        }
    }

    #[test]
    fn contains_thunk_empty_vec_does_not_call_thunk() {
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let needle = Point { x: 1, y: 2, z: 3 };
            assert_eq!(
                hew_vec_contains_thunk(
                    v,
                    (&raw const needle).cast(),
                    Some(point_eq_must_not_be_called),
                ),
                0
            );
            hew_vec_free(v);
        }
    }

    #[test]
    fn contains_thunk_null_vec_or_value_returns_zero() {
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        };

        unsafe {
            let needle = Point { x: 1, y: 2, z: 3 };
            assert_eq!(
                hew_vec_contains_thunk(ptr::null(), (&raw const needle).cast(), Some(point_eq)),
                0
            );
            let v = hew_vec_new_with_layout(&raw const layout);
            assert_eq!(hew_vec_contains_thunk(v, ptr::null(), Some(point_eq)), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn contains_thunk_null_eq_fn_fails_closed() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "generic_vec_tests::helper_contains_thunk_null_eq_fn_fails_closed",
            ])
            .env("RUST_TEST_THREADS", "1")
            .env(
                "HEW_DEATH_TEST",
                "helper_contains_thunk_null_eq_fn_fails_closed",
            )
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "null equality thunk must terminate abnormally"
        );
        assert!(
            String::from_utf8_lossy(&status.stderr)
                .contains("PANIC: Vec layout contains equality thunk is null"),
            "null equality thunk must report the fail-closed diagnostic"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn helper_contains_thunk_null_eq_fn_fails_closed() {
        if std::env::var("HEW_DEATH_TEST").map_or(true, |v| {
            v != "helper_contains_thunk_null_eq_fn_fails_closed"
        }) {
            return;
        }
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let needle = Point { x: 1, y: 2, z: 3 };
            let _ = hew_vec_contains_thunk(v, (&raw const needle).cast(), None);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn contains_thunk_layout_managed_fails_closed() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "generic_vec_tests::helper_contains_thunk_layout_managed_fails_closed",
            ])
            .env("RUST_TEST_THREADS", "1")
            .env(
                "HEW_DEATH_TEST",
                "helper_contains_thunk_layout_managed_fails_closed",
            )
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "layout-managed contains must terminate abnormally"
        );
        assert!(
            String::from_utf8_lossy(&status.stderr)
                .contains("PANIC: Vec layout-aware operation is not implemented"),
            "layout-managed contains must report the staged fail-closed diagnostic"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn helper_contains_thunk_layout_managed_fails_closed() {
        if std::env::var("HEW_DEATH_TEST").map_or(true, |v| {
            v != "helper_contains_thunk_layout_managed_fails_closed"
        }) {
            return;
        }
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let needle = Point { x: 1, y: 2, z: 3 };
            let _ = hew_vec_contains_thunk(v, (&raw const needle).cast(), Some(point_eq));
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn contains_thunk_elem_kind_mismatch_fails_closed() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "generic_vec_tests::helper_contains_thunk_elem_kind_mismatch_fails_closed",
            ])
            .env("RUST_TEST_THREADS", "1")
            .env(
                "HEW_DEATH_TEST",
                "helper_contains_thunk_elem_kind_mismatch_fails_closed",
            )
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "element-kind mismatch must terminate abnormally"
        );
        assert!(
            String::from_utf8_lossy(&status.stderr)
                .contains("PANIC: Vec layout-aware operation is not implemented"),
            "element-kind mismatch must report the staged fail-closed diagnostic"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn helper_contains_thunk_elem_kind_mismatch_fails_closed() {
        if std::env::var("HEW_DEATH_TEST").map_or(true, |v| {
            v != "helper_contains_thunk_elem_kind_mismatch_fails_closed"
        }) {
            return;
        }
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            (*v).elem_kind = ElemKind::String;
            let needle = Point { x: 1, y: 2, z: 3 };
            let _ = hew_vec_contains_thunk(v, (&raw const needle).cast(), Some(point_eq));
        }
    }

    #[test]
    fn layout_bitcopy_tuple_payload_preserves_byte_shape() {
        #[repr(C)]
        #[derive(Clone, Copy, Debug, PartialEq)]
        struct TuplePayload(i32, f64);

        let layout = HewTypeLayout {
            size: core::mem::size_of::<TuplePayload>(),
            align: core::mem::align_of::<TuplePayload>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let payload = TuplePayload(7, 8.5);
            hew_vec_push_layout(v, (&raw const payload).cast(), &raw const layout);
            let raw = hew_vec_get_layout(v, 0, &raw const layout);

            let expected = core::slice::from_raw_parts(
                (&raw const payload).cast::<u8>(),
                core::mem::size_of::<TuplePayload>(),
            );
            let got = core::slice::from_raw_parts(raw.cast::<u8>(), layout.size);
            assert_eq!(got, expected);
            hew_vec_free(v);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn layout_managed_push_remains_fail_closed() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "generic_vec_tests::helper_layout_managed_push_remains_fail_closed",
            ])
            .env("RUST_TEST_THREADS", "1")
            .env(
                "HEW_DEATH_TEST",
                "helper_layout_managed_push_remains_fail_closed",
            )
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "layout-managed push must terminate abnormally"
        );
        assert!(
            String::from_utf8_lossy(&status.stderr)
                .contains("PANIC: Vec layout-aware operation is not implemented"),
            "layout-managed push must report the staged fail-closed diagnostic"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn helper_layout_managed_push_remains_fail_closed() {
        if std::env::var("HEW_DEATH_TEST").map_or(true, |v| {
            v != "helper_layout_managed_push_remains_fail_closed"
        }) {
            return;
        }
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };

        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let point = Point { x: 1, y: 2, z: 3 };
            hew_vec_push_layout(v, (&raw const point).cast(), &raw const layout);
        }
    }

    #[test]
    fn set_generic() {
        unsafe {
            let v = hew_vec_new_generic(core::mem::size_of::<Point>() as i64, 0);
            let p1 = Point { x: 1, y: 2, z: 3 };
            hew_vec_push_generic(v, (&raw const p1).cast());
            let p2 = Point {
                x: 10,
                y: 20,
                z: 30,
            };
            hew_vec_set_generic(v, 0, (&raw const p2).cast());
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
            hew_vec_push_generic(v, (&raw const p).cast());
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

    // ── hew_vec_get_ptr stride-mismatch fail-closed guard ──────────────
    //
    // `hew_vec_get_ptr` assumes 8-byte pointer-sized elements. Calling it
    // on a layout vec (elem_size = 24 for Point{x,y,z}) must abort rather
    // than return silently-wrong data.  The guard is tested via the
    // subprocess death-test pattern so the libc::abort() does not terminate
    // the test runner process.

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn get_ptr_stride_mismatch_fails_closed() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "generic_vec_tests::helper_get_ptr_stride_mismatch_fails_closed",
            ])
            .env("RUST_TEST_THREADS", "1")
            .env(
                "HEW_DEATH_TEST",
                "helper_get_ptr_stride_mismatch_fails_closed",
            )
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "hew_vec_get_ptr on a wide-element vec must terminate abnormally"
        );
        assert!(
            String::from_utf8_lossy(&status.stderr)
                .contains("hew_vec_get_ptr called on a vec with elem_size != sizeof(pointer)"),
            "hew_vec_get_ptr stride mismatch must report the fail-closed diagnostic; \
             stderr was: {}",
            String::from_utf8_lossy(&status.stderr)
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn helper_get_ptr_stride_mismatch_fails_closed() {
        if std::env::var("HEW_DEATH_TEST")
            .map_or(true, |v| v != "helper_get_ptr_stride_mismatch_fails_closed")
        {
            return;
        }
        // Point is 24 bytes (three i64 fields) — wider than a pointer (8 bytes).
        // A layout vec for Point has elem_size=24; hew_vec_get_ptr must abort.
        let layout = HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        };
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let p = Point { x: 1, y: 2, z: 3 };
            hew_vec_push_layout(v, (&raw const p).cast(), &raw const layout);
            // This call must abort — elem_size=24 != sizeof(*mut c_void)=8.
            let _ = hew_vec_get_ptr(v, 0);
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
        unsafe { free_hew_string(result) };
    }

    #[test]
    fn substring_utf8_multibyte() {
        // "héllo" — take codepoints [1, 4) = "éll"
        let s = CString::new("héllo").unwrap();
        let result = unsafe { hew_string_substring_utf8(s.as_ptr(), 1, 4) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "éll");
        unsafe { free_hew_string(result) };
    }

    #[test]
    fn substring_utf8_cjk() {
        let s = CString::new("日本語").unwrap();
        let result = unsafe { hew_string_substring_utf8(s.as_ptr(), 1, 3) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "本語");
        unsafe { free_hew_string(result) };
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
        unsafe { free_hew_string(result) };
    }

    #[test]
    fn reverse_utf8_multibyte() {
        let s = CString::new("héllo").unwrap();
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "olléh");
        unsafe { free_hew_string(result) };
    }

    #[test]
    fn reverse_utf8_cjk() {
        let s = CString::new("日本語").unwrap();
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "語本日");
        unsafe { free_hew_string(result) };
    }

    #[test]
    fn reverse_utf8_emoji() {
        let s = CString::new("a🦀b").unwrap();
        let result = unsafe { hew_string_reverse_utf8(s.as_ptr()) };
        assert!(!result.is_null());
        let rs = unsafe { CStr::from_ptr(result) }.to_str().unwrap();
        assert_eq!(rs, "b🦀a");
        unsafe { free_hew_string(result) };
    }

    #[test]
    fn reverse_utf8_null() {
        assert!(unsafe { hew_string_reverse_utf8(ptr::null()) }.is_null());
    }

    // --- hew_string_to_bytes ---

    /// Read the active region of a `BytesTriple` as a byte slice for assertions.
    ///
    /// # Safety
    /// `t.ptr + t.offset` must be valid for `t.len` bytes (or `t.ptr` null with
    /// `t.len == 0`).
    unsafe fn triple_slice(t: &hew_runtime::bytes::BytesTriple) -> &[u8] {
        if t.len == 0 || t.ptr.is_null() {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(t.ptr.add(t.offset as usize), t.len as usize) }
        }
    }

    #[test]
    fn to_bytes_ascii() {
        let s = CString::new("hi").unwrap();
        let t = unsafe { hew_string_to_bytes(s.as_ptr()) };
        assert!(!t.ptr.is_null());
        assert_eq!(t.len, 2);
        assert_eq!(unsafe { triple_slice(&t) }, b"hi");
        unsafe { hew_runtime::bytes::hew_bytes_drop(t.ptr) };
    }

    #[test]
    fn to_bytes_multibyte() {
        // "é" is 2 bytes: 0xC3 0xA9
        let s = CString::new("é").unwrap();
        let t = unsafe { hew_string_to_bytes(s.as_ptr()) };
        assert!(!t.ptr.is_null());
        assert_eq!(t.len, 2);
        assert_eq!(unsafe { triple_slice(&t) }, &[0xC3, 0xA9]);
        unsafe { hew_runtime::bytes::hew_bytes_drop(t.ptr) };
    }

    #[test]
    fn to_bytes_null() {
        // Null input yields an empty triple (null ptr, len 0); drop is a no-op.
        let t = unsafe { hew_string_to_bytes(ptr::null()) };
        assert!(t.ptr.is_null());
        assert_eq!(t.len, 0);
        unsafe { hew_runtime::bytes::hew_bytes_drop(t.ptr) };
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

    /// Supervisor allocation/start/stop records the supervisor's self-actor in
    /// the runtime-owned live-actor registry, which resolves through
    /// `rt_current()` and fails closed when no runtime is installed.
    /// `hew_sched_init` installs the idempotent process-wide default runtime.
    fn ensure_runtime() {
        hew_runtime::scheduler::hew_sched_init();
    }

    #[test]
    fn supervisor_nesting_add_and_count() {
        ensure_runtime();
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
        ensure_runtime();
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

            // Null supervisor child_count returns -1.
            assert_eq!(hew_supervisor_child_count(std::ptr::null_mut()), -1);

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn supervisor_nesting_started_parent_stops_child() {
        ensure_runtime();
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
        hew_file_write, hew_path_exists,
    };
    use hew_runtime::string::hew_string_drop;

    fn tmp_path(name: &str) -> std::path::PathBuf {
        let pid = std::process::id();
        let tid = format!("{:?}", std::thread::current().id());
        std::env::temp_dir().join(format!("{name}_{pid}_{tid}"))
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
        // SAFETY: `hew_file_read` produces a header-aware allocation via
        // `str_to_malloc`; release it through the public `hew_string_drop`
        // consumer (recovers the base, validates the header), never bare
        // `libc::free` which would interior-free `base+16`.
        unsafe { hew_string_drop(p) };
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
    fn path_exists_accepts_directories() {
        let dir = tmp_path("hew_test_path_exists_dir");
        let c_dir = cstr(dir.to_str().unwrap());

        std::fs::create_dir_all(&dir).unwrap();

        unsafe {
            assert_eq!(hew_path_exists(c_dir.as_ptr()), 1);
        }

        std::fs::remove_dir_all(&dir).unwrap();
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
            assert_eq!(hew_path_exists(std::ptr::null()), 0);
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
        hew_sched_metrics_tasks_completed, hew_sched_metrics_tasks_spawned,
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

        // NOTE: Do NOT call hew_sched_shutdown() here. The global scheduler
        // cannot be re-initialized after shutdown, so shutting it down would
        // break any subsequent test that needs the scheduler (e.g. supervisor
        // tests that call hew_sched_init via ensure_scheduler).
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
            let rc = hew_rc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                None,
            );
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
            let rc = hew_rc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                None,
            );
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
            let arc = hew_arc_new(
                (&raw const val).cast(),
                size_of::<i32>(),
                align_of::<i32>(),
                None,
            );
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
    use hew_runtime::scheduler::hew_sched_init;

    /// The name registry lives on the default `RuntimeInner`; every registry FFI
    /// call resolves it through `rt_current()`, which fails closed when no
    /// runtime is installed. `hew_sched_init` installs the process-wide default
    /// runtime and is idempotent (a second call is a no-op), so each test calls
    /// it before touching the registry. The runtime is intentionally never torn
    /// down — it persists for the whole test binary, shared across tests, which
    /// is why `LOCK` serializes them.
    static LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn register_and_lookup() {
        let _guard = LOCK.lock().unwrap();
        hew_sched_init();
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
        hew_sched_init();
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
        hew_sched_init();
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
        hew_sched_init();
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

// ═══════════════════════════════════════════════════════════════════════
// Supervisor RestForOne strategy tests
// ═══════════════════════════════════════════════════════════════════════

mod rest_for_one_tests {
    use std::ffi::{c_char, c_void};
    use std::ptr;

    use hew_runtime::actor::hew_actor_trap;
    use hew_runtime::supervisor::{
        hew_supervisor_add_child_spec, hew_supervisor_get_child, hew_supervisor_new,
        hew_supervisor_set_restart_notify, hew_supervisor_start, hew_supervisor_stop,
        hew_supervisor_wait_restart, HewChildSpec,
    };

    const STRATEGY_REST_FOR_ONE: i32 = 2;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
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
            hew_supervisor_set_restart_notify(sup);

            let mut states: [i32; 3] = [0, 0, 0];

            for state in &mut states {
                let spec = HewChildSpec {
                    name: ptr::null::<c_char>(),
                    init_state: std::ptr::from_mut::<i32>(state).cast(),
                    init_state_size: size_of::<i32>(),
                    dispatch: Some(noop_dispatch),
                    restart_policy: RESTART_PERMANENT,
                    mailbox_capacity: -1,
                    overflow: OVERFLOW_DROP_NEW,
                    arena_cap_bytes: 0,
                    cycle_capable: 0,
                    on_crash: None,
                    lifecycle_fn: None,
                };
                assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
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

            // Wait for the restart cycle to complete (condvar, no polling).
            let count = hew_supervisor_wait_restart(sup, 1, 10_000);
            assert!(count >= 1, "restart cycle never completed");

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
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Condvar, Mutex};
    use std::time::{Duration, Instant};

    use hew_runtime::actor::{hew_actor_send, hew_actor_trap};
    use hew_runtime::supervisor::{
        hew_supervisor_add_child_spec, hew_supervisor_add_child_supervisor,
        hew_supervisor_add_child_supervisor_with_init, hew_supervisor_get_child,
        hew_supervisor_get_child_supervisor, hew_supervisor_get_child_wait,
        hew_supervisor_is_running, hew_supervisor_new, hew_supervisor_set_restart_notify,
        hew_supervisor_start, hew_supervisor_stop, hew_supervisor_wait_restart, HewChildSpec,
        HewSupervisor,
    };

    const STRATEGY_ONE_FOR_ONE: i32 = 0;
    const RESTART_PERMANENT: i32 = 0;
    const OVERFLOW_DROP_NEW: i32 = 1;

    /// Generous timeout for dispatch-wait synchronization. If the runtime
    /// does not dispatch within this window, something is genuinely broken —
    /// the previous `for _ in 0..20 { sleep 10ms }` pattern (200ms total)
    /// raced on slow CI runners and caused spurious failures.
    const DISPATCH_WAIT_TIMEOUT: Duration = Duration::from_secs(30);

    static TEST_LOCK: Mutex<()> = Mutex::new(());
    static NESTED_INIT_CALLS: AtomicUsize = AtomicUsize::new(0);
    /// (count, condvar) — dispatch callback increments the count under the
    /// mutex and notifies; tests wait with a bounded `wait_timeout` for the
    /// count to advance past a captured baseline. Replaces the racing
    /// `AtomicI32` + sleep-poll pattern.
    static NESTED_DISPATCH_COUNT: (Mutex<i32>, Condvar) = (Mutex::new(0), Condvar::new());
    static SIBLING_DISPATCH_COUNT: (Mutex<i32>, Condvar) = (Mutex::new(0), Condvar::new());

    /// Block until `signal.0` reaches at least `target`, or `DISPATCH_WAIT_TIMEOUT`
    /// elapses. Returns the observed count on success, or the last-seen count
    /// on timeout (so callers can assert and report the actual value).
    fn wait_for_count(signal: &(Mutex<i32>, Condvar), target: i32) -> i32 {
        let deadline = Instant::now() + DISPATCH_WAIT_TIMEOUT;
        let mut count = signal.0.lock().unwrap();
        while *count < target {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                break;
            }
            let (guard, _) = signal.1.wait_timeout(count, remaining).unwrap();
            count = guard;
        }
        *count
    }

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn counting_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let mut count = NESTED_DISPATCH_COUNT.0.lock().unwrap();
        *count += 1;
        NESTED_DISPATCH_COUNT.1.notify_all();

        std::ptr::null_mut()
    }

    unsafe extern "C-unwind" fn sibling_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        let mut count = SIBLING_DISPATCH_COUNT.0.lock().unwrap();
        *count += 1;
        SIBLING_DISPATCH_COUNT.1.notify_all();

        std::ptr::null_mut()
    }

    unsafe extern "C" fn nested_child_supervisor_init() -> *mut HewSupervisor {
        NESTED_INIT_CALLS.fetch_add(1, Ordering::SeqCst);

        let child = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 60) };
        if child.is_null() {
            return ptr::null_mut();
        }

        unsafe { hew_supervisor_set_restart_notify(child) };

        let mut state: i32 = 0;
        let spec = HewChildSpec {
            name: ptr::null::<c_char>(),
            init_state: (&raw mut state).cast(),
            init_state_size: std::mem::size_of::<i32>(),
            dispatch: Some(counting_dispatch),
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
        };

        if unsafe { hew_supervisor_add_child_spec(child, &raw const spec) } != 0 {
            unsafe { hew_supervisor_stop(child) };
            return ptr::null_mut();
        }
        if unsafe { hew_supervisor_start(child) } != 0 {
            unsafe { hew_supervisor_stop(child) };
            return ptr::null_mut();
        }

        child
    }

    static SCHED_INIT: std::sync::Once = std::sync::Once::new();

    fn ensure_scheduler() {
        SCHED_INIT.call_once(|| {
            hew_runtime::scheduler::hew_sched_init();
        });
    }

    #[test]
    fn exhausted_child_supervisor_escalates_to_parent() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        ensure_scheduler();

        unsafe {
            // Parent: generous budget.
            let parent = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 100, 60);
            // Child: budget of 1 restart in 60s — will exhaust on 2nd crash.
            let child = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 60);
            assert!(!parent.is_null());
            assert!(!child.is_null());

            // Install restart notification on both supervisors.
            hew_supervisor_set_restart_notify(child);
            hew_supervisor_set_restart_notify(parent);

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
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
            };
            assert_eq!(hew_supervisor_add_child_spec(child, &raw const spec), 0);

            // Start both and register child under parent.
            assert_eq!(hew_supervisor_start(child), 0);
            assert_eq!(hew_supervisor_start(parent), 0);
            assert_eq!(hew_supervisor_add_child_supervisor(parent, child), 0);

            assert_eq!(hew_supervisor_is_running(child), 1);
            assert_eq!(hew_supervisor_is_running(parent), 1);

            // First crash — child supervisor restarts actor (within budget).
            let actor = hew_supervisor_get_child(child, 0);
            assert!(!actor.is_null());
            hew_actor_trap(actor, 1);

            // Wait for first restart via condvar (no polling).
            let count = hew_supervisor_wait_restart(child, 1, 10_000);
            assert!(count >= 1, "first restart should have completed");
            assert_eq!(
                hew_supervisor_is_running(child),
                1,
                "child should still be running after 1st crash",
            );

            // Second crash — child supervisor exhausts budget, should escalate.
            let child_actor = hew_supervisor_get_child(child, 0);
            assert!(!child_actor.is_null());
            hew_actor_trap(child_actor, 1);

            // Wait for escalation: child supervisor notifies on budget exhaustion,
            // then parent notifies when it processes the escalation.
            let count = hew_supervisor_wait_restart(child, 2, 10_000);
            assert!(count >= 2, "child should notify on budget exhaustion");

            let count = hew_supervisor_wait_restart(parent, 1, 10_000);
            assert!(count >= 1, "parent should process escalation");

            // Both supervisors should have stopped.
            assert_eq!(
                hew_supervisor_is_running(child),
                0,
                "child supervisor should stop after exhausting restart budget",
            );
            assert_eq!(
                hew_supervisor_is_running(parent),
                0,
                "parent supervisor should stop after child escalation",
            );

            hew_supervisor_stop(parent);
        }
    }

    #[test]
    fn get_child_wait_returns_restarted_child_without_explicit_notifier_setup() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        ensure_scheduler();

        unsafe {
            let sup = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 10, 60);
            assert!(!sup.is_null());

            let mut state: i32 = 0;
            let spec = HewChildSpec {
                name: ptr::null::<c_char>(),
                init_state: (&raw mut state).cast(),
                init_state_size: size_of::<i32>(),
                dispatch: Some(noop_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
            };
            assert_eq!(hew_supervisor_add_child_spec(sup, &raw const spec), 0);
            assert_eq!(hew_supervisor_start(sup), 0);

            let child = hew_supervisor_get_child(sup, 0);
            assert!(!child.is_null());
            let first_id = (*child).id;

            hew_actor_trap(child, 1);

            let first_deadline = Instant::now() + Duration::from_secs(5);
            let restarted_once = loop {
                let current = hew_supervisor_get_child(sup, 0);
                if !current.is_null() && (*current).id != first_id {
                    break current;
                }
                assert!(
                    Instant::now() < first_deadline,
                    "first restart never completed"
                );
                std::thread::sleep(Duration::from_millis(1));
            };
            let second_id = (*restarted_once).id;

            hew_actor_trap(restarted_once, 1);

            let pending_deadline = Instant::now() + Duration::from_secs(5);
            loop {
                let current = hew_supervisor_get_child(sup, 0);
                if current.is_null() {
                    break;
                }
                assert_eq!(
                    (*current).id,
                    second_id,
                    "second crash should enter a null slot before delayed restart publishes"
                );
                assert!(
                    Instant::now() < pending_deadline,
                    "second crash never entered a waitable restart gap"
                );
                std::thread::sleep(Duration::from_millis(1));
            }

            let restarted_twice = hew_supervisor_get_child_wait(sup, 0, 10_000);
            assert!(!restarted_twice.is_null());
            assert_ne!(
                (*restarted_twice).id,
                second_id,
                "get_child_wait should publish the restarted child without wait_restart setup"
            );

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "FFI recovery scenario exercises the full nested subtree restart flow"
    )]
    fn child_supervisor_recovery_recreates_nested_subtree() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        ensure_scheduler();
        NESTED_INIT_CALLS.store(0, Ordering::SeqCst);
        *NESTED_DISPATCH_COUNT.0.lock().unwrap() = 0;
        *SIBLING_DISPATCH_COUNT.0.lock().unwrap() = 0;

        unsafe {
            let parent = hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 10, 60);
            assert!(!parent.is_null());
            hew_supervisor_set_restart_notify(parent);

            let child = nested_child_supervisor_init();
            assert!(!child.is_null(), "child supervisor init should succeed");
            assert_eq!(
                NESTED_INIT_CALLS.load(Ordering::SeqCst),
                1,
                "initial child supervisor should be created through init_fn",
            );

            assert_eq!(
                hew_supervisor_add_child_supervisor_with_init(
                    parent,
                    child,
                    nested_child_supervisor_init
                ),
                0,
            );

            let mut sibling_state: i32 = 42;
            let sibling_spec = HewChildSpec {
                name: ptr::null::<c_char>(),
                init_state: (&raw mut sibling_state).cast(),
                init_state_size: std::mem::size_of::<i32>(),
                dispatch: Some(sibling_dispatch),
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
            };
            assert_eq!(
                hew_supervisor_add_child_spec(parent, &raw const sibling_spec),
                0,
                "parent should add a healthy sibling actor",
            );
            assert_eq!(hew_supervisor_start(parent), 0);

            let first_actor = hew_supervisor_get_child(child, 0);
            assert!(
                !first_actor.is_null(),
                "nested child actor should be spawned"
            );

            let before = *NESTED_DISPATCH_COUNT.0.lock().unwrap();
            hew_actor_send(first_actor, 1, ptr::null_mut(), 0);
            let observed = wait_for_count(&NESTED_DISPATCH_COUNT, before + 1);
            assert!(
                observed > before,
                "nested actor should process messages before crashing \
                 (before={before}, observed={observed} after {DISPATCH_WAIT_TIMEOUT:?})",
            );

            let first_actor_id = (*first_actor).id;
            hew_actor_trap(first_actor, 1);

            // 30s: this wait twice missed a 10s budget on loaded hosted
            // Windows runners (0.21s isolated — the bound is contention
            // headroom, not expected duration).
            let count = hew_supervisor_wait_restart(child, 1, 30_000);
            assert!(
                count >= 1,
                "child supervisor should restart nested actor once"
            );
            assert_eq!(hew_supervisor_is_running(child), 1);

            let restarted_actor = hew_supervisor_get_child(child, 0);
            assert!(
                !restarted_actor.is_null(),
                "child supervisor should replace crashed actor"
            );
            assert_ne!(
                (*restarted_actor).id,
                first_actor_id,
                "first crash should restart the nested actor in-place",
            );

            hew_actor_trap(restarted_actor, 1);

            // 30s: same contention headroom as the child wait above.
            let count = hew_supervisor_wait_restart(parent, 1, 30_000);
            assert!(
                count >= 1,
                "parent should observe child supervisor escalation"
            );
            assert_eq!(
                hew_supervisor_is_running(parent),
                1,
                "parent should remain running and recover the nested subtree",
            );
            assert_eq!(
                NESTED_INIT_CALLS.load(Ordering::SeqCst),
                2,
                "parent should call init_fn to replace the exhausted child supervisor",
            );

            let restarted_child = hew_supervisor_get_child_supervisor(parent, 0);
            assert!(
                !restarted_child.is_null(),
                "parent should expose a replacement child supervisor",
            );
            assert_ne!(
                restarted_child, child,
                "parent should replace the exhausted child supervisor instance",
            );
            assert_eq!(hew_supervisor_is_running(restarted_child), 1);

            let recovered_actor = hew_supervisor_get_child(restarted_child, 0);
            assert!(
                !recovered_actor.is_null(),
                "replacement child supervisor should recreate its actor subtree",
            );

            let before = *NESTED_DISPATCH_COUNT.0.lock().unwrap();
            hew_actor_send(recovered_actor, 1, ptr::null_mut(), 0);
            let observed = wait_for_count(&NESTED_DISPATCH_COUNT, before + 1);
            assert!(
                observed > before,
                "replacement nested actor should continue processing messages \
                 (before={before}, observed={observed} after {DISPATCH_WAIT_TIMEOUT:?})",
            );

            let sibling = hew_supervisor_get_child(parent, 0);
            assert!(
                !sibling.is_null(),
                "healthy sibling actor should remain available after child supervisor recovery",
            );
            let before = *SIBLING_DISPATCH_COUNT.0.lock().unwrap();
            hew_actor_send(sibling, 1, ptr::null_mut(), 0);
            let observed = wait_for_count(&SIBLING_DISPATCH_COUNT, before + 1);
            assert!(
                observed > before,
                "healthy sibling actor should still process messages after nested recovery \
                 (before={before}, observed={observed} after {DISPATCH_WAIT_TIMEOUT:?})",
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

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
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
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
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

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _data_size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    /// Supervisor allocation/stop records its self-actor in the runtime-owned
    /// live-actor registry, which resolves through `rt_current()` and fails
    /// closed when no runtime is installed. `hew_sched_init` installs the
    /// idempotent process-wide default runtime.
    fn ensure_runtime() {
        hew_runtime::scheduler::hew_sched_init();
    }

    #[test]
    fn add_child_dynamic_returns_index() {
        ensure_runtime();
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
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
            };

            // Not started yet, so child won't be spawned but slot is allocated.
            let idx = hew_supervisor_add_child_dynamic(sup, &raw const spec);
            assert_eq!(idx, 0);

            let idx2 = hew_supervisor_add_child_dynamic(sup, &raw const spec);
            assert_eq!(idx2, 1);

            assert_eq!(hew_supervisor_child_count(sup), 2);
            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn remove_child_shrinks_count() {
        ensure_runtime();
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
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
            };

            hew_supervisor_add_child_dynamic(sup, &raw const spec);
            hew_supervisor_add_child_dynamic(sup, &raw const spec);
            assert_eq!(hew_supervisor_child_count(sup), 2);

            let rc = hew_supervisor_remove_child(sup, 0);
            assert_eq!(rc, 0);
            assert_eq!(hew_supervisor_child_count(sup), 1);

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn remove_child_invalid_index_returns_error() {
        ensure_runtime();
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
        ensure_runtime();
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
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
            };

            // Add more than the old SUP_MAX_CHILDREN (64) limit.
            for i in 0..100 {
                let idx = hew_supervisor_add_child_dynamic(sup, &raw const spec);
                assert_eq!(idx, i);
            }
            assert_eq!(hew_supervisor_child_count(sup), 100);

            hew_supervisor_stop(sup);
        }
    }

    #[test]
    fn get_child_after_remove() {
        ensure_runtime();
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
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
            };

            hew_supervisor_add_child_dynamic(sup, &raw const spec);
            hew_supervisor_add_child_dynamic(sup, &raw const spec);

            // Remove first child (swap-removes with last).
            hew_supervisor_remove_child(sup, 0);

            // After swap-remove, index 0 now has what was index 1.
            // Index 1 is out of range.
            assert!(hew_supervisor_get_child(sup, 1).is_null());

            hew_supervisor_stop(sup);
        }
    }
}
