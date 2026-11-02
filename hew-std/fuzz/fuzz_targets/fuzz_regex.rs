//! `std.text.regex` with an untrusted pattern, subject and replacement.
//!
//! Input: `pattern NUL text NUL replacement`. Properties: compiling any
//! pattern never panics and stays within the engine's size limits; every
//! query never panics; and the queries agree: a match yields a full capture
//! row, and `find_all` is empty exactly when nothing matches.
#![no_main]

use hew_cabi::vec::{hew_vec_free, hew_vec_len, HewVec};
use hew_std::regex::{
    hew_regex_capture, hew_regex_capture_width, hew_regex_captures_flat, hew_regex_find,
    hew_regex_find_all, hew_regex_free, hew_regex_is_match, hew_regex_new, hew_regex_replace,
};
use libfuzzer_sys::fuzz_target;

mod support;
use support::{take, Managed};

/// Length of an owned string vector, which is then released.
unsafe fn count(vec: *mut HewVec) -> i64 {
    // SAFETY: `vec` is an owned vector result, released once.
    unsafe {
        let len = hew_vec_len(vec);
        hew_vec_free(vec);
        len
    }
}

fuzz_target!(|data: &[u8]| {
    let Ok(text) = std::str::from_utf8(data) else {
        return;
    };
    let (pattern, rest) = support::two_fields(text);
    let (subject, replacement) = support::two_fields(rest);
    let pattern = Managed::new(pattern);
    let subject = Managed::new(subject);
    let replacement = Managed::new(replacement);
    // SAFETY: inputs are live for every call; each result is released once.
    unsafe {
        let re = hew_regex_new(pattern.ptr());
        if re.is_null() {
            return;
        }
        let matched = hew_regex_is_match(re, subject.ptr());
        let width = hew_regex_capture_width(re);
        let row = count(hew_regex_captures_flat(re, subject.ptr()));
        assert_eq!(
            row,
            if matched { width } else { 0 },
            "capture row disagrees with is_match"
        );
        let all = count(hew_regex_find_all(re, subject.ptr()));
        assert_eq!(all > 0, matched, "find_all disagrees with is_match");
        drop(take(hew_regex_find(re, subject.ptr())));
        drop(take(hew_regex_capture(re, subject.ptr(), width - 1)));
        drop(take(hew_regex_replace(
            re,
            subject.ptr(),
            replacement.ptr(),
        )));
        hew_regex_free(re);
    }
});
