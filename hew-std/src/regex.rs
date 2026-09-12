//! Hew runtime: `std::text::regex` module.
//!
//! Provides regular expression matching and replacement for compiled Hew
//! programs. Inputs borrow managed UTF-8 strings; text results transfer managed
//! owners that callers release with `string_release`. Null is canonical empty.
use hew_cabi::{
    string::{string_as_str, string_from_str, string_release, HewString},
    vec::{hew_vec_new_str, hew_vec_push_str, HewVec},
};

/// Opaque handle wrapping a compiled [`regex::Regex`].
///
/// Created by [`hew_regex_new`], freed by [`hew_regex_free`].
#[derive(Debug)]
pub struct HewRegex {
    inner: regex::Regex,
}

/// Compile a regular expression pattern.
///
/// Returns a heap-allocated [`HewRegex`], or null if the pattern is invalid.
/// The caller must free it with [`hew_regex_free`].
///
/// # Safety
///
/// `pattern` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_new(pattern: *const HewString) -> *mut HewRegex {
    // SAFETY: caller guarantees pattern is a live managed string handle (null means empty).
    let pat = unsafe { string_as_str(pattern) };
    match regex::Regex::new(pat) {
        Ok(re) => Box::into_raw(Box::new(HewRegex { inner: re })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Return true if `re` is a non-null regex handle.
#[no_mangle]
pub extern "C" fn hew_regex_is_valid(re: *const HewRegex) -> bool {
    !re.is_null()
}

/// Test whether `text` matches the compiled regex.
///
/// Returns `true` if the text matches, `false` otherwise.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_is_match(re: *const HewRegex, text: *const HewString) -> bool {
    if re.is_null() {
        return false;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    regex.inner.is_match(text_str)
}

/// Find the first match of the compiled regex in `text`.
///
/// Returns an owned managed match string, or canonical empty (null) when no
/// match is found or the match is empty. Release the result with `string_release`.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_find(
    re: *const HewRegex,
    text: *const HewString,
) -> *mut HewString {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    match regex.inner.find(text_str) {
        Some(m) => string_from_str(m.as_str()),
        None => string_from_str(""),
    }
}

unsafe fn new_string_vec() -> *mut HewVec {
    // SAFETY: runtime allocates a new Vec<string> handle owned by the caller.
    unsafe { hew_vec_new_str() }
}

unsafe fn push_string(vec: *mut HewVec, value: &str) {
    let string = string_from_str(value);
    // SAFETY: vec is a live Vec<string>; push retains this borrowed owner,
    // including the canonical empty handle, which still occupies one slot.
    unsafe { hew_vec_push_str(vec, string) };
    // SAFETY: the vector retained its own reference; release the producer's owner.
    unsafe { string_release(string) };
}

unsafe fn push_optional_capture(vec: *mut HewVec, capture: Option<regex::Match<'_>>) {
    // SAFETY: `vec` is a live Vec<string>; missing optional captures use Go-style "".
    unsafe { push_string(vec, capture.map_or("", |m| m.as_str())) };
}

/// Find every non-overlapping match of the compiled regex in `text`.
///
/// Returns a newly allocated `Vec<string>` containing all matches. The caller
/// owns the vector. Returns an empty vector for no matches, or null on invalid
/// input.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_find_all(
    re: *const HewRegex,
    text: *const HewString,
) -> *mut HewVec {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    for m in regex.inner.find_iter(text_str) {
        // SAFETY: `out` is a live Vec<string>.
        unsafe { push_string(out, m.as_str()) };
    }
    out
}

/// Return one indexed capture from the first match.
///
/// Group 0 is the whole match. The returned `Vec<string>` has one element when
/// the requested capture is present, otherwise zero elements. This preserves
/// `Option<string>` semantics for empty-string captures.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_capture_index_one(
    re: *const HewRegex,
    text: *const HewString,
    group: i64,
) -> *mut HewVec {
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    if re.is_null() {
        return out;
    }
    let Ok(idx) = usize::try_from(group) else {
        return out;
    };
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    let Some(caps) = regex.inner.captures(text_str) else {
        return out;
    };
    if let Some(m) = caps.get(idx) {
        // SAFETY: `out` is a live Vec<string>.
        unsafe { push_string(out, m.as_str()) };
    }
    out
}

/// Return one named capture from the first match.
///
/// The returned `Vec<string>` has one element when the requested capture is
/// present, otherwise zero elements.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` and `name` must be live managed string handles (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_capture_name_one(
    re: *const HewRegex,
    text: *const HewString,
    name: *const HewString,
) -> *mut HewVec {
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    if re.is_null() {
        return out;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    // SAFETY: name is a live managed string handle (null means empty) per caller contract.
    let name_str = unsafe { string_as_str(name) };
    let Some(caps) = regex.inner.captures(text_str) else {
        return out;
    };
    if let Some(m) = caps.name(name_str) {
        // SAFETY: `out` is a live Vec<string>.
        unsafe { push_string(out, m.as_str()) };
    }
    out
}

/// Return first-match submatches in row-major flat form.
///
/// The returned vector contains group 0 followed by capture groups. Missing
/// optional groups are represented as an empty string. No match returns an empty
/// vector.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_captures_flat(
    re: *const HewRegex,
    text: *const HewString,
) -> *mut HewVec {
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    if re.is_null() {
        return out;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    let Some(caps) = regex.inner.captures(text_str) else {
        return out;
    };
    for idx in 0..caps.len() {
        // SAFETY: `out` is a live Vec<string>.
        unsafe { push_optional_capture(out, caps.get(idx)) };
    }
    out
}

/// Return all matches' submatches in row-major flat form.
///
/// Each row has [`hew_regex_capture_width`] entries: group 0 followed by capture
/// groups. Missing optional groups are represented as an empty string.
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_find_all_submatch_flat(
    re: *const HewRegex,
    text: *const HewString,
) -> *mut HewVec {
    // SAFETY: allocates a new Vec<string> owned by the caller.
    let out = unsafe { new_string_vec() };
    if re.is_null() {
        return out;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    for caps in regex.inner.captures_iter(text_str) {
        for idx in 0..caps.len() {
            // SAFETY: `out` is a live Vec<string>.
            unsafe { push_optional_capture(out, caps.get(idx)) };
        }
    }
    out
}

/// Return the number of submatch groups per match row, including group 0.
///
/// # Safety
///
/// `re` must be a valid pointer returned by [`hew_regex_new`].
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    reason = "regex capture count fits in i64 for Hew programs"
)]
pub unsafe extern "C" fn hew_regex_capture_width(re: *const HewRegex) -> i64 {
    if re.is_null() {
        return 0;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    regex.inner.captures_len() as i64
}

/// Replace all matches of the compiled regex in `text` with `replacement`.
///
/// Returns an owned managed string; release it with `string_release`.
/// An empty result or invalid regex handle returns canonical empty (null).
///
/// # Safety
///
/// - `re` must be a valid pointer returned by [`hew_regex_new`].
/// - `text` and `replacement` must be live managed string handles (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_replace(
    re: *const HewRegex,
    text: *const HewString,
    replacement: *const HewString,
) -> *mut HewString {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    // SAFETY: replacement is a live managed string handle (null means empty) per caller contract.
    let repl_str = unsafe { string_as_str(replacement) };
    string_from_str(&regex.inner.replace_all(text_str, repl_str))
}

// ── Compiler-facing regex helpers ──────────────────────────────────────────
// These helpers use the same managed text protocol as the source-facing API.

/// Compile a regex pattern and return an owned [`HewRegex`] handle.
///
/// Returns null for an invalid pattern. Release the handle with [`hew_regex_free`].
///
/// # Safety
///
/// `pattern` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_compile(pattern: *const HewString) -> *mut HewRegex {
    // SAFETY: caller guarantees pattern is a live managed string handle (null means empty).
    let pat = unsafe { string_as_str(pattern) };
    match regex::Regex::new(pat) {
        Ok(re) => Box::into_raw(Box::new(HewRegex { inner: re })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Test whether `text` matches a compiled regex, identified by handle.
///
/// Returns `1` if `text` matches, `0` otherwise. A null regex handle returns
/// `0`; a null text handle is the empty string.
///
/// # Safety
///
/// - `re` must be a valid, non-null pointer returned by [`hew_regex_compile`].
/// - `text` must be a live managed string handle (null means empty).
#[no_mangle]
pub unsafe extern "C" fn hew_regex_match(re: *const HewRegex, text: *const HewString) -> i32 {
    if re.is_null() {
        return 0;
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    i32::from(regex.inner.is_match(text_str))
}

/// Extract a capture group by its 1-based regex group index from a successful match.
///
/// `capture_idx` is the 1-based regex group position (group 0 is the whole
/// match; group 1 is the first capture group regardless of whether it is
/// named). The compiler resolves name→group-index at type-check time and
/// stores the real group position in the HIR, so this function receives the
/// actual group slot even when unnamed positional groups precede named ones.
///
/// Returns an owned managed capture string. An empty or non-participating
/// capture returns canonical empty (null). Use [`hew_regex_capture_index_one`]
/// when absence must be distinguished from a present empty capture. Release
/// this result with [`hew_regex_free_capture`].
///
/// # Safety
///
/// - `re` must be a valid, non-null pointer returned by [`hew_regex_compile`].
/// - `text` must be a live managed string handle (null means empty).
/// - `capture_idx` must be a valid 1-based group index into the pattern's capture groups.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_capture(
    re: *const HewRegex,
    text: *const HewString,
    capture_idx: i64,
) -> *mut HewString {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let regex = unsafe { &*re };
    // SAFETY: text is a live managed string handle (null means empty) per caller contract.
    let text_str = unsafe { string_as_str(text) };
    let Ok(idx) = usize::try_from(capture_idx) else {
        return std::ptr::null_mut();
    };
    // `capture_idx` is the 1-based regex group index (group 0 is the whole
    // match). We use `caps.get(idx)` directly — no +1 offset — because the
    // compiler now stores the real group position, not the named-only ordinal.
    // This correctly handles patterns where unnamed groups precede named ones:
    // e.g. `(foo)(?P<bar>bar)` → group 1 is `foo` (unnamed), group 2 is `bar`
    // (named); the compiler passes idx=2 for `bar` so `caps.get(2)` is correct.
    let Some(caps) = regex.inner.captures(text_str) else {
        return std::ptr::null_mut();
    };
    let capture_str = match caps.get(idx) {
        Some(m) => m.as_str(),
        None => return std::ptr::null_mut(),
    };
    string_from_str(capture_str)
}

/// Release an owned managed capture string returned by [`hew_regex_capture`].
///
/// # Safety
///
/// `ptr` must be null or one live managed owner that has not been released.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_free_capture(ptr: *mut HewString) {
    // SAFETY: the caller transfers one managed owner, or canonical empty.
    unsafe { string_release(ptr) };
}

/// Clone a compiled [`HewRegex`].
///
/// Returns a heap-allocated copy that is independent of the original.
/// Both handles must be freed with [`hew_regex_free`].
///
/// # Safety
///
/// `re` must be a valid pointer returned by [`hew_regex_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_regex_clone(re: *const HewRegex) -> *mut HewRegex {
    if re.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: re is a valid HewRegex pointer per caller contract.
    let src = unsafe { &*re };
    Box::into_raw(Box::new(HewRegex {
        inner: src.inner.clone(),
    }))
}

/// Free a compiled [`HewRegex`] previously returned by [`hew_regex_new`].
///
/// # Safety
///
/// `re` must be a pointer previously returned by [`hew_regex_new`], and must
/// not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_regex_free(re: *mut HewRegex) {
    if re.is_null() {
        return;
    }
    // SAFETY: re was allocated with Box::into_raw in hew_regex_new.
    drop(unsafe { Box::from_raw(re) });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::vec::{hew_vec_free, hew_vec_get_str, hew_vec_len};

    unsafe fn vec_to_strings(vec: *mut HewVec) -> Vec<String> {
        assert!(!vec.is_null());
        // SAFETY: vec is a valid HewVec returned by the regex FFI under test.
        let len = unsafe { hew_vec_len(vec) };
        let mut out = Vec::new();
        for i in 0..len {
            // SAFETY: i is in bounds for vec.
            let ptr = unsafe { hew_vec_get_str(vec, i) };
            // SAFETY: hew_vec_get_str returns a retained managed owner, including empty.
            let s = unsafe { string_as_str(ptr) }.to_owned();
            // SAFETY: release the owner returned by the getter after copying its text.
            unsafe { string_release(ptr.cast_mut()) };
            out.push(s);
        }
        out
    }

    #[test]
    fn managed_nul_captures_and_vec_owners_survive_parent_release() {
        use hew_cabi::vec::{hew_vec_contains_str, hew_vec_pop_str, hew_vec_set_str};

        let pattern = ManagedString::new("(?P<part>[ab]\0[0-9]+)(x)?");
        let text = ManagedString::new("a\u{0}1 b\u{0}22");
        let name = ManagedString::new("part");
        let wrong_name = ManagedString::new("part\0suffix");
        // SAFETY: pattern is a live managed string borrowed during compilation.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());
        drop(pattern);
        // SAFETY: all handles are live; Vec getters retain a caller-owned reference.
        unsafe {
            assert!(hew_regex_is_match(re, text.as_ptr()));
            assert_eq!(hew_regex_match(re, text.as_ptr()), 1);
            let named = hew_regex_capture_name_one(re, text.as_ptr(), name.as_ptr());
            assert_eq!(vec_to_strings(named), ["a\u{0}1"]);
            hew_vec_free(named);
            let missing = hew_regex_capture_name_one(re, text.as_ptr(), wrong_name.as_ptr());
            assert_eq!(hew_vec_len(missing), 0);
            hew_vec_free(missing);
            let first_row = hew_regex_captures_flat(re, text.as_ptr());
            assert_eq!(vec_to_strings(first_row), ["a\u{0}1", "a\u{0}1", ""]);
            hew_vec_free(first_row);
            let rows = hew_regex_find_all_submatch_flat(re, text.as_ptr());
            assert_eq!(
                vec_to_strings(rows),
                ["a\u{0}1", "a\u{0}1", "", "b\u{0}22", "b\u{0}22", ""]
            );
            hew_vec_free(rows);

            let values = hew_regex_find_all(re, text.as_ptr());
            assert_eq!(vec_to_strings(values), ["a\u{0}1", "b\u{0}22"]);
            let retained = hew_vec_get_str(values, 0);
            let replacement = ManagedString::new("changed\0雪");
            let prefix = ManagedString::new("changed");
            hew_vec_set_str(values, 0, replacement.as_ptr());
            assert_eq!(hew_vec_contains_str(values, replacement.as_ptr()), 1);
            assert_eq!(hew_vec_contains_str(values, prefix.as_ptr()), 0);
            drop(replacement);
            let changed = hew_vec_get_str(values, 0);
            let popped = hew_vec_pop_str(values);
            hew_vec_set_str(values, 0, std::ptr::null());
            assert_eq!(hew_vec_contains_str(values, std::ptr::null()), 1);
            let empty = hew_vec_pop_str(values);
            assert!(empty.is_null());
            assert_eq!(hew_vec_len(values), 0);
            hew_vec_free(values);
            hew_regex_free(re);
            drop(text);

            for (value, expected) in [
                (retained, "a\u{0}1"),
                (changed, "changed\0雪"),
                (popped, "b\u{0}22"),
                (empty, ""),
            ] {
                assert_eq!(string_as_str(value), expected);
                string_release(value.cast_mut());
            }
        }
    }

    #[test]
    fn managed_empty_pattern_text_and_captures_preserve_presence() {
        let pattern = ManagedString::new("(?P<empty>)(x)?");
        let name = ManagedString::new("empty");
        // SAFETY: pattern is a live borrowed managed string.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());
        // SAFETY: the regex is live; null text/replacement handles mean empty strings.
        unsafe {
            let indexed = hew_regex_capture_index_one(re, std::ptr::null(), 1);
            let named = hew_regex_capture_name_one(re, std::ptr::null(), name.as_ptr());
            let absent = hew_regex_capture_index_one(re, std::ptr::null(), 2);
            assert_eq!(vec_to_strings(indexed), [""]);
            assert_eq!(vec_to_strings(named), [""]);
            assert_eq!(hew_vec_len(absent), 0);
            hew_vec_free(indexed);
            hew_vec_free(named);
            hew_vec_free(absent);
            assert!(hew_regex_is_match(re, std::ptr::null()));
            let result = hew_regex_replace(re, std::ptr::null(), std::ptr::null());
            assert!(result.is_null());
            string_release(result);
            hew_regex_free(re);

            let empty_pattern = hew_regex_new(std::ptr::null());
            assert!(!empty_pattern.is_null());
            let matches = hew_regex_find_all(empty_pattern, std::ptr::null());
            assert_eq!(vec_to_strings(matches), [""]);
            hew_vec_free(matches);
            hew_regex_free(empty_pattern);
        }
    }

    #[test]
    fn test_regex_is_match() {
        let pattern = ManagedString::new(r"\d+");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text_yes = ManagedString::new("abc123def");
        let text_no = ManagedString::new("abcdef");
        // SAFETY: re and text pointers are valid.
        assert!(unsafe { hew_regex_is_match(re, text_yes.as_ptr()) });
        // SAFETY: re and text pointers are valid.
        assert!(!unsafe { hew_regex_is_match(re, text_no.as_ptr()) });

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_find() {
        let pattern = ManagedString::new(r"[a-z]+");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("123hello456");
        // SAFETY: re and text pointers are valid.
        let matched = unsafe { hew_regex_find(re, text.as_ptr()) };
        assert!(!matched.is_null());
        // SAFETY: matched was allocated by hew_regex_find.
        let matched_str = unsafe { string_as_str(matched) };
        assert_eq!(matched_str, "hello");
        // SAFETY: matched was returned as an owned managed string.
        unsafe { string_release(matched) };

        // Test no match.
        let text_no = ManagedString::new("123456");
        // SAFETY: re and text pointers are valid.
        let no_match = unsafe { hew_regex_find(re, text_no.as_ptr()) };
        assert!(no_match.is_null());
        // SAFETY: no_match was allocated by hew_regex_find.
        let no_match_str = unsafe { string_as_str(no_match) };
        assert_eq!(no_match_str, "");
        // SAFETY: no_match was returned as an owned managed string.
        unsafe { string_release(no_match) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn find_all_returns_all_non_overlapping_matches() {
        let pattern = ManagedString::new(r"[a-z]+");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("12ab34cd");
        // SAFETY: re and text pointers are valid.
        let matches = unsafe { hew_regex_find_all(re, text.as_ptr()) };
        // SAFETY: matches is a valid Vec<string> returned by hew_regex_find_all.
        assert_eq!(unsafe { vec_to_strings(matches) }, ["ab", "cd"]);
        // SAFETY: matches was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(matches) };

        let text_no = ManagedString::new("1234");
        // SAFETY: re and text pointers are valid.
        let empty = unsafe { hew_regex_find_all(re, text_no.as_ptr()) };
        // SAFETY: empty is a valid Vec<string> returned by hew_regex_find_all.
        assert_eq!(unsafe { hew_vec_len(empty) }, 0);
        // SAFETY: empty was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(empty) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn capture_index_one_preserves_empty_capture_and_absence() {
        let pattern = ManagedString::new(r"([a-z]*)([0-9]+)");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("123");
        // SAFETY: re and text pointers are valid; group 0 is the whole match.
        let whole = unsafe { hew_regex_capture_index_one(re, text.as_ptr(), 0) };
        // SAFETY: whole is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { vec_to_strings(whole) }, ["123"]);
        // SAFETY: whole was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(whole) };

        // SAFETY: re and text pointers are valid; group 1 captures an empty string.
        let empty_capture = unsafe { hew_regex_capture_index_one(re, text.as_ptr(), 1) };
        // SAFETY: empty_capture is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { vec_to_strings(empty_capture) }, [""]);
        // SAFETY: empty_capture was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(empty_capture) };

        // SAFETY: re and text pointers are valid; group 99 is absent.
        let missing = unsafe { hew_regex_capture_index_one(re, text.as_ptr(), 99) };
        // SAFETY: missing is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { hew_vec_len(missing) }, 0);
        // SAFETY: missing was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(missing) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn capture_name_one_returns_named_capture() {
        let pattern = ManagedString::new(r"(?P<word>[a-z]+)-(?P<num>[0-9]+)");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("abc-123");
        let name = ManagedString::new("num");
        // SAFETY: re, text, and name pointers are valid.
        let capture = unsafe { hew_regex_capture_name_one(re, text.as_ptr(), name.as_ptr()) };
        // SAFETY: capture is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { vec_to_strings(capture) }, ["123"]);
        // SAFETY: capture was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(capture) };

        let missing_name = ManagedString::new("missing");
        // SAFETY: re, text, and name pointers are valid.
        let missing =
            unsafe { hew_regex_capture_name_one(re, text.as_ptr(), missing_name.as_ptr()) };
        // SAFETY: missing is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { hew_vec_len(missing) }, 0);
        // SAFETY: missing was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(missing) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn captures_flat_returns_first_match_row() {
        let pattern = ManagedString::new(r"([a-z]+)-([0-9]+)?");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        // SAFETY: re is valid.
        assert_eq!(unsafe { hew_regex_capture_width(re) }, 3);

        let text = ManagedString::new("abc-");
        // SAFETY: re and text pointers are valid.
        let captures = unsafe { hew_regex_captures_flat(re, text.as_ptr()) };
        // SAFETY: captures is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { vec_to_strings(captures) }, ["abc-", "abc", ""]);
        // SAFETY: captures was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(captures) };

        let text_no = ManagedString::new("123");
        // SAFETY: re and text pointers are valid.
        let no_match = unsafe { hew_regex_captures_flat(re, text_no.as_ptr()) };
        // SAFETY: no_match is a valid Vec<string> returned by the regex FFI.
        assert_eq!(unsafe { hew_vec_len(no_match) }, 0);
        // SAFETY: no_match was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(no_match) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn find_all_submatch_flat_returns_row_major_groups() {
        let pattern = ManagedString::new(r"([a-z]+)([0-9]+)");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("a1 b22");
        // SAFETY: re and text pointers are valid.
        let captures = unsafe { hew_regex_find_all_submatch_flat(re, text.as_ptr()) };
        assert_eq!(
            // SAFETY: captures is a valid Vec<string> returned by the regex FFI.
            unsafe { vec_to_strings(captures) },
            ["a1", "a", "1", "b22", "b", "22"]
        );
        // SAFETY: captures was allocated by hew_vec_new_str.
        unsafe { hew_vec_free(captures) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_replace() {
        let pattern = ManagedString::new(r"\d+");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_new(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("a1b2c3");
        let repl = ManagedString::new("X");
        // SAFETY: re, text, and repl pointers are valid.
        let result = unsafe { hew_regex_replace(re, text.as_ptr(), repl.as_ptr()) };
        assert!(!result.is_null());
        // SAFETY: result was allocated by hew_regex_replace.
        let result_str = unsafe { string_as_str(result) };
        assert_eq!(result_str, "aXbXcX");
        // SAFETY: result was returned as an owned managed string.
        unsafe { string_release(result) };

        // SAFETY: re was returned by hew_regex_new.
        unsafe { hew_regex_free(re) };
    }

    #[test]
    fn test_regex_null_safety() {
        // SAFETY: a null text handle is a valid empty pattern.
        let empty = unsafe { hew_regex_new(std::ptr::null()) };
        assert!(!empty.is_null());
        // SAFETY: the compiled empty pattern is owned by this test.
        unsafe { hew_regex_free(empty) };
        assert!(!hew_regex_is_valid(std::ptr::null()));
        let text = ManagedString::new("text");
        // SAFETY: Testing a null handle with a valid input string.
        assert!(unsafe { hew_regex_find(std::ptr::null(), text.as_ptr()) }.is_null());
        assert!(
            // SAFETY: Testing null pointer handling.
            !unsafe { hew_regex_is_match(std::ptr::null(), std::ptr::null()) },
        );
        // SAFETY: Testing null pointer handling — should not crash.
        unsafe { hew_regex_free(std::ptr::null_mut()) };
    }

    #[test]
    fn codegen_compile_valid_pattern_returns_non_null() {
        let pattern = ManagedString::new(r"(?P<year>\d{4})-(?P<month>\d{2})");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(
            !re.is_null(),
            "hew_regex_compile must return non-null for a valid pattern"
        );
        // The test owns the compiled handle and releases it at scope exit.
        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }

    #[test]
    fn codegen_compile_invalid_pattern_returns_null() {
        let pattern = ManagedString::new(r"(?P<bad");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(
            re.is_null(),
            "hew_regex_compile must return null for an invalid pattern"
        );
    }

    #[test]
    fn codegen_match_returns_one_on_match_zero_on_no_match() {
        let pattern = ManagedString::new(r"\d+");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text_yes = ManagedString::new("abc123");
        let text_no = ManagedString::new("abcdef");
        // SAFETY: re and text pointers are valid.
        assert_eq!(unsafe { hew_regex_match(re, text_yes.as_ptr()) }, 1);
        // SAFETY: re and text pointers are valid.
        assert_eq!(unsafe { hew_regex_match(re, text_no.as_ptr()) }, 0);

        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }

    #[test]
    fn codegen_match_null_re_returns_zero() {
        let text = ManagedString::new("hello");
        assert_eq!(
            // SAFETY: Testing null pointer handling — null re must return 0.
            unsafe { hew_regex_match(std::ptr::null(), text.as_ptr()) },
            0
        );
    }

    #[test]
    fn codegen_capture_by_index_returns_matched_group() {
        let pattern = ManagedString::new(r"(?P<year>\d{4})-(?P<month>\d{2})");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("date: 2024-03 end");
        // capture_idx is the 1-based regex group index (group 0 is the whole match).
        // Pattern `(?P<year>...)` is group 1, `(?P<month>...)` is group 2.
        // The compiler resolves name→group-index at check time and passes the real
        // group index; passing 1 here matches the value MIR would emit for `year`.
        // SAFETY: re and text pointers are valid; capture_idx is in-bounds.
        let year_raw = unsafe { hew_regex_capture(re, text.as_ptr(), 1) };
        assert!(
            !year_raw.is_null(),
            "capture idx 1 (year, group 1) must match"
        );
        // SAFETY: year_raw was returned as an owned managed capture.
        let year_val = unsafe { string_as_str(year_raw) };
        assert_eq!(year_val, "2024");
        // SAFETY: year_raw was allocated by hew_regex_capture; free via the wrapper.
        unsafe { hew_regex_free_capture(year_raw) };

        // SAFETY: re and text pointers are valid; capture_idx is in-bounds.
        let month_raw = unsafe { hew_regex_capture(re, text.as_ptr(), 2) };
        assert!(
            !month_raw.is_null(),
            "capture idx 2 (month, group 2) must match"
        );
        // SAFETY: month_raw was returned as an owned managed capture.
        let month_val = unsafe { string_as_str(month_raw) };
        assert_eq!(month_val, "03");
        // SAFETY: month_raw was allocated by hew_regex_capture; free via the wrapper.
        unsafe { hew_regex_free_capture(month_raw) };

        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }

    /// A pattern with an unnamed positional group BEFORE a named group: verifies
    /// the real group index lookup is used (not the named-only ordinal).
    /// Pattern `(foo)(?P<bar>bar)` → group 1 = `foo` (unnamed), group 2 = `bar` (named).
    /// The compiler emits `capture_idx=2` for the `bar` binding.
    #[test]
    fn codegen_capture_unnamed_before_named_uses_real_group_index() {
        let pattern = ManagedString::new(r"(foo)(?P<bar>bar)");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("foobar");
        // Group 2 is the named capture `bar`. The old code used idx+1=1 which
        // returns `foo` (wrong). The corrected code passes idx=2 → `bar`.
        // SAFETY: re and text pointers are valid.
        // SAFETY: re and text pointers are valid; capture_idx 2 is in-bounds.
        let bar_raw = unsafe { hew_regex_capture(re, text.as_ptr(), 2) };
        assert!(
            !bar_raw.is_null(),
            "capture at real group index 2 (bar) must match"
        );
        // SAFETY: bar_raw is a live managed string handle allocated by hew_regex_capture.
        let bar_val = unsafe { string_as_str(bar_raw) };
        assert_eq!(bar_val, "bar", "group 2 must be 'bar', not 'foo'");
        // SAFETY: bar_raw was allocated by hew_regex_capture; free via the wrapper.
        unsafe { hew_regex_free_capture(bar_raw) };

        // Also verify group 1 (unnamed `foo`) is accessible by real index.
        // SAFETY: re and text pointers are valid; capture_idx 1 is in-bounds.
        let foo_raw = unsafe { hew_regex_capture(re, text.as_ptr(), 1) };
        assert!(
            !foo_raw.is_null(),
            "group 1 (unnamed foo) must be accessible"
        );
        // SAFETY: foo_raw is a live managed string handle allocated by hew_regex_capture.
        let foo_val = unsafe { string_as_str(foo_raw) };
        assert_eq!(foo_val, "foo");
        // SAFETY: foo_raw was allocated by hew_regex_capture; free via the wrapper.
        unsafe { hew_regex_free_capture(foo_raw) };

        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }

    #[test]
    fn codegen_capture_out_of_bounds_returns_null() {
        let pattern = ManagedString::new(r"(?P<word>\w+)");
        // SAFETY: pattern is a live managed string handle.
        let re = unsafe { hew_regex_compile(pattern.as_ptr()) };
        assert!(!re.is_null());

        let text = ManagedString::new("hello");
        // capture_idx=5 is beyond the single named group — must return null.
        // SAFETY: re and text pointers are valid.
        let oob_ptr = unsafe { hew_regex_capture(re, text.as_ptr(), 5) };
        assert!(
            oob_ptr.is_null(),
            "out-of-bounds capture index must return null"
        );

        // SAFETY: re is a valid non-null pointer returned by hew_regex_compile.
        let _ = unsafe { Box::from_raw(re) };
    }
}
